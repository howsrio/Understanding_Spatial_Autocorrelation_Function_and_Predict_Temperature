#(20230901)Calculate chordal distance

#01. Calculate geodesic distance
geodesic_matrix <- geodist(rem_point, measure = "geodesic")/1000 #거리 km단위로 변환

#02. Convert geodesic distance into chordal distance(Lecture01 p16, p17)
radius <- 6371 #지구 반지름의 평균값(단위: km)
theta <- geodesic_matrix/radius
chordal_matrix <- 2 * radius * sin(theta/2)

#(20230901)Make covariance function with using besselK(base)
covariance_func= function(data,a,b,nu){
  x=data
  c=besselK(x/b,nu)*a*((x/b)**nu) #Calculate covariance matrix
  
  #covariance matrix의 diagonal 원소들은 값이 NaN(besselK에서 x = 0이면 계산이 안됨)
  #거리가 0인 것을 0에 아주 가까운 값으로 대입
  x=1.0e-10 #∎Question∎ : 이렇게 0에 아주 가까운 값으로 대입해도 되는지?
  c_0=besselK(x/b,nu)*a*((x/b)**nu)
  diag(c) = c_0
  
  #완성된 covariance matrix를 return
  return(c)
}

#Calculate negative log likelihood
#01. Calculate deviation matrix
mean <- mean(rem_tem)
dev_matrix <- as.matrix(rem_tem - mean)


#02. Choose initial value of parameters 
sill = v.fit[2,2]
range = v.fit[2,3]
smoothness_parameter = 1/2
parameters=c(sill, range, smoothness_parameter)

#03. Make function that computes negative log-likelihood
NegLogL_optim=function(parameters){
  
  a=parameters[1]  #sill
  b=parameters[2]  #spatial range
  nu=parameters[3] #smoothness parameter
  x=chordal_matrix 
  
  #04. Calculate covaraince matrix
  #∎Question∎. 이거 위에서 만든 covariance function을 사용해도 되나?
  covariance_matrix=besselK(x/b,nu)*a*((x/b)**nu)
  
  x=1.0e-100
  c_0=besselK(x/b,nu)*a*((x/b)**nu)
  diag(covariance_matrix) = c_0
  
  #05. Calculate determinant and inverse_matrix
  det=as.double(determinant(covariance_matrix)$modulus)
  inverse_matrix=solve(covariance_matrix)
  
  #06. Calculate negative log-likelihood
  (det+t(dev_matrix)%*%inverse_matrix%*%(dev_matrix))/2
}

#07. Use optim function, find the parameter values that minimize the negative log-likelihood.
optim_MLE=optim(parameters,NegLogL_optim,method='L-BFGS-B')

#----------------------------------------------------------------------------------
#optim의 결과
MLE=optim_MLE$par       #c(0.9813297,272.9676780,0.1658069) 2022 summer의 결과
NLL=optim_MLE$value
CovMat=covariance_func(chordal_matrix,MLE[1],MLE[2],MLE[3])

#통합 좌표 메트릭스 여기서 부터 짜르기기
total_point=rbind(rem_point,sam_point)

#
total_chol_dist=chordal_dsit(total_point)
tatol_CovMat=covariance_func(total_chol_dist, MLE[1], MLE[2], MLE[3])

#sigma 나누기
sigma_11=Summer22_tatol_CovMat[1:nrow(rem_point),1:nrow(rem_point)]
sigma_12=Summer22_tatol_CovMat[1:nrow(rem_point),(nrow(rem_point)+1):nrow(total_point)]
sigma_21=Summer22_tatol_CovMat[(nrow(rem_point)+1):nrow(total_point),1:nrow(rem_point)]
sigma_22=Summer22_tatol_CovMat[(nrow(rem_point)+1):nrow(total_point),(nrow(rem_point)+1):nrow(total_point)]

# kriging
inverse_sigma_11=solve(sigma_11)
sam_pred_vec=sigma_21%*%inverse_sigma_11%*%dev_matrix+mean
sam_compare=cbind(sam_df,sam_pred_vec)
sam_MSE= mean((sam_compare$sam_pred_vec - sam_compare$Summer)^2)

#---------------------------------------------------------------------------------------
#임의의 좌표 값 찾기기
grd_korea= matrix(NA,nrow=length(a)*length(b),ncol = 2)
x=1
a=seq(124,132,by=0.1)
b=seq(33,39,by=0.1)
for (i in seq_along(a)){
  for (j in seq_along(b)){
    coord[x,1]<-a[i]
    coord[x,2]<-b[j]
    x=x+1
  }
}

grd_point=rbind(rem_point,grd_korea)
grd_chol_dist=chordal_dsit(grd_point)
grd_CovMat=covariance_func(grd_chol_dist, MLE[1], MLE[2], MLE[3])

#grd_sigma 나누기
grd_sigma_11=grd_CovMat[1:nrow(rem_point),1:nrow(rem_point)]
grd_sigma_12=grd_CovMat[1:nrow(rem_point),(nrow(rem_point)+1):nrow(grd_point)]
grd_sigma_21=grd_CovMat[(nrow(rem_point)+1):nrow(grd_point),1:nrow(rem_point)]
grd_sigma_22=grd_CovMat[(nrow(rem_point)+1):nrow(grd_point),(nrow(rem_point)+1):nrow(grd_point)]

# grd_kriging
inverse_grd_sigma_11=solve(grd_sigma_11)
grd_vec=grd_sigma_21%*%inverse_grd_sigma_11%*%dev_matrix+mean
grd_mat=cbind(coord,grd_vec)
grd_df=as.data.frame(grd_mat)

write.xlsx(grd_df, cat(sub,"MLE kriging grd df.xlsx"))


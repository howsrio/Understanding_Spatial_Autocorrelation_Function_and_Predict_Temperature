#02 Negative Log Likelihood 
#이 코드는 다음과 같은 역할을 수행한다
#01. 03 Main Code에서 구한 Matern 함수의 parameter들의 초깃값을 이용하여
#    최적의 parameter 값을 도출

#(20230901)Calculate chordal distance
#(20230904)Use chordal distance calculating function
chordal_matrix <- chordal_dist(rem_point)

#Calculate negative log likelihood
#01. Calculate deviation matrix
mean <- mean(rem_tem)
dev_matrix <- as.matrix(rem_tem - mean)

#02. Choose initial value of parameters.
#We have to complete fitting variogram to get proper initial values
sill = v.fit[2,2]
range = v.fit[2,3]
smoothness_parameter = 1/2
nugget=v.fit[1,2]
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
MLE=optim_MLE$par
NLL=optim_MLE$value

#Make new covariance matrix with using MLE
CovMat=covariance_func(chordal_matrix,MLE[1],MLE[2],MLE[3],nugget)
#나중에 확인하고 지우기 (안쓴다면)

#Now, we are ready to predict temperature of unknown points
#Add new points to be predicted (이거 문법 맞나?)
total_point=rbind(rem_point,sam_point)

#Make covariance matrix with expanded coordinates
#(확장된 좌표들의 covaraicne matrix를 만듭니다.)
total_chol_dist=chordal_dist(total_point)
total_CovMat=covariance_func(total_chol_dist, MLE[1], MLE[2], MLE[3],nugget)

#Divide covariance matrix into 4 submatrices
subMat_11=total_CovMat[1:nrow(rem_point),1:nrow(rem_point)]
subMat_12=total_CovMat[1:nrow(rem_point),(nrow(rem_point)+1):nrow(total_point)]
subMat_21=total_CovMat[(nrow(rem_point)+1):nrow(total_point),1:nrow(rem_point)]
subMat_22=total_CovMat[(nrow(rem_point)+1):nrow(total_point),(nrow(rem_point)+1):nrow(total_point)]

inverse_subMat_11=solve(subMat_11)

#kriging(based on Lecture 03)
sam_pred_vec=subMat_21%*%inverse_subMat_11%*%dev_matrix + mean

#After making kriging model, we have to check MSE to check 성능
MSE= mean((sam_pred_vec - sam_tem)^2)

#---------------------------------------------------------------------------------------
#한국 지도 전부 다 채우기 위해 매우 많은 점들을 생성
#임의의 좌표 값 찾기
grd_korea = Korea_points()

#방금 생성한 전국의 모든 포인트의 예측값을 구하는 과정

grd_point=rbind(rem_point,grd_korea)
grd_chol_dist=chordal_dist(grd_point)
grd_CovMat=covariance_func(grd_chol_dist, MLE[1], MLE[2], MLE[3], nugget)

#grd_subMat 나누기
grd_subMat_11=grd_CovMat[1:nrow(rem_point),1:nrow(rem_point)]
grd_subMat_12=grd_CovMat[1:nrow(rem_point),(nrow(rem_point)+1):nrow(grd_point)]
grd_subMat_21=grd_CovMat[(nrow(rem_point)+1):nrow(grd_point),1:nrow(rem_point)]
grd_subMat_22=grd_CovMat[(nrow(rem_point)+1):nrow(grd_point),(nrow(rem_point)+1):nrow(grd_point)]

#kriging
inverse_grd_subMat_11=solve(grd_subMat_11)

#save kriging model
grd_vec=grd_subMat_21%*%inverse_grd_subMat_11%*%dev_matrix+mean
grd_df=cbind(grd_korea,grd_vec)
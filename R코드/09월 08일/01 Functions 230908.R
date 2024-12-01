#01. Matching Temperature and Colors
#영하 섭씨 20도 ~ 영상 섭씨 30도의 색을 미리 정해두고,
#각 계절의 최저기온과 최고기온을 입력받아 그 계절에 사용할 색의 팔레트를 반환해주는 함수
data_to_col=function(tem_min,tem_max){
  #colorRampPalette(RColorBrewer)함수를 이용
  #아래 여섯 개의 색 변화에 맞춰서 200개의 색 그라데이션을 만듦
  my_colors <-colorRampPalette(c('darkblue','blue','white','yellow','red','black'))(200)
  
  #영하 섭씨 20도가 숫자 0, 영상 섭씨 30도가 숫자 200에 대응되는 일차함수를 만들고,
  #각 계절의 최저기온, 최고기온을 그 일차함수에 대입하여 함숫값을 반환
  #이때 최저(최고)기온이 영하 섭씨 20도 보다 낮으면 함숫값은 0으로 고정
  #최고(최저)기온이 영상 섭씨 30도 보다 높으면 함숫값은 200으로 고정
  num=tem_min
  if (num < -20){
    min=0
  }
  else if (num > 30){
    min=200
  }
  else{
    min=((num+20)*4)
  }
  min=round(min, digits = 0)
  
  num=tem_max
  if (num < -20){
    max=0
  } 
  else if (num > 30){
    max=200
  }
  else{
    max=((num+20)*4)
  }
  max=round(max, digits = 0)
  
  #온도 map을 그릴 때 사용하기 위한 색 팔레트 반환
  return (my_colors[min:max])
}

#02. Covariance function(We are using Matern Covariance function)
#거리 Matrix와, Matern 함수의 세가지 parameter(Lecture 03 참고) alpha, beta, nu를 입력 받아,
#covariance 값을 반환해주는 함수(Matern Covariance function 기반)
covariance_func= function(dist_matrix, a, b, nu, nug){
  
  #01. By using covariance function, calculate covariance matrix
  x = dist_matrix
  c = besselK(x/b,nu)*a*((x/b)**nu) +nug
  
  #02.
  #이때, besselK 함수에서 x = 0 이면 NaN 결과 값을 배출
  #x = 0은 거리가 0인 것을 의미하므로, 0에 아주 가까운 값을 대입하여 대신 계산
  x = 1.0e-10
  c_0 = besselK(x/b,nu)*a*((x/b)**nu) +nug
  #거리 Matrix의 대각원소의 값이 전부 0 이었으므로, 대각원소만 수정
  diag(c) = c_0

  return(c)
}

#03. Calculate log-likelihood
#거리 matrix, 편차 matrix, Matern function의 parameter를 입력 받고
#(편차 matrix는 온도 matrix에서 그것의 평균을 뺀 matrix)
#이를 통해 계산한 log-likelihood 값을 반환하는 함수
LogL=function(dist_matrix,dev_matrix,a,b,nu){
  
  #01. Use covariance function to make covariance matrix
  covariance_matrix = covariance_func(dist_matrix,a,b,nu)  
  
  #아래 다섯 줄의 코드를 위에서 만든 covariance_func함수로 대체
  #x=dist_matrix
  #covariance_matrix=besselK(x/b,nu)*a*((x/b)**nu)
  
  #x=1.0e-100
  #c_0=besselK(x/b,nu)*a*((x/b)**nu)
  #diag(covariance_matrix) = c_0
  
  #02. Calculate determinant and inverse_matrix
  det=as.double(determinant(covariance_matrix)$modulus)
  inverse_matrix=solve(covariance_matrix)
  
  #03. Calculate and return log-likelihood
  LogL=-(det+t(dev_matrix)%*%inverse_matrix%*%(dev_matrix))/2-length(dev_matrix)*log(2*pi)/2
  LogL=as.numeric(LogL)
  return(LogL)
}

#04. Calculating Chordal distance
chordal_dist=function(LongLat_matrix){
  
  #01. Calculate geodesic distance first
  geodesic_matrix <- geodist(LongLat_matrix, measure = "geodesic")/1000 #거리 km단위로 변환
  
  #02. Convert geodesic distance into chordal distance(Lecture01 p16, p17)
  radius <- 6371 #지구 반지름의 평균값(단위: km)
  theta <- geodesic_matrix/radius
  chordal_matrix <- 2 * radius * sin(theta/2)
  
  return(chordal_matrix)
}

#05. Make grid of Korea
#대한민국을 포함하는 큰 네모를 그리고 그 내부의 모든 point들을 다 생성
Korea_points = function(){
  x=1
  a=seq(124,132,by=0.1)
  b=seq(33,39,by=0.1)
  grid_korea = matrix(NA,nrow=length(a)*length(b),ncol = 2)
  for (i in seq_along(a)){
    for (j in seq_along(b)){
      grid_korea[x,1]<-a[i]
      grid_korea[x,2]<-b[j]
      x=x+1
    }
  }
  
  return(grid_korea)
}

NegLogL_optim=function(parameters){
  
  a=parameters[1]  #sill
  b=parameters[2]  #spatial range
  nu=parameters[3] #smoothness parameter
  x=chordal_matrix 
  
  covariance_matrix=besselK(x/b,nu)*a*((x/b)**nu)
  
  x=1.0e-100
  c_0=besselK(x/b,nu)*a*((x/b)**nu)
  diag(covariance_matrix) = c_0
  
  det=as.double(determinant(covariance_matrix)$modulus)
  inverse_matrix=solve(covariance_matrix)
  
  (det+t(dev_matrix)%*%inverse_matrix%*%(dev_matrix))/2
}

idw_test=function(train.sp, train.tem, test.sp, test.tem){
  k_values=seq(2,5, by=0.1)
  mse_results <- c()
  
  for (i in seq_along(k_values)){
    pred=gstat::idw(train.tem~1, train.sp, test.sp, idp = k_values[i])
    comparison = data.frame(Predict = pred$var1.pred, Actual = test.tem)
    mse = mean((comparison$Predict - comparison$Actual)^2)# MSE 계산
    mse_results[i] <- mse
  }
  
  min_mse<- min(mse_results,na.rm = TRUE)
  min_mse_index <- which(mse_results== min_mse, arr.ind = TRUE)
  best_k = k_values[min_mse_index]
  best_mse = mse_results[min_mse_index]
  return(c(best_k, best_mse))
}


MLE_test=function(train_data, test_data){
  rem_point=as.matrix(train_data[,c("Long","Lat")])
  sam_point=as.matrix(test_data[,c('Long',"Lat")])
  train_dev=train_data$tem - mean(train_data$tem)
  train.sp = SpatialPointsDataFrame(coords=as.matrix(train_data[,c('Long',"Lat")]),train_data,
                                    proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))
  v <- variogram((train_dev) ~ 1, locations = coordinates(rem_point), train.sp)
  psill=var(rem_dev)
  range= v$dist[length(v$dist)-1]
  nugget= min(v$gamma)
  smoothness_paramete= 1/2
  model='Mat' 
  v.fit <- fit.variogram(v, vgm(psill=psill, model=model, range=range, nugget=nugget, nu=smoothness_paramete),
                         fit.method=1)
  parameters=c(v.fit[2,2], v.fit[2,3],v.fit[2,4])
  nugget=v.fit[1,2]
  
  #parameters =c(sill, range, nu, nugget)
  optim_MLE=optim(parameters,NegLogL_optim,method='L-BFGS-B')
  #optim의 결과
  MLE=optim_MLE$par
  #Make new covariance matrix with using MLE
  chordal_matrix <- chordal_dist(rem_point)
  CovMat=covariance_func(chordal_matrix, MLE[1], MLE[2], MLE[3], nugget)
  
  #Now, we are ready to predict temperature of unknown points
  total_point=rbind(rem_point,sam_point)
  
  #Make covariance matrix with expanded coordinates
  #(확장된 좌표들의 covaraicne matrix를 만듭니다.)
  total_chordal_dist = chordal_dist(total_point)
  total_CovMat = covariance_func(total_chordal_dist, MLE[1],MLE[2],MLE[3], nugget)
  
  #Divide covariance matrix into 4 submatrices
  subMat_11=total_CovMat[1:nrow(rem_point),1:nrow(rem_point)]
  subMat_12=total_CovMat[1:nrow(rem_point),(nrow(rem_point)+1):nrow(total_point)]
  subMat_21=total_CovMat[(nrow(rem_point)+1):nrow(total_point),1:nrow(rem_point)]
  subMat_22=total_CovMat[(nrow(rem_point)+1):nrow(total_point),(nrow(rem_point)+1):nrow(total_point)]
  
  #Deviation matrix 
  mean <- mean(train_data$tem)
  dev_matrix <- as.matrix(train_data$tem - mean)
  
  #kriging(based on Lecture 03)
  inverse_subMat_11=solve(subMat_11)
  sam_pred_vec=subMat_21%*%inverse_subMat_11%*%dev_matrix + mean
  
  #After making kriging model, we have to check MSE to check 성능
  MSE= mean((sam_pred_vec - test_data$tem)^2)
  return(c(MLE[1],MLE[2],MLE[3],MSE))
}

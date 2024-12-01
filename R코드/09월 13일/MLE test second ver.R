NegLogL_optim2=function(log_paras){
  
  a=exp(log_paras[1])  #sill
  b=exp(log_paras[2])  #spatial range
  nu=exp(log_paras[3]) #smoothness parameter
  x=chordal_matrix 
  covariance_matrix = covariance_func(x,a,b,nu,nugget)
  
  det=as.double(determinant(covariance_matrix)$modulus)
  inverse_matrix=solve(covariance_matrix)
  
  value=(det+t(dev_matrix)%*%inverse_matrix%*%(dev_matrix))/2
  print(value)
}

MLE_test2=function(train_data, test_data){
  rem_point=as.matrix(train_data[,c("Long","Lat")])
  sam_point=as.matrix(test_data[,c('Long',"Lat")])
  train_dev=train_data$tem - mean(train_data$tem)
  train.sp=SpatialPointsDataFrame(coords=as.matrix(train_data[,c('Long',"Lat")]),train_data,
                                  proj4string=CRS("+proj=utm +zone=48 +datum=WGS84")) 
  chordal_matrix <<- theta_dist(rem_point)
  
  #Find Initial value
  v <- variogram((train_dev) ~ 1, locations = coordinates(rem_point), train.sp)
  psill=var(train_dev)
  range= v$dist[length(v$dist)]
  nugget= min(v$gamma)
  smoothness_parameter= 1/2
  model='Mat'
  v.fit <- fit.variogram(v, vgm(psill=psill, model=model, range=range, nugget=nugget, nu=smoothness_paramete),
                         fit.method=1)
  
  parameters=c(v.fit[2,2], v.fit[2,3],v.fit[2,4])
  nugget <<- v.fit[1,2]
  log_paras=log(abs(parameters)+0.001)
  
  #Deviation matrix 
  mean <- mean(train_data$tem)
  dev_matrix <<- as.matrix(train_data$tem - mean)
  
  optim_MLE=optim(log_paras,NegLogL_optim2, method="Nelder-Mead")
  MLE=exp(optim_MLE$par)
  
  #Now, we are ready to predict temperature of unknown points
  total_point=rbind(rem_point,sam_point)
  
  #Make covariance matrix with expanded coordinates
  #(확장된 좌표들의 covaraicne matrix를 만듭니다.)
  total_chordal_dist = chordal_dist(total_point)
  range=2*sin(MLE[2]/2)*6371
  total_CovMat = covariance_func(total_chordal_dist, MLE[1], range, MLE[3], nugget)
  
  #Divide covariance matrix into 4 submatrices
  subMat_11=total_CovMat[1:nrow(rem_point),1:nrow(rem_point)]
  subMat_12=total_CovMat[1:nrow(rem_point),(nrow(rem_point)+1):nrow(total_point)]
  subMat_21=total_CovMat[(nrow(rem_point)+1):nrow(total_point),1:nrow(rem_point)]
  subMat_22=total_CovMat[(nrow(rem_point)+1):nrow(total_point),(nrow(rem_point)+1):nrow(total_point)]
  
  
  #kriging(based on Lecture 03)
  inverse_subMat_11=solve(subMat_11)
  sam_pred_vec=subMat_21%*%inverse_subMat_11%*%dev_matrix + mean
  
  #After making kriging model, we have to check MSE to check 성능
  MSE= mean((sam_pred_vec - test_data$tem)^2)
  return(c(MLE[1], range, MLE[3], nugget, MSE))
}

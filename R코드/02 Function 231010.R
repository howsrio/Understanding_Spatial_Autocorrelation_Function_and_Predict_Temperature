#01. Matching Temperature and Colors
#영하 섭씨 20도 ~ 영상 섭씨 30도의 색을 미리 정해두고,
#각 계절의 최저기온과 최고기온을 입력받아 그 계절에 사용할 색의 팔레트를 반환해주는 함수
data_to_col=function(tem_min,tem_max){
  #colorRampPalette(RColorBrewer)함수를 이용
  #아래 여섯 개의 색 변화에 맞춰서 200개의 색 그라데이션을 만듦
  my_colors <-colorRampPalette(c('green','yellow','red','darkred'))(200)
  #영하 섭씨 13도가 숫자 0, 영상 섭씨 27도가 숫자 200에 대응되는 일차함수를 만들고,
  #각 계절의 최저기온, 최고기온을 그 일차함수에 대입하여 함숫값을 반환
  #이때 최저(최고)기온이 영하 섭씨 13도 보다 낮으면 함숫값은 0으로 고정
  #최고(최저)기온이 영상 섭씨 27도 보다 높으면 함숫값은 200으로 고정
  
  num=floor(tem_min)
  if (num < 7){
    min=0
  }else if (num > 27){
    min=200
  }else{
    min=((num-7)*10)
  }
  
  num=floor(tem_max)
  if (num < 7){
    max=0
  }else if (num > 27){
    max=200
  }else{
    max=((num-7)*10)
  }
  
  #온도 map을 그릴 때 사용하기 위한 색 팔레트 반환
  return (my_colors[min:max])
}
#02. Covariance function(We are using Matern Covariance function)
#거리 Matrix와, Matern 함수의 세가지 parameter(Lecture 03 참고) alpha, beta, nu를 입력 받아,
#covariance 값을 반환해주는 함수(Matern Covariance function 기반)
covariance_func= function(dist_matrix, sill, range, nu, nug){
  
  a = sill
  b = range
  
  #01. By using covariance function, calculate covariance matrix
  x = dist_matrix
  c = besselK(x/b,nu)*a*((x/b)**nu)
  #nugget을 왜 더하지?
  #besselK 함수를 뜯어봐서, x/b가 아니라 x만 넣어도 알아서 /b 를 해주는지 확인
  #아니면 besselK(x,b,nu)로 들어가는건지?
  
  #02.
  #이때, besselK 함수에서 x = 0 이면 NaN 결과 값을 배출
  #x = 0은 거리가 0인 것을 의미하므로, 0에 아주 가까운 값을 대입하여 대신 계산
  x = 1.0e-10    
  c_0 = besselK(x/b,nu)*a*((x/b)**nu) +nug
  #거리 Matrix의 대각원소의 값이 전부 0 이었으므로, 대각원소만 수정
  diag(c) = c_0
  c[is.nan(c)] =c_0

  return(c)
}

#03. Calculate log-likelihood
#거리 matrix, 편차 matrix, Matern function의 parameter를 입력 받고
#(편차 matrix는 온도 matrix에서 그것의 평균을 뺀 matrix)
#이를 통해 계산한 log-likelihood 값을 반환하는 함수
LogL=function(dist_matrix,dev_matrix,sill,range,nu,nug){
  
  a = sill
  b = range
  
  #01. Use covariance function to make covariance matrix
  covariance_matrix = covariance_func(dist_matrix,a,b,nu,nug)  
  
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

theta_dist=function(LongLat_matrix){
  
  #01. Calculate geodesic distance first
  geodesic_matrix <- geodist(LongLat_matrix, measure = "geodesic")/1000 #거리 km단위로 변환
  
  #02. Convert geodesic distance into chordal distance(Lecture01 p16, p17)
  radius <- 6371 #지구 반지름의 평균값(단위: km)
  theta <- geodesic_matrix/radius
  
  return(theta)
}

#05. Make grid of Korea
#대한민국을 포함하는 큰 네모를 그리고 그 내부의 모든 point들을 다 생성
Korea_points = function(){
  x=1
  a=seq(33,39,by=0.1)
  b=seq(124, 132,by=0.1)
  grid_korea = matrix(NA,nrow=length(a)*length(b),ncol = 2)
  for (i in seq_along(a)){
    for (j in seq_along(b)){
      grid_korea[x,2]<-a[i]
      grid_korea[x,1]<-b[j]
      x=x+1
    }
  }
  
  return(grid_korea)
}


idw_test=function(train_data, test_data){
  
  train.sp = SpatialPointsDataFrame(coords=as.matrix(train_data[,c('Long',"Lat")]),train_data,
                                    proj4string=CRS("+proj=utm +zone=48 +datum=WGS84")) 
  train.tem = train_data$tem
  test.sp = SpatialPointsDataFrame(coords=as.matrix(test_data[,c('Long',"Lat")]),test_data,
                                   proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))
  test.tem = test_data$tem
  
  k_values=seq(2,5, by=0.25)
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

MLE_test1=function(train_data, test_data){
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
  paras=c(psill, range, smoothness_parameter)
  
  #Deviation matrix 
  mean <- mean(train_data$tem)
  dev_matrix <<- as.matrix(train_data$tem - mean)
  
  optim_MLE=optim(log_paras, NegLogL_optim, method="L-BFGS-B")
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

#Calculate negative log likelihood for optim
NegLogL_optim2=function(log_paras){
  value= NA
  tryCatch({
  a=exp(log_paras[1])  #sill
  b=exp(log_paras[2])  #spatial range
  nu=exp(log_paras[3]) #smoothness parameter
  x=chordal_matrix 
  covariance_matrix = covariance_func(x,a,b,nu,nugget)
  
  det=as.double(determinant(covariance_matrix)$modulus)
  inverse_matrix=solve(covariance_matrix)
  
  value=(det+t(dev_matrix)%*%inverse_matrix%*%(dev_matrix))/2
  },error=function(e){
  })
  if (is.na(value)){
    cat('there is error in optim with',exp(log_paras),'\n')
    value=0
  }
  return(value)
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
  psill=max(v$gamma)
  range= v$dist[length(v$dist)]
  nugget <<- min(v$gamma)
  smoothness_parameter= 1/2
  parameters=c(psill, range, smoothness_parameter)
  log_paras=log(abs(parameters)+0.001)
  
  #Deviation matrix 
  mean <- mean(train_data$tem)
  dev_matrix <<- as.matrix(train_data$tem - mean)
  

  NLM=nlm(NegLogL_optim2,log_paras)
  MLE=exp(NLM$estimate)

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
  
  MSE =NA
  
  tryCatch({
  inverse_subMat_11=solve(subMat_11)
  sam_pred_vec=subMat_21%*%inverse_subMat_11%*%dev_matrix + mean
  #After making kriging model, we have to check MSE to check 성능
  MSE= mean((sam_pred_vec - test_data$tem)^2)
  },error=function(e){
  })
  #kriging(based on Lecture 03)
  return(c(MLE[1], range, MLE[3], nugget, MSE))
}

vario_test=function(train_data,test_data){
  
  rem_df=train_data
  sam_df=test_data
  
  rem_point <- matrix(NA, nrow=nrow(rem_df),ncol=2)
  rem_point[,1] <-rem_df$Long
  rem_point[,2] <-rem_df$Lat
  colnames(rem_point) <- c("Long","Lat")
  
  rem_coords<- as.data.frame(rem_point)
  coordinates(rem_coords) <- c("Long", "Lat")
  proj4string(rem_coords) <- CRS("+proj=longlat +datum=WGS84")
  rem_coords <- spTransform(rem_coords, CRS("+proj=utm +zone=48 +datum=WGS84"))
  
  rem_cprime<-coordinates(rem_coords)
  rem_c<-(rem_cprime)/1000
  rem_tem = rem_df$tem
  
  sam_point <- matrix(NA, nrow=nrow(sam_df),ncol=2)
  sam_point[,1] <-sam_df$Long
  sam_point[,2] <-sam_df$Lat
  colnames(sam_point) <- c("Long","Lat")
  
  sam_coords<- as.data.frame(sam_point)
  coordinates(sam_coords) <- c("Long", "Lat")
  proj4string(sam_coords) <- CRS("+proj=longlat +datum=WGS84")
  sam_coords <- spTransform(sam_coords, CRS("+proj=utm +zone=48 +datum=WGS84"))
  
  sam_cprime<-coordinates(sam_coords)
  sam_c<-(sam_cprime)/1000
  sam_tem = sam_df$tem
  mean=mean(rem_tem)
  
  max.dist <- max(chordal_dist(rem_point))
  bins <- 70 #대략 10km단위의 구간으로 binning
  vario.rem <- variog(coords=rem_c, data=(rem_tem-mean), uvec=(seq(0,max.dist,length=bins)))
  fit.rem <- variofit(vario.rem, ini.cov.pars=c(6,300), fix.nugget=TRUE, nugget=1.038454, fix.kappa=TRUE,kappa=1.5, cov.model="matern",minimisation.function="nls", weights="equal")
  sp.dat2 <- as.geodata(cbind(rem_c,rem_tem))
  krigpreds <- krige.conv(sp.dat2, coords=rem_c, data=rem_tem, location=sam_c, krige=krige.control(obj.model=fit.rem, cov.model="matern",cov.pars=c(fit.rem$cov.pars[1], fit.rem$cov.pars[2]),kappa=fit.rem$kappa,nugget=fit.rem$nugget))
  sam_predict<-cbind(sam_point,krigpreds$predict)
  colnames(sam_predict) <- c("Long", "Lat","Temp")
  mse = mean((sam_predict[,'Temp']-sam_tem)^2)
  return(c(fit.rem$cov.pars[1], fit.rem$cov.pars[2], fit.rem$nugget, mse))
}

tps_test=function(train_data,test_data){
  
  rem_point=as.matrix(train_data[,c("Long","Lat")])
  sam_point=as.matrix(test_data[,c('Long',"Lat")])
  rem_dev=train_data$tem - mean(train_data$tem)
  sam_dat<-data.frame(sam_point)
  rem_dat<-data.frame(rem_point,rem_dev)
  
  m_tprs2 <- gam(rem_dev ~ s(Long, Lat, k = 50, bs = "tp", m =2), data = rem_dat, method = "REML")
  predict_sam2 <- predict(m_tprs2, newdata = sam_dat, type = "response") + mean(train_data$tem)
  sam_predict<-cbind(sam_point,predict_sam2)
  colnames(sam_predict) <- c("Long", "Lat","Temp")
  mse=mean((sam_predict[,'Temp']-test_data$tem)^2)
  return(mse)
}



mat_test=function(train_data, test_data){
  
  train.sp = SpatialPointsDataFrame(coords=as.matrix(train_data[,c('Long',"Lat")]),train_data,
                                    proj4string=CRS("+proj=utm +zone=48 +datum=WGS84")) 
  test.sp = SpatialPointsDataFrame(coords=as.matrix(test_data[,c('Long',"Lat")]),test_data,
                                   proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))
  train_res=train_data$tem-mean(train_data$tem)
  v <- variogram((train_res) ~ 1, locations = as.matrix(train_data[,c("Long","Lat")]), train.sp, width = 0.1)
  psill=max(v$gamma)
  range= v$dist[length(v$dist)-1]
  nugget= min(v$gamma)
  min_mse=3
  tryCatch({
    for(j in seq(0.1 , 1, by= 0.1)){
      v.fit <- fit.variogram(v, vgm(psill=psill, model="Mat", range=range, nugget=nugget, kappa = j), fit.method=1)
      pred=krige(train_res~1, train.sp, test.sp, model=v.fit)
      comparison <- data.frame(Predict = pred$var1.pred+mean(train_data$tem), Actual = test_data$tem)
      mse <- mean((comparison$Predict - comparison$Actual)^2)
      if(!is.na(mse)){
        if(mse < min_mse){
          min_mse=mse
          psill = v.fit[2,2]
          range = v.fit[2,3]
          nu = j
          nugget=v.fit[1,2]
        }
      }
    }
    },error=function(e){
  })
  
  tryCatch({
    for(j in seq(nu-0.1 , nu+0.1, by= 0.01)){
      v.fit <- fit.variogram(v, vgm(psill=psill, model="Mat", range=range, nugget=nugget, kappa = j), fit.method=1)
      pred=krige(train_res~1, train.sp, test.sp, model=v.fit)
      comparison <- data.frame(Predict = pred$var1.pred+mean(train_data$tem), Actual = test_data$tem)
      mse <- mean((comparison$Predict - comparison$Actual)^2)
      
      if(!is.na(mse)){
        if(mse < min_mse){
          min_mse=mse
          psill = v.fit[2,2]
          range = v.fit[2,3]
          nu = j
          nugget=v.fit[1,2]
        }
      }
    }
  },error=function(e){
  })
  
  return(c(psill, range, nu, nugget, min_mse))
}

mat_test2=function(train_data, test_data){ #km 기준에서 찾는 함수수
  
  train.sp = SpatialPointsDataFrame(coords=as.matrix(train_data[,c('Long',"Lat")]),train_data,
                                    proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84")) 
  test.sp = SpatialPointsDataFrame(coords=as.matrix(test_data[,c('Long',"Lat")]),test_data,
                                   proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))
  train_res=train_data$tem-mean(train_data$tem)
  v <- variogram((train_res) ~ 1, locations = as.matrix(train_data[,c("Long","Lat")]), train.sp , width=10)
  psill=max(v$gamma)
  range= v$dist[length(v$dist)-1]
  nugget= min(v$gamma)
  min_mse=3
  tryCatch({
    for(j in seq(0.1 , 1, by= 0.1)){
      v.fit <- fit.variogram(v, vgm(psill=psill, model="Mat", range=range, nugget=nugget, kappa = j), fit.method=1)
      pred=krige(train_res~1, train.sp, test.sp, model=v.fit)
      comparison <- data.frame(Predict = pred$var1.pred+mean(train_data$tem), Actual = test_data$tem)
      mse <- mean((comparison$Predict - comparison$Actual)^2)
      if(!is.na(mse)){
        if(mse < min_mse){
          min_mse=mse
          psill = v.fit[2,2]
          range = v.fit[2,3]
          nu = j
          nugget=v.fit[1,2]
        }
      }
    }
    },error=function(e){
  })
  
  tryCatch({
    for(j in seq(nu-0.1 , nu+0.1, by= 0.05)){
      v.fit <- fit.variogram(v, vgm(psill=psill, model="Mat", range=range, nugget=nugget, kappa = j), fit.method=1)
      pred=krige(train_res~1, train.sp, test.sp, model=v.fit)
      comparison <- data.frame(Predict = pred$var1.pred+mean(train_data$tem), Actual = test_data$tem)
      mse <- mean((comparison$Predict - comparison$Actual)^2)
      
      if(!is.na(mse)){
        if(mse < min_mse){
          min_mse=mse
          psill = v.fit[2,2]
          range = v.fit[2,3]
          nu = j
          nugget=v.fit[1,2]
        }
      }
      
    }
  },error=function(e){
  })

  return(c(psill, range, nu, nugget, min_mse))
}




# 데이터 분할 및 K-fold 교차 검증 설정
set.seed(1111) # 시드를 설정하여 결과 재현성 확보
data <- df # 데이터를 불러오거나 생성된 데이터 사용
df1 =data[1:4]
df2=data[7]
df3=cbind(df1,df2)
data=df3
colnames(data) <- c("Index","Point","Lat","Long","tem")

folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설정



#---------------------------------------------------------------------------------------------------------

IDW_MSE_result=matrix(NA, nrow=10,ncol=2)
colnames(IDW_MSE_result) <- c("k","MSE")

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

# K-fold 교차 검증 수행 및 모델 학습 및 평가
for (i in 1:10) { # k 값에 따라 범위 조정
  train_data <- data[-folds[[i]], ]    # 현재 폴드를 제외한 나머지 데이터를 학습 데이터로 사용
  test_data <- data[folds[[i]], ]      # 현재 폴드를 검증 데이터로 사용
  
  train.sp = SpatialPointsDataFrame(coords=as.matrix(train_data[,c('Long',"Lat")]),train_data,
                                    proj4string=CRS("+proj=utm +zone=48 +datum=WGS84")) 
  train.tem = train_data$tem
  test.sp = SpatialPointsDataFrame(coords=as.matrix(test_data[,c('Long',"Lat")]),test_data,
                                    proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))
  test.tem = test_data$tem
  
  IDW_MSE_result[i,]=idw_test(train.sp, train.tem, test.sp, test.tem)
}
#결과 추출출
mean_list=c(mean(na.omit(IDW_MSE_result[,'k'])),mean(na.omit(IDW_MSE_result[,'MSE'])))
IDW_MSE_result2=rbind(IDW_MSE_result,mean_list)
idw_dataframe=as.data.frame(IDW_MSE_result2)
write.xlsx(idw_dataframe, file = paste(sub,"IDW cross validstion result.xlsx"))


#_________________________________________________________________________

#MLE K-fold

MLE_MSE_result=matrix(NA, nrow=10,ncol=4)
colnames(MLE_MSE_result) <- c("psill","range","nu","MSE")



for (i in 1:10) {                      
  train_data <- data[-folds[[i]], ]   
  test_data <- data[folds[[i]], ]      
  tryCatch({
    MLE_MSE_result[i,]=MLE_test(train_data, test_data)
  },error=function(e){
  })
  print(MLE_MSE_result)
}
mean_list=c(mean(na.omit(MLE_MSE_result[,'psill'])),mean(na.omit(MLE_MSE_result[,'range'])),
            mean(na.omit(MLE_MSE_result[,'nu'])), mean(na.omit(MLE_MSE_result[,'MSE'])))
MLE_datafrme=as.data.frame(MLE_MSE_result)
MLE_MSE_result2=rbind(MLE_MSE_result,mean_list)
write.xlsx(MLE_datafrme, file = paste(sub,"MLE cross validstion result.xlsx"))


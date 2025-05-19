df = read_excel("C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Without Island/Renewed Without Island/Temps_by_Obs_2022_Fall.xlsx")

sub='2022 Fall 섬제외'
# 데이터 분할 및 K-fold 교차 검증 설정
set.seed(1111) # 시드를 설정하여 결과 재현성 확보
data <- df # 데이터를 불러오거나 생성된 데이터 사용
df1 =data[1:4]
df2=data[7]
df3=cbind(df1,df2)
data=df3
colnames(data) <- c("Index","Point","Lat","Long","tem")

head(data)

folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설정

#---------------------------------------------------------------------------------------------------------

IDW_MSE_result=matrix(NA, nrow=10,ncol=2)
colnames(IDW_MSE_result) <- c("k","MSE")
source("02 Functions.R")

# K-fold 교차 검증 수행 및 모델 학습 및 평가
for (i in 1:10) { # k 값에 따라 범위 조정
  train_data <- data[-folds[[i]], ]    # 현재 폴드를 제외한 나머지 데이터를 학습 데이터로 사용
  test_data <- data[folds[[i]], ]      # 현재 폴드를 검증 데이터로 사용
  
  IDW_MSE_result[i,]=idw_test(train_data, test_data)
}
#결과 추출출
mean_list=c(mean(na.omit(IDW_MSE_result[,'k'])),mean(na.omit(IDW_MSE_result[,'MSE'])))
median_list = c(median(IDW_MSE_result[,'k']),median(IDW_MSE_result[,'MSE']))
IDW_MSE_result2=rbind(IDW_MSE_result,mean_list,median_list)
idw_dataframe=as.data.frame(IDW_MSE_result2)
idw_dataframe
write.xlsx(idw_dataframe, file = paste(sub,"IDW cross validstion result.xlsx"))


#-----------------------------------------------------------------------------
#MLE K-fold

MLE_MSE_result=matrix(NA, nrow=10,ncol=5)
colnames(MLE_MSE_result) <- c("psill","range","nu","nugget","MSE")

source("02 Functions.R")

for (i in 1:10) {                      
  train_data <- data[-folds[[i]], ]   
  test_data <- data[folds[[i]], ]      
  tryCatch({
    MLE_MSE_result[i,]=MLE_test2(train_data, test_data)
  },error=function(e){
  })
  print(MLE_MSE_result)
}

mat_clean <- MLE_MSE_result[!apply(is.nan(MLE_MSE_result), 1, any), ]
mean_list=colMeans(mat_clean)
median_list=c(median(mat_clean[,1]),median(mat_clean[,2]),median(mat_clean[,3]),median(mat_clean[,4]),median(mat_clean[,5]))
MLE_MSE_result2=rbind(MLE_MSE_result,mean_list,median_list)
MLE_datafrme=as.data.frame(MLE_MSE_result2, row.names = c(1:(nrow(MLE_MSE_result2)-2),'mean','median'))
MLE_datafrme
write.xlsx(MLE_datafrme, file = paste(sub,"MLE cross validstion result.xlsx"), overwrite = FALSE)

#----------------------------------------------------------
vario_MSE_result=matrix(NA, nrow=10, ncol=4)
colnames(vario_MSE_result) <- c("sill","range","nugget",'MSE')
source("02 Functions.R")

# K-fold 교차 검증 수행 및 모델 학습 및 평가
for (i in 1:10) { # k 값에 따라 범위 조정
  train_data <- data[-folds[[i]], ]    # 현재 폴드를 제외한 나머지 데이터를 학습 데이터로 사용
  test_data <- data[folds[[i]], ]      # 현재 폴드를 검증 데이터로 사용
  
  vario_MSE_result[i,]=vario_test(train_data,test_data)
}
#결과 추출출
mat_clean <- vario_MSE_result[!apply(is.nan(vario_MSE_result), 1, any), ]
mean_list=colMeans(mat_clean)
median_list=c(median(mat_clean[,1]),median(mat_clean[,2]),median(mat_clean[,3]),median(mat_clean[,4]))
vario_MSE_result=rbind(vario_MSE_result,mean_list,median_list)
vario_datafrme=as.data.frame(vario_MSE_result)
vario_datafrme
write.xlsx(vario_datafrme, file = paste(sub,"vario cross validstion result.xlsx"))

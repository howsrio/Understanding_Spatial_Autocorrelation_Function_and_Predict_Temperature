# 필요한 라이브러리 불러오기
library(readxl)
library(dplyr)
library(purrr)

# 엑셀 파일이 들어있는 폴더 경로 지정
folder_path <-"C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Data/Renewed Data"

# 폴더 내의 엑셀 파일 목록 얻기
Spring_files <- list.files(path = folder_path, pattern = "Spring\\.xlsx$", full.names = TRUE)
Summer_files <- list.files(path = folder_path, pattern = "Summer\\.xlsx$", full.names = TRUE)
Fall_files <- list.files(path = folder_path, pattern = "Fall\\.xlsx$", full.names = TRUE)
Winter_files <- list.files(path = folder_path, pattern = "Winter\\.xlsx$", full.names = TRUE)
folders=c(Spring_files, Summer_files, Fall_files, Winter_files)
Compare_model_result=matrix(NA,nrow=16,ncol=3)
Compare_model_var_result=matrix(NA,nrow=16,ncol=3)
models=c('Exp','Gau','Mat')
colnames(Compare_model_result)=models
count=0

for (folder in folders){
  for (file in folder){
    df= read_excel(path=file)
    data = df
    df1 =data[1:4]
    df2=data[6]
    df3=cbind(df1,df2)
    data=df3
    colnames(data) <- c("Index","Point","Lat","Long","tem")
    folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설정
    Matrix_k = matrix(NA,nrow=10,ncol=3)
    colnames(Matrix_k)=models
    
    for (k in 1:10) {                      
      train_data <- data[-folds[[k]], ]   
      test_data <- data[folds[[k]], ]   
      train.sp = SpatialPointsDataFrame(coords=as.matrix(train_data[,c('Long',"Lat")]),train_data,
                                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84")) 
      test.sp = SpatialPointsDataFrame(coords=as.matrix(test_data[,c('Long',"Lat")]),test_data,
                                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))
      train_res=train_data$tem-mean(train_data$tem)
      v <- variogram((train_res) ~ 1, locations = as.matrix(train_data[,c("Long","Lat")]), train.sp, width = 0.1)
      psill=var(train_res)
      range= v$dist[length(v$dist)-1]
      nugget= min(v$gamma)
      tryCatch({
        for(i in models){
          model = i 
          if (i == 'Mat') {
            for(j in seq(0.1 , 1.5, by= 0.02)){
              
              v.fit <- fit.variogram(v, vgm(psill=psill, model=model, range=range, nugget=nugget, kappa = j), fit.method=1)
              pred=krige(train_res~1, train.sp, test.sp, model=v.fit)
              comparison <- data.frame(Predict = pred$var1.pred+mean(train_data$tem), Actual = test_data$tem)
              mse <- mean((comparison$Predict - comparison$Actual)^2)
              
              if(!is.na(Matrix_k[k,i]) & mse > Matrix_k[k,i]){
              }else{
                Matrix_k[k,i]=mse
              }
            }
          }else{
            v.fit <- fit.variogram(v, vgm(psill=psill, model=model, range=range, nugget=nugget),fit.method=1)
            v.fit <- fit.variogram(v, vgm(psill=psill, model=model, range=range, nugget=nugget, kappa = j), fit.method=1)
            pred=krige(train_res~1, train.sp, test.sp, model=v.fit)
            comparison <- data.frame(Predict = pred$var1.pred+mean(train_data$tem), Actual = test_data$tem)
            mse <- mean((comparison$Predict - comparison$Actual)^2)
            Matrix_k[k,i]=mse
          }
        }
      },error=function(e){
      })
      
    }
    mean_list=colMeans(Matrix_k)
    var_list=apply(Matrix_k, 2, function(x) var(x, na.rm = TRUE))
    count=count+1
    Compare_model_result[count,]=mean_list
    Compare_model_var_result[count,]=var_list
    print(Compare_model_var_result)
  }
}

Compare_model_var_result
Compare_model_result

year=rep(c(2007,2012,2017,2022), 4)
season=rep(c('Spring',"Summer","Fall","Winter"), c(4,4,4,4))
a=cbind(year,season,Compare_model_var_result)
var_df=as.data.frame(a[1:16,])
write.xlsx(var_df,"Compare autocorelation fn variance.xlsx")

ResultOf4Year=a[-c(1,6,11,16),]
ResultOf4Year=as.data.frame(ResultOf4Year)
write.xlsx(ResultOf4Year,"Compare autocorelation fn.xlsx")

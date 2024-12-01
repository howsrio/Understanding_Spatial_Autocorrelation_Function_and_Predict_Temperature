# 데이터 분할 및 K-fold 교차 검증 설정
set.seed(1111) # 시드를 설정하여 결과 재현성 확보
df= read_excel("C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Data/Renewed Data/Temps_by_Obs_2002_Fall.xlsx")
sub='2022 Spring'
data = df
df1 =data[1:4]
df2=data[6]
df3=cbind(df1,df2)
data=df3

colnames(data) <- c("Index","Point","Lat","Long","tem")
head(data)
head(df)

folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설정



#---------------------------------------------------------------------------------------------------------
models=c('Exp','Gau','Mat')

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
        for(j in seq(0.2 , 1.5, by= 0.1)){
          
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
  print(Matrix_k)
}




mean_list=colMeans(Matrix_k)
median_list=c(median(Matrix_k[,1]),median(Matrix_k[,2]),median(Matrix_k[,3]))
Matrix_k=rbind(Matrix_k,mean_list,median_list)
datafrme=as.data.frame(Matrix_k)
datafrme
write.xlsx(datafrme, file = paste(sub,"compare models cross validstion result.xlsx"))

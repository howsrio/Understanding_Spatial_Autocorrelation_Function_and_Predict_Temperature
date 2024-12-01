df = read_excel("C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Data/Renewed Data/Temps_by_Obs_2017_Summer.xlsx")
head(df)
sub='2022 Fall'
set.seed(1111) # 시드를 설정하여 결과 재현성 확보
data <- df # 데이터를 불러오거나 생성된 데이터 사용
df1 =data[1:4]
df2=data[6]
df3=cbind(df1,df2)
data=df3
colnames(data) <- c("Index","Point","Lat","Long","tem")

head(data)
#--------
point <- matrix(NA, nrow=nrow(data),ncol=2)
point[,1] <-data$Long
point[,2] <-data$Lat
colnames(point) <- c("Long","Lat")

point.sp <- SpatialPointsDataFrame(coords=point, data,
                                 proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))  

pointkm.sp <- SpatialPointsDataFrame(coords=point, data,
                                  proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))
grd=Korea_points()
grd=rbind(c(127.044013,37.558378),grd)
grd.sp=SpatialPoints(coords=grd, proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))  


folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설정
source("02 Function 231010.R")

Mat_MSE_result2=matrix(NA, nrow=10, ncol=5)
colnames(Mat_MSE_result2) <- c("sill","range","nu","nugget",'MSE')
# K-fold 교차 검증 수행 및 모델 학습 및 평가
for (i in 1:10) { # k 값에 따라 범위 조정
  train_data <- data[-folds[[i]], ]    # 현재 폴드를 제외한 나머지 데이터를 학습 데이터로 사용
  test_data <- data[folds[[i]], ]      # 현재 폴드를 검증 데이터로 사용
  
  Mat_MSE_result2[i,]=mat_test2(train_data,test_data)
}

#결과 추출출
Mat_MSE_result2
v <- variogram((data$tem-mean(data$tem)) ~ 1, locations = coordinates(point), point.sp, width=10)
index=2
sill = Mat_MSE_result2[index,1]
nugget=Mat_MSE_result2[index,4]
range=Mat_MSE_result2[index,2]
kappa=Mat_MSE_result2[index,3]
v.fit <- fit.variogram(v, vgm(psill=sill, model="Mat", range=range, kappa=Mat_MSE_result2[index,3], nugget=nugget), fit.method=1)
plot(v, v.fit, main=paste(sub,' Variogram Fitting',sep=''), pch = 16, cex=.5, xlab ='distance(km)',
      par.settings = list(
       layout.heights = list(top.padding = 5,bottom.padding=2, main.key.padding = 1),
       layout.widths = list(left.padding = 2, right.padding = 2)))

krige_df <- krige(((data$tem-mean(data$tem)))~1, point.sp, grd.sp, model=v.fit)

Mat_pred_matrix=cbind(grd, krige_df$var1.pred+mean(data$tem))
Mat_pred_matrix[1,]

Mat_var_matrix=cbind(grd, krige_df$var1.var)
df_var=as.data.frame(Mat_var_matrix)
df_pred= as.data.frame(Mat_pred_matrix)
write.xlsx(df_var, file=paste(sub,'vario_var.xlsx'))
write.xlsx(df_pred, file=paste(sub, 'vario_pred.xlsx'))






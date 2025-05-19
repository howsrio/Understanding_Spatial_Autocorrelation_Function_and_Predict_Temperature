#03 - 1. IDW interpolation

#find value of k minimizing MSE
k_values=seq(2,5, by=0.01)
mse_results <- c()

for (i in seq_along(k_values)){
  pred=gstat::idw(rem_tem~1, remV.sp, samV.sp, idp = k_values[i])
  comparison = data.frame(Predict = pred$var1.pred, Actual = sam_tem)
  mse = mean((comparison$Predict - comparison$Actual)^2)# MSE 계산
  mse_results[i] <- mse
}

min_mse<- min(mse_results,na.rm = TRUE)
min_mse_index <- which(mse_results== min_mse, arr.ind = TRUE)
best_k= k_values[min_mse_index]

#Print value of minimum value of MSE and the corresponding value of k
cat('Min MSE =',min_mse,'k = ',best_k)

#대한민국을 포함하는 grid 만들기
grd_korea = Korea_points()

grd_korea_df=as.data.frame(grd_korea)

grd.sp <- SpatialPointsDataFrame(coords=grd_korea,grd_korea_df, proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  

#위에서 생성한 좌표들의 온도 예측값 구하기
idw.grd <- gstat::idw(rem_tem~1, rem.sp, grd.sp, idp=best_k)
grd_idw_df=cbind(grd_korea_df, idw.grd$var1.pred)
colnames(grd_idw_df) <- c("Long", "Lat", "predict")

#파일로 반환
#(20230911) 코드를 수정할 필요가 있음
#(20230912) 코드 수정 완
file_name <- paste0(sub," idw.xlsx")
write.xlsx(grd_idw_df, file_name)

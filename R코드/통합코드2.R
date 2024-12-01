sub = "2022 Fall"  #주제
df=read_excel("C:/Users/82109/OneDrive/공간통계 학술제/데이터/Processed Data/Processed Data/Temps_by_Obs_2022_Fall.xlsx") #자료
tem = df$Fall #df$변수명
col = data_to_col(min(tem),max(tem)) #함수 페이지 실행



set.seed(1111)
# 데이터프레임 df를 무작위로 10%로 분리합니다.
sam_df <- df %>% sample_frac(0.1)

# 10% 무작위 표본을 제외한 나머지 행들을 따로 저장합니다.
rem_df <- df %>% anti_join(sam_df)

#지도 생성성
data(wrld_simpl) 

rem_point <- matrix(NA, nrow=nrow(rem_df),ncol=2)
rem_point[,1] <-rem_df$Long
rem_point[,2] <-rem_df$Lat
colnames(rem_point) <- c("Long","Lat")
rem.sp <- SpatialPointsDataFrame(coords=rem_point,rem_df,
                                 proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  
remV.sp <- SpatialPointsDataFrame(coords=rem_point,rem_df,
                                  proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))

rem_tem = rem_df$Fall

sam_point <- matrix(NA, nrow=nrow(sam_df),ncol=2)
sam_point[,1] <-sam_df$Long
sam_point[,2] <-sam_df$Lat
colnames(sam_point) <- c("Long","Lat")
sam.sp <- SpatialPointsDataFrame(coords=sam_point,sam_df,
                                 proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  
samV.sp <- SpatialPointsDataFrame(coords=sam_point,sam_df,
                                  proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))


sam_tem = sam_df$Fall

grd <- Sobj_SpatialGrid(remV.sp, maxDim=400)$SG

#------------------------------------------------------------------------------------------------

v <- variogram((rem_tem) ~ 1, locations = coordinates(rem_point), remV.sp)

##Fit Variogram With Exp Model
psill= max(v$gamma)
range= v$dist[length(v$dist)-1]
nurget= min(v$gamma)
model='Exp' 

#variogram 학습 0.5까지( 약5%의 데이터는 제외하고 학습)
v.fit <- fit.variogram(v[3:ncol(v),], vgm(psill=psill, model=model, range=range, nurget=nurget))


plot(v, v.fit,main=paste(sub,' Fit Variogram by Exp Model',sep=''), pch = 16, cex=.5,
     sub=paste("Model =",model,", Sill =",round(psill, 4),', Range =',round(range, 4),', Nurget =',round(nurget, 4)),
     par.settings = list(
       layout.heights = list(top.padding = 5,bottom.padding=2, main.key.padding = 1),
       layout.widths = list(left.padding = 2, right.padding = 2)))
#MSE
pred=krige(rem_tem~1, remV.sp, samV.sp, model=v.fit)
comparison <- data.frame(Predict = pred$var1.pred, Actual = sam_tem)
mse <- mean((comparison$Predict - comparison$Actual)^2)
cat(sub,'MSE:',mse)

kr <- krige(rem_tem~1, remV.sp, grd, model=v.fit)

spplot(kr[1], col.regions=col, main=paste(sub," Kriging Interpolation",sep=''),
       sub=paste("Model = ",model,", Sill = ",round(psill, 4),', Range = ',round(range, 4),', Nurget = ',round(nurget, 4),
                 '\n','MSE : ',round(mse, 4),sep=''),
       scales = list(draw = TRUE),
       par.settings = list(
         layout.heights = list(top.padding = 2,bottom.padding=2, main.key.padding = 1),
         layout.widths = list(left.padding = 2, right.padding = 2)
       ))

#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------

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
cat('Min MSE =',min_mse,'k = ',best_k)
idw.out <- gstat::idw(rem_tem~1, remV.sp, grd, idp=best_k)
spplot(idw.out[1], col.regions=col, main=paste(sub," IDW Interpolation",sep=''), 
       sub=paste("k = ",best_k,', MSE : ',round(min_mse, 4),sep=''),
       scales = list(draw = TRUE),
       par.settings = list(
         layout.heights = list(top.padding = 2,bottom.padding=2, main.key.padding = 1),
         layout.widths = list(left.padding = 2, right.padding = 2)))

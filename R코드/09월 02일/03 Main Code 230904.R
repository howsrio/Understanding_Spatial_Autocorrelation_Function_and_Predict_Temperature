sub = "2022 Fall"  #주제

df = read_excel("C:/Users/82109/OneDrive/공간통계 학술제/데이터/Processed Data/Processed Data/Temps_by_Obs_2022_Fall.xlsx") #자료
tem = df$Fall #df$변수명
col = data_to_col(min(tem),max(tem)) #함수 페이지 실행

#데이터프레임 df에서 무작위로 10%의 데이터를 추출하여 sam_df에 저장.
set.seed(1111)
sam_df <- df %>% sample_frac(0.1)

#위에서 무작위로 추출한 데이터를 제외한 나머지 데이터를 rem_df에 저장.
rem_df <- df %>% anti_join(sam_df)

#rem_df에 대한 작업
#rem_df의 위도와 경도 값을 rem_point matrix에 저장.
rem_point <- matrix(NA, nrow=nrow(rem_df),ncol=2)
rem_point[,1] <-rem_df$Long
rem_point[,2] <-rem_df$Lat
colnames(rem_point) <- c("Long","Lat")
#projection 방법에 따라 SPDF를 두 가지로 저장
#∎Question∎ : 근데 우리가 이제 covariance matrix도 있고, variogram을 직접 만들어 볼 수는 없을까?
#              직접 만들 수 있으면 이런 과정도 필요 없을수도 있지 않나?
#              아 그래도 kriging map을 그리려면 필요한가?
rem.sp <- SpatialPointsDataFrame(coords=rem_point,rem_df,
                                 proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  
remV.sp <- SpatialPointsDataFrame(coords=rem_point,rem_df,
                                  proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))
#rem_df의 한 계절의 온도 값을 rem_tem에 저장
rem_tem = rem_df$Fall

#sam_df에도 마찬가지 작업
sam_point <- matrix(NA, nrow=nrow(sam_df),ncol=2)
sam_point[,1] <-sam_df$Long
sam_point[,2] <-sam_df$Lat
colnames(sam_point) <- c("Long","Lat")
sam.sp <- SpatialPointsDataFrame(coords=sam_point,sam_df,
                                 proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  
samV.sp <- SpatialPointsDataFrame(coords=sam_point,sam_df,
                                  proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))
sam_tem = sam_df$Fall

#-------------------------------------------------------------------
#여기까지 하고 02 negative log-likelihood 계산하는 페이지로 넘어가서 계산 후,
#negative log-likelihood가 최소가 되도록하는 ( = likelihood가 최대가 되는)
#parameter 값을 알아오기.
#-------------------------------------------------------------------

#∎Question∎ :'추후 Kriging map을 위한 그리드 생성'이 목적 맞음?
grd <- Sobj_SpatialGrid(remV.sp, maxDim=400)$SG

#------------------------------------------------------------------------------------------------
#우리는 평균과의 차이를 이용해서 variogram fitting 할 예정
#(20230902 수정) 잔차(residual)은 예측값과 실제값의 차이를 의미.
#                따라서 평균과의 차이인 편차(deviation)을 이용하여 변수명 변경
#                rem_res >>> rem_dev
rem_dev=rem_tem - mean(rem_tem)

#이 두 줄 필요 없지 않음?
#rem_df[,6] <-rem_dev #왜 하필 6번째에 넣는거지?
#colnames(rem_df)[6]='deviation'

v <- variogram((rem_dev) ~ 1, locations = coordinates(rem_point), remV.sp, width = 10)

##Fit Variogram With Exp Model
psill=var(rem_dev)
range= v$dist[length(v$dist)-1]
nugget= min(v$gamma)
model='Exp' 

#variogram 학습 0.5까지( 약5%의 데이터는 제외하고 학습)
v.fit <- fit.variogram(v, vgm(psill=psill, model=model, range=range, nugget=nugget),
                       fit.method=1)

cat(' sill = ',psill,'\n','range = ',range,'\n','nugget = ',1)
cat(' sill = ',v.fit[2,2],'\n','range = ',v.fit[2,3],'\n','nugget = ',v.fit[1,2])


plot(v, v.fit,main=paste(sub,' Fit Variogram by Exp Model',sep=''), pch = 16, cex=.5,
     sub=paste("Model =",model,", Sill =",round(v.fit[2,2], 4),', Range =',round(v.fit[2,3], 4),', Nugget =',round(v.fit[1,2],4)),
     par.settings = list(
       layout.heights = list(top.padding = 5,bottom.padding=2, main.key.padding = 1),
       layout.widths = list(left.padding = 2, right.padding = 2)))


#-------------------------------------------------------------------------------------------------
#MSE
pred=krige(rem_dev~1, remV.sp, samV.sp, model=v.fit)
comparison <- data.frame(sam_df,Predict = mean(rem_tem)+pred$var1.pred)
mse <- mean((comparison$Predict - comparison$Fall)^2)
cat(sub,'MSE:',mse)

kr <- krige(rem_dev~1, remV.sp, grd, model=v.fit)
kr$var1.pred=kr$var1.pred+mean(rem_tem)

spplot(kr[1], col.regions=col, main=paste(sub," Kriging Interpolation",sep=''),
     sp.layout=list('sp.lines',Korea),alpha.regions = 0.7,
     sub=paste("Model = ",model,', Sill = ',round(v.fit[2,2],4),', Range = ',round(v.fit[2,3],4),', Nugget = ',round(v.fit[1,2],4),
               '\n','MSE : ',round(mse, 4),sep=''),
     scales = list(draw = TRUE),
     par.settings = list(
       layout.heights = list(top.padding = 2,bottom.padding=2, main.key.padding = 1),
       layout.widths = list(left.padding = 2, right.padding = 2)
     ))


# MLE for Exponential covariance function
(v.fit)



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
idspplot(idw.out[1], col.regions=col, main=paste(sub," IDW Interpolation",sep=''), 
       sp.layout=list('sp.lines',Korea),alpha.regions = 0.7,
       sub=paste("k = ",best_k,', MSE : ',round(min_mse, 4),sep=''),
       scales = list(draw = TRUE),
       par.settings = list(
         layout.heights = list(top.padding = 2,bottom.padding=2, main.key.padding = 1),
         layout.widths = list(left.padding = 2, right.padding = 2)))




#05 Altitute Corrections
#이 코드는 다음과 같은 역할을 수행한다
#01. 해발고도 및 기온 측정 장비의 높이가 추가된 데이터를 불러오고, 약간의 데이터 가공
#02. 여러가지 방법으로 예측 모델을 만들기 위한 그 사전 작업

#Subject
sub = "2017 Summer"

df = read_excel("C:/Users/asdf1/Desktop/한양대학교/2023학술제/Main Project/가공된 데이터/고도 보정/summer17.xlsx") #자료
tem = df$temp_after_correction
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
#(20230907 추가) Answer: variogram fitting을 할 필요가 없음

#직접 만들 수 있으면 이런 과정도 필요 없을수도 있지 않나?
#아 그래도 kriging map을 그리려면 필요한가?
#(20230907 추가) 그러면 아에 필요 없을 듯

#∎Question∎ : 이 두가지 SpatialPointsDataFrame 더 필요함?
rem.sp <- SpatialPointsDataFrame(coords=rem_point,rem_df,
                                 proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  
remV.sp <- SpatialPointsDataFrame(coords=rem_point,rem_df,
                                  proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))
#rem_df의 한 계절의 온도 값을 rem_tem에 저장
rem_tem = rem_df$Summer

#sam_df에도 마찬가지 작업
sam_point <- matrix(NA, nrow=nrow(sam_df),ncol=2)
sam_point[,1] <-sam_df$Long
sam_point[,2] <-sam_df$Lat
colnames(sam_point) <- c("Long","Lat")

#∎Question∎ : 이 두가지 SpatialPointsDataFrame 더 필요함?
sam.sp <- SpatialPointsDataFrame(coords=sam_point,sam_df,
                                 proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  
samV.sp <- SpatialPointsDataFrame(coords=sam_point,sam_df,
                                  proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))
sam_tem = sam_df$Summer

#------------------------------------------------------------------------------------------------
#우리는 평균과의 차이를 이용해서 variogram fitting 할 예정
#(20230907 추가) 이 fitting 과정은 MLE 추정에서 사용될 parameter들의 초깃값을 알기 위한 작업임. 

#(20230902 수정) 잔차(residual)은 예측값과 실제값의 차이를 의미.
#                따라서 평균과의 차이인 편차(deviation)을 이용하여 변수명 변경
#                rem_res >>> rem_dev
rem_dev=rem_tem - mean(rem_tem)

v <- variogram((rem_dev) ~ 1, locations = coordinates(rem_point), remV.sp, width = 10)

##Fit Variogram With Exp Model
psill=var(rem_dev)
range= v$dist[length(v$dist)-1]
nugget= min(v$gamma)
model='Exp' 

#variogram 학습
v.fit <- fit.variogram(v, vgm(psill=psill, model=model, range=range, nugget=nugget),
                       fit.method=1)

cat(' sill = ',psill,'\n','range = ',range,'\n','nugget = ',1)
cat(' sill = ',v.fit[2,2],'\n','range = ',v.fit[2,3],'\n','nugget = ',v.fit[1,2])

plot(v, v.fit,main=paste(sub,' Fit Variogram by Exp Model',sep=''), pch = 16, cex=.5,
     sub=paste("Model =",model,", Sill =",round(v.fit[2,2], 4),', Range =',round(v.fit[2,3], 4),', Nugget =',round(v.fit[1,2],4)),
     par.settings = list(
       layout.heights = list(top.padding = 5,bottom.padding=2, main.key.padding = 1),
       layout.widths = list(left.padding = 2, right.padding = 2)))

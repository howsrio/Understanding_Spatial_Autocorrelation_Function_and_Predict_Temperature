install.packages("readxl")
library(readxl)
df=read_excel("C:/Users/82109/OneDrive/공간통계 학술제/데이터/관측소 위치 별 계절 온도 평균.xlsx")

names(df)
head(df)
dim(df)
data(wrld_simpl) #지도 생성성
plot(wrld_simpl)

sp_point <- matrix(NA, nrow=nrow(df),ncol=2)
sp_point[,1] <- jitter(df$경도,.001)
sp_point[,2] <- jitter(df$위도, .001)
colnames(sp_point) <- c("경도","위도")

data.sp <- SpatialPointsDataFrame(coords=sp_point,df,proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  

par(mar=rep(0,4))
plot(data.sp,pch=1,cex=(data.sp$겨울)/10)

#2022_관측소_위치
par(mar=c(2,2,0.5,0.5))
plot(wrld_simpl,xlim=bbox(data.sp)[1,]+c(-0.01,0.01),ylim=bbox(data.sp)[2,]+c(-0.1,0.1),col="lightgrey",axes=T) ## World Map
#생성 볌위를 지정
plot(data.sp,pch=16,cex=.5,col="red",add=T)

# Winter_Hist
par(mar=c(5,5,3,3))
hist(df$겨울, main = "Winter_Hist", xlab = "Avg_Temp", ylab = "Frequency",breaks=100,probability = TRUE)
#Winter_boxplot
par(mar=c(1,5,3,1))
boxplot(df$겨울, main="Winter_Boxplot",ylab='Avg_Temp',hotizontial=TRUE)

bubble(data.sp,"가을")

## Winter_Variogram_Cloud
plot(variogram(겨울~1, locations=coordinates(sp_point), data=data.sp, cloud=T),main='Winter_Variogram_Cloud', pch=16, cex=1)

## Winter_Sample_Variogram
plot(variogram(겨울~1, locations=coordinates(sp_point), data=data.sp, cloud=F),main='Winter_Sample_Variogram', type="b", pch=16)

## Winter_Directional_Search
plot(variogram(겨울~1, locations=coordinates(sp_point), data=data.sp, alpha=c(0,45,90,135),cloud=T), pch=16,main='Winter_Directional_Search')

##Winter_Modify_Cutoff
plot(variogram(겨울~1, locations=coordinates(sp_point), data=data.sp, cutoff=.5 ,cloud=F), type="b",main='Winter_Modify_Cutoff' , pch=16)

##Winter_Fit_Variogram_with_Gaussian_Model
v <- variogram((겨울) ~ 1, locations=coordinates(sp_point), data.sp)
v.fit <- fit.variogram(v, vgm(psill=10, model="Gau", range=12))
#In fit.variogram(v, vgm(psill = 1, model = "Exp", range = 1)) :
#No convergence after 200 iterations: try different initial values?
#Variogram 모델을 적합(fit)하는 과정에서 수렴(convergence)이 되지 않았음을 알려주는 메시지(?). 
#psill이 초기값, range가 범위라고함함
#겨울만   범위를 12으로 변경경
plot(v, v.fit,main='Winter_Fit_Variogram_with_Gaussian_Model', pch = 16, cex=.5)

#Winter_IDW_Interpolation
grd <- Sobj_SpatialGrid(data.sp, maxDim=200)$SG
plot(grd, axes=T, col="grey")
points(data.sp)

# colorRampPalette 함수를 사용하여 원하는 색상 맵을 생성
my_colors <- colorRampPalette(c("green", "blue", "purple"))

#거리 가중치
#Winter_IDW_Interpolation_'k=1'
idw.out <- gstat::idw(겨울~1, data.sp, grd, idp=1)
kr <- krige(겨울~1, data.sp, grd, model=v.fit)
#There were 50 or more warnings (use warnings() to see the first 50)
spplot(idw.out[1], col.regions=rev(my_colors(100)), main="Winter_IDW_Interpolation", sub="k =1")

#Winter_IDW_Interpolation_'k=5'
idw.out <- gstat::idw(겨울~1, data.sp, grd, idp=5)
kr <- krige(겨울~1, data.sp, grd, model=v.fit)
#There were 50 or more warnings (use warnings() to see the first 50)
spplot(idw.out[1], col.regions=rev(my_colors(100)), main="Winter_IDW_Interpolation", sub="k =5")

#Winter_IDW_Interpolation_'k=0.2'
idw.out <- gstat::idw(겨울~1, data.sp, grd, idp=0.2)
kr <- krige(겨울~1, data.sp, grd, model=v.fit)
#There were 50 or more warnings (use warnings() to see the first 50)
spplot(idw.out[1], col.regions=rev(my_colors(100)), main="Winter_IDW_Interpolation", sub="k =0.2")

sub="2022 Fall"  #주제
df=read_excel("C:/Users/howsr/OneDrive/공간통계 학술제/데이터/관측소 위치 별 계절 온도 평균.xlsx") #자료
tem=df$여름 #df$변수명
col= data_to_col(min(tem),max(tem)) #함수 페이지 실행행

#지도 생성성
data(wrld_simpl) 

sp_point <- matrix(NA, nrow=nrow(df),ncol=2)
sp_point[,1] <-df$경도
sp_point[,2] <-df$위도
colnames(sp_point) <- c("경도","위도")

data.sp <- SpatialPointsDataFrame(coords=sp_point,df,proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))  
grd <- Sobj_SpatialGrid(data.sp, maxDim=200)$SG

#variogram 학습
v <- variogram((tem) ~ 1, locations=coordinates(sp_point), data.sp)


plot(grd, axes=T, col="grey")

#_______________________________________________________________________________________________________________
# _Hist
par(mar=c(5,5,3,3))
hist(tem, main = paste(sub,"_Hist",sep=''), xlab = paste(sub,"_Avg_Temp",sep=''), ylab = "Frequency",breaks=100,probability = TRUE)

#_Boxplot
par(mar=c(1,5,3,1))
boxplot(tem, main=paste(sub,"_Boxplot",sep=''),ylab=paste(sub,"Avg_Temp",sep=''),hotizontial=TRUE)


## _Variogram_Cloud
plot(variogram(tem~1, locations=coordinates(sp_point), data=data.sp, cloud=T),main=paste(sub,'_Variogram_Cloud',sep=''), pch=16, cex=1)

## _Sample_Variogram
plot(variogram(tem~1, locations=coordinates(sp_point), data=data.sp, cloud=F),
     main=paste(sub,'_Sample_Variogram',sep=''), type="b", pch=16)

##_Sample_Directional_Search
plot(variogram(tem~1, locations=coordinates(sp_point), data=data.sp, alpha=c(0,45,90,135),cloud=F),
     pch=16,main=paste(sub,'_Sample_Directional_Search',sep=''))

##_Sample_Directional_Search
plot(variogram(tem~1, locations=coordinates(sp_point), data=data.sp, alpha=c(0,45,90,135),cloud=T),
     pch=16,main=paste(sub,'_Directional_Search',sep=''))

##_Modify_Cutoff
plot(variogram(tem~1, locations=coordinates(sp_point), data=data.sp, cutoff=.5 ,cloud=F),
     type="b",main=paste(sub,'_Modify_Cutoff',sep='') , pch=16)

##_Fit_Variogram_with_Gaussian_Model
psill= max(v$gamma)
range= v$dist[length(v$dist)-1]
nurget= min(v$gamma)
model='Exp' 
v.fit <- fit.variogram(v[3:ncol(v),], vgm(psill=psill, model=model, range=range,nurget=nurget))

plot(v, v.fit,main=paste(sub,'_Fit_Variogram_with_Exp_Model',sep=''), pch = 16, cex=.5)

kr <- krige(tem~1, data.sp, grd, model=v.fit)
spplot(kr[1], col.regions=col, main=paste(sub,"_Kriging_Interpolation",sep=''),
       sub=paste("Model =",model,", Sill =",psill,', Range =',range,', Nurget =',nurget)
       )
       
#_IDW_Interpolation_'k=i'

k=20 #k값
idw.out <- gstat::idw(tem~1, data.sp, grd, idp=k)
spplot(idw.out[1], col.regions=col, main=paste(sub,"_IDW_Interpolation",sep=''), sub=paste("k =",k))

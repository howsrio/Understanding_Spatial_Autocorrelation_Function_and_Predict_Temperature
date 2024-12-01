col.br <- colorRampPalette(c("blue","cyan","yellow","red"))

df = read_excel("C:/Users/USER/Desktop/파일 공유/Datas/Processed Data/Temps_by_Obs_2017_Spring.xlsx") #자료
tem = df$Spring #df$변수명
#col = data_to_col(min(tem),max(tem)) #함수 페이지 실행

# 데이터프레임 df를 무작위로 10%로 분리합니다.
set.seed(1111)
sam_df <- df %>% sample_frac(0.1)

# 10% 무작위 표본을 제외한 나머지 행들을 따로 저장합니다.
rem_df <- df %>% anti_join(sam_df)

#지도 생성성
#data(wrld_simpl) 

rem_point <- matrix(NA, nrow=nrow(rem_df),ncol=2)
rem_point[,1] <-rem_df$Long
rem_point[,2] <-rem_df$Lat
colnames(rem_point) <- c("Long","Lat")

rem_coords<- as.data.frame(rem_point)
coordinates(rem_coords) <- c("Long", "Lat")
proj4string(rem_coords) <- CRS("+proj=longlat +datum=WGS84")
rem_coords <- spTransform(rem_coords, CRS("+proj=utm +zone=48 +datum=WGS84"))

rem_cprime<-coordinates(rem_coords)
rem_c<-(rem_cprime)/1000
rem_tem = rem_df$Spring

sam_point <- matrix(NA, nrow=nrow(sam_df),ncol=2)
sam_point[,1] <-sam_df$Long
sam_point[,2] <-sam_df$Lat
colnames(sam_point) <- c("Long","Lat")

sam_coords<- as.data.frame(sam_point)
coordinates(sam_coords) <- c("Long", "Lat")
proj4string(sam_coords) <- CRS("+proj=longlat +datum=WGS84")
sam_coords <- spTransform(sam_coords, CRS("+proj=utm +zone=48 +datum=WGS84"))

sam_cprime<-coordinates(sam_coords)
sam_c<-(sam_cprime)/1000
sam_tem = sam_df$Spring

#----------------------------------------------

#ddd=iDist(rem_point)

max.dist <- max(chordal_dist(rem_point))
#max.dist <- max(iDist(rem_point))

bins <- 50

#vario.rem <- variog(coords=rem_point, data=rem_tem, uvec=(seq(0,max.dist,length=bins)))
vario.rem <- variog(coords=rem_c, data=rem_tem, uvec=(seq(0,max.dist,length=bins)))

fit.rem <- variofit(vario.rem, ini.cov.pars=c(12,750), cov.model="matern",minimisation.function="nls", weights="equal")

##lm.rem <- lm(S, data = df)

par(mar = c(4, 4, 2, 2))
plot(vario.rem,xlim=c(0,800),ylim=c(0,12))
lines(fit.rem)
abline(h=fit.rem$nugget, col="blue")
abline(h=fit.rem$cov.pars[1] + fit.rem$nugget, col="green")

#------------------------------------------

sp.dat2 <- as.geodata(cbind(rem_tem,rem_c))
loci <- expand.grid(seq(min(rem_c[,1]),max(rem_c[,1]),by=10), seq(min(rem_c[,2]),max(rem_c[,2]),by=10))

krigpreds <- krige.conv(sp.dat2, coords=rem_c, data=rem_tem, location=loci, krige=krige.control(cov.pars=c(fit.rem$cov.pars[1], fit.rem$cov.pars[2])))

surf1 <- mba.surf(cbind(loci,krigpreds$predict),no.X=300,no.Y=300,h=12,m=1,extend=FALSE)$xyz.est

image.plot(surf1,xaxs="r",yaxs="r",xlab=" ",ylab=" ",col=col.br(25),main="Kriging Predictor")


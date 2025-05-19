## Packages
library('ggplot2')
library('tibble')
library('tidyr')
library('dplyr')
library('mgcv')

library('remotes')
library('gratia')
library('patchwork')
remotes::install_github("clauswilke/colorblindr")
library('colorblindr')
remotes::install_github("clauswilke/relayer")
library('relayer')

#---------------------------------------------------------------------------------
df = read_excel("C:/Users/USER/Desktop/파일 공유/Datas/Renewed Data/Temps_by_Obs_2022_Summer.xlsx") 
#df = read_excel("C:/Users/USER/Desktop/파일 공유/Datas/Processed Data/Temps_by_Obs_2022_Summer.xlsx") #자료

tem = df$Fall #df$변수명
#col = data_to_col(min(tem),max(tem)) #함수 페이지 실행

# 데이터프레임 df를 무작위로 10%로 분리합니다.
set.seed(1111)
sam_df <- df %>% sample_frac(0.1)

# 10% 무작위 표본을 제외한 나머지 행들을 따로 저장합니다.
rem_df <- df %>% anti_join(sam_df)

rem_point <- matrix(NA, nrow=nrow(rem_df),ncol=2)
rem_point[,1] <-rem_df$Long
rem_point[,2] <-rem_df$Lat
colnames(rem_point) <- c("Long","Lat")

rem_tem = rem_df$Fall

sam_point <- matrix(NA, nrow=nrow(sam_df),ncol=2)
sam_point[,1] <-sam_df$Long
sam_point[,2] <-sam_df$Lat
colnames(sam_point) <- c("Long","Lat")

sam_tem = sam_df$Fall

rem_dat<-data.frame(rem_point,rem_tem)


#--------------------------------------------------------------------------------

loci <- expand.grid(seq(124,132,by=0.1), seq(33,39,by=0.1))
colnames(loci) <- c("Long","Lat")

sam_dat<-data.frame(sam_point)
#-------------------------------------------------

m_tprs2 <- gam(rem_tem ~ s(Long, Lat, k = 50, bs = "tp", m =2), data = rem_dat, method = "REML")
#m_tprs1 <- gam(rem_tem ~ s(Long, Lat, k = 50, bs = "tp", m =1), data = rem_dat, method = "REML")

predict_grid <- predict(m_tprs2, newdata = loci, type = "response")
predict_sam2 <- predict(m_tprs2, newdata = sam_dat, type = "response")
#predict_sam1 <- predict(m_tprs1, newdata = sam_dat, type = "response")

sam_predict<-cbind(sam_point,predict_sam2)
colnames(sam_predict) <- c("Long", "Lat","Temp")

grd<-data.frame(loci,predict_grid)
#surf1 <- mba.surf(grd,no.X=300,no.Y=300,h=12,m=1,extend=FALSE)$xyz.est

#image.plot(surf1,xaxs="r",yaxs="r",xlab=" ",ylab=" ",col=col,main="TPS")

write.xlsx(grd, file=paste(sub,'tps_pred.xlsx'))
#Korea = raster::getData("GADM", country = "South Korea", level = 0)
#plot(Korea,lwd=0.1,add=T)
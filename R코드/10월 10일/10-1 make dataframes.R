df = read_excel("C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Data/Renewed Data/Temps_by_Obs_2022_Fall.xlsx")
sub='2022 Fall'
set.seed(1111) # 시드를 설정하여 결과 재현성 확보
data <- df # 데이터를 불러오거나 생성된 데이터 사용
df1 =data[1:4]
df2=data[6]
df3=cbind(df1,df2)
data=df3
head(df)
head(data)
colnames(data) <- c("Index","Point","Lat","Long","tem")

point <- matrix(NA, nrow=nrow(data),ncol=2)
point[,1] <-data$Long
point[,2] <-data$Lat
point.sp <-SpatialPoints(coords=point, proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))
colnames(point) <- c("Long","Lat")
grd=Korea_points()
grd.sp=SpatialPoints(coords=grd, proj4string=CRS("+proj=longlat +zone=48 +datum=WGS84"))  

# vario grid dataframe
krige_df <- krige(((data$tem-mean(data$tem)))~1, point.sp, grd.sp, model=v.fit)
Mat_pred_matrix=cbind(grd, krige_df$var1.pred+mean(data$tem))
Mat_var_matrix=cbind(grd, krige_df$var1.var)
df_var=as.data.frame(Mat_var_matrix)
df_pred= as.data.frame(Mat_pred_matrix)
write.xlsx(df_var, file=paste(sub,'vario_var.xlsx'))
write.xlsx(df_pred, file=paste(sub, 'vario_pred.xlsx'))

#MLE grid data frame
v <- variogram((data$tem-mean(data$tem)) ~ 1, locations = coordinates(point), point.sp, width=10)
psill=max(v$gamma)
range= v$dist[length(v$dist)]
nugget <<- min(v$gamma)
smoothness_parameter= 1/2
parameters=c(psill, range, smoothness_parameter)
log_paras=log(abs(parameters)+0.001)

chordal_matrix <<- chordal_dist(point)
#Deviation matrix 
mean <- mean(data$tem)
dev_matrix <<- as.matrix(data$tem - mean)

#돌리기 위해서는 chordal_matrix와nugget을 먼저 지정해줘야 안에서 돌아감감
NLM=nlm(NegLogL_optim2,log_paras)
MLE=exp(NLM$estimate)

v.fit <- fit.variogram(v, vgm(psill=MLE[1], model="Mat", range=MLE[2], kappa=MLE[3], nugget=nugget), fit.method=1)
plot(v, v.fit, main=paste(sub,' Variogram Fitting',sep=''), pch = 16, cex=.5, xlab ='distance(km)',
     par.settings = list(
       layout.heights = list(top.padding = 5,bottom.padding=2, main.key.padding = 1),
       layout.widths = list(left.padding = 2, right.padding = 2)))

#Make covariance matrix with expanded coordinates
#(확장된 좌표들의 covaraicne matrix를 만듭니다.)

rem_point=point
total_point=rbind(rem_point,grd)
total_chordal_dist = chordal_dist(total_point)
#chordal_matrix = chordal_dist(point)
total_CovMat = covariance_func(total_chordal_dist, MLE[1], MLE[1], MLE[3], nugget)

#Divide covariance matrix into 4 submatrices
subMat_11=total_CovMat[1:nrow(rem_point),1:nrow(rem_point)]
subMat_12=total_CovMat[1:nrow(rem_point),(nrow(rem_point)+1):nrow(total_point)]
subMat_21=total_CovMat[(nrow(rem_point)+1):nrow(total_point),1:nrow(rem_point)]
subMat_22=total_CovMat[(nrow(rem_point)+1):nrow(total_point),(nrow(rem_point)+1):nrow(total_point)]

#kriging
inverse_subMat_11=solve(subMat_11)

#save kriging model
grd_vec=subMat_21%*%inverse_subMat_11%*%dev_matrix+mean
grd_df=as.data.frame(cbind(grd, grd_vec))
grd_var_mat = subMat_22 - subMat_21%*%inverse_subMat_11%*%subMat_12
grd_var =as.data.frame(cbind(grd, diag(grd_var_mat)))
write.xlsx(grd_var, file=paste(sub,'MLE_var.xlsx'))
write.xlsx(grd_df, file=paste(sub, 'MLE_pred.xlsx'))




# idw

idw.grd <- gstat::idw((data$tem-mean(data$tem))~1, point.sp, grd.sp, idp=3)
grd_vec=as.data.frame(cbind(grd, idw.grd$var1.pred + mean(data$tem)))

file_name <- paste0(sub," idw_pred.xlsx")
write.xlsx(grd_vec, file_name)

# tps
m_tprs2 <- gam((data$tem-mean(data$tem)) ~ s(point, k = 50, bs = "tp", m =2), data = as.data.frame(point, data$tem), method = "REML")
loci <- expand.grid(seq(124, 132, by = 0.1), seq(33, 39, by = 0.1))
colnames(loci) <- c("Long", "Lat")

# 모델을 사용하여 값 예측
predicted_values <- predict(m_tprs2, newdata = loci, type = "response")
grd_pred = as.data.frame(cbind(grd,predict+mean))
write.xlsx(grd_pred, file=paste(sub,'tps_pred.xlsx'))
?predict


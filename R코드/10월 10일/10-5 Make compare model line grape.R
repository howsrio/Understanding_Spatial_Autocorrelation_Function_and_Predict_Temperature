mean_data1 <- read_excel("결과데이터/중요 데이터/Compare models mean for normal data.xlsx")
varience_data1 <- read_excel("결과데이터/중요 데이터/Compare models varience for normal data.xlsx")
mean_data=mean_data1
varience_data=varience_data1
for(i in 0:3){
  for (j in 0:3){
    mean_data[4*i+j+1,]=mean_data1[4*j+i+1,]
    varience_data[4*i+j+1,]=varience_data1[4*j+i+1,]
  }
}
varience_data=varience_data**(0.5)
colnames(mean_data)=c('vario_mean', 'MLE_mean', 'idw_mean', 'tps_mean')
colnames(varience_data)=c('vario_sd', 'MLE_sd', 'idw_sd', 'tps_sd')
mean_data=as.data.frame(mean_data)
varience_data=as.data.frame(varience_data)
year=rep(c(2007,2012,2017,2022), c(4,4,4,4))
season=rep(c('Spring', 'Summer', 'Fall','Winter'), 4)
data_name=paste(year, season,sep = '_')
index=1:16
data=cbind(data_name,index, mean_data, varience_data)




ggplot(data= data, aes(x=index))+
  geom_line(aes(y=vario_mean), color = "darkred")+
  geom_point(aes(y=vario_mean,), color = "darkred", shape = 15, cex = 3)+
  geom_line(aes(y=vario_sd), color = "darkred")+
  geom_point(aes(y=vario_sd), color = "darkred", shape = 0, cex = 3)+
  geom_line(aes(y=MLE_mean), color = "black")+
  geom_point(aes(y=MLE_mean), color = "black", shape = 19, cex = 3)+
  geom_line(aes(y=MLE_sd), color = "black")+
  geom_point(aes(y=MLE_sd), color = "black", shape = 1, cex = 3)+
  geom_line(aes(y=idw_mean), color = "blue")+
  geom_point(aes(y=idw_mean), color = "blue", shape = 17, cex = 3)+
  geom_line(aes(y=idw_sd), color = "blue")+
  geom_point(aes(y=idw_sd), color = "blue", shape = 2, cex = 3)+
  geom_line(aes(y=tps_mean), color = "darkgreen")+
  geom_point(aes(y=tps_mean), color = "darkgreen", shape = 18, cex = 4)+
  geom_line(aes(y=tps_sd), color = "darkgreen")+
  geom_point(aes(y=tps_sd), color = "darkgreen", shape = 5, cex = 3)+
  scale_y_continuous(name = "SD                      AVE", limits=c(0,2.6)) +
  theme(panel.background = element_rect(fill="white",colour="black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())
  

ggplot(data= data, aes(x=index))+
  geom_point(aes(y=vario_mean, color="vario"), color = "darkred", shape = 15, cex = 3)+
  geom_point(aes(y=MLE_mean, color='MLE'), color = "black", shape = 19, cex = 3)+
  geom_point(aes(y=idw_mean, color='idw'), color = "blue", shape = 17, cex = 3)+
  geom_point(aes(y=tps_mean, color='tps'), color = "darkgreen", shape = 18, cex = 4)+
  scale_shape_manual(values = c("vario" = 15, "MLE" = 17))
    


theme(panel.background = element_rect(fill="white",colour="black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())

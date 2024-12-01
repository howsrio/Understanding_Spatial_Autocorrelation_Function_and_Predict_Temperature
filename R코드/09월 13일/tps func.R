tps_test=function(train_data,test_data){
  
  rem_point=as.matrix(train_data[,c("Long","Lat")])
  sam_point=as.matrix(test_data[,c('Long',"Lat")])
  rem_dev=train_data$tem - mean(train_data$tem)
  sam_dat<-data.frame(sam_point)
  rem_dat<-data.frame(rem_point,rem_dev)
  
  m_tprs2 <- gam(rem_dev ~ s(Long, Lat, k = 50, bs = "tp", m =2), data = rem_dat, method = "REML")
  predict_sam2 <- predict(m_tprs2, newdata = sam_dat, type = "response") + mean(train_data$tem)
  sam_predict<-cbind(sam_point,predict_sam2)
  colnames(sam_predict) <- c("Long", "Lat","Temp")
  mse=mean((sam_predict[,'Temp']-test_data$tem)^2)
  print(mse)
  return(mse)
}

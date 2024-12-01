folder_path <-"C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Data/Renewed Data/"
Spring_files <- list.files(path = folder_path, pattern = "Spring\\.xlsx$", full.names = TRUE)
Summer_files <- list.files(path = folder_path, pattern = "Summer\\.xlsx$", full.names = TRUE)
Fall_files <- list.files(path = folder_path, pattern = "Fall\\.xlsx$", full.names = TRUE)
Winter_files <- list.files(path = folder_path, pattern = "Winter\\.xlsx$", full.names = TRUE)
folder=c(Spring_files, Summer_files, Fall_files, Winter_files)

set.seed(1111)
Compare_models_result=matrix(NA,nrow=16,ncol=4)
models=c('kriging based on variogram','kriging based on MLE','IDW interpolation','Numeric interpolation')
colnames(Compare_models_result)=models
All_result=matrix(NA,nrow=192,ncol=4)
colnames(All_result)=models



for(file in folder[12:16]){
  count= which(file == folder) -1
  df= read_excel(path=file)
  data = df
  df1 =data[1:4]
  df2=data[7]
  df3=cbind(df1,df2)
  data=df3
  colnames(data) <- c("Index","Point","Lat","Long","tem")
  folds <- createFolds(data$tem, k = 10, list = TRUE)
  for (k in 1:10) {
    
    train_data <- data[-folds[[k]], ]   
    test_data <- data[folds[[k]], ] 
    
    All_result[(k+count*12),1]=mat_test(train_data,test_data)  #=vario_test(train_data,test_data)[4]
    All_result[(k+count*12),2]=MLE_test2(train_data,test_data)[5]
    All_result[(k+count*12),3]=idw_test(train_data,test_data)[2]
    All_result[(k+count*12),4]=tps_test(train_data,test_data)
    print(All_result[(1+count*12):(10+count*12),1:4])
    all.data.frame=as.data.frame(All_result)
    write.xlsx(all.data.frame, file="CorVer Compare models all result save12~.xlsx" ,overwite=TRUE)
  }
  All_result[count*12+11,]=colMeans(All_result[(1+count*12):(10+count*12),],na.rm = TRUE)
  Compare_models_result[count+1,] = All_result[count*12+11,]
  print(Compare_models_result)
  mean.data.frame=as.data.frame(Compare_models_result)
  write.xlsx(mean.data.frame, file="CorVer Compare models result save12~.xlsx",overwite=TRUE)
}

All_result
Compare_models_result

mean.df=as.data.frame(Compare_models_result)
all.df=as.data.frame(All_result)

write.xlsx(mean.df, file="Compare models result for normal data(1-6, 12-16).xlsx",overwite=FALSE)
write.xlsx(all.df, file="Compare models All result for normal data(1-6, 12-16).xlsx",overwite=FALSE)

Compare_models_variance=matrix(NA,nrow=16,ncol=4)
Compare_models_trimed_mean=matrix(NA,nrow=16,ncol=4)

for (count in 0:15){
  
  Compare_models_variance[count+1,] = 
    apply(All_result[(1+count*12):(10+count*12),], 2, function(x) var(x, na.rm = TRUE))
  Compare_models_trimed_mean[count+1,] = 
    apply(All_result[(1+count*12):(10+count*12),], 2, function(x) mean(x, proportion = 0.2, na.rm = TRUE))
}

var.df=as.data.frame(Compare_models_variance)
trimmean.df=as.data.frame(Compare_models_trimed_mean)

compare_model_summary =matrix(NA, nrow=3, ncol=4)
compare_model_summary[1,]= apply(Compare_models_result, 2, mean)
compare_model_summary[2,]= apply(Compare_models_variance, 2, mean)
compare_model_summary[3,]= apply(Compare_models_trimed_mean, 2, mean)

summary.df=as.data.frame(compare_model_summary)

write.xlsx(trimmean.df, file="Compare models trimmed mean for normal data.xlsx",overwite=FALSE)
write.xlsx(var.df, file="Compare models varience for normal data.xlsx",overwite=FALSE)
write.xlsx(summary.df, file="Compare models summary.xlsx",overwite=FALSE)



year=rep(c(2007,2012,2017,2022), 4)
season=rep(c('Spring',"Summer","Fall","Winter"), c(4,4,4,4))
Compare_models_result=cbind(year,season,Compare_models_result)

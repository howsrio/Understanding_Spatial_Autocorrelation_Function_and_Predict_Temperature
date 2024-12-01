All_result2= read_excel("Compare models All result for normal data.xlsx")
All_result=as.matrix(All_result2)


all.df=as.data.frame(All_result)


Compare_models_variance=matrix(NA,nrow=16,ncol=4)
Compare_models_mean=matrix(NA,nrow=16,ncol=4)
Compare_models_trimed_mean=matrix(NA,nrow=16,ncol=4)

for (count in 0:15){
  
  Compare_models_mean[count+1,] = 
    apply(All_result[(1+count*12):(10+count*12),], 2, function(x) mean(x, na.rm = TRUE))
  Compare_models_variance[count+1,] = 
    apply(All_result[(1+count*12):(10+count*12),], 2, function(x) var(x, na.rm = TRUE))
  Compare_models_trimed_mean[count+1,] = 
    apply(All_result[(1+count*12):(10+count*12),], 2, function(x) mean(x, trim = 0.2, na.rm = TRUE))
}

Compare_models_variance
Compare_models_mean
Compare_models_trimed_mean


mean.df=as.data.frame(Compare_models_mean)
var.df=as.data.frame(Compare_models_variance)
trimmean.df=as.data.frame(Compare_models_trimed_mean)

compare_model_summary =matrix(NA, nrow=3, ncol=4)
compare_model_summary[1,]= apply(Compare_models_mean, 2, mean)
compare_model_summary[2,]= apply(Compare_models_trimed_mean, 2, mean)
compare_model_summary[3,]= apply(Compare_models_variance, 2, mean)
compare_model_summary

summary.df=as.data.frame(compare_model_summary)

write.xlsx(trimmean.df, file="Compare models trimmed mean for normal data.xlsx",overwite=FALSE)
write.xlsx(var.df, file="Compare models varience for normal data.xlsx",overwite=FALSE)
write.xlsx(mean.df, file="Compare models mean for normal data.xlsx",overwite=FALSE)
write.xlsx(summary.df, file="Compare models summary.xlsx",overwite=FALSE)



year=rep(c(2007,2012,2017,2022), 4)
season=rep(c('Spring',"Summer","Fall","Winter"), c(4,4,4,4))
Compare_models_result=cbind(year,season,Compare_models_result)
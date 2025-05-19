folder_path1 <-"C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Data/Renewed Data"
folder_path2 <-"C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Without Island/Renewed Without Island"
set.seed(1111)
# 1은 기본, 2는 섬제외 데이터
Spring_files1 <- list.files(path = folder_path1, pattern = "Spring\\.xlsx$", full.names = TRUE)
Spring_files2 <- list.files(path = folder_path2, pattern = "Spring\\.xlsx$", full.names = TRUE)
Summer_files1 <- list.files(path = folder_path1, pattern = "Summer\\.xlsx$", full.names = TRUE)
Summer_files2 <- list.files(path = folder_path2, pattern = "Summer\\.xlsx$", full.names = TRUE)
Fall_files1 <- list.files(path = folder_path1, pattern = "Fall\\.xlsx$", full.names = TRUE)
Fall_files2 <- list.files(path = folder_path2, pattern = "Fall\\.xlsx$", full.names = TRUE)
Winter_files1 <- list.files(path = folder_path1, pattern = "Winter\\.xlsx$", full.names = TRUE)
Winter_files2 <- list.files(path = folder_path2, pattern = "Winter\\.xlsx$", full.names = TRUE)
folders1=c(Spring_files1, Summer_files1, Fall_files1, Winter_files1)
folders2=c(Spring_files2, Summer_files2, Fall_files2, Winter_files2)

Compare_data_result=matrix(NA,nrow=16,ncol=4)
datas=c('Raw','Altitude Correction','Without Island','Both')
colnames(Compare_data_result)=datas
year=rep(c(2007,2012,2017,2022), 4)
season=rep(c('Spring',"Summer","Fall","Winter"), c(4,4,4,4))
Compare_data_result=cbind(year,season,Compare_data_result)
All_result=matrix(NA,nrow=192,ncol=20)


count=0

for (folder in folders1){
  for (file in folder){
    #file=folders1[1][1]
    count=count+1
    df= read_excel(path=file)
    data = df
    df1 =data[1:4]
    df2=data[6]
    df3=cbind(df1,df2)
    data=df3
    colnames(data) <- c("Index","Point","Lat","Long","tem")
    MLE_MSE_result=matrix(NA, nrow=10,ncol=5)
    colnames(MLE_MSE_result) <- c("psill","range","nu","nugget","MSE")
    folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설

    index = 1
    for (k in 1:10) {   
      
      train_data <- data[-folds[[k]], ]   
      test_data <- data[folds[[k]], ]   
      tryCatch({
        MLE_MSE_result[k,]=MLE_test2(train_data, test_data)
        
      },error=function(e){
      })
    }
    mat_clean <- MLE_MSE_result[!apply(is.nan(MLE_MSE_result), 1, any), ]
    mean_list=colMeans(mat_clean)
    MLE_MSE_result=rbind(MLE_MSE_result,mean_list)
    Compare_data_result[count,index+2]=mean_list[5]
    print(Compare_data_result)
    for(i in 1:11){
      for(j in 1:5){
        All_result[count*12+i-12,index*5+j-5] = MLE_MSE_result[i,j]
      }
    }
    print(All_result)
    
    data = df
    df1 =data[1:4]
    df2=data[7]
    df3=cbind(df1,df2)
    data=df3
    colnames(data) <- c("Index","Point","Lat","Long","tem")
    MLE_MSE_result=matrix(NA, nrow=10,ncol=5)
    colnames(MLE_MSE_result) <- c("psill","range","nu","nugget","MSE")
    folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설
    
    index = 2
    for (k in 1:10) {                      
      train_data <- data[-folds[[k]], ]   
      test_data <- data[folds[[k]], ]   
      tryCatch({
        MLE_MSE_result[k,]=MLE_test2(train_data, test_data)
      },error=function(e){
      })
    }
    mat_clean <- MLE_MSE_result[!apply(is.nan(MLE_MSE_result), 1, any), ]
    mean_list=colMeans(mat_clean)
    MLE_MSE_result=rbind(MLE_MSE_result,mean_list)
    Compare_data_result[count,index+2]=mean_list[5]
    print(Compare_data_result)
    for(i in 1:11){
      for(j in 1:5){
        All_result[count*12+i-12,index*5+j-5] = MLE_MSE_result[i,j]
      }
    }
    print(All_result)
  }
}

#------------------------------
count=0

for (folder in folders2){
  for (file in folder){
    count=count+1
    df= read_excel(path=file)
    data = df
    df1 =data[1:4]
    df2=data[6]
    df3=cbind(df1,df2)
    data=df3
    colnames(data) <- c("Index","Point","Lat","Long","tem")
    MLE_MSE_result=matrix(NA, nrow=10,ncol=5)
    colnames(MLE_MSE_result) <- c("psill","range","nu","nugget","MSE")
    folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설

    index = 3
    for (k in 1:10) {                      
      train_data <- data[-folds[[k]], ]   
      test_data <- data[folds[[k]], ]   
      tryCatch({
        MLE_MSE_result[k,]=MLE_test2(train_data, test_data)
        
      },error=function(e){
      })
    }
    mat_clean <- MLE_MSE_result[!apply(is.nan(MLE_MSE_result), 1, any), ]
    mean_list=colMeans(mat_clean)
    MLE_MSE_result=rbind(MLE_MSE_result,mean_list)
    Compare_data_result[count,index+2]=mean_list[5]
    print(Compare_data_result)
    for(i in 1:11){
      for(j in 1:5){
        All_result[count*12+i-12,index*5+j-5] = MLE_MSE_result[i,j]
      }
    }
    print(All_result)
    
    data = df
    df1 =data[1:4]
    df2=data[7]
    df3=cbind(df1,df2)
    data=df3
    colnames(data) <- c("Index","Point","Lat","Long","tem")
    MLE_MSE_result=matrix(NA, nrow=10,ncol=5)
    colnames(MLE_MSE_result) <- c("psill","range","nu","nugget","MSE")
    folds <- createFolds(data$tem, k = 10, list = TRUE) # 10-fold 교차 검증을 수행하려면 k = 10으로 설
    
    index = 4
    for (k in 1:10) {                      
      train_data <- data[-folds[[k]], ]   
      test_data <- data[folds[[k]], ]   
      tryCatch({
        MLE_MSE_result[k,]=MLE_test2(train_data, test_data)
      },error=function(e){
      })
    }
    mat_clean <- MLE_MSE_result[!apply(is.nan(MLE_MSE_result), 1, any), ]
    mean_list=colMeans(mat_clean)
    MLE_MSE_result=rbind(MLE_MSE_result,mean_list)
    
    Compare_data_result[count,index+2]=mean_list[5]
    print(Compare_data_result)
    for(i in 1:11){
      for(j in 1:5){
        All_result[count*12+i-12,index*5+j-5] = MLE_MSE_result[i,j]
      }
    }
    print(All_result)
  }
}

MLE_df =read_excel("compare data cross validstion all result.xlsx")
MLE_compare_data_result=matrix(NA,nrow=16,ncol=4)
for(i in 1:16){
  for (k in 1:4){
    mat=MLE_df[(i*12-11):(i*12-2),5*k]
    mat <- mat[!apply(is.na(mat), 1, any), ]
    
    trim_mean=(sum(mat)-max(mat))/9
    if(trim_mean < (sum(mat)/10)) {
      MLE_compare_data_result[i,k] = trim_mean
    } else{
      MLE_compare_data_result[i,k] = (sum(mat)/10)
      
    }
    
  }
}
MLE_compare_data_result

MLE_compare_data_datafrme= as.data.frame(MLE_compare_data_result)
All_datafrme =as.data.frame(All_result)
write.xlsx(MLE_compare_data_datafrme, file = paste("compare data cross validstion result.xlsx"),overwrite = FALSE)
write.xlsx(All_datafrme, file = paste("compare data cross validstion all result.xlsx"),overwrite = FALSE)


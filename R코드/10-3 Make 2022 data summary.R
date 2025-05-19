folder_path <-"C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Data/Renewed Data/"
file_list <- list.files(path = folder_path,full.names = TRUE)
# "2022"를 포함하는 파일만 선택
file_list <- file_list[grep("2022", file_list)]
file_list[c(1,2,3)]=file_list[c(2,3,1)]
data_list <- lapply(file_list, read_excel)
tem_matrix=cbind(data_list[[1]]$Spring, data_list[[2]]$Summer,
                 data_list[[3]]$Fall, data_list[[4]]$Winter)
Spring=summary(tem_matrix[,1])
Summer=summary(tem_matrix[,2])
Fall=summary(tem_matrix[,3])
Winter=summary(tem_matrix[,4])
df=rbind(A1,A2,A3,A4)
sd=apply(tem_matrix, 2, sd)
df=as.data.frame(cbind(df,sd))
write.xlsx(df, file="2022 Summary.xlsx")

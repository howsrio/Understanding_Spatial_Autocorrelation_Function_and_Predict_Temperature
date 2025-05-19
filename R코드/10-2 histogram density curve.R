folder_path <-"C:/Users/82109/OneDrive/공간통계 학술제/데이터/Renewed Data/Renewed Data/"
file_list <- list.files(path = folder_path,full.names = TRUE)
# "2022"를 포함하는 파일만 선택
file_list <- file_list[grep("2022", file_list)]
file_list[c(1,2,3)]=file_list[c(2,3,1)]

# 선택된 파일들을 불러오기 (예: 데이터 프레임으로 불러옴)
data_list <- lapply(file_list, read_excel)
tem_matrix=cbind(data_list[[1]]$Spring, data_list[[2]]$Summer,
                 data_list[[3]]$Fall, data_list[[4]]$Winter)
tem_df=as.data.frame(tem_matrix)
colnames(tem_df)=c('2022_Spring','2022_Summer', '2022_Fall', '2022_Winter')

ggplot() +
  geom_density(data = tem_df, aes(x = tem_df[,1], color = "Spring"), size=1,color = "green")+
  geom_density(data = tem_df, aes(x = tem_df[,2], color = "Summer"), size=1, color = "red")+
  geom_density(data = tem_df, aes(x = tem_df[,3], color = "Fall"), size=1, color = "yellow")+
  geom_density(data = tem_df, aes(x = tem_df[,4], color = "Winter"), size=1, color = "blue")+
  labs(title = "2022 Temperature Distribution", x = "Temperature", y = "density")+
  theme(panel.background = element_rect(fill="white",colour="black"),legend.position = "inside")

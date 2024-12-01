compare_df <- read_excel("결과데이터/중요 데이터/compare data cross validstion result with Matern.xlsx")
compare_df=as.matrix(compare_df[,3:6])
Raw=summary(compare_df[,1])
Correction_Altitude=summary(compare_df[,2])
Without_island=summary(compare_df[,3])
Both=summary(compare_df[,4])
data=cbind(Raw, Correction_Altitude,Without_island,Both)
name=data.frame(name=c('Raw', 'Correction_Altitude','Without_island','Both'))
data=t(data)
index=1:4
df=cbind(name,index,data)
df=as.data.frame(df)

# 막대 그래프 생성
ggplot(df, aes(x=index)) +
  geom_bar(aes(y=Max.), stat="identity", position="dodge", fill="skyblue",color='black', alpha=0.1, width=0.5) +
  geom_bar(aes(y=Mean), stat="identity", position="dodge", fill="skyblue", alpha=0.1, width=0.5,color='black')+
  geom_bar(aes(y=Min.), stat="identity", position="dodge", fill="skyblue", alpha=0.1, width=0.5,color='black')+
  theme(panel.background = element_rect(fill="white",colour="black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())+
  scale_y_continuous(name = 'Min, Mean, Max')

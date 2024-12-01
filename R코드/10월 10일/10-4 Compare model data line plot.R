library(readxl)
library(ggplot2)

df = read_excel("C:/Users/asdf1/Desktop/HYU/2023학술제/최종 서류들/Compare data with MLE, Matern.xlsx")

ggplot(data = df, aes(x = index)) +
  geom_point(aes(y=Vario_raw), color = "darkred", shape = 15, cex = 2.5) +
  geom_line(aes(y=Vario_raw), color = "darkred") +
  geom_point(aes(y=Vario_altitute), color = "darkblue", shape = 16, cex = 2.5) +
  geom_line(aes(y=Vario_altitute), color = "darkblue") +
  geom_point(aes(y=Vario_island), color = "darkGreen", shape = 17, cex = 2.5) +
  geom_line(aes(y=Vario_island), color = "darkGreen") +
  geom_point(aes(y=Vario_both), color = "black", shape = 18, cex = 2.5) +
  geom_line(aes(y=Vario_both), color = "black") +
  scale_y_continuous(name = "MSE", limits=c(0,2.5)) +
  theme(panel.background = element_rect(fill="white",colour="black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +

vario_both= df[11]
vario_altitute = df[9]
vario_both = as.matrix(vario_both)
vario_altitute = as.matrix(vario_altitute)
mean(vario_both)
mean(vario_altitute)

MLE_both= df[7]
MLE_altitute = df[5]
MLE_both = as.matrix(MLE_both)
MLE_altitute = as.matrix(MLE_altitute)
mean(MLE_both)
mean(MLE_altitute)

#-----------------
#데이터 위도 오름차순(필수)
#-----------------
df_plot=function(df,m){
  M=as.matrix(df)
  data=as.geodata(M)
  #매트릭스 데이터 형태 변경
  locat.grid=expand.grid(seq(min(M[,1]),max(M[,1]),by=0.1),
                         seq(min(M[,2]),max(M[,2]),by=0.1))
  #grid 생성(경도, 위도, 0.1 단위로 잘라서)
  surf=mba.surf(cbind(locat.grid,M[,3]),no.X=300,no.Y=300,m=1,h=13,extend=F)$xyz.est
  #surf 형태로 데이터 변환, x,y에 300개의 칸으로 그리기, h: 대략 2^13개 만큼의 grid 공간
  #mba surf에서 xyz 좌표만 불러와 변수에 저장
  Korea = raster::getData("GADM", country = "South Korea", level = 0)
  #raster에서 우리나라 국경 map 불러오기
  col.br = data_to_col(min(M[,3]),max(M[,3]))
  #color palette 설정, blue,cyan,green,yellow,red
  image.plot(surf,col=col.br)
  #제목 설정하여 plot 그리기
  plot(Korea,add=T,lwd=0.1)
  contour(surf,add=T)
  #국경 추가
}


#-----------------------------------------------------------------------------------
idw_pred <- read_excel("2022 Fall idw_pred.xlsx")
MLE_pred <- read_excel("결과데이터/보고서용 데이터/2022 Fall MLE_pred.xlsx")
tps_pred <- read_excel("2022 Fall tps_pred.xlsx")
vario_pred <- read_excel("결과데이터/보고서용 데이터/2022 Fall vario_pred.xlsx")
max(tps_pred[,3])
min(idw_pred[,3])

par(mfrow=c(2,2))
df_plot(vario_pred,"vario")
df_plot(MLE_pred,"MLE")
df_plot(idw_pred,"idw")
df_plot(tps_pred,"tps")


vario_var <- read_excel("결과데이터/보고서용 데이터/2022 Fall vario_var.xlsx")
MLE_var <- read_excel("결과데이터/보고서용 데이터/2022 Fall MLE_var.xlsx")
df_plot(vario_var,"vari")
df_plot(MLE_var,"tps")


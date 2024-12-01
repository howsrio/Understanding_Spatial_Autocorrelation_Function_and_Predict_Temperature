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
  image.plot(surf,xlab="Long",ylab="Lat",col=col.br,main=m)
  #제목 설정하여 plot 그리기
  plot(Korea,add=T,lwd=0.1)
  contour(surf,add=T)
  #국경 추가
}


#-----------------------------------------------------------------------------------
method=list('IDW','IDW_cor','NLL')
for (m in method){
  df=read_excel(paste("C:/Users/bumju/Desktop/공간통계 학술제/processed_data/Prediction/2022 Summer",m ,"data.xlsx",sep=" "))
  df_plot(df,m)
}


library(readxl)       ## Load the Data
library(RColorBrewer) ## colorRampPalette
library(geodist)      ## Calculate geodesic distance
library(base)         ## besselK 
library(dplyr)        ## For Using pipe operator %>% and making random sample
library(sp)           ## For Making Spatial Points Data Frame(SPDF)
library(maptools)     ## For Making grid Data
#maptools가 없어질 예정임. 우리가 사용하는 Sobj_SpatialGrid가 sp로 넘어갈지는 모르겠음,
#대신 SpatialGrid 패키지의 GridTopology라는 함수를 사용 할 생각도 해야할 듯
library(gstat)        ## Sample Variogram, IDW interpolation
library(geoR)

library(fields)
library(latticeExtra)
library(rgeos)
library(lattice)
library(sf)

library(gridExtra)
library(raster)
library(rgdal)
library(tmap)
library(maps)         ## Projections


library(spdep)        ## Spatial autocorrelation

library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(pgirmess)     ## Spatial autocorrelation

library(classInt)     ## Class intervals
library(spgwr)        ## GWR
library(rgdal)
library(ggplot2)

#9월 11일
# 필요한 패키지 설치
install.packages("caret")

# 패키지 로드
library(caret)
# 패키지 설치
install.packages("openxlsx")

# 패키지 불러오기
library(openxlsx)


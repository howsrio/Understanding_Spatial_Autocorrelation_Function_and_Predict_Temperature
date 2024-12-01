library(stats4)
library(readxl)       ## Load the Data
library(openxlsx)     ## (20230911) Save 예측 데이터
library(RColorBrewer) ## colorRampPalette
library(geodist)      ## Calculate geodesic distance
library(base)         ## besselK 
library(dplyr)        ## For Using pipe operator %>% and making random sample
library(sp)           ## For Making Spatial Points Data Frame(SPDF)
library(maptools)     ## For Making grid Data
#maptools가 없어질 예정임. 우리가 사용하는 Sobj_SpatialGrid가 sp로 넘어갈지는 모르겠음,
#대신 SpatialGrid 패키지의 GridTopology라는 함수를 사용 할 생각도 해야할 듯
library(gstat)        ## Sample Variogram, IDW interpolation
library(caret)        ## (20230911) k-fold
library(geoR)         ## (20230912) For geoR kriging
library(MBA)          ## (20230912) For geoR kriging 그림 그리기
library(fields)       ## (20230912) For geoR kriging 그림 그리기

library(dplyr)
library(purrr)

#tps
library('ggplot2')
library('tibble')
library('tidyr')
library('dplyr')
library('mgcv')
#---------------------------------------------------------
library(raster)
library(rgdal)
#---------------------------------------------------------

library(latticeExtra)
library(rgeos)
library(lattice)
library(sf)

library(gridExtra)

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
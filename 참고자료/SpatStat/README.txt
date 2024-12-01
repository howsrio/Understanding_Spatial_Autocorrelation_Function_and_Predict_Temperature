
한국통계학과 2023년 겨울 학교

GIS를 활용한 공간 빅데이터 분석, 정재홍 (jaehongjeong@hanyang.ac.kr)


(수업 내용, 2023년 2월 24일)

1. Lecture 1. Overview of spatial data problems

2. Lecture 2. Handling spatial data in R
 - Lab 1. Spatial data and basic visualization

3. Lecture 3. Geostatistical data
 - Lab 2. Introduction to geostatistics 
 - Lab 3. Variogram and Kriging

4. Lecture 4. Areal data
 - Lab 4. Areal data modeling

5. Lecture 5. Point pattern data
 - Lab 5. Modeling and analysis for point pattern

6. Lecture 6. Spatio-temporal data (EDA example)


(설치해야 되는 R packages)

list.of.packages <- c("rgeos","geosphere","sf","sp","tidyverse","raster","rgdal","tmap",
                      "maps","maptools","spdep","gstat","splancs","spatstat","pgirmess",
                      "RColorBrewer","classInt","spgwr","spBayes","MBA","fields","geoR",
                      "spData","spatialreg","spatstat.explore")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

* 원활한 설치를 위하여 사전에 Rtools (https://cran.r-project.org/bin/windows/Rtools/)의 설치가 필요할 수 있습니다.


(추가로 공부하면 좋은 주제)

1. Bayesian spatial modeling
2. Nonstationary covariance models
3. Multivariate random fields
4. Spatio-temporal models
5. Computations for large spatial data
6. Non-Gaussian models
7. Disease mapping


(참고하면 좋은 교재)

1. 이론 및 방법론
 - Statistics for spatial data by Noel Cressie
 - Statistics for spatio-temporal data by Noel Cressie and Christopher Wikle
 - Hierarchical modeling and analysis for spatial data (2nd) by Sudipto Banerjee, Bradley Carlin, and Alan Gelfand
 - Interpolation of spatial data: some theory for Kriging by Michael Stein

2. 데이터시각화, 분석, 응용 with R
 - An introduction to R for spatial analysis and mapping by Chris Brunsdon and Lex Comber (한글 번역본 존재)
 - Geocomputation with R by Robin Lovelace, Jakub Nowosad, and Jannes Muenchow (한글 번역본 존재)
 - Spatio-temporal statistics with R by Christopher Wikle, Andrew Zammit-Mangion, and Noel Cressie



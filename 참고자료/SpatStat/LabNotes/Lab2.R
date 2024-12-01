#' ---
#' title: "Lab 2. Introduction to Geostatistics"
#' author: "Spatial Big Data Analysis with GIS (jaehongjeong@hanyang.ac.kr)"
#' date: "Korean Statistical Society, Winter School, February 24, 2023"
#' 
#' output:
#'   html_document:
#'     number_sections: yes
#'     toc: yes
#'   pdf_document:
#'     toc: yes
#'   fonttheme: professionalfonts
#' ---
#' This document is based on "Applied Spatial Statistics in R" by Yuri M. Zhukov and "Intro to GIS and Spatial Analysis" by Manuel Gimond (https://mgimond.github.io/Spatial/index.html).
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, out.width = "75%", fig.align = "center", error = FALSE, warning = FALSE)
## Load spatial packages
library(raster)
library(rgdal)
library(tmap)
library(maps)         ## Projections
library(maptools)     ## Data management
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(spgwr)        ## GWR

#' 
## ----eval = FALSE------------------------------------------------------------------------------------------------
## # Required packages
## library(raster)
## library(rgdal)
## library(tmap)
## library(maps)         ## Projections
## library(maptools)     ## Data management
## library(sp)           ## Data management
## library(spdep)        ## Spatial autocorrelation
## library(gstat)        ## Geostatistics
## library(splancs)      ## Kernel Density
## library(spatstat)     ## Geostatistics
## library(pgirmess)     ## Spatial autocorrelation
## library(RColorBrewer) ## Visualization
## library(classInt)     ## Class intervals
## library(spgwr)        ## GWR

#' 
#' # Geostatistics
#' 
#' We often want to determine where new data should be collected, identify which observations are spatial outliers, perform spatial prediction, interpolate missing data from nearby observed locations, and estimate local averages of spatially autocorrelated variables. These problems are the domain of a subfield called $\bf{\text geostatistics}$.
#' 
#' Let's look at some data on U.S. Air Strikes in Laos during the Vietnam War. The variable we are interested is ${\tt LOAD}\_{\tt LBS}$, the payload of each bomb dropped.
#' 
## ----echo = FALSE, eval = FALSE----------------------------------------------------------------------------------
## # Required packages
## library(raster)
## library(rgdal)
## library(tmap)
## library(maps)         ## Projections
## library(maptools)     ## Data management
## library(sp)           ## Data management
## library(spdep)        ## Spatial autocorrelation
## library(gstat)        ## Geostatistics
## library(splancs)      ## Kernel Density
## library(spatstat)     ## Geostatistics
## library(pgirmess)     ## Spatial autocorrelation
## library(RColorBrewer) ## Visualization
## library(classInt)     ## Class intervals
## library(spgwr)        ## GWR

#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
## Load spatial packages
#library(maps)         ## Projections
#library(maptools)     ## Data management
#library(sp)           ## Data management
#library(spdep)        ## Spatial autocorrelation
#library(gstat)        ## Geostatistics
#library(splancs)      ## Kernel Density
#library(spatstat)     ## Geostatistics
#library(pgirmess)     ## Spatial autocorrelation
#library(RColorBrewer) ## Visualization
#library(classInt)     ## Class intervals
#library(spgwr)        ## GWR

# Load data
load(file="../Dataset/Datasets.RData")
ls()

## Data on U.S. air strikes in Laos
data <- laos

## Take subsample of Laos bombing dataset
set.seed(20230224)
data <- data[sample(500,replace=F),]
names(data)
head(data)
dim(data)

## Open simple world map
data(wrld_simpl)
plot(wrld_simpl)

## Create matrix of coordinates 
sp_point <- matrix(NA, nrow=nrow(data),ncol=2)
sp_point[,1] <- jitter(data$LONG,.001)
sp_point[,2] <- jitter(data$LAT, .001)
colnames(sp_point) <- c("LONG","LAT")

## Create spatial object
## Projection: UTM zone 48
data.sp <- SpatialPointsDataFrame(coords=sp_point,data,proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))            

## Explore 
par(mar=rep(0,4))
plot(data.sp,pch=1,cex=log(data.sp$LOAD_LBS)/5)

## Zoom in on study region
par(mar=c(2,2,0.5,0.5))
plot(wrld_simpl,xlim=bbox(data.sp)[1,]+c(-1,1),ylim=bbox(data.sp)[2,]+c(-2,2),col="lightgrey",axes=T) ## World Map
plot(data.sp,pch=16,cex=.5,col="red",add=T)

## Take a look at the payload variable
bubble(data.sp,"LOAD_LBS")

#' 
#' # Variogram
#' 
#' In geostatistics, spatial autocorrelation has traditionally been modeled by a $\bf{\text variogram}$, which describes the degree to which nearby locations have similar values.
#' 
#' A variogram cloud is a scatterplot of data pairs, in which the semivariance is plotted against interpoint distance. The semivariance is formally defined as the squared difference in height between locations.
#' 
#' Below is the variogram cloud for Laos bomb load (natural log).
#' 
#' * ${\bf Upper\ left\ corner}$: point pairs are close together, but have very different values.
#' * ${\bf Lower\ left\ corner}$: point pairs are close together, and have similar values.
#' * ${\bf Upper\ right\ corner}$: point pairs are far apart, and have different values.
#' * ${\bf Lower\ right\ corner}$: point pairs are far apart, but have similar values.
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
## Variogram cloud
plot(variogram(log(LOAD_LBS)~1, locations=coordinates(sp_point), data=data.sp, cloud=T), pch=16, cex=1)

#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
## Sample variogram
plot(variogram(log(LOAD_LBS)~1, locations=coordinates(sp_point), data=data.sp, cloud=F), type="b", pch=16)

## Directional search
plot(variogram(log(LOAD_LBS)~1, locations=coordinates(sp_point), data=data.sp, alpha=c(0,45,90,135),cloud=T), pch=16)

## Modify cutoff
plot(variogram(log(LOAD_LBS)~1, locations=coordinates(sp_point), data=data.sp, cutoff=.5 ,cloud=F), type="b",  pch=16)

#' 
#' The variogram can be used for spatial prediction. This can be done by fitting a parametric model to the variogram. 
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
## Fit variogram with exponential model
v <- variogram(log(LOAD_LBS) ~ 1, locations=coordinates(sp_point), data.sp)
v.fit <- fit.variogram(v, vgm(psill=1, model="Exp", range=1))
plot(v, v.fit, pch = 16, cex=.5)


#' 
#' In the Laos example, an exponential model was used. The shape of the curve indicates that at small separation distances, the variance in $z$ is small. After a certain level of separation (.5 degrees), the variance in $z$ values becomes somewhat random and the model flattens out to a value corresponding to the average variance.
#' 
#' # Ordinary Kriging
#' 
#' Spatial interpolation is the prediction of values of attributes at unsampled locations $s_{0}$ from existing measurements at $s_{0}$. This procedure converts a sample of point observations into an alternative representation, such as a contour map or grid. 
#' 
#' Kriging is used to interpolate a value $Z(s_{0})$ of a random field $Z(s)$ at unobserved location $s_{0}$, using data from observed location $s_{i}$. Allows variance to be non-constant, dependent on distance between points as modeled by the variogram $\gamma(d)$. The kriging estimator is given by $\hat{Z}(s_{0})=\sum_{i=1}^{n}w_{i}(s_{0})Z(s_{i})$ where $w_{i}(s_{0}), i=1,\dots,n$ is a spatial weight.
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
## Create empty grid
grd <- Sobj_SpatialGrid(data.sp, maxDim=200)$SG
plot(grd, axes=T, col="grey")
points(data.sp)

#' 
#' Predicted values and variance for ordinary kriging is shown below for the Laos bombing data.
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
## Generate predictions
kr <- krige(log(LOAD_LBS)~1, data.sp, grd, model=v.fit)
spplot(kr, col.regions=rev(terrain.colors(100)), names.attr=c("Predictions","Variance"), main="Ordinary Kriging,  Bomb Load (log)", pch=2, cex=2)

#' 
#' 
#' 
#' # IDW Interpolation
#' 
#' One approach to interpolation is to use a locally-weighted average of nearby values. Inverse-distance weighted (IDW) interpolation computes one such weighted average: $\hat{Z}(s_{0})=\sum_{i=1}^{n}w_{i0}Z(s_{1})$ where weights $w_{ij}$s are determined according to the distance between points $s_{i}$ and $s_{j}$, and scaled by parameter $k$, $w_{ij}=1/d_{ij}^{k}$.
#' 
#' Prected values and variance for Laos bombing data are shown below. Values of $k>1$ reduce the relative impact of distant points and produce a peaky map. Values of $k<1$ increase the impact of distant points and produce a smooth map.
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
## Create empty grid
grd <- Sobj_SpatialGrid(data.sp, maxDim=200)$SG
plot(grd, axes=T, col="grey")
points(data.sp)


## Generate predictions

# k=.2
idw.out <- gstat::idw(log(LOAD_LBS)~1, data.sp, grd, idp=.2)
spplot(idw.out[1], col.regions=rev(heat.colors(100)), main="IDW Interpolation, Bomb Load (log)", sub="k = 1/5")

# k=1
idw.out <- gstat::idw(log(LOAD_LBS)~1, data.sp, grd, idp=1)
spplot(idw.out[1], col.regions=rev(heat.colors(100)), main="IDW Interpolation, Bomb Load (log)", sub="k = 1")

# k=5
idw.out <- gstat::idw(log(LOAD_LBS)~1, data.sp, grd, idp=5)
spplot(idw.out[1], col.regions=rev(heat.colors(100)), main="IDW Interpolation, Bomb Load (log)",sub="k = 5")


#' 
#' ## Example of Texas precipitation data.
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
#library(rgdal)
#library(tmap)
#library(raster)

# Load precipitation data
#z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/precip.rds"))
#P <- readRDS(z)
P <- readRDS("../Dataset/precip.rds")

# Load Texas boudary map
#z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
#W <- readRDS(z)
W <- readRDS("../Dataset/texas.rds")

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="Precip_in", palette="RdBu", auto.palette.mapping=FALSE,
             title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)

#' 
#' The IDW output is a raster. This requires that we first create an empty raster grid, then interpolate the precipitation values to each unsampled grid cell. 
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#' 

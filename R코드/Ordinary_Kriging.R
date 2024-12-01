grd <- Sobj_SpatialGrid(data.sp, maxDim=200)$SG
plot(grd, axes=T, col="grey")
points(data.sp)
## Generate predictions

# k=.2
#IDW의 가중치 인자로, 이 값이 작을수록 근접한 이웃의 값에 더 큰 가중치가 부여됩니다.
idw.out <- gstat::idw(log(LOAD_LBS)~1, data.sp, grd, idp=.2) ## 역거리 가중법

spplot(idw.out[1], col.regions=rev(heat.colors(100)), main="IDW Interpolation, Bomb Load (log)", sub="k = 1/5")

# k=1
idw.out <- gstat::idw(log(LOAD_LBS)~1, data.sp, grd, idp=1)


#Exam
#library(rgdal)
#library(tmap)
#library(raster)

# Load precipitation data
#z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/precip.rds"))
#P <- readRDS(z)
spplot(idw.out[1], col.regions=rev(heat.colors(100)), main="IDW Interpolation, Bomb Load (log)", sub="k = 1")

P <- readRDS("C:/Users/howsr/Desktop/공간통계/SpatStat/Dataset/precip.rds")

# Load Texas boudary map
#z <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
#W <- readRDS(z)
W <- readRDS("C:/Users/howsr/Desktop/공간통계/SpatStat/Dataset/texas.rds")

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="Precip_in", palette="RdBu", auto.palette.mapping=FALSE,
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE) # 옆에 글자 위치

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
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

#' ---
#' title: "Lab 4. Area Data Modeling"
#' author: "Spatial Big Data Analysis with GIS (jaehongjeong@hanyang.ac.kr)"
#' date: "Korean Statistical Society, Winter School, February 24, 2023"
#' 
#' output:
#'   html_document:
#'     number_sections: yes
#'     toc: yes
#'   pdf_document:
#'     toc: yes
#' ---
#' 
#' This document is based on "Hierarchical Modeling and Analysis for Spatial Data" by Banerjee, Carlin, and Gelfand (https://conservancy.umn.edu/handle/11299/200500).
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, out.width = "75%", fig.align = "center", error = FALSE, warning = FALSE)
library(spdep)
library(maps)
library(maptools)
library(classInt)
library(RColorBrewer)
library(spData)
library(spatialreg)

#' 
## ----eval =FALSE-------------------------------------------------------------------------------------------------
## # Required packages
## library(spdep)
## library(maps)
## library(maptools)
## library(classInt)
## library(RColorBrewer)
## library(spData)
## library(spatialreg)

#' 
#' # Area Data Modeling
#' 
#' We outline the use of some GIS functions in ${\tt R}$ for obtaining neighborhood (adjacency) matrices, computing Moran's and Geary's statistic and fitting CAR and SAR models using traditional maximum likelihood techniques, and mapping the results for certain classes of problems.
#' 
#' 
#' ## Computing Moran's I and Geary's C
#' 
#' We can invoke the ${\tt moran.test}$ and ${\tt geary.test}$ functions in the ${\tt spdep}$ package to obtain Moran's $I$ and Geary's $C$ statistics. 
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
#library(spdep)
#library(maps)
#library(maptools)
#library(classInt)
#library(RColorBrewer)
#library(spData)
#library(spatialreg)

usa.state <- map(database="state",fill=TRUE,plot=FALSE)
state.ID <- sapply(strsplit(usa.state$names,":"),function(x) x[1])
usa.poly <- map2SpatialPolygons(usa.state,IDs=state.ID)
usa.nb <- poly2nb(usa.poly)

usa.adj.mat <- nb2mat(usa.nb,style="B")

usa.listw <- nb2listw(usa.nb,style="W")

state.sat.scores <- read.table("../Dataset/state-sat.dat",header=FALSE)

verbal <- c(state.sat.scores$V2[1],state.sat.scores$V2[3:11],state.sat.scores$V2[13:51])

moran.test(verbal,listw=usa.listw)

geary.test(verbal,listw=usa.listw)

#' 
#' Using the 1999 average verbal SAT scores, lower 48 U.S. states and the district of Columbia, the sample estimate of Moran's I and standard error are 0.6125 and 0.0979, respectively. Greary's C yields the sample estimate and standard error of 0.3577 and 0.0984.
#' 
#' ## SAR and CAR modeling 
#' 
#' ### Sudden Infant Death Syndrome (SIDS)
#' 
#' We turn to fitting spatial autoregression models using available functions in the ${\tt spdep}$ package. The dataset can be read from a shapefile ${\tt sids.shp}$ and an ${\tt nb}$ object. The data relates to counts aggregated from 1979 to 1983. This dataset contains counts of SIDS deaths from 1974 to 1978 and counts from 1979 to 1983 along with related covariate information for the 100 counties in the U.S. State of North Carolina (https://geodacenter.github.io/data-and-lab/sids/).
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
nc.sids <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
rn <- as.character(nc.sids$FIPS)
ncCC89_nb <- read.gal(system.file("weights/ncCC89.gal", package="spData")[1],
region.id=rn)


##Create a map of raw SIDS rates
##First create a vector of rates from counts
nc.sids.rates.raw <- probmap(nc.sids$SID79, nc.sids$BIR79)$raw
##Alternatvely, one may want to use Freeman-Tukey transformations
nc.sids.rates.FT <- sqrt(1000) * (sqrt(nc.sids$SID79/nc.sids$BIR79) + sqrt((nc.sids$SID79 + 1)/nc.sids$BIR79))

##Append the rates we want to plot to the nc.sids data frame
nc.sids$rates.FT <- nc.sids.rates.FT

brks <- c(0, 2.0, 3.0, 3.5, 6.0)
#spplot(nc.sids, "rates.FT", at = brks, col.regions = rev(brewer.pal(4,"RdBu")))

#turn neighborhoods into list info
ncCC89.listw <- nb2listw(ncCC89.nb,style="B", zero.policy = TRUE)

##Compute Moran's I and Geary's C
##ncCR85.listw = nb2listw(ncCR85.nb, style="W", zero.policy=TRUE)
nc.sids.moran.out <- moran.test(nc.sids.rates.FT, listw=ncCC89.listw, zero.policy=TRUE)
nc.sids.moran.out
nc.sids.geary.out <- geary.test(nc.sids.rates.FT, listw=ncCC89.listw, zero.policy=TRUE)
nc.sids.geary.out

##find islands
nc.county.id <- attr(ncCC89.nb,"region.id")
nc.no.neighbors <- card(ncCC89.nb)
nc.islands <- as.character(nc.sids[card(ncCC89.nb)==0,]$NAME)
nc.islands # Two counties, Dare and Hyde, have zero neighbors.


#Feedman-Tukey transformation (to get Gaussian) on SIDS rates
nc.sides.rates.FT <- sqrt(1000)*(sqrt(nc.sids$SID79/nc.sids$BIR79)
                                + sqrt((nc.sids$SID79+1)/nc.sids$BIR79))
nc.sids$rates.FT <- nc.sides.rates.FT

#Feedman-Tukey transformation (to get Gaussian) on SIDS rates for non-white births
nc.sid.nwbir.FT <- sqrt(1000)*(sqrt(nc.sids$NWBIR79/nc.sids$BIR79)
                              + sqrt((nc.sids$NWBIR79+1)/nc.sids$BIR79))
nc.sids$nwbir.FT <- nc.sid.nwbir.FT


#Fit a SAR model (replace family = "SAR" with family = "CARfixedBreaks, to fit a CAR model)
nc.sids.sar.out <- spautolm(rates.FT~nwbir.FT, data=nc.sids,family="SAR",
listw <- ncCC89.listw,zero.policy = TRUE)

summary(nc.sids.sar.out)

##CAR model regressing rates.FT on NWBIR79.FT
nc.sids.car.out <- spautolm(rates.FT~ nwbir.FT, data=nc.sids, family="CAR", listw=ncCC89.listw, zero.policy=TRUE)
nc.sids.car.fitted <- fitted(nc.sids.car.out)
nc.sids$fitted.car <- nc.sids.car.fitted

summary(nc.sids.car.out)

#plot rates.FT
brks <- c(0,2.0,3.0,3.5,6.0)
color.pallete <- rev(brewer.pal(4,"RdBu"))
class.fitted <- classIntervals(var=nc.sids$rates.FT,n=4,style="fixed",
fixedBreaks=brks,dataPrecision=4)
class.fitted
color.code.fitted <- findColours(class.fitted, color.pallete)
brks <- c(-Inf,2,3,3.5,Inf)
plot(nc.sids[,24],col=color.code.fitted)
legend("bottomleft", fill=color.pallete, legend=leglabs(brks))

#' 
#' (Figure above) Raw Freeman-Tukey transformed SIDS rates (Note: FTT seeks to adjust data to make the distribution more similar to a Normal distribution.)
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
#plot SAR predictions
nc.sids$fitted.sar <- fitted(nc.sids.sar.out)
brks <- c(0,2.0,3.0,3.5,6.0)
color.pallete <- rev(brewer.pal(4,"RdBu"))
class.fitted <- classIntervals(var=nc.sids$fitted.sar,n=4,style="fixed",
fixedBreaks=brks,dataPrecision=4)
class.fitted
color.code.fitted <- findColours(class.fitted, color.pallete)
brks <- c(-Inf,2,3,3.5,Inf)
plot(nc.sids[,27],col = color.code.fitted)
legend("bottomleft", fill=color.pallete, legend=leglabs(brks))

#' 
#' (Figure above) Fitted SIDS rates from SAR model
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
#plot CAR predictions
nc.sids$fitted.car <- fitted(nc.sids.car.out)
brks <- c(0,2.0,3.0,3.5,6.0)
color.pallete <- rev(brewer.pal(4,"RdBu"))
class.fitted <- classIntervals(var=nc.sids$fitted.car,n=4,style="fixed",
fixedBreaks=brks,dataPrecision=4)
class.fitted
color.code.fitted <- findColours(class.fitted, color.pallete)
brks <- c(-Inf,2,3,3.5,Inf)
plot(nc.sids[,26],col = color.code.fitted)
legend("bottomleft", fill=color.pallete, legend=leglabs(brks))



#' 
#' (Figure above) Fitted SIDS rates from CAR model
#' 
#' 
#' ### Columbus Crime 1980
#' 
#' Instead of defining a neighborhood structure completely in terms of spatial adjacency on the map, we may want to construct neighborhors using a distance function (e.g., between centroids of regions). We use a dataset offering neighborhood-level information on crime, mean home value, mean income, and other variables for 49 neighborhoods in Columbus, OH, during 1980 (https://geodacenter.github.io/data-and-lab/columbus/).
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
## Constructing neighbors using distances: Columbus example
columbus <- st_read(system.file("shapes/columbus.shp", package="spData")[1], quiet=TRUE)
columbus.poly <- readShapePoly(system.file("etc/shapes/columbus.shp", package="spdep")[1])


##There are two different ways to create a neighbor object for columbus
##Method 1a: Use poly2nb with queen type neighbors
columbus.queen.nb <- poly2nb(columbus.poly, queen=TRUE)
columbus.queen.nb
##Method 1b: Use poly2nb with rook type neighbors
columbus.rook.nb <- poly2nb(columbus.poly, queen =FALSE)
columbus.rook.nb
##Method 2: Use the read.gal file to read a GAL file 
columbus.gal.nb <- read.gal(system.file("etc/weights/columbus.gal", package="spdep")[1])
columbus.gal.nb

##Compute Moran's I and Geary's C
##We make two distinct listw objects for the rook and gal neighbors
columbus.gal.listw <- nb2listw(columbus.gal.nb, style="B", zero.policy=TRUE)
columbus.gal.listw
columbus.rook.listw <- nb2listw(columbus.rook.nb, style="B", zero.policy=TRUE)
columbus.rook.listw
##We now compute Moran's I and Geary's C for each of these neighbors
columbus.gal.moran.out <- moran.test(columbus.poly$CRIME, listw=columbus.gal.listw, zero.policy=TRUE)
columbus.gal.moran.out
columbus.rook.moran.out <- moran.test(columbus.poly$CRIME, listw=columbus.rook.listw, zero.policy=TRUE)
columbus.rook.moran.out
columbus.gal.geary.out <- geary.test(columbus.poly$CRIME, listw=columbus.gal.listw, zero.policy=TRUE)
columbus.gal.geary.out
columbus.rook.geary.out <- geary.test(columbus.poly$CRIME, listw=columbus.rook.listw, zero.policy=TRUE)
columbus.rook.geary.out


##SAR model regressing HOVAL+INC
columbus.gal.sar.out <- spautolm(CRIME~HOVAL+INC, data=columbus.poly, family="SAR", listw=columbus.gal.listw, zero.policy=TRUE)
columbus.gal.sar.out
columbus.gal.sar.fitted <- fitted(columbus.gal.sar.out)
columbus.poly$fitted.gal.sar = columbus.gal.sar.fitted

columbus.rook.sar.out <- spautolm(CRIME~HOVAL+INC, data=columbus.poly, family="SAR", listw=columbus.rook.listw, zero.policy=TRUE)
columbus.rook.sar.out
columbus.rook.sar.fitted <- fitted(columbus.rook.sar.out)
columbus.poly$fitted.rook.sar = columbus.rook.sar.fitted

##CAR model regressing CRIME on HOVAL + INCOME
columbus.car.out <- spautolm(CRIME~HOVAL+INC, data=columbus.poly, family="CAR", listw=columbus.rook.listw, zero.policy=TRUE)
columbus.car.out
columbus.car.fitted <- fitted(columbus.car.out)
columbus.poly$fitted.car <- columbus.car.fitted

##Distance based neighbors in spdep
columbus.coords <- coordinates(columbus.poly)
columbus.knn <- knearneigh(columbus.coords)
columbus.knn2nb <- knn2nb(columbus.knn)
columbus.dist.list <- nbdists(columbus.knn2nb, columbus.coords)
columbus.dist.vec <- unlist(columbus.dist.list)
columbus.dist.max <- max(columbus.dist.vec)
columbus.dnn.nb <- dnearneigh(columbus.coords, 0, 0.25*columbus.dist.max)

##Form a listw object using the distance-based nearest neighbors 
columbus.dnn.listw <- nb2listw(columbus.dnn.nb, style="B", zero.policy=TRUE)

##SAR model regressing HOUSE_VAL+INCOME using distance-based nearest neighbors
columbus.dnn.sar.out <- spautolm(CRIME~HOVAL+INC, data=columbus.poly, family="SAR", listw=columbus.dnn.listw, zero.policy=TRUE)
columbus.dnn.sar.out
columbus.dnn.sar.fitted <- fitted(columbus.dnn.sar.out)
columbus.poly$fitted.dnn.sar <- columbus.dnn.sar.fitted


##CAR model regressing HOUSE_VAL+INCOME using distance-based nearest neighbors
columbus.dnn.car.out <- spautolm(CRIME~HOVAL+INC, data=columbus.poly, family="CAR", listw=columbus.dnn.listw, zero.policy=TRUE)
columbus.dnn.car.out
columbus.dnn.car.fitted <- fitted(columbus.dnn.car.out)
columbus.poly$fitted.dnn.car <- columbus.dnn.car.fitted


#plot CRIME
summary(columbus$CRIME)
brks <- c(0,20,35,50,70)
color.pallete <- rev(brewer.pal(4,"RdBu"))
class.fitted <- classIntervals(var=columbus$CRIME,n=4,style="fixed",
fixedBreaks=brks,dataPrecision=4)
class.fitted
color.code.fitted <- findColours(class.fitted, color.pallete)
brks <- c(-Inf,20,35,50,Inf)
plot(columbus[,9],col=color.code.fitted)
legend("topleft", fill=color.pallete, legend=leglabs(brks))

#plot SAR predictions (regressing HOUSE_VAL+INCOME using distance-based nearest neighbors)
columbus$fitted.sar <- fitted(columbus.dnn.sar.out)
summary(columbus$fitted.sar)
brks <- c(0,20,35,50,70)
color.pallete <- rev(brewer.pal(4,"RdBu"))
class.fitted <- classIntervals(var=columbus$fitted.sar,n=4,style="fixed",
fixedBreaks=brks,dataPrecision=4)
class.fitted
color.code.fitted <- findColours(class.fitted, color.pallete)
brks <- c(-Inf,20,35,50,Inf)
plot(columbus[,22],col=color.code.fitted)
legend("topleft", fill=color.pallete, legend=leglabs(brks))


#plot CAR predictions (regressing HOUSE_VAL+INCOME using distance-based nearest neighbors)
columbus$fitted.car <- fitted(columbus.dnn.car.out)
summary(columbus$fitted.sar)
brks <- c(0,20,35,50,70)
color.pallete <- rev(brewer.pal(4,"RdBu"))
class.fitted <- classIntervals(var=columbus$fitted.car,n=4,style="fixed",
fixedBreaks=brks,dataPrecision=4)
class.fitted
color.code.fitted <- findColours(class.fitted, color.pallete)
brks <- c(-Inf,20,35,50,Inf)
plot(columbus[,23],col=color.code.fitted)
legend("topleft", fill=color.pallete, legend=leglabs(brks))

##Draw the maps using the spplot (trellis) graphics function
#postscript(file="nc_sids_sar_actual.eps")
#print(spplot(nc.sids, "rates.FT", at = brks, col.regions = rev(brewer.pal(4,"RdBu")), main="a) Actual SIDS rates"))
#dev.off()
#postscript(file="nc_sids_sar_fitted.eps")
#print(spplot(nc.sids, "fitted.sar", at = brks, col.regions = rev(brewer.pal(4,"RdBu")), main="b) Fitted SIDS rates from SAR model"))
#dev.off()

##detach(package:spdep)
##detach(package:maptools)
##detach(package:RColorBrewer)


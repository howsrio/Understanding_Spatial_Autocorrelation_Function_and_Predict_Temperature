#' ---
#' title: "Lab 3. Variogram and Kriging"
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
#' This document is based on "Hierarchical Modeling and Analysis for Spatial Data" by Banerjee, Carlin, and Gelfand (https://conservancy.umn.edu/handle/11299/200500).
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, out.width = "75%", fig.align = "center", error = FALSE, warning = FALSE)
## Load spatial packages
library(spBayes)
library(classInt)
library(RColorBrewer)
library(MBA)
library(fields)
library(geoR)
library(gstat)
library(sp)

#' 
## ----eval = FALSE------------------------------------------------------------------------------------------------
## # Required packages
## library(spBayes)
## library(classInt)
## library(RColorBrewer)
## library(MBA)
## library(fields)
## library(geoR)
## library(gstat)
## library(sp)

#' 
#' # Visualizing Data
#' 
#' We will use WEF forest inventory data from a long-term ecological research site in western Oregon. Diameter at breast height (DBH) and tree height (HT) have been measured for all trees.
#' 
#' We simply use these data to demonstrate some basics of spatial data manipulation, visualization, and exploratory analysis.
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
##Visualizing Data
#library(sp)
#library(spBayes)
#library(classInt)
#library(RColorBrewer)

#load data
data(WEF.dat)

#remove possible NaNs
WEF.dat <- WEF.dat[!apply(WEF.dat[,c("East_m","North_m","DBH_cm","Tree_height_m","ELEV_m")],1,function(x){any(is.na(x))}),]
DBH <- WEF.dat$DBH_cm
HT <- WEF.dat$Tree_height_m
coords <- as.matrix(WEF.dat[,c("East_m","North_m")])

plot(coords,pch=1,cex=sqrt(DBH)/10,col="darkgreen", xlab="Easting (m)", ylab="Northing (m)")
leg.vals <- round(quantile(DBH),10)
legend("topleft",pch=1,legend=leg.vals,col="darkgreen", pt.cex=sqrt(leg.vals)/10,bty="n",title="DBH (cm)")

#' 
#' To further explore spatial pattern ${\tt DBH}$, it is often useful to use a color gradient or ramp to construct the pallet. 
#' 
## ----------------------------------------------------------------------------------------------------------------
#Plot by tree species
col.br <- colorRampPalette(c("blue","cyan","yellow","red"))
col.pal <- col.br(5)
fixed  <- classIntervals(DBH,n=4,style="fixed", fixedBreaks=c(0,12.7,30.48,60,max(DBH)+1))

fixed.col<-findColours(fixed,col.pal)

plot(coords, col=fixed.col, pch=19, cex=0.5, main="Forestry tree size classes", xlab="Easting (m)", ylab = "Northing (m)")
legend("topleft",fill=attr(fixed.col,"palette"), legend=c("sapling","poletimber","sawtimber","large sawtimber"), bty="n", text.width=2)

#' 
#' If the data observations are well distributed over the domain, spatial pattern can often be detected by estimating a continuous surface using an interpolation function. ${\tt MBA}$ provides efficient interpolation of large data sets with multilevel B-splines.
#' 
## ----------------------------------------------------------------------------------------------------------------
#surface and contour plots
#library(MBA)
#library(fields)

x.res<-100
y.res<-100

surf <- mba.surf(cbind(coords,DBH), no.X=x.res, no.Y=y.res, h=5, m=2, extend=FALSE)$xyz.est

image.plot(surf,xaxs="r",yaxs="r",xlab="Easting (m)",ylab="Northing (m)",col=col.br(25))

contour(surf,add=T)

#' 
#' # Variogram 
#' 
#' We begin by fitting an isotropic empirical semivariogram using functions within the ${\tt geoR}$ package. We first fit an exponential variogram model to ${\tt DBH}$, then fit a second variogram model to the residuals of a linear regression of ${\tt DBH}$ onto three species.
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
#variogram plots
#library(geoR)
max.dist <- 0.25*max(iDist(coords))

bins <- 50

vario.DBH <- variog(coords=coords, data=DBH, uvec=(seq(0,max.dist,length=bins)))

fit.DB <- variofit(vario.DBH, ini.cov.pars=c(600,200/-log(0.05)), cov.model="exponential", minimisation.function="nls", weights="equal")

summary(fit.DB)

lm.DBH <- lm(DBH~Species, data = WEF.dat)
summary(lm.DBH)
DBH.resid <- resid(lm.DBH)

vario.DBH.resid <- variog(coords=coords, data = DBH.resid, uvec=(seq(0,max.dist,length=bins)))
fit.DB.resid <- variofit(vario.DBH.resid, ini.cov.pars=c(300,200/-log(0.05)), cov.model = "exponential", minimisation.function="nls", weights="equal")

par(mfrow=c(1,2))
plot(vario.DBH,ylim=c(200,1200),main="DBH")
lines(fit.DB)
abline(h=fit.DB$nugget, col="blue")
abline(h=fit.DB$cov.pars[1] + fit.DB$nugget, col="green")

plot(vario.DBH.resid,ylim=c(200,600),main="DBH Residuals")
lines(fit.DB.resid)
abline(h=fit.DB.resid$nugget, col="blue")
abline(h=fit.DB.resid$cov.pars[1] + fit.DB.resid$nugget, col="green")
abline(v=-log(0.05)*fit.DB.resid$cov.pars[2],col="red3")

#' 
#' We can check for possible anisotropic patterns in spatial dependence. The ${\tt gstat}$ package offers a ${\tt variogram}$ function that can be regarded as more versatile than its ${\tt geoR}$ counterpart in that it allows us to specify a linear regression model ${\it within}$ it and conduct the variogram analysis on the residuals without having to explicitly collect the residuals.
#' 
## ----------------------------------------------------------------------------------------------------------------
#library(gstat)
ELEV_m <- WEF.dat$ELEV_m
dats = matrix(0,1955,2)
dats[,1] = DBH
dats[,2] = ELEV_m
sp.dat <- SpatialPointsDataFrame(as.data.frame(coords), as.data.frame(dats))
vario.DBH <- variogram(DBH~ELEV_m, data=sp.dat, cutoff=max.dist,width=5,alpha = (0:3)*45)
fit.DBH <- fit.variogram(vario.DBH,vgm(1000,"Exp",200/-log(0.05),600))
print(plot(vario.DBH,fit.DBH))

#' 
#' # Kriging
#' 
#' We will perform ordinary Kriging based on previous variogram fitting using residuals. We can obtain Kriging predicted values and variances coupled with ${\tt krige.conv}$ function. Kriging surface maps for both can be obtained using various functions, e.g., ${\tt mba.surf}$ (based on multilevel B-spline).
#' 
## ----out.width="75%"---------------------------------------------------------------------------------------------
####kriging
sp.dat2 <- as.geodata(cbind(DBH.resid,coords))
loci <- expand.grid(seq(min(coords[,1]),max(coords[,1]),l=40), seq(min(coords[,2]),max(coords[,2]),l=40))

krigpreds <- krige.conv(sp.dat2, coords=coords, data=DBH.resid, location=loci, krige=krige.control(cov.pars=c(fit.DB.resid$cov.pars[1], fit.DB.resid$cov.pars[2])))

# Surface approximation using multilevel B-splines
surf1 <- mba.surf(cbind(loci,krigpreds$predict),no.X=40,no.Y=40,h=5,m=1,extend=FALSE)$xyz.est
surf2 <- mba.surf(cbind(loci,krigpreds$krige.var),no.X=40,no.Y=40,h=5,m=1,extend=FALSE)$xyz.est

par(mfrow=c(1,2))
image.plot(surf1,xaxs="r",yaxs="r",xlab=" ",ylab=" ",col=col.br(25),main="Kriging Predictor")
image.plot(surf2,xaxs="r",yaxs="r",xlab=" ",ylab=" ",col=col.br(25),main="Kriging Variances")


#' 
#' On the other hand, we can also perform Kriging based on the covariance model. Since it takes some time, we only use sub-sample of size 150. We first fit a Matern covariance model and then create Kriging surface maps on a grid using ${\tt predictSurface}$ and ${\tt predictSurfaceSE}$ functions (thin plate spline estimates). 
#' 
## ----------------------------------------------------------------------------------------------------------------
# Kriging with a stationary Matern covariance function using sub-samples
set.seed(20230224)
sample.idx <- sample(1:length(DBH.resid),150,replace=FALSE)
fit <- Krig(coords[sample.idx,], DBH.resid[sample.idx],
Covariance="Matern", aRange=10, smoothness=1.0)  
summary(fit)

# prediction and standard errors on a grid
out <- predictSurface(fit,extrap=TRUE,nx=40,ny=40)
out.p <- predictSurfaceSE(fit,extrap=TRUE,nx=40,ny=40) # this takes some time!

par(mfrow=c(1,2))
image.plot(out,xaxs="r",yaxs="r",xlab=" ",ylab=" ",col=col.br(25),main="Kriging Predictor")
points(fit$x)
image.plot(out.p,xaxs="r",yaxs="r",xlab=" ",ylab=" ",col=col.br(25),main="Kriging Variances")
points(fit$x)

#par(mfrow=c(1,2))
#surface(out, type="C")
#points(fit$x)
#surface(out.p, type="C")
#points(fit$x)

#' 
#' Note that Kriging variances between the two models are quite different, so one may think that the covariance model works better than the variogram model. However, this is not true because they considered different sample sizes and not the same dependence structures. Moreover, we applied different approaches to creating Kriging surface maps.
#' 
#' 

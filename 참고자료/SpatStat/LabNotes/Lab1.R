#' ---
#' title: "Lab 1. Spatial Data and Basic Visualization"
#' author: "Spatial Big Data Analysis with GIS (jaehongjeong@hanyang.ac.kr)"
#' date: "Korean Statistical Society, Winter School, February 24, 2023"
#' output:
#'   html_document:
#'     number_sections: yes
#'     toc: yes
#'   pdf_document:
#'     toc: yes
#'   fonttheme: professionalfonts
#' ---
#' 
#' This document is based on ``Applied Spatial Statistics in R`` by Yuri M. Zhukov and ``Introduction to spatial points in R`` by Michael T. Hallworth.
#' 
#' # Software options for spatial data
#' 
#' * ArcGIS (license; geoprocessing, visualization)
#' * GeoBUGS (free; Bayesian analysis)
#' * GRASS (free; image processing, spatial modeling)
#' * R (free; spatial econometrics, geostatistics)
#' * STARS (free, space-time analysis).
#' 
#' # Spatial analysis in R
#' 
#' * Data management: ${\tt sp, rgdal, maptools}$
#' * Integration with other GIS: ${\tt rgdal, RArcInfo, SQLiteMap, RgoogleMaps, spgrass6, RPyGo, R2WinBUGS, geonames}$
#' * Geostatistics: ${\tt gstat, geoR, geoRglm, spBayes}$
#' * Spatial regression: ${\tt spdep, spacecounts}$
#' * Point pattern analysis: ${\tt spatstat, splancs, spatialkernel}$
#' * Disease mapping: ${\tt DCluster, spgwr, glmmBUGS, diseasemapping}$
#' 
#' # Where to find spatial data?
#' 
#' * Coordinate and Basemaps: Geographical Place Names (http://www.geonames.org/), Global Administrative Areas (http://gadm.org/country), Land Cover and Elevation (http://eros.usgs.gov/#/Find_Data)
#' * Geo-reference Data: 2000 U.S. Census Data (http://disasternets.calit2.uci.edu/census2000/), Natural Resources (http://www.prio.no/CSCW/Datasets/Geographical-and-Resource/), International Conflict Data (http://www.acleddata.com/)
#' 
#' 
#' # Points
#' 
#' Points are the most basic form of spatial data. Points are pairs of coordinates $(x,y)$, representing events, observation posts, individuals, cities or any other discrete object defined in space.
#' 
#' Spatial points can be combined with data frames to create what's called a $\tt{SpatialPointsDataFrame}$.
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, out.width = "75%", fig.align = "center", error = FALSE, warning = FALSE)
## Load spatial packages
library(sp)
library(raster)
library(rgeos)
library(geosphere)
library(sf)
library(tidyverse)
library(ggplot2)

#' 
## ----eval=FALSE--------------------------------------------------------------------------------------------------
## # Required packages
## library(sp)
## library(raster)
## library(rgeos)
## library(geosphere)
## library(sf)
## library(tidyverse)
## library(ggplot2)

#' 
#' ## Spatial points in R
#' 
#' To create a $\tt{SpatialPoints}$ object, we can use the $\tt{SpatialPoints()}$ after loading the $\tt{sp}$ library.
#' 
## ----------------------------------------------------------------------------------------------------------------
# load library
# library(sp)

# Generate 100 random X and Y coordinates 
# with longitude and latitude in the familiar degrees

set.seed(20230224)

x_coords <- runif(n = 100, min = -100, max = -80)
y_coords <- runif(n = 100, min = 25, max = 45)

# Have a look at the first coordinates
head(cbind(x_coords,y_coords))

#' 
#' We have generated random coordinates we can make those data spatially explicit.
#' 
## ----------------------------------------------------------------------------------------------------------------
# See what arguments we need to pass to SpatialPoints
args("SpatialPoints")


#' 
#' The $\tt{SpatialPoints}$ function is looking for coordinates, a projection / datum argument, and a bounding box. 
#' 
## ----------------------------------------------------------------------------------------------------------------
# coords = c(longitude,latitude)
firstPoints <- SpatialPoints(coords = cbind(x_coords,y_coords))

str(firstPoints)

plot(firstPoints, pch = 19)

#' 
#' 
#' ## Writing a shapefile
#' 
#' We can save our $\tt{firstPoints}$ object as a shapefile using the $\tt{raster}$ package. The $\tt{shapefile}$ function is very convenient in that it can both read a shapefile into R but it can also write a $\tt{SpatialPoints}$ or other spatial object classes (lines, polygons, etc.) to a shapefile.
#' 
## ----------------------------------------------------------------------------------------------------------------
# library(raster)
# dir.create("output")
# shapefile(x = firstPoints, file = "output/file_points.shp")

#' 
#' 
#' ## Calculate distance between points
#' 
#' One task that is useful when we have spatial points is calculating the distance between points either within the same layer or between two layers.
#' 
## ----------------------------------------------------------------------------------------------------------------
# longlat = FALSE returns Euclidean distance
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

# longlat = TRUE returns GreatCircle distance
gcDist <- sp::spDists(firstPoints,longlat = TRUE)
hist(gcDist,xlab="Distance (km)")

#' 
#' # Polygons
#' 
#' Spatial polygons are a set of spatially explicit shapes/polygons that represent a geographic location. Spatial polygons are composed of vertices which are a set of a series of x and y coordinates or Spatial points.
#' 
#' 
#' ## Spatial polygons in R
#' 
#' Here is the general workflow for generating polygons from scratch:
#' 
#' * Determine a set of coordinates for the vertices 
#' 
#' $\bf{\text{The first and last vertex need to be the same to close the polygon.}}$
#' 
#' * Give the polygon an ID
#' 
## ----------------------------------------------------------------------------------------------------------------
# load library
#library(sp)

# Make a polygon from our matrix of vertices
poly1 <- sp::Polygon(cbind(x_coords,y_coords))

# Make a polygon class
firstPoly <- sp::Polygons(list(poly1), ID = "A")

str(firstPoly,1)

# Make firstPoly into a SpatialPolygons
firstSpatialPoly <- sp::SpatialPolygons(list(firstPoly))

firstSpatialPoly

#' 
#' We can create two or more polygons into a single SpatialPolygon file as well.
#' 
## ----------------------------------------------------------------------------------------------------------------
# Define the vertices
x1 <- c(-60,-60,-62,-62,-60)
x2 <- c(-50,-50,-55,-55,-50)
y1 <- c(20,25,25,20,20)
y2 <- c(15,25,25,15,15)

# Assign the vertices to a `polygon` 
poly1 <- sp::Polygon(cbind(x1,y1))
poly2 <- sp::Polygon(cbind(x2,y2))

# This step combines the last two together - making Polygons and then SpatialPolygons
TwoPolys <- sp::SpatialPolygons(list(sp::Polygons(list(poly1), ID = "A"),
                                     sp::Polygons(list(poly2), ID = "B")))

TwoPolys

#' 
## ----------------------------------------------------------------------------------------------------------------
plot(TwoPolys)

#' 
#' 
#' ## Writing a shapefile
#' 
## ----------------------------------------------------------------------------------------------------------------
# library(raster)
# shapefile(x = firstPoints, file = "output/file_polygons.shp")

#' 
#' 
#' ## Reading a SpatialPolygon from file
#' 
#' Here, we will download a $\tt{SpatialPolygonDataFrame}$ that contains US State boundaries. We can get the data directly using $\tt{getData}$ function in the $\tt{raster}$ package.
#' 
## ----------------------------------------------------------------------------------------------------------------
# This looks up the GADM dataset - for the country US and returns 
# the first level of administration which in this case is state boundaries. 

#States <- raster::getData("GADM", country = "United States", level = 1)
#https://gadm.org/download_country.html  
States <- readRDS("../Dataset/gadm36_USA_1_sp.rds")

# Have a look at the data
States

#' 
#' The $\tt{States}$ object is a $\tt{SpatialPolygonsDataFrame}$ that contains spatial information about the state boundaries (also additional data such as the name).
#' 
## ----------------------------------------------------------------------------------------------------------------
plot(States)

# Subset out Alaska and Hawaii from the current data
States <- States[States$NAME_1 != "Alaska" & States$NAME_1 != "Hawaii",]

plot(States)

#' 
#' 
#' ## Dissolving boundaries
#' 
#' Often we find that we have lots of spatial polygons that represent the same information. We can get around that by dissolving boundaries based on similar attributes. Essentially collapsing multiple polygons into a single polygon.
#' 
#' We will dissolve the state boundaries to make a single United States border polygon using $\tt{rgeos}$ package.
#' 
## ----------------------------------------------------------------------------------------------------------------
# library(rgeos)

#' 
#' Within the $\tt{rgeos}$ package there are lots of handy spatial functions. We’ll use a special case of the $\tt{gUnion}$ function to dissolve our state boundaries. In order to dissolve we need a common identifier in all the polygons we want to ‘merge’ together. The $\tt{ISO}$ field is the same for all polygons. We’ll use that to dissolve boundaries into one polygon.
#' 
## ----------------------------------------------------------------------------------------------------------------
USborder <- rgeos::gUnaryUnion(States, id = States$ISO) # this takes some time

USborder

plot(USborder)

#' 
#' Here is the code to create a new map using $\tt{USborder}$ and the state level data.
#' 
## ----------------------------------------------------------------------------------------------------------------
plot(States, 
     col = "gray70", # fill color
     border = "white") # outline color
plot(USborder, 
     lwd = 2,
     add = TRUE) # add to current plot

#' 
#' 
#' 
#' # Lines
#' 
#' Creating spatial lines is similar to making polygons from scratch. To construct lines we first need to start with the locations of the endpoints. For this example, let's use the 100 random points and add an ID field as well.
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------
# generate 100 random XY coords 
set.seed(20230224)

x_coords <- runif(n = 100, min = -100, max = -80)
y_coords <- runif(n = 100, min = 25, max = 45)

# make a unique identifier for each line
ID <- paste0("line_",1:100)

#' 
#' 
#' Now that we have the endpoints, we need to create a $\tt{Line}$ object then convert that to a Lines object and give them an ID then we can finally convert them to $\tt{SpatialLines}$. Here, we will use the first two points for our first line.
#' 
## ----------------------------------------------------------------------------------------------------------------
line_obj <- sp::Line(cbind(x_coords[1:2],y_coords[1:2]))
lines_obj <- sp::Lines(list(line_obj),ID=ID[1])
firstLine <- sp::SpatialLines(list(lines_obj))

plot(firstLine)

# repeat it with all coordinates
line_obj2 <- sp::Line(cbind(x_coords,y_coords))
lines_obj2 <- sp::Lines(list(line_obj2),ID="lines")
allLines <- sp::SpatialLines(list(lines_obj2))

plot(allLines)

#' 
#' 
## ----------------------------------------------------------------------------------------------------------------
# make SpatialPoints
points <- sp::SpatialPoints(cbind(x_coords,y_coords))

# use as to convert to line
sp_line <- as(points,"SpatialLines")

plot(sp_line)

#' 
#' 
#' ## Distance / length
#' 
#' Calculating the distance along a line / length of a line can be done using the $\tt{rgeos}$ package. We can use the $\tt{gLength}$ function. We need to be a little careful here because the units it returns depends on the $\tt{coordinate reference system}$ the data have. 
#' 
## ----------------------------------------------------------------------------------------------------------------
# returns length in coordinate reference system units
# here it's degrees - it assumes planar coordinates
rgeos::gLength(sp_line)

# great circle distance (meters) along our line
geosphere::lengthLine(sp_line)

#' 
#' 
#' # Rasters
#' 
#' A raster is a spatially explicit matrix or $\tt{grid}$ where each cell represents a geographic location. Each cell represents a pixel on a surface. The size of each pixel defines the resolution or $\tt{res}$ of raster. The smaller the pixel size the finer the spatial resolution. The $\tt{extent}$ or spatial coverage of a raster is defined by the minima and maxima for both x and y coordinates.
#' 
#' Rasters can be created from the following data classes: numeric, integer, and categorical classes.
#' 
## ----------------------------------------------------------------------------------------------------------------
# Raster data are stored in a variety of formats.
# raster::writeFormats()


#' 
#' ## Raster data in R
#' 
#' We will use the $\tt{raster}$ package to make an empty raster, set the $\tt{extent}$ and resolution ($\tt{res}$), and assign values. 
#' 
## ----------------------------------------------------------------------------------------------------------------
# load library
# library(raster)

# Create a raster from scratch using raster
firstRaster <- raster(xmn = -100,   # set minimum x coordinate
                      xmx = -60,    # set maximum x coordinate
                      ymn = 25,     # set minimum y coordinate
                      ymx = 50,     # set maximum y coordinate
                      res = c(1,1)) # resolution in c(x,y) direction

# Take a look at what the raster looks like
firstRaster

#' Note that the object has 25 rows, 40 columns, and 1000 cells. The raster's $\tt{extent}$ ranges from -100 to -60 degrees longitude and 25 to 50 degrees latitude. The coordinate reference is WGS84 by default because $\tt{raster}$ recognized our inputs as degrees longitude/latitude.
#' 
#' 
#' ## Setting raster values
#' 
#' We can assign values to the raster in a few ways. We will set the values of the raster using the $\tt{[]}$ convention (See the $\tt{setValues}$ function for another way to set values of a raster). We will sequence values from 1 to the number of cells within the raster. You can extract the number of cells within a raster using the $\tt{ncell}$ function.
#' 
#' $\bf{\text Note:}$ The number of values needs to be equivalent to the number of cells in the raster.
#' 
## ----------------------------------------------------------------------------------------------------------------
# Assign values to raster 
firstRaster[] <- seq(from = 1, to = ncell(firstRaster), by = 1)

# Take a look at the raster now
firstRaster

#' 
#' There are a few new attributes such as $\tt{data source}$, $\tt{names}$, and $\tt{values}$ fields. 
#' 
## ----------------------------------------------------------------------------------------------------------------
plot(firstRaster)

#' 
#' 
#' ## Reading rasters from file
#' 
#' The $\tt{raster()}$ function can be used to read in a raster from file. 
#' 
## ----------------------------------------------------------------------------------------------------------------
# read in raster layer using raster function
# NDVI <- raster("path/file")
NDVI <- raster::raster("../Dataset/MOD_NDVI_M_2018-01-01_rgb_3600x1800.FLOAT.TIFF")

NDVI

par(bty = "n", mar = c(0,0,0,3))
plot(NDVI, axes = FALSE)

#' 
## ----------------------------------------------------------------------------------------------------------------
# First option for making NDVI appear as expected
# set values larger than 2 to NA 
#NDVI[NDVI>2]<-NA
par(bty = "n", mar = c(0,0,2,3))
#plot(NDVI, axes = FALSE)

# Second option - leave NDVI values intact but
# only plot values within the range of 'normal' 
# NDVI values c(-0.1,0.9)
raster::plot(NDVI, axes = FALSE, zlim = c(-0.1,0.9), legend.args=list("NDVI"))

#' 
#' 
#' 
#' 
#' # Projections
#' 
#' The reason we need projections is so that we can map a three dimensional surface - like the earth in two dimensional space. Unfortunately, not all properties of the 3D surface are maintained when plotting in 2D space. Attributes of the 3D surface such as area, distance, shape and direction are distorted when creating a 2D map.
#' 
#' Projections define the way we distort the 3D surface in order to render it in 2D. The projection is the mathematical equation used to `flatten’ the world. Every time we create a map we distort the true surface in some fashion. Different projections preserve different aspects of the 3D properties. Therefore, knowing which projections to use is important when doing spatial analyses.
#'  
## ----------------------------------------------------------------------------------------------------------------
par(mar = c(0,0,2,0),mfrow = c(2,2))
world <- raster::shapefile("../Dataset/TM_WORLD_BORDERS-0.3.shp")
graticules <- sp::gridlines(world, easts = seq(-180,180,10), norths = seq(-90,90,10))
raster::plot(graticules, col = "gray80",main = "WGS84")
raster::plot(world,col = "gray70",add = TRUE)

worldROB <- sp::spTransform(world,sp::CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
graticulesROB <- sp::spTransform(graticules,sp::CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
raster::plot(graticulesROB, col = "gray80", main = "Robinson")
raster::plot(worldROB, add = TRUE, col = "gray70")

worldMOLL <- sp::spTransform(world,sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
graticulesMOLL <- sp::spTransform(graticules,sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
raster::plot(graticulesMOLL, col = "gray80",main = "Mollewide")
raster::plot(worldMOLL, col = "gray70", add = TRUE)

worldConic <- sp::spTransform(world, sp::CRS("+proj=aea +lat_1=50 +lat_2=90 +lat_0=-90 +lon_0=-10 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
graticulesConic <- sp::spTransform(graticules, sp::CRS("+proj=aea +lat_1=50 +lat_2=90 +lat_0=-90 +lon_0=-10 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
raster::plot(graticulesConic, col = "gray80",main = "Albers Equal Area")
raster::plot(worldConic, col = "gray70", add = TRUE)

#' 
#' 
#' $\bf{\text Note:}$ The interactive map (https://projections.mgis.psu.edu/) shows different projections. Each projection has a set of instructions on how to distort the earth. For a detailed information, see PROJ (https://proj.org/index.html).
#' 
#' ## Projecting spatial data
#' 
#' It is simple to project spatial data in R. We need to know whether an object has a coordinate reference system or not. We will use a $\tt{SpatialPolygonDataFrame}$ that we can get through the $\tt{raster}$ package.
#' 
## ----------------------------------------------------------------------------------------------------------------
States <- readRDS("../Dataset/GADM_2.8_USA_adm1.rds")
States
States <- States[States$NAME_1 != "Alaska" & States$NAME_1 != "Hawaii",]

plot(States)

#' 
#' 
#' ## Access projection of object
#' 
#' We can find the projection information under the $\tt{coord. ref.}$ field. 
#' 
## ----------------------------------------------------------------------------------------------------------------
# Use the crs() function in raster
raster::crs(States)

# access the projection slot directly
States@proj4string

# access projection as character 
# - this can be very useful when 
#   using the projection of one
#   object to project another
States@proj4string@projargs

#' 
#' 
#' ## Projection using $\tt{sp}$ package
#' 
#' The $\tt{spTransform}$ function makes projecting $\tt{Spatial}$ objects possible with only a single line of code. We will change the projection from WGS84 into North America Lambert Equal Area. One way to assign the projection is to refer to the EPSG authority number. 
#' 
#' Best practice is to use the EPSG code (https://epsg.io) because the database is updated and if changes occur to the projection it will update your map accordingly if you re-run the code. Providing a character string of the projection does will not automatically update unless you manually go back and change the values. 
#' 
## ----------------------------------------------------------------------------------------------------------------
# Define the proj4 string 
EqArea <- "+proj=aea 
           +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
           +ellps=GRS80 
           +datum=NAD83 
           +units=m +no_defs"

# project using the string of the proj4 definition
States_EqArea2 <- sp::spTransform(x = States, CRSobj = CRS(EqArea))

# project using the ESPG authority number
States_EqArea1 <- sp::spTransform(x = States, CRS("+init=epsg:5070"))

par(mar = c(0,0,2,0),mfrow = c(1,2))
raster::plot(States_EqArea1, col = "gray70", border = "white", main = "States_EqArea1")
raster::plot(States_EqArea2, col = "gray70", border = "white", main = "States_EqArea2")


#' 
## ----------------------------------------------------------------------------------------------------------------
# Remove the coordinate reference system (crs) from States
crs(States) <- NA
States

#' 
#' 
#' ## Defining projection to $\tt{SpatialObject}$
#' 
#' If your $\tt{SpatialObject}$ does not have a coordinate reference system defined it will not know how to project the data. In that case, you need to assign it. 
#' 
## ----------------------------------------------------------------------------------------------------------------
# Let's take a look at the extent of the 
# shapefile to give us a clue to what projection
# it may be. 
States

# Define WGS84 in proj4 string format
WGS84 <- "+proj=longlat +datum=WGS84 
          +no_defs +ellps=WGS84 +towgs84=0,0,0"

# use the crs() function in the raster package
# to define the projection
crs(States) <- WGS84

# take a look
States

#' 
#' 
#' ## Projecting rasters
#' 
#' For mapping purpose we will go over how to project rasters. Again we use the NDVI raster from January 2018.
#' 
## ----------------------------------------------------------------------------------------------------------------
# read in raster layer using raster function
# NDVI <- raster("path/to/raster/file")
NDVI <- raster::raster("../Dataset/MOD_NDVI_M_2018-01-01_rgb_3600x1800.FLOAT.TIFF")

#' 
#' The NDVI raster is pretty large. It has 6,480,000 cells. It takes a little while to project the raster.
#' 
## ----------------------------------------------------------------------------------------------------------------
a <- Sys.time()
NDVIproj <- raster::projectRaster(from = NDVI, crs = sp::CRS("+init=epsg:5070"))
Sys.time()-a


#' 
## ----------------------------------------------------------------------------------------------------------------
NDVIproj <- raster("../Dataset/MOD_NDVI_M_2018-01-01_rgb_3600x1800.FLOAT_proj.tif")

NDVIproj

NDVIproj[NDVIproj>5] <- NA

par(bty = "n")
plot(NDVIproj, axes = FALSE, legend.args = list("NDVI"))

#' 
#' 
#' 
#' # spatial data and the tidyverse
#' 
#' Compared to other data science topics, analysis of spatial data using the $\tt{tidyverse}$ is relatively underdeveloped. Because the common spatial packages ($\tt{sp}$, $\tt{rgdal}$, and $\tt{rgeos}$) use S4 objects to represent spatial data, they do not play nice with tidyverse packages. 
#' 
#' The powerful new spatial package $\tt{sf}$ (stands for `Simple Features`) is starting to bride the divide. $\tt{sf}$ is built around data frames, so manipulation of $\tt{sf}$ objects is generally more intuitive even if you do not want to use the tidyverse. 
#' 
#' Since $\tt{sf}$ appears to be the future of spatial analysis in R, especially with regards to the tidyverse, we will start with a brief introduction to the structure, creation, and visualization of $\tt{sf}$ objects in R.
#' 
#' 
#' ## Structure of $\tt{sf}$ objects
#' 
#' The Simple Features standard is used to represent geographic vector data by many GIS software, including PostGIS, GeoJSON, and ArcGIS. A simple feature contains, at a minimum, a geometry that includes the coordinates of one or more points. Simple features may also contain (and often do) lines connecting the points, a CRS, and attributes associated with each geographic element.
#' 
#' The basics units of $\tt{sf}$ objects are called $\tt{sfg}$ objects. They provide the coordinates, dimension, and type of geometry for a single spatial feature. The $\tt{sf}$ package supports seven geometry types:
#' 
#' * $\tt{POINT}$
#' 
#' * $\tt{MULTIPOINT}$
#' 
#' * $\tt{LINESTRING}$
#' 
#' * $\tt{MULTILINESTRING}$
#' 
#' * $\tt{POLYGON}$
#' 
#' * $\tt{MULTIPOLYGON}$
#' 
#' * $\tt{GEOMETRYCOLLECTION }$ (any combination of the other 6 types)
#' 
#' $\bf{\text Note:}$ All functions in the $\tt{sf}$ package start with $\tt{st\_}$.
#' 
#' We will use the $\tt{st\_point()}$ function to create individual $\tt{sfg}$ objects for four Alaskan cities. The function requires a vector containing the longitude and latitude of each point.
#' 
## ----------------------------------------------------------------------------------------------------------------
#library(sf)
#library(tidyverse)

ju_sfg <- st_point(c(-134.4333, 58.3059)) #Juneau
an_sfg <- st_point(c(-149.8631, 61.2174)) #Anchorage
fa_sfg <- st_point(c(-147.7767, 64.8354)) #Fairbanks
nm_sfg <- st_point(c(-165.4064, 64.5011)) #Nome

# Extract the coordinates of an sfg object
st_coordinates(ju_sfg)

#' 
#' To create MULTIPOINT or LINESTRING $\tt{sfg}$ objects, we combine the coordinates of the individual points into a matrix, which is then used as the input argument to the corresponding functions.
#' 
## ----------------------------------------------------------------------------------------------------------------
## Create MULTIPOINT object
ak_sfg <- st_multipoint(rbind(c(-134.4333, 58.3059), 
                              c(-149.8631, 61.2174),
                              c(-147.7767, 64.8354),
                              c(-165.4064, 64.5011)))
plot(ak_sfg)

#' 
## ----------------------------------------------------------------------------------------------------------------
# Create LINESTRING object
ak_sfg <- st_linestring(rbind(c(-134.4333, 58.3059), 
                              c(-149.8631, 61.2174),
                              c(-147.7767, 64.8354),
                              c(-165.4064, 64.5011)))
plot(ak_sfg)

#' 
#' 
#' ## $\tt{sfc}$ class objects
#' 
#' The $\tt{sfg}$ objects we created contain coordinates and the geometry type of spatial objects but $\tt{sfg}$ objects are not truly geospatial objects because they lack a coordinate reference system (CRS). 
#' 
#' The $\tt{sf}$ package uses another object class called $\tt{sfc}$ to add a CRS to one or more $\tt{sfg}$ objects. To create a $\tt{sfc}$ object, we use (you guessed it) the $\tt{st\_sfc}$ function
#' 
#' $\tt{st\_sfc}$ takes one or more $\tt{sfg}$ objects, and a $\tt{crs}$ attribute, which can be an $\tt{epsg}$ code or a $\tt{proj4string}$ string. We use $\tt{epsg}$ 4326, which corresponds to latitude and longitude coordinates on the WGS84 ellipsoid. The $\tt{crs}$, $\tt{epsg}$, geometry type, dimensions, and individual $\tt{sfg}$ objects can be viewed by printing the $\tt{sfc}$ objects in the console.
#' 
## ----------------------------------------------------------------------------------------------------------------
cities_sfc <- st_sfc(ju_sfg, an_sfg, fa_sfg, nm_sfg, crs = 4326)
cities_sfc

st_crs(cities_sfc)

# Sfc objects can be easily visualized
plot(cities_sfc)

#' 
#' 
#' ## $\tt{sf}$ objects
#' 
#' The $\tt{sf}$ object contains all of the geospatial data associated with our points but generally we want to include attributes along with the spatial data.
#' 
#' One of the advantages of $\tt{sf}$ object is that they are just data frames.
#' 
## ----------------------------------------------------------------------------------------------------------------
# Create data.frame with attributes
cities_df <- data.frame(Name = c("Juneau", "Anchorage", "Fairbanks", "Nome"),
                        Population = c(31276, 291826, 3598, 31535),
                        Elevation = c(17, 31, 6, 136))

# Combine data.frame and spatial data
cities_sf <- st_sf(cities_df, geometry = cities_sfc)

cities_sf

class(cities_sf)

str(cities_sf)


#' 
#' 
#' ## Creating $\tt{sf}$ objects from other spatial objects
#' 
#' $\tt{st\_as\_sf()}$ can be used to convert other types of spaital objects to class $\tt{sf}$. Here is the example of creating a polygon containing the borders of Alaska from a shapefile.
#' 
## ----------------------------------------------------------------------------------------------------------------
ak <- raster::shapefile("../Dataset/ak.shp")
class(ak)


#' 
#' Here $\tt{ak}$ is a $\tt{SpatialPolygonsDataframe}$. Next, let's convert $\tt{ak}$ to a $\tt{sf}$ object and set the CRS.
#' 
## ----------------------------------------------------------------------------------------------------------------
### Covert from SpatialPolygonsDataframe to sf
ak_sf <- st_as_sf(ak)

### Set CRS to WGS84
ak_sf <- st_transform(ak_sf, crs = 4326)

### View object
ak_sf

#' 
#' Once we have an $\tt{sf}$ object, manipulation using the tidyverse is straightforward. 
#' 
#' 
#' ## Basic maps using $\tt{ggplot2}$
#' 
#' The recent version of $\tt{ggplot2}$ gives us access to a new functionality: $\tt{geom\_sf()}$. Unlike other $\tt{ggplot2}$ figures, where the user needs to specify a specific geometry (e.g., $\tt{geom\_point()}$, $\tt{geom\_polygon()}$), $\tt{geom\_sf()}$ can plot different geometries depending on the geometry type of the $\tt{sf}$ object. $\tt{geom\_sf()}$ requires a column called $\tt{geometry}$ and then it takes care of the rest.
#' 
## ----------------------------------------------------------------------------------------------------------------
#library(ggplot2)
ggplot() +
  geom_sf(data = ak_sf) +     # Alaska border polygon
  geom_sf(data = cities_sf, color = "red", size = 3) +    # Cities
  theme_minimal()

#' 
#' Now we will make use point size to show differences in population size of the cities by setting $\tt{size=Population}$.
#' 
## ----------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_sf(data = ak_sf) +
  geom_sf(data = cities_sf, color = "red",
          aes(size = Population), 
          show.legend = "point") + 
  theme_minimal()

#' 
#' Finally, let's label the cities.
#' 
## ----------------------------------------------------------------------------------------------------------------
cities_sf <- mutate(cities_sf, 
                    x = purrr::map_dbl(geometry, 1), 
                    y = purrr::map_dbl(geometry, 2))

ggplot() +
  geom_sf(data = ak_sf) +
  geom_sf(data = cities_sf, color = "red",
          aes(size = Population), 
          show.legend = "point") + 
  geom_text(data = cities_sf, 
            aes(x = x, y = y, label = Name), 
            hjust = 1.2) +
  theme_minimal() +
  theme(axis.title = element_blank())

#' 
#' There may be many other tweaks we would make before being satisfied with this map.

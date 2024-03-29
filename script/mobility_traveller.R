#### Imports ####

# set working directory
curr_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(curr_path))
#Sys.setenv(LANG = "en")
#Sys.setlocale("LC_TIME", "C")

# load data
library(rgdal)
us.state <- readOGR("../data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
airport <- read.csv("../data/mobility_data/airport-codes_csv.csv")
airtraffic <- read.csv("../data/mobility_data/flightlist_20200701_20200731.csv")

#### Preprocessing ####

## air traffic ##

# filter out flights without destination airport code
airtraffic.filtered <- airtraffic[airtraffic$destination!="",]
rm(airtraffic)

# aggregate count by destination
airtraffic.count <- aggregate(callsign~destination, data = airtraffic.filtered, FUN = length)
colnames(airtraffic.count)[2] <- "count"
rm(airtraffic.filtered)
head(airtraffic.count,5)

## merging of air traffic & airport ##

# merge with airport
airtraffic.airport <- merge(airtraffic.count, airport, 
                       by.x = "destination", by.y = "ident", all.y = TRUE)
rm(airport)

## airport ##

# filter to US
airtraffic.us <- airtraffic.airport[airtraffic.airport$iso_country=="US",]

# filter out airport with no coordinates
airtraffic.us <- airtraffic.us[!is.na(airtraffic.us$coordinates),]
summary(airtraffic.us$count)

# fill NA with 0 count
airtraffic.us$count[is.na(airtraffic.us$count)] <- 0

# define coordinates
library(tidyr)
airtraffic.coord <- separate(data = airtraffic.us, col = coordinates, into = c("long", "lat"), sep = ",")
airtraffic.coord$long <- as.numeric(airtraffic.coord$long)
airtraffic.coord$lat <- as.numeric(airtraffic.coord$lat)
coords.tmp <- cbind(airtraffic.coord$long, airtraffic.coord$lat)

# create spatial point df
library(sp)
airtraffic.sp <- SpatialPointsDataFrame(coords = coords.tmp, data = airtraffic.coord)

# histogram of count
hist(airtraffic.sp$count, 
     main="Histogram of Flight Count at US Airports", 
     xlab="flight count")

# log count
airtraffic.sp$logcount <- log(airtraffic.sp$count+1)
summary(airtraffic.sp$logcount)

# histogram of log count
hist(airtraffic.sp$logcount, 
     main="Histogram of Logged Flight Count at US Airports", 
     xlab="logged flight count")

## US states polygon ##

# filter to 48 states
us.state <- us.state[!(us.state$STUSPS %in% c("AK","HI","PR")),]

#### EDA ####

library(tmap)
tmap_mode('plot')

# plot air traffic of airports
tm_shape(us.state) + tm_polygons('grey') +
  tm_shape(airtraffic.sp) + 
  tm_dots("logcount", palette = "Reds", title="Logged flight count", alpha=0.5) +
  tm_layout(main.title="Air Traffic in US Airports",
            main.title.size=1.2, main.title.position="centre",
            legend.title.size=1,legend.text.size=0.7, legend.position=c("right", "bottom")) +
  tm_compass(type="rose", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(position = c("left", "bottom")) +
  tmap_style("beaver")

#### Interpolation ####

## proximity approach ##

library(spatstat)
library(maptools)
library(raster)
th <- as(dirichlet(as.ppp(airtraffic.sp)), "SpatialPolygons") 
proj4string(th) <- proj4string(airtraffic.sp)
th.z <- over(th, airtraffic.sp, fn=mean) 
th.spdf <- SpatialPolygonsDataFrame(th, th.z) 
th.clp <- raster::intersect(us.state,th.spdf)

# plot interpolated traveller movement
tm_shape(th.clp) + 
  tm_fill(col="logcount", palette="Purples", title="Interpolated traveller movement") +
  tm_shape(us.state) +
  tm_polygons(alpha = 0) +
  tm_layout(main.title="Interpolated Traveller Movement in US",
            main.title.size=1.2, main.title.position="centre",
            legend.title.size=0.8,legend.text.size=0.7, legend.position=c("right", "bottom")) +
  tm_compass(type="rose", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(position = c("left", "bottom")) 

## IDW approach ##
  
library(gstat)
library(sp)
airtraffic.sp <- SpatialPointsDataFrame(coords = coords.tmp, data = airtraffic.coord,proj4string = CRS("+proj=longlat"))
airtraffic.sp$logcount <- log(airtraffic.sp$count+1)
grd <- as.data.frame(spsample(airtraffic.sp, "regular", n=50000)) 
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y") 
gridded(grd) <- TRUE 
fullgrid(grd) <- TRUE 
proj4string(airtraffic.sp) <- proj4string(airtraffic.sp)
proj4string(grd) <- proj4string(airtraffic.sp)
airtraffic.idw <- gstat::idw(logcount ~ 1, airtraffic.sp, newdata=grd, idp=2)
r <- raster(airtraffic.idw)
r.m <- mask(r, us.state) 

# crop raster with row and column numbers
r.m.cropped <- crop(r.m, extent(r.m, 30, 75, 70, 180))

# plot interpolated traveller movement
tm_shape(r.m.cropped) + 
  tm_raster(n = 5, palette = "Purples", title="Interpolated traveller movement") +
  tm_shape(us.state) +
  tm_polygons(alpha = 0) +
  tm_layout(main.title="Interpolated Traveller Movement in US (IDW)",
            main.title.size=1.2, main.title.position="centre",
            legend.title.size=0.8,legend.text.size=0.7, legend.position=c("right", "bottom")) +
  tm_compass(type="rose", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(position = c("left", "bottom"))

## 2nd order trend surface approach ##

f.2 <- as.formula(logcount ~ X + Y + I(X*X) + I(Y*Y) + I(X*Y))
airtraffic.sp$X <- airtraffic.sp$long
airtraffic.sp$Y <- airtraffic.sp$lat
lm.2 <- lm( f.2, data=airtraffic.sp) 
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 
r <- raster(dat.2nd) 
r.m <- mask(r, us.state) 

# crop raster with row and column numbers
r.m.cropped <- crop(r.m, extent(r.m, 30, 75, 70, 180))

# plot interpolated traveller movement
tm_shape(r.m.cropped) + 
  tm_raster(n = 5, palette = "Purples", title="Interpolated traveller movement") +
  tm_shape(us.state) +
  tm_polygons(alpha = 0) +
  tm_layout(main.title="Interpolated Traveller Movement in US (2nd order trend surface)",
            main.title.size=1.2, main.title.position="centre",
            legend.title.size=0.8,legend.text.size=0.7, legend.position=c("right", "bottom")) +
  tm_compass(type="rose", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(position = c("left", "bottom"))

## kriging approach ##

var.smpl <- variogram(f.2, airtraffic.sp, cloud = FALSE, cutoff=1000000, width=89900) 
dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE, vgm(psill=14, model="Sph", range=590000, nugget=0)) 
dat.krg <- krige( f.2, airtraffic.sp, grd, dat.fit) 
r <- raster(dat.krg) 
r.m <- mask(r, us.state) 

# crop raster with row and column numbers
r.m.cropped <- crop(r.m, extent(r.m, 30, 75, 70, 180))

# plot interpolated traveller movement
tm_shape(r.m.cropped) + 
  tm_raster(n = 5, palette = "Purples", title="Interpolated traveller movement") +
  tm_shape(us.state) +
  tm_polygons(alpha = 0) +
  tm_layout(main.title="Interpolated Traveller Movement in US (2nd order trend surface)",
            main.title.size=1.2, main.title.position="centre",
            legend.title.size=0.8,legend.text.size=0.7, legend.position=c("right", "bottom")) +
  tm_compass(type="rose", position = c("left", "bottom"), size = 4) +
  tm_scale_bar(position = c("left", "bottom"))
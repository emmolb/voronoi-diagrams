# Voronoi

library(maptools)
library(rgdal)

voronoipolygons = function(layer) {
  require(deldir)
  crds = layer@coords
  z = deldir(crds[,1], crds[,2])
  w = tile.list(z)
  polys = vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], 
                                                         y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                      function(x) slot(x, 'ID'))))
}

# Read in a point shapefile to be converted to a Voronoi diagram
library(rgdal)
dsn <- system.file("vectors", package = "rgdal")[1]
cities <- readOGR(dsn=dsn, layer="cities")
class(cities)
str(cities)
v <- voronoipolygons(cities)
plot(v)



setwd("c:/R/Rdata")
data <- read.csv("ziekenhuizen_wvl_geocoded.csv",header=T)
str(data)
data

hospitals <- subset(data,select=c(long,lat))
str(hospitals)
names(hospitals) <- c("x","y")
str(hospitals)
hospitals
# create a spatialpointsdataframe
library(maptools)
hospitals.spdf <- SpatialPointsDataFrame(cbind(hospitals$x,hospitals$y),hospitals)
class(hospitals.spdf)
str(hospitals.spdf)
plot(hospitals.spdf)
proj4string(hospitals.spdf)
proj4string(hospitals.spdf) <- CRS("+proj=longlat")
h <- voronoipolygons(hospitals.spdf)
plot(h)

setwd("c:/circulation")
writeOGR(hospitals.spdf,".","ziekenhuis",driver="ESRI Shapefile")
Z <- readOGR(".","ziekenhuis")
class(Z)
proj4string(Z)
plot(Z)
Z2 <- voronoipolygons(Z)
  
coords<-data.frame(LONG=c(16.9252,16.9363,16.9408,16.8720,16.9167,16.9461,16.9093,16.9457,16.9171,16.8506,16.9471,16.8723,16.9444,16.9212,16.8809,16.9191,16.8968,16.8719,16.9669,16.8845),
                   LAT=c(52.4064,52.4266,52.3836,52.3959,52.4496,52.3924,52.4012,52.3924,52.3777,52.4368,52.4574,52.3945,52.4572,52.3962,52.3816,52.3809,52.3956,52.3761,52.4236,52.4539))
class(coords)
coords.spdf <- SpatialPointsDataFrame(cbind(coords$LONG,coords$LAT),coords)
class(coords.spdf)
plot(coords.spdf)
c <- voronoipolygons(coords.spdf)
plot(c)
proj4string(coords.spdf)
proj4string(coords.spdf) <- CRS("+proj=longlat")












#Let's generate some fake data
set.seed(105)
long<-rnorm(20,-98,15)
lat<-rnorm(20,39,10)
df <- data.frame(lat,long)

library(deldir)
library(ggplot2)

#This creates the voronoi line segments
voronoi <- deldir(df$long, df$lat)
class(voronoi)
plot(voronoi)


#Now we can make a plot
ggplot(data=df, aes(x=long,y=lat)) +
  #Plot the voronoi lines
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 2,
    data = voronoi$dirsgs,
    linetype = 1,
    color= "#FFB958") + 
  #Plot the points
  geom_point(
    fill=rgb(70,130,180,255,maxColorValue=255),
    pch=21,
    size = 4,
    color="#333333") +
  #(Optional) Specify a theme to use
  ltd_theme


#This creates the voronoi line segments
voronoi <- deldir(hospitals$x, hospitals$y)
class(voronoi)
plot(voronoi)


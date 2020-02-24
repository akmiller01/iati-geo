list.of.packages <- c("sp","rgdal","leaflet","data.table","ggplot2","scales","rgeos","maptools","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/iati-geo/output/"
setwd(wd)

agg <- fread("iati_unfiltered_agg.csv")

location_names = subset(agg,location_coordinates_lat=="" & location_point_pos=="" & location_name!="")
v1_points = subset(agg,location_coordinates_lat!="")
v2_points = subset(agg,location_point_pos!="")

rm(agg)

coordinates(v1_points)=~location_coordinates_long+location_coordinates_lat
plot(v1_points)

split_points = colsplit(v2_points$location_point_pos, " ", c("lat", "long"))
v2_points$lat = as.numeric(split_points$lat)
v2_points$long = as.numeric(split_points$long)
v2_points = subset(v2_points,!is.na(lat) & !is.na(long))
v2_points = subset(v2_points,abs(lat)<=80 & abs(long)<=180)
coordinates(v2_points)=~long+lat
plot(v2_points)
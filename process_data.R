# Instructions: 
# Option 1: Load in location_workspace below. Then, only run introduction and 'Further scoping analysis' section onwards.
# Option 2: Run the whole thing with geocoding commented out (slow).
# Note: It is important we don't lose the geo_cache_output.df since this will be chalenging to run again (both time and duplication not being liked by the API)

load("C:/git/iati-geo/location_workspace.RData")

#### Introduction and initial manipulation ####

# Install packages required

list.of.packages <- c("rgdal","leaflet","data.table","ggplot2","tmap","sf","tmaptools","countrycode","openxlsx","grDevices","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Set up working directory

if(Sys.info()[["user"]]=="deanb"){
  prefix <- "C:" 
}else{
  prefix <- "~"
}
wd = paste0(prefix,"/git/iati-geo/output")
setwd(wd)

#### Read in initial data ####

agg <- read.csv("iati_unfiltered_agg.csv",na.strings="",as.is=T)

# Separate data into old-style coordinates, new-style coordinates and no coordinates

location_names = subset(agg,is.na(location_coordinates_lat) & is.na(location_point_pos) & location_name!="")
v1_points = subset(agg,location_coordinates_lat!="")
v1_points$lat=v1_points$location_coordinates_lat
v1_points$long=v1_points$location_coordinates_long
v2_points = subset(agg,location_point_pos!="")
coordinates(v1_points)=~location_coordinates_long+location_coordinates_lat
split_points = colsplit(v2_points$location_point_pos, " ", c("lat", "long"))
v2_points$lat = as.numeric(split_points$lat)
v2_points$long = as.numeric(split_points$long)
v2_points = subset(v2_points,!is.na(lat) & !is.na(long))
v2_points = subset(v2_points,abs(lat)<=80 & abs(long)<=180)
coordinates(v2_points)=~long+lat

#### Geocoding where coordinates are not available using OSM ####

# unique_names=data.frame(unique(location_names$location_name))
# unique_names$'unique.location_names.location_name.'=enc2utf8(as.character(unique_names$'unique.location_names.location_name.'))
# unique_names=subset(unique_names,unique_names$'unique.location_names.location_name.'!="N/A" & unique_names$'unique.location_names.location_name.'!="NA")
# unique_names$'unique.location_names.location_name.'=gsub("#39","",unique_names$'unique.location_names.location_name.')
# names(unique_names)[1]="query"
# 
# geo_cache = list()
# osm_geocode = function(place_name, agent="dean.breed@devinit.org;IATI_geocoder"){
#   if(!(place_name %in% names(geo_cache))){
#     Sys.sleep(1)
#     query_url = paste0(
#       "https://nominatim.openstreetmap.org/search?format=json&q=",
#       URLencode(place_name)
#     )
#     geo_cache[[place_name]] <<- fromJSON(query_url)
#   }
#   return(geo_cache[[place_name]])
# }
# 
# while (length(geo_cache)<length(unique_names$query)){
# for (i in c((length(geo_cache)+1):(length(geo_cache)+500))){
# osm_geocode(as.character(unique_names$query[i]))}
# }
# 
# geo_cache_output=geo_cache[lengths(geo_cache) > 0L]
# geo_cache_output.df=as.data.frame(sapply(geo_cache_output, function(x) {output=c(x$place_id[1],x$lat[1],x$lon[1],x$class[1],x$type[1],x$importance[1])
#                               return(output)}))
# geo_cache_output.df=as.data.frame(t(geo_cache_output.df))
# geo_cache_output.df$location_name=rownames(geo_cache_output.df)
# rownames(geo_cache_output.df)=NULL
# colnames(geo_cache_output.df)=c("place_id","lat","long","class","type","importance","location_name")
# 
# no_geolocation = merge(geo_cache_output.df,location_names,by=c("location_name"),all.y=TRUE)
# no_geolocation$place_id=NULL
# no_geolocation$class=NULL
# no_geolocation$type=NULL
# no_geolocation$importance=NULL
# no_geolocation$long=as.numeric(levels(no_geolocation$long))[no_geolocation$long]
# no_geolocation$lat=as.numeric(levels(no_geolocation$lat))[no_geolocation$lat]
# no_geolocation = subset(no_geolocation,abs(lat)<=80 & abs(long)<=180)
# coordinates(no_geolocation)=~long+lat

#### Bringing together data frames of where there is a geolcation and where there is not ####

no_geolocation=location_names
v1_points.f=as.data.frame(v1_points)
v2_points.f=as.data.frame(v2_points)
geolocation.f=rbind(v1_points.f,v2_points.f)
geolocation.f$geolocation_status=1
no_geolocation.f=data.frame(no_geolocation)
no_geolocation.f$lat=NA
no_geolocation.f$long=NA
no_geolocation.f$geolocation_status=0
no_geolocation.f$optional=NULL
all_entries=rbind(geolocation.f,no_geolocation.f)
all_entries.dt$reporting_org=as.character(levels(all_entries.dt$reporting_org))[all_entries.dt$reporting_org]
all_entries.dt$receiving_country=as.character(levels(all_entries.dt$receiving_country))[all_entries.dt$receiving_country]
all_entries.dt=data.table(all_entries)

#### Further scoping analysis ####

geolocation.f=subset(all_entries.dt,geolocation_status==1)
no_geolocation.f=subset(all_entries.dt,geolocation_status==0)

figurea=nrow(subset(geolocation.f,location_exactness==1))
figureb=nrow(subset(geolocation.f,location_exactness==2))
figurec=nrow(subset(geolocation.f,is.na(location_exactness)))

figures=table(geolocation.f$activity_scope)

#### Analysis of geolocation availability by donor and by country ####

all_transactions.dt=subset(all_entries.dt,all_entries.dt$'budget_or_transaction'=="Transaction"&!(all_entries.dt$activity_scope %in% c("1","2","3"))) # Filter by transactions only and national level or below (or unfilled) only.

rm(list=c("geolocation.f","no_geolocation.f","figurea","figureb","figurec","figures"))

# By country

all_transactions_rec.dt = all_transactions.dt[,.(total_entries=.N,total_spent=sum(usd_disbursement, na.rm=TRUE),geolocation_entries=sum(geolocation_status==1,na.rm=T),exactness_entries=sum(location_exactness==1,na.rm=T)),by=.(recipient_country_code)]
all_transactions_rec.dt$geolocation_percentage = all_transactions_rec.dt$geolocation_entries / all_transactions_rec.dt$total_entries
all_transactions_rec.dt$exactness_percentage = all_transactions_rec.dt$exactness_entries / all_transactions_rec.dt$total_entries
write.csv(all_transactions_rec.dt,file="iati-analysis/recipients.csv")

# By donor only
all_transactions_rep.dt = all_transactions.dt[,.(total_entries=.N,total_spent=sum(usd_disbursement, na.rm=TRUE),geolocation_entries=sum(geolocation_status==1,na.rm=T),exactness_entries=sum(location_exactness==1,na.rm=T)),by=.(reporting_org)]
all_transactions_rep.dt$geolocation_percentage = all_transactions_rep.dt$geolocation_entries / all_transactions_rep.dt$total_entries
all_transactions_rep.dt$exactness_percentage = all_transactions_rep.dt$exactness_entries / all_transactions_rep.dt$total_entries
write.csv(all_transactions_rep.dt,file="iati-analysis/reporters.csv")

rm(list=c("all_transactions_rec.dt","all_transactions_rep.dt"))

#### Subnational - MICS ####

load("C:/git/iati-geo/location_workspace.RData")

if(Sys.info()[["user"]]=="deanb"){
  prefix <- "C:" 
}else{
  prefix <- "~"
}
wd = paste0(prefix,"/git/iati-geo/output")
setwd(wd)

# Read in from the MICS recode activity

{
  install.packages(c("rgeos","maptools","countrycode"))
  library(rgeos)
  library(maptools)
  library(countrycode)
}

load("C:/git/MICS_recode/project_data/ITEP_HC_mics.RData")
load("C:/git/MICS_recode/project_data/ITEP_HC_mics_vars.RData")

names(longlist)=c("Cameroon","Guinea","Bangladesh")
list2env(longlist,.GlobalEnv)
names(varlist)=c("CMR","GIN","BGD")
list2env(varlist,.GlobalEnv)

#### Analysis by country ####

for (country in names(longlist)){
  
  iso2=countrycode(country,"country.name","iso2c")
  iso3=countrycode(country,"country.name","iso3c")
  regions <- rgdal::readOGR(dsn = paste0("country-analysis/maps/",country,"/gadm36_",iso3,"_1.shp"),layer = paste0("gadm36_",iso3,"_1"))
  regions@data$NAME_1=iconv(regions@data$NAME_1, "UTF8", "ASCII//TRANSLIT")
  
  # Extract points from IATI dataset for transaction activities
  
  entries=data.frame(subset(all_entries.dt,all_entries.dt$recipient_country_code==iso2&all_entries.dt$'budget_or_transaction'=="Transaction"))
  coordinates(entries)<- ~ long + lat
  proj4string(entries) <- proj4string(regions)
  entries=data.table(cbind(as.data.frame(entries),over(entries,regions)))[,.(total_spent=sum(usd_disbursement, na.rm=TRUE)),by=.(NAME_1)]
  names(entries)=c("region","total_spent")
  entries=subset(entries,!is.na(entries$region))
  
  # Read in populations
  
  population=read.csv(file="./populations/Subnational-Population_csv/Subnational-PopulationData.csv",na.strings="",as.is=T)
  population=population[which(substr(population$Country.Code,1,3)==iso3),]
  population=subset(population,population$Indicator.Name=="Population, total")
  population$region=substr(population$"ï..Country.Name",nchar(country)+3,nchar(population$"ï..Country.Name"))
  population=population[which(population$region!=""),]
  population=population[,c("region","X2016")]
  names(population)=c("region","population")
  entries=merge(population,entries, by="region")
  entries$total_spent_per_cap=round(entries$total_spent/entries$population,0)
  
  # Extract MICS data for CA1 and CA5
  
  data_holder=get(country)
  data_holder$ch.CA1.count=NA
  data_holder$ch.CA1.count[which(data_holder$ch.CA1 %in% c(1,2,8,9))]=0
  data_holder$ch.CA1.count[which(data_holder$ch.CA1==1)]=1
  data_holder$ch.CA5.count=NA
  data_holder$ch.CA5.count[which(data_holder$ch.CA5 %in% c(1,2,8,9))]=0
  data_holder$ch.CA5.count[which(data_holder$ch.CA5==1)]=1
  
  # Match up levels - general
  
  mapping1=regions@data$NAME_1
  mapping2=as.data.frame(attr(data_holder$HH7,"value.labels"),row.names=NULL)
  mapping2=cbind(mapping2,rownames(mapping2))
  colnames(mapping2)=c("number","region")
  rownames(mapping2)=NULL
  mapping2$region=iconv(mapping2$region, "latin1", "ASCII//TRANSLIT")
  mapping2$number=as.numeric(as.character(mapping2$number))
  for (i in 1:nrow(mapping2)){
    data_holder$HH7[which(data_holder$HH7==as.numeric(mapping2$number[i]))] <- as.character(mapping2$region[i])
  }
  
  # Match up levels - country-specific
  data_holder$HH7[which(data_holder$HH7%in% c("Yaounde","Centre (sans Yaounde)"))] <- "Centre"
  data_holder$HH7[which(data_holder$HH7 %in% c("Douala","Littoral (sans Douala)"))] <- "Littoral"
  data_holder$HH7[which(data_holder$HH7 == "N'Zerekore")] <- "Nzerekore"
  data_holder$HH7[which(data_holder$HH7 == "Chattogram")] <- "Chittagong"
  data_holder$HH7[which(data_holder$HH7 == "Barishal")] <- "Barisal"
  
  data_holder_CA5=subset(data_holder,data_holder$ch.CA5==1)
  country_data = data.table(data_holder)[,.(
    value1=mean(as.numeric(ch.CA1.count),na.rm=T),value5=mean(as.numeric(ch.CA5.count),na.rm=T)),by=.(HH7)]
  country_data_CA6 = data.table(data_holder_CA5)[,.(value6=mean(grepl("A|B",ch.CA6)),count=.N),by=.(HH7)]
  names(country_data)[1]=c("region")
  names(country_data_CA6)[1]=c("region")
  
  # Set color palettes
  palette_CA <- c("lightgray","lightgreen","darkgreen")
  
  # Merge shapefile with attributes
  colnames(regions@data)[which(names(regions@data) == "NAME_1")] <- "region"
  merged <- merge(regions,country_data,by = "region")
  merged <- merge(merged,entries,by = "region")
  merged <- merge(merged,country_data_CA6,by = "region")

  # Graphs
  CA1 <- tmap::tm_shape(merged) + 
    tmap::tm_polygons(col = "value1",style = "cat",palette = palette_CA,
                      title = paste0("Proportion of children that have experienced diarrhoea in last 2 weeks in ",country)) +
    # tmap::tm_text(c("total_spent_per_cap"),size = 0.7) +
    tmap::tm_layout(legend.title.size = 1,
                    legend.text.size = 0.6,
                    legend.bg.alpha = 1,
                    legend.outside = T,
                    legend.outside.position = "left",
                    frame = F)
  CA1
  tmap::tmap_save(CA1,paste0("country-analysis/maps/",country,"/output/CA1.png"))
  
  CA5 <- tmap::tm_shape(merged) + 
    tmap::tm_polygons(col = "value5",style = "cat",palette = palette_CA,
                      title = paste0("Sought advice or treatment for the diarrhoea in ",country)) +
    # tmap::tm_text(c("total_spent_per_cap"),size = 0.7) +
    tmap::tm_layout(legend.title.size = 1,
                    legend.text.size = 0.6,
                    legend.bg.alpha = 1,
                    legend.outside = T,
                    legend.outside.position = "left",
                    frame = F)
  CA5
  tmap::tmap_save(CA5,paste0("country-analysis/maps/",country,"/output/CA5.png"))
  
  CA6 <- tmap::tm_shape(merged) + 
    tmap::tm_polygons(col = "value6",style = "cat",palette = palette_CA,
                      title = paste0("Public hospital/health centre usage for diarrhea in ",country)) +
    # tmap::tm_text(c("total_spent_per_cap"),size = 0.7) +
    tmap::tm_layout(legend.title.size = 1,
                    legend.text.size = 0.6,
                    legend.bg.alpha = 1,
                    legend.outside = T,
                    legend.outside.position = "left",
                    frame = F)
  CA6
  tmap::tmap_save(CA6,paste0("country-analysis/maps/",country,"/output/CA6.png"))
  
  Spending <- tmap::tm_shape(merged) + 
    tmap::tm_polygons(col = "total_spent_per_cap",style = "cat",palette = palette_CA,
                      title = paste0("Total ODA spend per capita in regions of ",country)) +
    # tmap::tm_text(c("total_spent_per_cap"),size = 0.7) +
    tmap::tm_layout(legend.title.size = 1,
                    legend.text.size = 0.6,
                    legend.bg.alpha = 1,
                    legend.outside = T,
                    legend.outside.position = "left",
                    frame = F)
  Spending
  tmap::tmap_save(Spending,paste0("country-analysis/maps/",country,"/output/Spending.png"))
  
}

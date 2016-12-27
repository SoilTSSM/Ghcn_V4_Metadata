
#### Population SCript

source("libraries.R")
source("filenames.R")
source("getInventory.R")

GHCN <- getInventory(Inventory)

 

lonlat <-cbind(GHCN$Longitude,GHCN$Latitude)

 

#################################  Gpw  version 4

gpw_Pop_V4_00                <- raster(gpwV4_00) 
gpw_Pop_V4_05                <- raster(gpwV4_05) 
gpw_Pop_V4_10                <- raster(gpwV4_10) 
gpw_Pop_V4_15                <- raster(gpwV4_15)


GPwV4_00 <- raster::extract(gpw_Pop_V4_00, lonlat)
GPwV4_05 <- raster::extract(gpw_Pop_V4_05, lonlat)
GPwV4_10 <- raster::extract(gpw_Pop_V4_10, lonlat)
GPwV4_15 <- raster::extract(gpw_Pop_V4_15, lonlat)
GPwV4_Area <- raster::extract(area(gpw_Pop_V4_00), lonlat)


GHCN <- GHCN %>% mutate(GPwV4_Area=GPwV4_Area,GPwV4_00=GPwV4_00,GPwV4_05=GPwV4_05,GPwV4_10=GPwV4_10,GPwV4_15=GPwV4_15)

rm(GPwV4_00)
rm(GPwV4_05)
rm(GPwV4_10)
rm(GPwV4_15)
rm(GPwV4_Area)

Hyde  <- brick(HydePop)
earlyYears <- max(which(getZ(Hyde)< 1980))

H <- raster::extract(Hyde,y=lonlat,layer=earlyYears,nl= 1+ nlayers(Hyde)-earlyYears)
H_Area <-raster::extract(area(Hyde),y=lonlat,layer= nlayers(Hyde),nl= 1)
H<-as.data.frame(H)

GHCN <- GHCN %>% mutate(Hyde_Area=H_Area,
                        Hyde1970=H$X1970,Hyde1980=H$X1980,Hyde1990=H$X1990,Hyde2000=H$X2000,
                        Hyde2005=H$X2005)

GHCN <- GHCN %>% mutate( GpwV4_density00 = GPwV4_00/GPwV4_Area,
                         Hyde_density00 = Hyde2000/Hyde_Area)
                        
 
 
 




####################  GPW   version 4

 

GPwV4_15_10km<-raster::extract(gpw_Pop_V4_15, lonlat,buffer=10000, fun = sum)
   

GHCN <- GHCN %>% mutate(GPWV4_Area10 = pi*100,GPwV4_15_10km=GPwV4_15_10km )
                        
 
write.csv(GHCN, "GHCN_V4_Population.csv")

###########################################################


 

Settlement <- read.csv(GrumpCites,stringsAsFactors=F)


GHCN <- GHCN %>% mutate(PopulatedPlace=NA,Populated_Lon=NA,Populated_Lat=NA,Populated_Type=NA,EST_POP2000=NA,DistanceToPlace=NA)


for(s in 1:nrow(GHCN)){
  
  print(s)
  icoords <- cbind(GHCN$Longitude[s],GHCN$Latitude[s])
  bcoords <- cbind(Settlement$LONGITUDE,Settlement$LATITUDE)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  GHCN$PopulatedPlace[s]<-Settlement$SCHNM[o[1]]
  GHCN$Populated_Lon[s]<-Settlement$LONGITUDE[o[1]]
  GHCN$Populated_Lat[s]<-Settlement$LATITUDE[o[1]]
  GHCN$EST_POP2000[s]   <-Settlement$ES00POP[o[1]]
  GHCN$Populated_Type[s]<- Settlement$URBORRUR[o[1]]
  GHCN$DistanceToPlace[s]<- dist[o[1]]
  
  
}


GeoCity <- read.delim(GeoNameCities,stringsAsFactors = FALSE, header=F)

GeoCity <- GeoCity[ ,c(3,5,6,8,9,15,19)]

colnames(GeoCity)<-c("GeoPlace","GeoLat","GeoLon","FeatureCode","CountryCode","GeoPop","GeoUpdateDate")

GeoCity <- GeoCity[!is.na(GeoCity$GeoPop),]

GeoCity <- GeoCity[GeoCity$GeoPop >0,]

GeoCity$GeoLat <- as.numeric(GeoCity$GeoLat)
GeoCity$GeoLon <- as.numeric(GeoCity$GeoLon)

GHCN <- GHCN %>% mutate(GeoPlace=NA,Geo_Lon=NA,Geo_Lat=NA,FeatureCode=NA,GeoPop=NA,GeoUpdateDate=NA,DistanceToGeoPlace=NA)



for(s in 1:nrow(GHCN)){
  
  print(s)
  icoords <- cbind(GHCN$Longitude[s],GHCN$Latitude[s])
  bcoords <- cbind(GeoCity$GeoLon,GeoCity$GeoLat)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  GHCN$GeoPlace[s]<-GeoCity$GeoPlace[o[1]]
  GHCN$Geo_Lon[s]<-GeoCity$GeoLon[o[1]]
  GHCN$Geo_Lat[s]<-GeoCity$GeoLat[o[1]]
  GHCN$FeatureCode[s]   <-GeoCity$FeatureCode[o[1]]
  GHCN$GeoPop[s]<- GeoCity$GeoPop[o[1]]
  
  GHCN$GeoUpdateDate[s]<- GeoCity$GeoUpdateDate[o[1]]
  GHCN$DistanceToGeoPlace[s]<- dist[o[1]]
  
  
}



S50 <- Settlement[Settlement$ES00POP > 49999,]


GHCN <- GHCN %>% mutate(PopulatedPlace50K=NA,Populated_Lon50K=NA,Populated_Lat50K=NA, EST_POP2000_50K=NA,DistanceToPlace50K=NA)


for(s in 1:nrow(GHCN)){
  
  print(s)
  icoords <- cbind(GHCN$Longitude[s],GHCN$Latitude[s])
  bcoords <- cbind(S50$LONGITUDE,S50$LATITUDE)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  GHCN$PopulatedPlace50K[s]<-S50$SCHNM[o[1]]
  GHCN$Populated_Lon50K[s]<-S50$LONGITUDE[o[1]]
  GHCN$Populated_Lat50K[s]<-S50$LATITUDE[o[1]]
  GHCN$EST_POP2000_50K[s]   <-S50$ES00POP[o[1]]
   
  GHCN$DistanceToPlace50K[s]<- dist[o[1]]
  
  
}


write.csv(GHCN, "GHCN_V4_Population_Cities.csv")


MissingGWPV4 <- GHCN %>% filter(is.na(GPwV4_15)) %>% select(Station_ID,Longitude,Latitude,Name)

coordinates(MissingGWPV4) <- ~Longitude+Latitude
proj4string(MissingGWPV4) <-WGS84
 

shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"

kml(MissingGWPV4, labels = Station_ID, size=2, shape=shape, file="Nogwpv4.kml",color="blue")



MissingGWPV4_10K <- GHCN %>% filter(is.na(GPwV4_15)&is.na(GPwV4_15_10km)) %>% select(Station_ID,Longitude,Latitude,Name)

coordinates(MissingGWPV4_10K) <- ~Longitude+Latitude
proj4string(MissingGWPV4_10K) <-WGS84


shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"

kml(MissingGWPV4_10K, labels = Station_ID, size=2, shape=shape, file="Nogwpv4_10K.kml",color="blue")




ARP <- read.csv(Airports,stringsAsFactors=F)

###  eliminate seaplace, heliport, balloonport, and closed

ARP <- ARP[ARP$type %in% c("large_airport","medium_airport","small_airport"),]


GHCN <- GHCN %>% mutate(Airport_Type=NA,Airport_Lon=NA,Airport_Lat=NA, Airport_Name=NA,Airport_Dist=NA)

#####   Add Airports


for(s in 1:nrow(GHCN)){
  
  print(s)
  icoords <- cbind(GHCN$Longitude[s],GHCN$Latitude[s])
  
  bcoords <- cbind(ARP$longitude_deg,ARP$latitude_deg)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  
  GHCN$Airport_Type[s]<-ARP$type[o[1]]
  GHCN$Airport_Lon[s]<-ARP$longitude_deg[o[1]]
  GHCN$Airport_Lat[s]<-ARP$latitude_deg[o[1]]
  GHCN$Airport_Name[s]<- ARP$name[o[1]]
  GHCN$Airport_Dist[s]<- dist[o[1]]
  
  
}


ARP2<- ARP[ARP$type %in% c("large_airport","medium_airport"),]


GHCN <- GHCN %>% mutate(Airport_Type2=NA,Airport_Lon2=NA,Airport_Lat2=NA, Airport_Name2=NA,Airport_Dist2=NA)
 
#####   Add Airports


for(s in 1:nrow(GHCN)){
  
  print(s)
  icoords <- cbind(GHCN$Longitude[s],GHCN$Latitude[s])
  
  bcoords <- cbind(ARP2$longitude_deg,ARP2$latitude_deg)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  
  GHCN$Airport_Type2[s]<-ARP2$type[o[1]]
  GHCN$Airport_Lon2[s]<-ARP2$longitude_deg[o[1]]
  GHCN$Airport_Lat2[s]<-ARP2$latitude_deg[o[1]]
  GHCN$Airport_Name2[s]<- ARP2$name[o[1]]
  GHCN$Airport_Dist2[s]<- dist[o[1]]
  
  
}


DistCoast <- raster(Coast)

dc  <-  raster::extract(DistCoast, lonlat)

GHCN <- GHCN %>% mutate(DistancetoCoast = dc)




Lights <- raster(Night)

nl  <-  raster::extract(Lights, lonlat)

GHCN <- GHCN %>% mutate(Lights = nl)

Elevation <- raster(DEM)

elv <-  raster::extract(Elevation, lonlat)

GHCN <- GHCN %>% mutate(DEM1km = elv)


write.csv(GHCN, "GHCN_POP_Cities_Airports.csv")



TOPOGRAPHY <-raster(Topo)
ltype <-read.dbf(TopoTbl)
ltype <- ltype[,c("Value", "EF_LF_Desc","ELU_GLC_De" )]
 
LonLat <- GHCN[,c("Longitude","Latitude")]
coordinates(LonLat)<- ~Longitude+Latitude
proj4string(LonLat)<-projection(TOPOGRAPHY)

LandForm <- raster::extract(TOPOGRAPHY,LonLat)

GHCN <- GHCN %>%mutate(Landform=LandForm)

GHCN <- merge(GHCN,ltype, by.x="Landform",by.y="Value",all.x=TRUE)

write.csv(GHCN, "GHCN_POP_Cities_Airport_DEM_Landform.csv") 
 

lstMax <- raster(MAX_LST)

lstsummermax <-  raster::extract(lstMax, lonlat)

GHCN <- GHCN %>% mutate(LST_Max = lstsummermax)
####  Add water Fraction.

lstMin <- raster(MIN_LST)

lstsummermin <-  raster::extract(lstMin, lonlat)

GHCN <- GHCN %>% mutate(LST_Min = lstsummermin)


UHISHP <- readOGR("uhishp",layer="sdei-global-uhi-2013")
 
 


LonLat <- GHCN[,c("Longitude","Latitude")]
coordinates(LonLat)<- ~Longitude+Latitude
proj4string(LonLat)<-proj4string(UHISHP)


UHI <- over(LonLat,UHISHP)
 
GHCN <- GHCN %>% mutate(UrbanArea = UHI$SQKM_FINAL,Urban_ES00Pop=UHI$ES00POP,
                        UrbanDailyMean=UHI$URB_D_MEAN,UrbanNightMean=UHI$BUF_N_MEAN,
                        BufferDailyMean=UHI$BUF_D_MEAN,BufferNightMean=UHI$BUF_N_MEAN,
                        UrbanRuralDailyDiff=UHI$D_T_DIFF,UrbanRuralNightDiff=UHI$N_T_DIFF,
                        UrbanAreaName=UHI$NAME, UrbanLon=UHI$LONGITUDE,UrbanLat=UHI$LATITUDE)




write.csv(GHCN, "GHCN_POP_Cities_Airport_DEM_Landform_LST.csv") 



####  Add Urban Fraction


#####  Night Lights


###  Add DEM

###  NVDI?



 

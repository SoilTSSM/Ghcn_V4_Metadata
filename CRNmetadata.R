
#### Population SCript

source("libraries.R")
source("filenames.R")
source("getInventory.R")

CRN <- tbl_df(read.csv(crninv,header=TRUE, stringsAsFactors =F ))

CRN <- CRN %>% select(WBAN,Name,Lon,Lat,ELEVATION) %>% 
  rename(Latitude=Lat,Longitude=Lon,Elevation=ELEVATION,Station_ID=WBAN)

 

lonlat <-cbind(CRN$Longitude,CRN$Latitude)

 

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


CRN <- CRN %>% mutate(GPwV4_Area=GPwV4_Area,GPwV4_00=GPwV4_00,GPwV4_05=GPwV4_05,GPwV4_10=GPwV4_10,GPwV4_15=GPwV4_15)

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

CRN <- CRN %>% mutate(Hyde_Area=H_Area,
                        Hyde1970=H$X1970,Hyde1980=H$X1980,Hyde1990=H$X1990,Hyde2000=H$X2000,
                        Hyde2005=H$X2005)

CRN <- CRN %>% mutate( GpwV4_density00 = GPwV4_00/GPwV4_Area,
                         Hyde_density00 = Hyde2000/Hyde_Area)
                        
 
 
 




####################  GPW   version 4

 

GPwV4_15_10km<-raster::extract(gpw_Pop_V4_15, lonlat,buffer=10000, fun = sum)
   

CRN <- CRN %>% mutate(GPWV4_Area10 = pi*100,GPwV4_15_10km=GPwV4_15_10km )
                        
 


 

Settlement <- read.csv(GrumpCites,stringsAsFactors=F)


CRN <- CRN %>% mutate(PopulatedPlace=NA,Populated_Lon=NA,Populated_Lat=NA,Populated_Type=NA,EST_POP2000=NA,DistanceToPlace=NA)


for(s in 1:nrow(CRN)){
  
  print(s)
  icoords <- cbind(CRN$Longitude[s],CRN$Latitude[s])
  bcoords <- cbind(Settlement$LONGITUDE,Settlement$LATITUDE)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  CRN$PopulatedPlace[s]<-Settlement$SCHNM[o[1]]
  CRN$Populated_Lon[s]<-Settlement$LONGITUDE[o[1]]
  CRN$Populated_Lat[s]<-Settlement$LATITUDE[o[1]]
  CRN$EST_POP2000[s]   <-Settlement$ES00POP[o[1]]
  CRN$Populated_Type[s]<- Settlement$URBORRUR[o[1]]
  CRN$DistanceToPlace[s]<- dist[o[1]]
  
  
}


 

 

S50 <- Settlement[Settlement$ES00POP > 49999,]


CRN <- CRN %>% mutate(PopulatedPlace50K=NA,Populated_Lon50K=NA,Populated_Lat50K=NA, EST_POP2000_50K=NA,DistanceToPlace50K=NA)


for(s in 1:nrow(CRN)){
  
  print(s)
  icoords <- cbind(CRN$Longitude[s],CRN$Latitude[s])
  bcoords <- cbind(S50$LONGITUDE,S50$LATITUDE)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  CRN$PopulatedPlace50K[s]<-S50$SCHNM[o[1]]
  CRN$Populated_Lon50K[s]<-S50$LONGITUDE[o[1]]
  CRN$Populated_Lat50K[s]<-S50$LATITUDE[o[1]]
  CRN$EST_POP2000_50K[s]   <-S50$ES00POP[o[1]]
   
  CRN$DistanceToPlace50K[s]<- dist[o[1]]
  
  
}

S300 <- Settlement[Settlement$ES00POP > 299999,]


CRN <- CRN %>% mutate(PopulatedPlace300K=NA,Populated_Lon300K=NA,Populated_Lat300K=NA, EST_POP2000_300K=NA,DistanceToPlace300K=NA)


for(s in 1:nrow(CRN)){
  
  print(s)
  icoords <- cbind(CRN$Longitude[s],CRN$Latitude[s])
  bcoords <- cbind(S300$LONGITUDE,S300$LATITUDE)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  CRN$PopulatedPlace300K[s]<-S300$SCHNM[o[1]]
  CRN$Populated_Lon300K[s]<-S300$LONGITUDE[o[1]]
  CRN$Populated_Lat300K[s]<-S300$LATITUDE[o[1]]
  CRN$EST_POP2000_300K[s]   <-S300$ES00POP[o[1]]
  
  CRN$DistanceToPlace300K[s]<- dist[o[1]]
  
  
}
 


ARP <- read.csv(Airports,stringsAsFactors=F)

###  eliminate seaplace, heliport, balloonport, and closed

ARP <- ARP[ARP$type %in% c("large_airport","medium_airport","small_airport"),]


CRN <- CRN %>% mutate(Airport_Type=NA,Airport_Lon=NA,Airport_Lat=NA, Airport_Name=NA,Airport_Dist=NA)

#####   Add Airports


for(s in 1:nrow(CRN)){
  
  print(s)
  icoords <- cbind(CRN$Longitude[s],CRN$Latitude[s])
  
  bcoords <- cbind(ARP$longitude_deg,ARP$latitude_deg)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  
  CRN$Airport_Type[s]<-ARP$type[o[1]]
  CRN$Airport_Lon[s]<-ARP$longitude_deg[o[1]]
  CRN$Airport_Lat[s]<-ARP$latitude_deg[o[1]]
  CRN$Airport_Name[s]<- ARP$name[o[1]]
  CRN$Airport_Dist[s]<- dist[o[1]]
  
  
}


ARP2<- ARP[ARP$type %in% c("large_airport","medium_airport"),]


CRN <- CRN %>% mutate(Airport_Type2=NA,Airport_Lon2=NA,Airport_Lat2=NA, Airport_Name2=NA,Airport_Dist2=NA)
 
#####   Add Airports


for(s in 1:nrow(CRN)){
  
  print(s)
  icoords <- cbind(CRN$Longitude[s],CRN$Latitude[s])
  
  bcoords <- cbind(ARP2$longitude_deg,ARP2$latitude_deg)
  dist    <- spDistsN1(bcoords,icoords,longlat=TRUE)
  o       <-order(dist,decreasing=FALSE)
  
  
  CRN$Airport_Type2[s]<-ARP2$type[o[1]]
  CRN$Airport_Lon2[s]<-ARP2$longitude_deg[o[1]]
  CRN$Airport_Lat2[s]<-ARP2$latitude_deg[o[1]]
  CRN$Airport_Name2[s]<- ARP2$name[o[1]]
  CRN$Airport_Dist2[s]<- dist[o[1]]
  
  
}


DistCoast <- raster(Coast)

dc  <-  raster::extract(DistCoast, lonlat)

CRN <- CRN %>% mutate(DistancetoCoast = dc)




Lights <- raster(Night)

nl  <-  raster::extract(Lights, lonlat)

CRN <- CRN %>% mutate(Lights = nl)

Elevation <- raster(DEM)

elv <-  raster::extract(Elevation, lonlat)

CRN <- CRN %>% mutate(DEM1km = elv)

 


TOPOGRAPHY <-raster(Topo)
ltype <-read.dbf(TopoTbl)
ltype <- ltype[,c("Value", "EF_LF_Desc" )]
 
LonLat <- CRN[,c("Longitude","Latitude")]
coordinates(LonLat)<- ~Longitude+Latitude
proj4string(LonLat)<-projection(TOPOGRAPHY)

LandForm <- raster::extract(TOPOGRAPHY,LonLat)

CRN <- CRN %>%mutate(Landform=LandForm)

CRN <- merge(CRN,ltype, by.x="Landform",by.y="Value",all.x=TRUE)
 
 

 


LC <- raster(esaland)
lctype <- read.csv(esatype,header=T, stringsAsFactors = FALSE,sep =";")

lctype <- lctype[, c("NB_LAB","LCCOwnLabel")]
 

LandCover <- raster::extract(LC, lonlat)
LandCover[is.na(LandCover)]<-0

CRN <- CRN %>% mutate(Landcover=LandCover)

CRN <- merge(CRN, lctype, by.x= "Landcover", by.y = "NB_LAB", all.x=TRUE)


CRN <- CRN %>% mutate(WaterArea=NA, UrbanArea10K=NA)


LatDistance <- 111
dTr <- pi/180

for(l in 1:nrow(CRN)){
  print(l)
   
    LonDistance <- LatDistance* cos(CRN$Latitude[l]*dTr)
    Mult        <-LatDistance/LonDistance
    LatBump10   <- .2
    LonBump10   <- LatBump10*Mult
    
    E10         <-extent(CRN$Longitude[l]-LonBump10,CRN$Longitude[l]+LonBump10,
                         CRN$Latitude[l]-LatBump10,CRN$Latitude[l]+LatBump10)
    
    K <-  crop(LC,E10)
    A <-  area(K)
    ##  Water=210
    ##  Urban = 190
    W <- reclassify(K,rcl = c(-Inf,209,0,209.5,210.5,1,210.5,+Inf,0))
    U <- reclassify(K,rcl = c(-Inf,189,0,189.5,190.5,1,190.5,+Inf,0))
    W <- W*A
    U <- U*A
    CRN$WaterArea[l] <- raster::extract(W, cbind(CRN$Longitude[l],CRN$Latitude[l]), buffer=10000, fun = sum)
    CRN$UrbanArea10K[l] <- raster::extract(U, cbind(CRN$Longitude[l],CRN$Latitude[l]), buffer=10000, fun = sum)
    
   
  
}

####  Add Urban Fraction

 

CRN <- CRN %>% mutate(GPW10km_15_Density = GPwV4_15_10km/GPWV4_Area10)

CRN <- CRN %>% select(Station_ID,Name,Longitude, Latitude,Elevation,DEM1km,DistancetoCoast, LCCOwnLabel,EF_LF_Desc,
                      WaterArea,UrbanArea10K,GPwV4_Area,GPwV4_00,GPwV4_05,GPwV4_10,GPwV4_15,GPWV4_Area10,
                      GPwV4_15_10km, Hyde_Area,
                      Hyde1970,Hyde1980,Hyde1990,Hyde2000,Hyde2005,GpwV4_density00,Hyde_density00,
                      GPW10km_15_Density,EST_POP2000,EST_POP2000_50K,EST_POP2000_300K,DistanceToPlace,DistanceToPlace50K,
                      DistanceToPlace300K,PopulatedPlace,PopulatedPlace50K,PopulatedPlace300K,Populated_Lon,Populated_Lat,Populated_Lon50K,
                      Populated_Lat50K,Populated_Lon300K,Populated_Lat300K,  
                      Airport_Dist,Airport_Dist2,Airport_Name,Airport_Name2,Airport_Type,Airport_Type2,
                      Airport_Lon,Airport_Lat,Airport_Lon2,Airport_Lat2,Lights)


write.csv(CRN, "CRN_Metadata_Final.csv") 


 



 

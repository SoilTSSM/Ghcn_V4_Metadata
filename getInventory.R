

getInventory<- function(fname){
  
  X <-  tbl_df(read.fwf(fname,stringsAsFactors=FALSE, widths = c(11,9,10,7,33) ,
                           col.names=c("Station_ID","Latitude","Longitude","Elevation","Name")))
  
  X <- X %>% mutate(Station_ID = str_trim(Station_ID),Name= str_trim(Name)) %>% 
    mutate(Elevation = ifelse(Elevation==9999,NA,Elevation))
  
  return(X)
}
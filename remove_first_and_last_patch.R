### This function uses the input CSV file, categorizes the end-zone bus stops (all ends are stored in a signle shapefile) 
### and then it deletes all data before the first end zone. If filtered by time, this function is ignored.

remove_first_and_last_patch <- function(x){

  bus <-x
  
  #check the file type (new ones obtained had extra columns)
  if(ncol(bus) > 7){
    bus <- bus[ -c(2:18) ]
    bus <- bus[,c(2, 1, 3, 4, 5, 6)]
  }
  
  #Get Ends.shp and merge with existing data set
  area <- readOGR(".", 'patch100') 
  bus <- na.omit(bus)
  names(bus)[1] <- "Latitude"
  names(bus)[2] <- "Longitude"
  pts2 <- cbind(bus$Longitude,bus$Latitude)
  pts2 <- as.data.frame(pts2)
  pts2 <- na.omit(pts2)
  pts2 <- as.data.frame(pts2)
  names(pts2)[1] <- "x"
  names(pts2)[2] <- "y"
  
  area <- spTransform(area, CRS("+proj=longlat +datum=WGS84"))
  coordinates(pts2) <- ~x+y 
  proj4string(pts2) <- proj4string(area)
  over(pts2, area)
  output <- cbind.data.frame(pts2, area=over(pts2, area))
  busoutput <- cbind(bus,output)
  busoutput[is.na(busoutput)] <- 0
  
  first_patch <- busoutput$id[1]
  j <- 1
  while(j < nrow(busoutput)) {
    
    if(busoutput$id[j] != first_patch){
      break
    }
    busoutput$id[j] <- "delete"
    j <- j+1
  } 
  
  j <- 1
  bottom_up <- length(busoutput$id)
  last_patch <- busoutput$id[bottom_up]
  while(j < nrow(busoutput)) {
    
    if(busoutput$id[bottom_up] != last_patch){
      break
    }
    busoutput$id[bottom_up] <- "delete"
    bottom_up <- bottom_up-1
    j <- j+1
  } 
  
  #remove the unnecessary columns
  #busoutput <- subset(busoutput,area.id != 2)
  #busoutput<-busoutput[,-c(7,8,9,10)] 
  busoutput <- subset(busoutput,id != "delete")
  busoutput
}







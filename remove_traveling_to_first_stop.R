### This function uses the input CSV file, categorizes the end-zone bus stops (all ends are stored in a signle shapefile) 
### and then it deletes all data before the first end zone. If filtered by time, this function is ignored.

remove_traveling_to_first_stop <- function(x){

  bus <-x
  
  #check the file type (new ones obtained had extra columns)
  if(ncol(bus) > 7){
    bus <- bus[ -c(2:18) ]
    bus <- bus[,c(2, 1, 3, 4, 5, 6)]
  }
  
  #Get Ends.shp and merge with existing data set
  area <- readOGR(".", 'Ends') 
  bus <- na.omit(bus)
  names(bus)[3] <- "Latitude"
  names(bus)[4] <- "Longitude"
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
  
  j = 1
  
  #Find first end bus stop and delete all data before this point
  while(j < nrow(busoutput)) {
    
    if(busoutput$area.id[j] == 1){
      break
      cat("BREAK")
    }
    else{
      busoutput$area.id[j] = 2
    }
    j <- j+1;
  }
  
  #remove the unnecessary columns
  busoutput <- subset(busoutput,area.id != 2)
  busoutput<-busoutput[,-c(7,8,9,10)] 
  busoutput
  }







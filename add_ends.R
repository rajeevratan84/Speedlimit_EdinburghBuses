## FUnction adds the end bus stops to a column 
## Input Format below  
## Date_Time  Time  Latitude  Longitude Speed  Heading  Patch

add_ends <- function(x){

  area <- readOGR(".", 'Ends') ## For 100 & 31

  bus <- na.omit(x) ###REPLACE WITH X

  names(bus)[3] <- "Latitude"
  names(bus)[4] <- "Longitude"
  pts2 <- cbind(bus$Longitude,bus$Latitude)
  pts2 <- as.data.frame(pts2)
  pts2 <- na.omit(pts2)
  pts2 <- as.data.frame(pts2)
  names(pts2)[1] <- "x"
  names(pts2)[2] <- "y"
  
  area <- spTransform(area, CRS("+proj=longlat +datum=WGS84"))
  coordinates(pts2) <- ~x+y  # pts needs to be a data.frame for this to work
  proj4string(pts2) <- proj4string(area)
  over(pts2, area)
  output <- cbind.data.frame(pts2, area=over(pts2, area))
  busout <- cbind(bus,output)
  busout[is.na(busout)] <- 0
  
  j = 1
  last_patch <- length(unique(bus$Patch))
  
  ### A & B represent different End Points Not Directions!
  
  while(j < nrow(busout)) {
    if(is.na(busout$area.Location[j])){
    }
    else{
      if(busout$area.Location[j] == "A"){
        busout$Patch[j] = (last_patch+1)
      }
      if(busout$area.Location[j] == "B"){
        busout$Patch[j] = 0
      }    
    }
    j <- j+1;
  }

  busout<-busout[,-c(8,9,10,11)] 
  busout
}







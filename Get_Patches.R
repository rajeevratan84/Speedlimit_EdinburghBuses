## Function adds column with patch categories
## Input File Format 
## Date_Time  Time  Latitude  Longitude Speed  Heading  Patch

Get_Patches <- function(x,y){

  bus <- x
  Route <- y
  
  if(Route == 100){
    area <- readOGR(".", 'patch100') 
  }
  if(Route == 1000){
    area <- readOGR(".", 'onepatch100') 
  }
  if(Route == 11){
    area <- readOGR(".", 'patchn11')
  }
  if(Route == 31){
    area <- readOGR(".", 'patch31')
  }
  
  
  bus <- na.omit(x)
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
  busoutput <- cbind(bus,output)

  busoutput<-busoutput[,-c(7,8)]
  names(busoutput)[7] <- "Patch"
  busoutput
}







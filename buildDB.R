library(maptools)
library(sp)
library(rgdal)
library(maps)
library(stringr)
library(chron)
library(splitstackshape)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(grid)
library(gridExtra)
library(geosphere)
library(plyr)
library(lubridate)

setwd("C:/data/new20/20mph")
source("c:/data/remove_traveling_to_first_stop.R")
source("c:/data/get_only_route_data.R")
source("c:/data/get_route_times.R")
source("c:/data/get_patches_times.R")
source("c:/data/add_ends.R")
source("c:/data/add_directions.R")
source("c:/data/preprocessall.R")

s <- function() structure(list(patchID = numeric(), 
                               timeinpatch = numeric(), 
                               Direction = character()), 
                          class = "data.frame")
All_Bus_Data <- s()

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

i <- 1; j <- 1; k <- 1;
(date_list <- list.files())

for(i in 1:length(date_list)){
#for(i in 1:1){
  Day_Dir<- paste0("C:/data/new20/20mph/",date_list[i])
  setwd(Day_Dir)
  (routes_in_dir <- list.files())
  Day <- date_list[i]

  for(j in 1:length(routes_in_dir)){
#  for(j in 1:1){
    route <- routes_in_dir[j]
    (full_dir <- paste0(Day_Dir,"/",route))
    setwd(full_dir)
    (file_list <- list.files())
  
   for(k in 1:length(file_list)){
#    for(k in 1:1){ 
     cat("THIS IS KKKK == ", k,"\n")
     
      file_name <- paste0(full_dir,"/",file_list[k])
      if(file.size(file_name) > 10000){
        setwd(full_dir)
        bus <- read.csv(file_list[k])
        RouteSurvey <- substr(file_list[k],13,15)
        #bus <- bus[ -c(2:18) ]
        #bus <- bus[,c(2, 1, 3, 4, 5, 6)]
        setwd("C:/data/")
        if (route == "100"){area <- readOGR(".", 'patch100')}
        if (route == "31"){area <- readOGR(".", 'patch31')}
        if (route == "N11"){area <- readOGR(".", 'patchn11')}
        if (route == "67"){area <- readOGR(".", 'patch67')}
        setwd("C:/data/new20/20mph")
        
        
        names(bus)[20] <- "Latitude"
        names(bus)[21] <- "Longitude"
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
        bust <- completeFun(bus, "Latitude")
        
        busout <- cbind(bust,output)
        
        
        
        bus <- preprocessall(busout)
        
        bus$Day <- Day
        bus$Route <- route
        bus$Route_Survey <- RouteSurvey  
        All_Bus_Data <- rbind(All_Bus_Data, bus)
        setwd(full_dir)
      }
      else{}
    }
  }
}

All_Bus_Databk <- All_Bus_Data
#saveRDS(All_Bus_Data, "BUSDB.rds")

setwd("C:/data/")
area <- readOGR(".", 'widespeedlimitroads') ## For 100 but elimate long waiting times at ends

names(All_Bus_Databk)[1] <- "Latitude"
names(All_Bus_Databk)[2] <- "Longitude"
pts2 <- cbind(All_Bus_Databk$Longitude,All_Bus_Databk$Latitude)
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
All_Bus_Databk2 <- cbind(All_Bus_Databk,output)


names(All_Bus_Databk2)[1] <- "lat"
names(All_Bus_Databk2)[2] <- "lon"
All_Bus_Databk2<-All_Bus_Databk2[,-c(20,21,22)] 
names(All_Bus_Databk2)[20] <- "Street"
names(All_Bus_Databk2)[21] <- "Speedlimit20"
All_Bus_Databk2$Speedlimit20 <- ifelse(All_Bus_Databk2$Speedlimit20 == 1 ,"Road Being Changed to 20mph", "No")
 



saveRDS(All_Bus_Databk2, "BUSDB2.rds")

#title1 <- paste0(file_list[i]," Records captured per hour (~every 5 seconds)")
#title2 <- paste0(file_list[i]," Speed vs Time")
#qplot(Date_Time, data=bus, geom="histogram")

#ggplot(bus) + geom_histogram(aes(x = Date_Time)) + ggtitle(title1)
#title1_save <- paste0("c:/data/images/",title1,".png")
#ggsave(file=title1_save)
#Speed vs Time (Line Graph)
#ggplot(bus) + geom_line( aes( x = Date_Time, y = Speed, color = Status)) + ggtitle(title2)
#title2_save <- paste0("c:/data/images/",title2,".png")
#ggsave(file=title2_save)
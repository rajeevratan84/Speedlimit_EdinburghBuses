#######################3
## 1. Produces patch files for the selected route which can be loaded into HyperStar
## 2. Loads the .CSV Data sets from the files sent by Lothian Buses (remove all .KML files from these folders)
## 3. Remove the travel to first end stop and non route areas
## 4. Preprocesses file and identifies patches from pre-defined shapefiles
## 5. Uses function get_patches_times() to obtain the times spent per patch
## 6. Outputs files as text (C:/patches/) 
## 7. Optional drawing for several plots (commented out by default)

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

## Set working directory to where our shapefiles are located. 
setwd("C:/data")

#Load functions
source("remove_traveling_to_first_stop.R")
source("preprocess2.R")
source("Get_Patches.R")
source("get_patch_times.R")
source("add_ends.R")
source("add_directions.R")
source("Output_Patch_Times.R")
source("remove_first_and_last_patch.R")



### CHOOSE SETTING HERE ###
Select_Route <- 1000 #Enter 100 for airlink, 31 for route 31 and 11 for N11

Filter_by_Speed <- FALSE
Filter_Moving_Data_Only <- FALSE
Filter_by_time <- TRUE
Speed_Limits_On <- FALSE

#Time_to_filter <- c("00:00:00","01:00:00","02:00:00","03:00:00","04:00:00")
Time_to_filter <- c("09:00:00","10:00:00","11:00:00","12:00:00","13:00:00","14:00:00","15:00:00")
#Time_to_filter <- c("07:00:00","08:00:00")

## Make empty dataframe with fields for appending patch times for each processed .CSV file
s <- function() structure(list(patchID = numeric(), 
                               timeinpatch = numeric(), 
                               Direction = character()), 
                          class = "data.frame")
Consolidated_Patches <- s()

#Set working directories for each route
if(Select_Route == 100){
  setwd("c:/data/100_all")
  file_directory <- "c:/data/100_all"
}

if(Select_Route == 1000){
  setwd("c:/data/100_all")
  file_directory <- "c:/data/100_all"
}

if(Select_Route == 31){
  setwd("c:/data/31_all")   
  file_directory <- "c:/data/31_all"
}

if(Select_Route == 11){
  setwd("c:/data/N11_all")   
  file_directory <- "c:/data/N11_all"
}

file_list <- list.files()
file_count <- 0

for(i in 1:length(file_list)){
#  for(i in 5:5){
  cat("Processing file # ",i,"\n")
  file_count <- i
  setwd(file_directory)
  bus <- read.csv(file_list[i])
      
      
  setwd("C:/data")
  bus <- remove_traveling_to_first_stop(bus)
  setwd("C:/data")

  bus <- na.omit(bus)
  bustest <- preprocess2(bus)
  daterange <- unique(bustest$DateH)
  

  if(length(daterange)>1){
   # cat("\n 2-day file",file_list[i],"\n")
   for (l in 1:length(daterange)){
     per_day <- subset(bustest,DateH == daterange[l])
     cat("DOING FILE",l,"\n")
     
     if (Filter_by_time == TRUE){
       per_day <- subset(per_day,HourF %in% Time_to_filter)
     }
     
     if(length(per_day$lat) > 50){
       
       #deleting uncessary columns
       per_day <- per_day[,-c(5,8,9,10,11,12,13,14,15)] 
       
       #per_day <- remove_first_and_last_patch(per_day)
       if(nrow(per_day) != 0){
         #re-ordering of columns for future functions
         bus <- per_day[, c(5, 6, 1, 2, 3, 4)]
         
         if(Filter_by_Speed == TRUE){
           if(Filter_Moving_Data_Only == TRUE){bus <- subset(bus,Speed > 2)}
           else{bus <- subset(bus,Speed < 2)}
         }
         
         names(bus)[3] <- "Latitude"
         names(bus)[4] <- "Longitude"
         
         #Map patches to coordiantes
         Patches_Added <- Get_Patches(bus,Select_Route)
         Patches_Added <- na.omit(Patches_Added)
         
         #Add end points
         if(nrow(Patches_Added) == 0){
           Ends_Added <- na.omit(Patches_Added)
         }
         else{
          Ends_Added <- add_ends(Patches_Added)
         }
         
         if(length(unique(Ends_Added$Patch))>2){
           #Get Time Spent in Each Patch Use 1 if we want to add constraints
           Time_per_patch <- get_patch_times(Ends_Added,Speed_Limits_On,Select_Route)
           
           ##Add Directions (A or B)
           Time_per_patch_directions <- add_directions(Time_per_patch)
           Time_per_patch_directions <- Time_per_patch_directions[-c(1),] #Delete first row since it is irelavent for the model.
           
           #Append to master list of patch times for all files
           Consolidated_Patches <- rbind(Consolidated_Patches, Time_per_patch_directions) 
         }
         else{}
       }
       else{}
     }
     else{
       cat("No data in this file ","\n")
     }
   }
  }  
  
  else{
    if (Filter_by_time == TRUE){
      bustest <- subset(bustest,HourF %in% Time_to_filter)
    }
      
    if(length(bustest$lat) > 50){
      
      #deleting uncessary columns
      bustest <- bustest[,-c(5,8,9,10,11,12,13,14,15)] 
      
      #re-ordering of columns for future functions
      bus <- bustest[, c(5, 6, 1, 2, 3, 4)]
      if(Filter_by_Speed == TRUE){
        if(Filter_Moving_Data_Only == TRUE){bus <- subset(bus,Speed > 2)}
        else{bus <- subset(bus,Speed < 2)}
      }
      
      names(bus)[3] <- "Latitude"
      names(bus)[4] <- "Longitude"
      
      #Map patches to coordiantes
      Patches_Added <- Get_Patches(bus,Select_Route)
      
      #Add end points
      Ends_Added <- add_ends(Patches_Added)
       
      #Get Time Spent in Each Patch Use 1 if we want to add constraints
      Time_per_patch <- get_patch_times(Ends_Added,Speed_Limits_On,Select_Route)
        
      ##Add Directions (A or B)
      Time_per_patch_directions <- add_directions(Time_per_patch)
      Time_per_patch_directions <- Time_per_patch_directions[-c(1),] #Delete first row since it is irelavent for the model.
      
      #Append to master list of patch times for all files
      Consolidated_Patches <- rbind(Consolidated_Patches, Time_per_patch_directions)  
  
    }
    else{
      cat("No data in this file ","\n")
    }
  }
}
  
#Output Patches to text file
if(file_count > 3){
  Output_Patch_Times(Consolidated_Patches,Select_Route) ###Also Removes Outliers
}else{
  cat("Need more data!!!\n")
}
  
  
  
  










  
  
  
  
  
  
#   Route_Name <- Select_Route
#   Time_per_patch_all <- Consolidated_Patches
#   Outlier_Count <- 1
#   Outlier_Report <- 0
#   #Outlier_Report <- as.data.frame(Outlier_Report)
#   Time_per_patch_all <- na.omit(Time_per_patch_all)
#   Trip_Direction <- "X"
#   
#   aDirection <- 0
#   aPatch <- 0
#   aMean <- 0
#   aSTD <- 0
#   aN <- 0
#   aValue <- 0
#   
#   
#   #Direction Selection
#   for(j in 1:2){
#     cat(j,"\n")
#     if(j ==1){Trip_Direction = "A";i = 0;}
#     if(j==2){Trip_Direction =  "B";i = 1;}
#     
#     Time_per_patch <- subset(Time_per_patch_all,Direction == Trip_Direction)
#     
#     
#     patch_temp = 0
#     patch_output = 0
#     mean = 0
#     file_name = 0
#     count = 0
#     
#     while(i < (1+length(unique(Time_per_patch$patchID)))){
#       patch_temp <- subset(Time_per_patch, patchID == i)
#       
#       std<- sd(patch_temp$timeinpatch)
#       meann <- mean(patch_temp$timeinpatch)
#       
#       #patch_temp <- subset(patch_temp, timeinpatch < (average*5))
#       cat("Direction - ",Trip_Direction, "Patch #", i,"Mean - ", meann, "STD - ",std, " \n")
#       for(k in 1:length(patch_temp$patchID)){
#         
#         #Long Patches  
#         if(meann > 100){
#           #if(patch_temp$timeinpatch[k] > ((3*std)+meann) | patch_temp$timeinpatch[k] < 50){
#           patch_temp <- subset(patch_temp,timeinpatch < ((3*std)+meann))
#           patch_temp <- subset(patch_temp,timeinpatch > 50)
#         }
#         
#         #Short Patches  
#         else{
#             patch_temp <- subset(patch_temp,timeinpatch < ((3*std)+meann))
#             patch_temp <- subset(patch_temp,timeinpatch > 35)
#         }
#       }
#       
#       #Remove = Remove[-1,]
#       #patch_temp <- patch_temp[-Remove]
#       
#       #patch_output <- subset(patch_temp,Remove != "Yes")
#       patch_output <- as.numeric(patch_temp$timeinpatch)
#       current_time <- Sys.time()
#       current_time <- as.character(current_time)
#       current_time <- gsub(":", "", current_time)
#       file_name <- paste("C:/patch/", current_time,"_patch_",Route_Name,"_",Trip_Direction, i, ".txt", sep="")
#       write.table(patch_output, file_name, sep="/.0",col.names = FALSE, row.names = FALSE)
#       if(i == (length(unique(Time_per_patch$patchID))-1) && Trip_Direction ==  "A"){
#         i <- i +2
#       }
#       else{i <- i+1}
#     }
#   }
#   
#   #Outlier_Report <- as.data.frame(cbind(aDirection,aPatch,aMean,aSTD,aN,aValue))
#   #Outlier_Report
  
  
  
# ###PLotting Plots of PLots  
#   
#   
#   #Plot bar of counts per patch  
#   barplot(table(Time_per_patch1$patchID),lim=c(0,30),xlab = "Patches", ylab = "Count", main = "Patch Counts for Single Bus")
# 
#   
#   #Plot Violin plots  
#   #Average_time_in_patch <- aggregate(Time_per_patch1$timeinpatch, list(Hour = Time_per_patch1$patchID), mean)
#   p <- qplot(factor(patchID), timeinpatch, data = Time_per_patch1, geom = "violin")
#   p + geom_violin(aes(fill = factor(patchID)))+ ggtitle("Violin Plot of Patch Distribution Times") 
# 
#   #Plot histogram of all patch times  
#   hp <- ggplot(Time_per_patch1, aes(x=timeinpatch)) + geom_histogram(binwidth=bw)
#   hp + facet_grid(~ patchID, scales="free") + ggtitle("Time per patch distribution - All Directions + Ends ")   
#   
#   Princes_to_Airport <- subset(Time_per_patch2,Direction == "A")
#   #Princes_to_Airport <- subset(Princes_to_Airport,patchID != 0)
#   Airport_to_Princes <- subset(Time_per_patch2,Direction == "B")
#   #Airport_to_Princes <- subset(Airport_to_Princes,patchID != 10)
#   
#   #Plot bar of counts per patch  
#   barplot(table(Princes_to_Airport$patchID),main="Direction A",  xlab="Patch")
#   barplot(table(Airport_to_Princes$patchID),main="Direction B",  xlab="Patch")
#   
#   
#   #Plot histogram of patch times  
#   hp <- ggplot(Princes_to_Airport, aes(x=timeinpatch)) + geom_histogram(binwidth=bw)
#   hp + facet_grid(~ patchID, scales="free") + ggtitle("Time per patch distribution - East to West") 
#   #Plot Violin plots  
#   p <- qplot(factor(patchID), timeinpatch, data = Princes_to_Airport, geom = "violin")
#   p + geom_violin(aes(fill = factor(patchID)))+ ggtitle("Time per patch distribution - Direction A")   
#   
#   
#   #Plot histogram of patch times  
#   hp <- ggplot(Airport_to_Princes, aes(x=timeinpatch)) + geom_histogram(binwidth=bw)
#   hp + facet_wrap(~ patchID, scales="free") + ggtitle("Time per patch distribution - West to East") 
#   
#   #Plot Violin plots  
#   p <- qplot(factor(patchID), timeinpatch, data = Airport_to_Princes, geom = "violin")
#   p + geom_violin(aes(fill = factor(patchID)))+ ggtitle("Time per patch distribution - Direction B") 
#   
#   unique_patches <- unique(Time_per_patch2$patchID)
#   count_unique_patches <- length(unique(Time_per_patch2$patchID))
# 
#   
#   
# ######Finding Route Times without end points
#   Route_times <- subset(Time_per_patch2, patchID > min(unique_patches) & patchID < (count_unique_patches-2))
#   Route_times <- as.data.frame(Route_times)
#   
#   #Violin
#   p <- qplot(factor(patchID), timeinpatch, data = Route_times, geom = "violin")
#   p + geom_violin(aes(fill = factor(Direction)))+ ggtitle("Time per patch distribution - Per Route Direction") 
#   
#   
#   unique_patches <- unique(Route_times$patchID)
#   count_unique_patches <- length(unique(Route_times$patchID))
#   
#   Total_Trips = nrow(Route_times)/(count_unique_patches*2)
#   j = 0
#   TC = 1
#   
#   for(i in 1:nrow(Route_times)){
#     if(j == (count_unique_patches*2)){
#       #cat(j,"\n")
#       j = 0
#       TC = TC + 1
#     }
#     cat(i,"_",TC,"\n")
#     #cat(i,"\n")
#     j = j + 1
#     Route_times$Trip_Count[i] <- TC 
#   }
# 
#   Route_A_times <- subset(Route_times,Direction == 'A')
#   Route_B_times <- subset(Route_times,Direction == 'B')
#  
#   Times_per_Trip_A <- aggregate(Route_A_times$timeinpatch, list(Trip = Route_A_times$Trip_Count), sum)
#   Times_per_Trip_B <- aggregate(Route_B_times$timeinpatch, list(Trip = Route_B_times$Trip_Count), sum)
#   names(Times_per_Trip_A)[2] <- "Time"
#   
#   ggplot(Times_per_Trip_A, aes(x = factor(Trip), y = Time)) + geom_bar(stat = "identity") + ggtitle("Times per route without buffer zone (end)") + geom_hline(color = 'red', yintercept = schedule_time)
#   
#   
# #############
#   
#   ###Finding Route Times including end points
#   Route_times_Buffer <- Time_per_patch2
#   Route_times_Buffer <- as.data.frame(Route_times_Buffer)
# 
#   unique_patches <- unique(Route_times_Buffer$patchID)
#   count_unique_patches <- length(unique(Route_times_Buffer$patchID))
#   
#   Total_Trips = nrow(Route_times_Buffer)/((count_unique_patches-1)*2)
#   j = 0
#   TC = 1
#   
#   for(i in 1:nrow(Route_times_Buffer)){
#     if(j == ((count_unique_patches-1)*2)){
#       #cat(j,"\n")
#       j = 0
#       TC = TC + 1
#     }
#     cat(i,"_",TC,"\n")
#     #cat(i,"\n")
#     j = j + 1
#     Route_times_Buffer$Trip_Count[i] <- TC 
#   }
#   
#   Route_A_times_WB <- subset(Route_times_Buffer,Direction == 'A')
#   Route_B_times_WB <- subset(Route_times_Buffer,Direction == 'B')
#   
#   Times_per_Trip_A_WB <- aggregate(Route_A_times_WB$timeinpatch, list(Trip = Route_A_times_WB$Trip_Count), sum)
#   Times_per_Trip_B_WB <- aggregate(Route_B_times_WB$timeinpatch, list(Trip = Route_B_times_WB$Trip_Count), sum)
#   names(Times_per_Trip_A_WB)[2] <- "Time"
#   
#   ggplot(Times_per_Trip_A_WB, aes(x = factor(Trip), y = Time)) + geom_bar(stat = "identity") + ggtitle("Times per route with buffer zone (end)") + geom_hline(color = 'red', yintercept=schedule_time)
#   
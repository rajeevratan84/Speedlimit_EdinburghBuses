##Function outputs a file giving the time spent per-patch 
## Inputs are bus data-set with Ends added, after being processes by add_ends(), if speed limits are to be constrained and the route selected

get_patch_times <- function(x,y,z){

  #Getting time spent in each patch
  bus_data <- x
  constrain <- y #if 1, we add constaints
  Route <- z
  
  ##These are the patches affected by the speed limits for each route
  if (Route == 100){speed_limited_patches <- c(8,9,10)}
  if (Route == 31){speed_limited_patches <- c(4,5,6)}
  if (Route == 11){speed_limited_patches <- c(1,3,4,5,6)}
  
  #Clean up file before finding time spent per patch  
  patch_time <- bus_data[,-c(2,3,4,6)]
  names(patch_time)[1] <- "Time"
  names(patch_time)[2] <- "Speed"
  names(patch_time)[3] <- "Patch"
  patch_time <- patch_time[c(1,3,2)]
  
  ##Variables used in getting patch times
  j = 1;
  patch_count = 0
  current_time = 0
  current_patch = 0
  start_patch = 0;
  start_time = 0;
  time_lost = 0
  patch_time <- na.omit(patch_time)
  pt <-patch_time
  patch_time_lost = 0;
  times_patch = pt[1,1]
  patches_times = pt[1,2]

  while(j < nrow(pt)) {
    if(pt$Speed[j] > 20){
      time_lost = ((pt$Speed[j]/20)) + time_lost
    }

    j <- j+1;
    if(j == 2){
      start_time = pt[j,1]
      start_patch = pt[j,2]
    }
    else{
      current_time = pt[j,1]
      current_patch = pt[j,2]
      if(start_patch != current_patch){
        patch_count <- patch_count + 1
        start_patch = current_patch
        times_patch[patch_count+1] = pt[j,1]
        patches_times[patch_count+1] = pt[j,2]
        patch_time_lost[patch_count]<- time_lost
        time_lost = 0
      }
    }
  }
  
  patch_time_lost[patch_count+1]<- 0
  patch_time_lost <- as.data.frame(patch_time_lost)
  times_patch <- as.data.frame(times_patch)
  patches_times  <- as.data.frame(patches_times)
  TimesPatchDF <- cbind(times_patch,patches_times,patch_time_lost)
  names(TimesPatchDF)[1] <- "Date"
  names(TimesPatchDF)[2] <- "Patch"  
  names(TimesPatchDF)[3] <- "Add_Time_if_SpeedLim20"  
  
  #Format Date to POSIX 
  TimesPatchDF$Date <- as.character(TimesPatchDF$Date)
  TimesPatchDF$Date <- as.POSIXct(TimesPatchDF$Date, format="%Y-%m-%d %H:%M:%S")

  
  #Finding time differences between T_Enter and T_Exit
  i = 1;
  timeinpatch = 0
  time_add = 0
  patchID = 1;
  timadd <- 0
  

  while(i < nrow(TimesPatchDF)) {
    patchID[i] <- TimesPatchDF[i,2]
    Value <- difftime(TimesPatchDF[i,1],TimesPatchDF[(i-1),1],units="secs")
    Value <- as.double(Value)   
    timeinpatch[i-1] <- Value  
    timadd <- TimesPatchDF$Add_Time_if_SpeedLim20[i]
    time_add[i] <- timadd

    i <- i+1;
  }
  

  timeinpatch<-as.data.frame(timeinpatch)
  time_add<-as.data.frame(time_add)
  time_add <- time_add[-nrow(time_add),] 
  patchID<-as.data.frame(patchID)
  patchID <- patchID[-nrow(patchID),] 
  patchID<-as.data.frame(patchID)
  Time_per_patch <- cbind(patchID,timeinpatch,time_add)
  
  ##Add constraints to speed limited patches, adds the time delays obtained for those patches
  if(constrain == TRUE){
    i <- 1
    while(i < nrow(Time_per_patch)) {  
      if(Time_per_patch$patchID[i] %in% speed_limited_patches){
        Time_per_patch$timeinpatch[i] <- (Time_per_patch$time_add[i] + Time_per_patch$timeinpatch[i])
      }
      i <- i + 1
    } 
  }
  Time_per_patch <- Time_per_patch[ -c(3) ]
  Time_per_patch
}
    

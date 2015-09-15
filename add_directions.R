## This function adds the direction of travel to the input file
## Input format is below 
## Date_Time  Time  Latitude  Longitude Speed  Heading  Patch
## Note when patches are out of order, or have many gaps this function does not work well at the end points, but those can be ignored. Just beaware of this limitation.
## Improvements should be made here to make this more reliable. 


add_directions <- function(x){
  

  patch_time <- na.omit(x)
  
  j = 1
  last_patch <- length(unique(x$Patch))
  first_patch <- patch_time$patchID[1]
  previous_patch <- first_patch
  next_patch <- 0;
  current_patch = 0;
  end_A <- min(patch_time$patchID)
  end_B <- max(patch_time$patchID)
  
  while(j < (nrow(patch_time))){
    current_patch <- patch_time$patchID[j]
    next_patch <- patch_time$patchID[j+1]
    if(current_patch == end_A || current_patch == end_B){
      patch_time$Direction[j] = "End"
    }
    else{
      if(current_patch > next_patch){
        patch_time$Direction[j] = "A"
        
      }
      if(current_patch < next_patch){
        patch_time$Direction[j] = "B"
        
      }
      if(abs(current_patch - next_patch) > 1)
        patch_time$Direction[j] = patch_time$Direction[j-1]
    }
    j <- j+1
  }
  
  ###Label end patches
  i = 1
  while(i < (nrow(patch_time)+1)){
    if(patch_time$Direction[i] == "End" && patch_time$patchID[i] == end_A){
      patch_time$Direction[i] <- "A"
    }
    if(patch_time$Direction[i] == "End" && patch_time$patchID[i] == end_B){
      patch_time$Direction[i] <- "B"
    }
    i <- i+1
  }
  
  if(patch_time$Direction[nrow(patch_time)] == "End"){
    patch_time$Direction[nrow(patch_time)] <- patch_time$Direction[nrow(patch_time)-1]
  }
  
  patch_time
}
## This Function takes the consolidated patch time files and outputs in to patch text files that can be opened in HyperStar
## It also does outlier rejection

Output_Patch_Times <- function(x,y){
  
  Route_Name <- y
  Time_per_patch_all <- x
  Time_per_patch_all <- na.omit(Time_per_patch_all)
  Trip_Direction <- "X"

  #Direction Selection
  for(j in 1:2){

    if(j == 1){Trip_Direction = "A";i = 1;}
    if(j == 2){Trip_Direction = "B";i = 1;}
    
    cat("Direction", Trip_Direction,"Completed \n")
        
    Time_per_patch <- subset(Time_per_patch_all,Direction == Trip_Direction)
    
    
    patch_temp = 0
    patch_output = 0
    mean = 0
    file_name = 0
    count = 0
    
    ## Initial outlier detector removes large values as those skew the mean and std when the number of data measurements are low
    Time_per_patch <- subset(Time_per_patch, timeinpatch < 5000)    
    
    ## Outlier Detection and removal
    while(i < length(unique(Time_per_patch$patchID))){
      patch_temp <- subset(Time_per_patch, patchID == i)
      
      std<- sd(patch_temp$timeinpatch)
      outlier <- quantile(patch_temp$timeinpatch,.97)
      meann <- mean(patch_temp$timeinpatch)
      
      for(k in 1:length(patch_temp$patchID)){
       
        #Long Patches  
        if(meann > 100){
          patch_temp <- subset(patch_temp,timeinpatch < (3*std)+meann)
          patch_temp <- subset(patch_temp,timeinpatch > 50)
        }
        
        #Short Patches  
        else{
          patch_temp <- subset(patch_temp,timeinpatch < (3*std)+meann)
          patch_temp <- subset(patch_temp,timeinpatch > 35)
        }
      }

      patch_output <- as.numeric(patch_temp$timeinpatch)
      current_time <- Sys.time()
      current_time <- as.character(current_time)
      current_time <- gsub(":", "", current_time)
      file_name <- paste("C:/patch/", current_time,"Route",Route_Name,"_",Trip_Direction,"_patch_", i, ".txt", sep="")
      write.table(patch_output, file_name, sep="/.0",col.names = FALSE, row.names = FALSE)
      i <- i+1
#       if(i == (length(unique(Time_per_patch$patchID))-1) && Trip_Direction ==  "A"){
#         i <- i +2
#       }
#       else{i <- i+1}
    }
  }
}
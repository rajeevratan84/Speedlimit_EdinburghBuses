preprocess2 <- function(x){
  
  edinburghbus100 <- bus
  
  if(ncol(edinburghbus100) > 7){
    edinburghbus100 <- edinburghbus100[ -c(2:18) ]
    edinburghbus100 <- edinburghbus100[,c(2, 1, 3, 4, 5, 6)]
  }
  
  #edinburghbus100 <- na.omit(edinburghbus100)
  
  # Label moving points
  edinburghbus100$Status <- cut(edinburghbus100$Speed, 
                                breaks = c(-Inf, 2, 20, Inf), 
                                labels = c("Stationary", "Moving", "Over 20mph"), 
                                right = FALSE)
  
  names(edinburghbus100)[1] <- "Date"
  names(edinburghbus100)[2] <- "time"
  x <- bus$Time[1]
  x<-as.character(x)
  time_type = 1
  #Check first Time element (assumes same for all)  
  if(nchar(x) > 12){
    edinburghbus100$time <- str_sub(edinburghbus100$time,-8,-1)  
    time_type = 0
  }
  
  
  #Extract Accurate Date 
  data_and_time <- data.frame(do.call('rbind', strsplit(as.character(edinburghbus100$Date),' ',fixed=TRUE))) #Splits Date column in two
  acc_time <- edinburghbus100$time #Separate out the time values
  acc_time2 <- as.data.frame(acc_time)
  edinburghbus100 <- cbind(data_and_time, acc_time, edinburghbus100) #combine them into mainfile
  edinburghbus100$Date_Time = paste(edinburghbus100$X1, edinburghbus100$acc_time, sep=" ") #Extract the time formatted time data
  drops <- c("X1","X2","acc_time","Date","time") #Create list of columns to be dropped 
  edinburghbus100 <- edinburghbus100[,!(names(edinburghbus100) %in% drops)] #Remove unnecessary columns freq=FALSE
  
  #Format Date to POSIX  2015-07-20 07:00:42
  if (time_type == 0){  
    edinburghbus100$Date_Time = strptime(edinburghbus100$Date_Time,format='%Y-%m-%d %H:%M:%S')
  }
  else{
   edinburghbus100$Date_Time = strptime(edinburghbus100$Date_Time,format='%d/%m/%Y %H:%M:%S')
  }
  edinburghbus100$Time <- times(format(edinburghbus100$Date_Time, "%H:%M:%S"))
  t <- strftime(edinburghbus100$Date_Time, format="%H:%M:%S")
  edinburghbus100$Hour <- as.POSIXct(t, format="%H")
  
  a <-as.POSIXlt(edinburghbus100$Date_Time)
  quaters <-as.POSIXlt(round(as.double(a)/(15*60))*(15*60),origin=(as.POSIXlt('1970-01-01')))
  edinburghbus100$NewTimeQ <- as.character(quaters)
  edinburghbus100$DateH <- substr(edinburghbus100$NewTime, 1, 10)
  edinburghbus100$HourQuarters <- substr(edinburghbus100$NewTimeQ, 12, 20)  
  edinburghbus100$Quarter <- paste (edinburghbus100$DateH, edinburghbus100$HourQuarters, sep = " ", collapse = NULL)
  edinburghbus100$Quarter <- as.POSIXct(edinburghbus100$Quarter, format="%Y-%m-%d %H:%M:%S")
  
  hourssss <-as.POSIXlt(round(as.double(a)/(60*60))*(60*60),origin=(as.POSIXlt('1970-01-01')))
  edinburghbus100$NewTimeH <- as.character(hourssss)
  edinburghbus100$HourF <- substr(edinburghbus100$Hour, 12, 20)  
  edinburghbus100$Hourly <- paste (edinburghbus100$DateH, edinburghbus100$HourF, sep = " ", collapse = NULL)
  edinburghbus100$Hourly <- as.POSIXct(edinburghbus100$Hourly, format="%Y-%m-%d %H:%M:%S")
  
  #Rename lat & lon columns
  names(edinburghbus100)[1] <- "lat"
  names(edinburghbus100)[2] <- "lon"
  
  edinburghbus100
  
}


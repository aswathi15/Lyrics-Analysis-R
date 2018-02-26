#create a directory
?dir.create("C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata")
getwd()
unzip("C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata_zip.zip", 
      exdir = "C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata",
      unzip= "internal")

directory <- "C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata/specdata"
directory

?read.csv

pollutantmean <- function(directory,pollutant,id){
  setwd(directory)
  mean_value <- 0
  for ( i in id)
    {
      if (i < 10)
      {
        filename <- paste("00",i,".csv",sep="")
      }
      else if ( i >= 10 & i <= 99)
      {
        filename <- paste("0",i,".csv",sep="")
      }
      else if (i >= 100)
      {
        filename <- paste(i,".csv",sep="") 
      }
    
      f <- read.csv(filename,header = TRUE)
      mean_value <- mean(mean_value + mean(f[,pollutant],na.rm = TRUE),na.rm = TRUE)
      
      i <- i+1
    }
    print(mean_value)
  } 
pollutantmean(directory,"sulfate",1:10)


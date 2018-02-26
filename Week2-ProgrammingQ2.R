?dir.create("C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata")
getwd()
unzip("C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata_zip.zip", 
      exdir = "C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata",
      unzip= "internal")

directory <- "C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata/specdata"
directory

complete <- function(directory,id)
{
  setwd(directory)
  data <- data.frame()
  
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
    
    obs <- sum(complete.cases(f))
    df <- data.frame(i,obs)
    data <- rbind(data,df)
    i <- i+1
  }
  print(data)
} 


complete(directory,1:17)

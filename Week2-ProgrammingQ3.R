?dir.create("C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata")
getwd()
unzip("C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata_zip.zip", 
      exdir = "C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata",
      unzip= "internal")

directory <- "C:/Users/Aswathi/Dropbox/Spring 2018/R A-Z/specdata/specdata"
directory

corr <- function(directory,threshold)
{
  setwd(directory)
  data <- NULL
  
  for ( i in 1:332)
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
    if(obs > threshold)
    {
        data <- c(data,var(f$sulfate,f$nitrate,na.rm = TRUE))
    }
    
    i <- i+1
  }
  print(data)
} 


head(corr(directory,600),6)

?cor

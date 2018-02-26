#import data
getwd()
setwd("C:\\Users\\Aswathi\\Dropbox\\Spring 2018\\R A-Z")
dataset = read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE)
head(dataset)
nrow(dataset)
ncol(dataset)
summary(dataset)
colnames(dataset)


#Rename mortality due to Heart Attack, Heart Failure and Pneumonia
colnames(dataset)[colnames(dataset)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "Mortality_HA"
colnames(dataset)[colnames(dataset) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "Mortality_HF"
colnames(dataset)[colnames(dataset) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- "Mortality_PN"

#dataset[, c("Mortality_HA", "Mortality_HF", "Mortality_PN")] <-
# lapply(dataset[, c("Mortality_HA", "Mortality_HF", "Mortality_PN")],
#       function(x) as.numeric(as.character(x)))

dataset$Mortality_HA <- as.numeric(as.character(dataset$Mortality_HA))
dataset$Mortality_HF <- as.numeric(dataset$Mortality_HF)
dataset$Mortality_PN <- as.numeric(dataset$Mortality_PN)
head(dataset$Mortality_HA)
head(dataset$Mortality_HF)
head(dataset$Mortality_PN)
class(dataset$Mortality_HA)

#histogram for 30-day mortality rates
library(ggplot2)
 ggplot(data=dataset) + 
  geom_histogram(aes(dataset$Mortality_HA),binwidth=20,na.rm = TRUE,
                 color="black",fill="blue")+
  ggtitle("Mortality rate due to Heart Attack") +
  labs(x="Mortality rate Heart Attack")


 #Find the best hospital in a state

 dataset_1 <- dataset[,c("Hospital.Name","State","Mortality_HA","Mortality_HF","Mortality_PN")]
 
 head(dataset_1)
 best <- function(state,outcome)
 {
   row <- NULL
   if(state %in% dataset_1$State & outcome %in% c("heart attack","heart failure","pneumonia"))
   {
     if(outcome == "heart attack")
     {
       row <- which(dataset_1$Mortality_HA == min(dataset_1$Mortality_HA,na.rm = TRUE))
       print(row)
       print(dataset_1[row,"Hospital.Name"])    
     }
     else if (outcome == "heart failure")
     {
       row <- which(dataset_1$Mortality_HF == min(dataset_1$Mortality_HF,na.rm = TRUE))
       print(row)
       print(dataset_1[row,"Hospital.Name"])  
     }
     else if (outcome == "pneumonia")
     {
       row <- which(dataset_1$Mortality_PN == min(dataset_1$Mortality_PN,na.rm = TRUE))
       print(row)
       print(dataset_1[row,"Hospital.Name"])  
     }
   }
   else if(!state %in% dataset_1$State)
   {
     stop("invalid state")
   }
   else if(!outcome %in% c("heart attack","heart failure","pneumonia"))
           {
             stop("invalid outcome")
           }
}
 
 best("TX","heart failure")
 best("NJ","heart failure")
 best("MD","pneumonia") 
 
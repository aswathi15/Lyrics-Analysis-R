#import libraries
library(dplyr) #data manipulation
library(ggplot2) #data visualization 
library(tidyr) #text mining
#install.packages("gridExtra")
library(gridExtra)
#install.packages("wordcloud2")
library(wordcloud2)

#Read the data
getwd()
prince_org = read.csv("prince_raw_data.csv")
head(prince_org,1)
ncol(prince_org)
nrow(prince_org)
colnames(prince_org)

str(prince_org)

#Extract specific columns and rename them
#Note select() allows you to select and rename columns together!
prince <- prince_org %>%
  select(lyrics = text,song,year,album,peak,
         us_pop=US.Pop,us_rnb = US.R.B)

glimpse(prince[139,])
dim(prince)

# Pattern replacement. Get rid of those pesky contractions by creating a little function
# that handles most scenarios using gsub()

fix.contractions <- function(doc)
{
  doc <- gsub("won't","will not",doc)
  doc <- gsub("can't","can not",doc)
  doc <- gsub("n't","not",doc)
  doc <- gsub("'ll","will",doc)
  doc <- gsub("'re","are",doc)
  doc <- gsub("'ve","have",doc)
  doc <- gsub("'m","am",doc)
  doc <- gsub("'d","would",doc)
  doc <- gsub("'s","",doc)
  return(doc)
}

prince$lyrics <- sapply(prince$lyrics,fix.contractions)

removeSpecialChars <- function(x)
{
  x <- gsub("[^a-zA-Z0-9]"," ",x)
  return(x)
}

#remove special characters
prince$lyrics <- sapply(prince$lyrics,removeSpecialChars)

prince$lyrics <- sapply(prince$lyrics,tolower)

str(prince[139, ]$lyrics, nchar.max = 300)
summary(prince)

prince <- prince %>%
  mutate(decade = 
           ifelse(prince$year %in% 1978:1979, "1970s",
           ifelse(prince$year %in% 1980:1989, "1980s",
           ifelse(prince$year %in% 1990:1999, "1990s",
           ifelse(prince$year %in% 2000:2009, "2000s",
           ifelse(prince$year %in% 2010:2015, "2010s",
                  "NA"))))))

prince <- prince %>%
  mutate(chart_level =
           ifelse(prince$peak %in% 1:10,"Top 10",
           ifelse(prince$peak %in% 11:100, "Top 100","Uncharted")))

prince <- prince %>%
  mutate(charted =
           ifelse(prince$peak %in% 1:100, "Charted","Uncharted"))

#save the dataset to new .csv for later use in tutorials
write.csv(prince,file="prince_new.csv")

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

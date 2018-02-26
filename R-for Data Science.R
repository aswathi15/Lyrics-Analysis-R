install.packages('tidyverse')
library(tidyverse)
?tidyverse
?mpg
library(ggplot2)
head(mpg)
nrow(mpg)
ncol(mpg)
summary(mpg)
head(mpg)

#------------------
ggplot(data = mpg) + geom_point(aes(x=displ,y=hwy))
#-- Make a scatterplot of hwy vs cyl.
ggplot(data=mpg) +
  geom_point(aes(x=class,y=drv))

ggplot(data=mpg) +
  geom_point(aes(x=displ,y=hwy,color=class))

ggplot(data=mpg) +
  geom_point(aes(x=displ,y=hwy,color=class),stroke=1)

ggplot(data=mpg) +
  geom_point(aes(color = displ<5))

ggplot(data=mpg) +
  geom_point(aes(x =displ,y=hwy,color=class)) +
  facet_grid(~class)

ggplot(data=mpg) +
  geom_point(aes(x =displ,y=hwy,color=class)) +
  facet_wrap(~class,nrow=4)


ggplot(data=mpg) +
  geom_smooth(aes(x= displ,y=hwy,linetype  =drv),color="black") +
  geom_point(aes(x=displ,y=hwy,color=drv)) 
  

ggplot(data=mpg) +
  geom_smooth(aes(x=displ,y=hwy,group=drv))

ggplot(data=mpg) +
  geom_point(aes(x=displ,y=hwy,color=class)) +
  geom_smooth(data=filter(mpg,class=="subcompact"),aes(x=displ,y=hwy),se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se=FALSE)

#==1
 ggplot(data = mpg,aes(x=displ,y=hwy)) +
   geom_point(size=4) +
   geom_smooth(color="blue",se=FALSE)
 
 #==2
 ggplot(data = mpg,aes(x=displ,y=hwy)) +
   geom_point(size=4) +
   geom_smooth(aes(group=drv),color="blue",se=FALSE)
 
 #==3
 ggplot(data = mpg,aes(x=displ,y=hwy)) +
   geom_point(aes(color=drv),size=4) +
   geom_smooth(aes(color = drv,group=drv),size=2,se=FALSE)

 #==4
 ggplot(data = mpg,aes(x=displ,y=hwy)) +
   geom_point(aes(color=drv),size=4) +
   geom_smooth(size=2,se=FALSE)

 #-------- Diamonds dataset
 
 head(diamonds)
 nrow(diamonds)
 ggplot(data=diamonds) +
   geom_col(aes(x=cut,y=depth))
 

 ggplot(data=diamonds) +
   geom_bar(aes(x=cut,y=depth,stat="identity"))
 ggplot(data = diamonds) + 
   geom_bar(mapping = aes(x = cut, fill = clarity)) 

 ggplot(data = diamonds) + 
   geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")   
 
 ggplot(data =diamonds) +
   geom_bar(aes(x=cut,fill=cut))
head(diamonds) 
unique(diamonds$clarity)

ggplot(data =diamonds) +
  geom_bar(aes(x=cut,fill=clarity),position='dodge')

ggplot(data=mpg) +
  geom_point(aes(x=displ,y=hwy),position = 'jitter') +
  coord_flip()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position = 'jitter')

ggplot(data = mpg,aes(x=class,y=hwy)) +
  geom_boxplot(aes(fill = class)) +
  coord_flip()

nz <- map_data("nz")
install.packages("maps")
head(nz)

ggplot(data=nz,aes(x=long,y=lat,group=group)) +
  geom_polygon(fill='white',color="black")

?geom_map
?seq

#-------------- Data Transformation 

install.packages("nycflights13")
library(nycflights13)
?flights

install.packages("dplyr")
library(dplyr)
?dplyr
head(flights)
nrow(flights)
ncol(flights)

flights

?filter
filter(flights,month==1,day==1)
head(flights)

filter(flights,year ==2013,month==3,day==15)

sqrt(2^2) == 2
?near()
(1/49*49) == 1

filter(flights,month==12 | month==11)
filter(flights, month %in% c(11,12))

filter(flights, arr_delay >= 2)
colnames(flights)

filter(flights,dest == "IAH" | dest == 'HOU')
filter(flights,carrier %in% c('UA,AA'))

filter(flights,month %in% c(7,8,9))
colnames(flights)
filter(flights,arr_delay > 2 & dep_delay ==0)
filter(flights,sched_dep_time >= 1200 & sched_dep_time <= 600)

#How many flights have missing dept_time
count(filter(flights,is.na(dep_time)))

NA^0
NA | TRUE
FALSE & NA
NA*0

arrange(flights,desc(arr_delay))
?select
colnames(flights)
select(flights, year,month,day)
select(flights,year,air_time : time_hour)

select(flights,dest)
select(flights,starts_with("de"))
select(flights,ends_with("ay"))
select(flights,contains("dep"))

colnames(flights)
select(flights,minute,time_hour,everything())

select(flights,dep_time,dep_delay,arr_time,arr_delay)
select(flights,starts_with("dep"),starts_with("arr"))
select(flights,ends_with("time"),ends_with("delay"))
select(flights,arr_time,arr_time)

select(flights, contains("time"))

flights_sml <- select(flights,year:day,ends_with("delay"),distance,air_time)
flights_sml

flights_sml <- mutate(flights_sml,gain = arr_delay - dep_delay,
       speed = distance/air_time)

summarise(flights,dist = mean(distance,na.rm = TRUE))

flights %>%
  group_by(year,month,day) %>%
  summarise(mean = mean(dep_delay,na.rm = TRUE))

#-------------------- Exploratory Data Analysis------------------
library(tidyverse)

diamonds %>%
  count(cut)
head(diamonds)

ggplot(data=diamonds) +
  geom_histogram(aes(x=x),binwidth = 0.5) +
  coord_cartesian(xlim = c(0,10))

ggplot(data=diamonds) +
  geom_histogram(aes(x=y),binwidth=0.5) +
  coord_cartesian(xlim = c(0,10))

ggplot(data=diamonds) +
  geom_histogram(aes(x=z),binwidth = 0.5) +
  coord_cartesian(xlim = c(0,10))

ggplot(data=diamonds) +
  geom_histogram(aes(x=price),binwidth = 1000)

diamonds %>%
  group_by(carat) %>%
  filter(carat==0.99 | carat == 1.00) %>%
  count()

diamonds %>%
  count(carat)

diamonds %>%
  count(color,cut) %>%
  ggplot(aes(x = color,y=cut)) +
  geom_tile(aes(fill = n))

#-------------------------------------------------------------

table1 %>%
  mutate(rate = cases/population *10000)

#cases per year
table1 %>%
  count(year,wt = cases)
  
#visualize changes over time
head(table1)
table1

tb1 <- table1 %>%
  arrange(year)
tb1

ggplot(data=tb1,aes(year,cases)) +
  geom_line(aes(color=country)) +
  geom_point((aes(color=country)))

table2

 table2 %>%
  filter(type == 'cases') %>%
  count(country,year,type,wt=count)

 table4a %>%
   gather(`1999`,`2000`,key='year',value='cases')

 tidy4a <- table4a %>% 
   gather(`1999`, `2000`, key = "year", value = "cases")
 tidy4b <- table4b %>% 
   gather(`1999`, `2000`, key = "year", value = "population")
 left_join(tidy4a, tidy4b)

 stocks <- tibble(
   year   = c(2015, 2015, 2016, 2016),
   half  = c(   1,    2,     1,    2),
   return = c(1.88, 0.59, 0.92, 0.17)
 )
 
 stocks
 
 stocks %>% 
   spread(year, return) %>% 
   gather("year", "return", `2015`:`2016`)

 people <- tribble(
   ~name,             ~key,    ~value,
   #-----------------|--------|------
   "Phillip Woods",   "age",       45,
   "Phillip Woods",   "height",   186,
   "Phillip Woods",   "age",       50,
   "Jessica Cordero", "age",       37,
   "Jessica Cordero", "height",   156
 )

colnames(people) <- c("name","feature","measurement")
people$id <- rnorm(1:5)

people
people %>%
  spread(feature,measurement)

table3 %>%
  separate(rate,into = c('cases','population'),sep ='/',convert = TRUE)

who
colnames(who)

who1 <- who %>%
  gather(new_sp_m014:newrel_f65,key="key",value ="cases",na.rm=TRUE)

who1 %>%
  count(key)
unique(who1$key)
                                                    
who2 <- who1 %>%
  mutate(key = stringr::str_replace(key,"newrel","new_rel"))

who2
who3 <- who2 %>%
  separate(key,c("new","type","sexage"),sep="_")

who3
who4 <- who3 %>%
  select(-iso2,-iso3,new)
who4
who5 <- who4 %>%
  separate(sexage,c("sex","age"),sep=1)
who5
#----------------------------------------------------------------
airlines
colnames(airlines)
colnames(planes)
colnames(airports)

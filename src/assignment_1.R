library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)


#Read in individual data files
ndmi <- read.csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')

ndsi <- read.csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read.csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))


##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

full_wide <- spread(data=full_long,key='data',value='value') %>%
  filter_if(is.numeric,all_vars(!is.na(.))) %>%
  mutate(month = month(DateTime),
         year = year(DateTime))

summer_only <- filter(full_wide,month %in% c(6,7,8,9))

ggplot(summer_only,aes(x=ndmi,y=ndvi,color=site)) + 
  geom_point() + 
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.8,0.8))


## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 

## Your code here
NDSI_NDVI <- mutate(full_long, month=month(DateTime), 
                    year=year(DateTime)) %>%
            filter((data=='ndsi' & month %in% c(1:4))|(data=='ndvi' & month %in% c(6,7,8))) %>% 
            group_by(data,site,year) %>% 
            summarize(mean_value=mean(value)) %>% 
            spread(key='data', value='mean_value')

ggplot(NDSI_NDVI, aes(x=ndsi, y=ndvi, color=year)) +
  geom_point() +
  theme_few() +
  theme(legend.position=c(0.8,0.2)) +
  facet_wrap(~site)

## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

## Your code here

## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

Green_ndvi <- gather(ndvi, key='site',value='NDVI',-DateTime,-data) %>%
              filter(!is.na(NDVI)) %>% 
              mutate(month = month(DateTime)) %>%
              group_by(site, month) %>% 
              summarize(mean_NDVI=mean(NDVI))

ggplot(Green_ndvi, aes(x=month, y=mean_NDVI, color=site)) +
  geom_point() +
  geom_line(linetype=2) +
  theme_few() +
  scale_color_few() +
  theme(legend.position=c(0.5,0.2))




##### Question 5 ####
#What month is the snowiest on average?

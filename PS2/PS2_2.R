#========2======
#======Wind speed in Shenzhen during the past 10 years========
library(tidyr)
library(dplyr)
library(ggplot2)
setwd('D:/ESE5023/ESE5023_Assignments/Data')

shengzheng <- as_tibble(read.csv("2281305.csv",header = T))


shengzheng %>% 
  select(DATE,WND) %>% 
  mutate(winspeed = ifelse((substr(WND,14,14) == 2 | substr(WND,14,14) == 3 |
                              substr(WND,14,14) == 6 | substr(WND,14,14) == 7),
                           NA,as.numeric(substr(WND,9,12)) * 10)) %>% 
  filter(winspeed != 9999) %>% 
  mutate(date2 = as.numeric(paste(substr(DATE,1,4),substr(DATE,6,7),sep = ""))) %>% 
  group_by(date2) %>% 
  summarize(month_mean = mean(winspeed,na.rm = T)) %>% 
  mutate(year = substr(date2,1,4)) %>% 
  mutate(month = as.numeric(substr(date2,5,6))) %>% 
  ggplot(aes(month,month_mean,color = year)) +
  geom_line() +
  labs(title = "Monthly average wind speed from 2010 to 2020")+
  ylab("Monthly average wind speed(m/s)")+
  xlab("Month")
  
  

#========3======
#======Revisit a data set========
library(tidyr)
library(dplyr)
library(ggplot2)
setwd('D:/ESE5023/ESE5023_Assignments/Data')

Rice_export <- as_tibble(read.csv("Rice_China_export_Value.csv",header = T))
Rice_export %>% 
  filter(Partner.Country.Code == 96) %>% 
  ggplot(aes(Year,Value*1000))+
  geom_line()+
  labs(title = "Rice export to Hongkong in 1987---2016")+
  xlab("Year")+
  ylab('value($)')
  

#======对处理好的金砖五国年平均LAI栅格数据进行数据分析==============
library("sp")
library("rgdal")
library("sf")
library("raster")
library(pacman)
library(tidyverse)
setwd("D:/ESE5023/ESE5023_Assignments/Final Project/Converted Date/COUNTRY_LAY_YEAR_MEAN/")

#遍历的时间跨度
year <- seq(1982,2011)
year <- as.character(year)
#遍历的国家
country <- c("China", "Brazil", "India", "Russia", "SouthAfrica")


China <- data.frame()
Brazil <- data.frame()
India <- data.frame()
Russia <- data.frame()
SouthAfrica <- data.frame()
for (i in 1:length(country)) {
  a <- list.files(paste("./",country[i],sep = ""),pattern = "*.tif$", full.names = TRUE)
  n = length(a)
  dir(paste("./",country[i],sep = ""),full.names = TRUE)%>%
    stack()->LAI_COUNTRY
  names(LAI_COUNTRY) <- year
  eval(parse(text = paste(country[i],
                          "<- as_tibble(as.matrix(LAI_COUNTRY))",
                          seq = "")))
}

China %>% 
  gather("year","lai") %>% 
  mutate(Year = substr(year,2,5)) %>% 
  group_by(Year) %>% 
  summarise(annual_mean_lai = mean(lai,na.rm = T),
            annual_varience_lai = var(lai,na.rm = T)) %>% 
  ggplot(aes(Year,annual_mean_lai,fill=Year,group=factor(1)))+
  geom_bar(stat="identity",width=0.5)+
  geom_text(aes(label = round(annual_mean_lai,1), vjust = -0.8, hjust = 0.5, color = Year), 
            show.legend = TRUE)

Brazil %>% 
  gather("year","lai") %>% 
  mutate(Year = substr(year,2,5)) %>% 
  group_by(Year) %>% 
  summarise(annual_mean_lai = mean(lai,na.rm = T),
            annual_varience_lai = var(lai,na.rm = T)) %>% 
  ggplot(aes(Year,annual_mean_lai,fill=Year,group=factor(1)))+
  geom_bar(stat="identity",width=0.5)+
  geom_text(aes(label = round(annual_mean_lai,1), vjust = -0.8, hjust = 0.5, color = Year), 
            show.legend = TRUE)

India %>% 
  gather("year","lai") %>% 
  mutate(Year = substr(year,2,5)) %>% 
  group_by(Year) %>% 
  summarise(annual_mean_lai = mean(lai,na.rm = T),
            annual_varience_lai = var(lai,na.rm = T)) %>% 
  ggplot(aes(Year,annual_mean_lai,fill=Year,group=factor(1)))+
  geom_bar(stat="identity",width=0.5)+
  geom_text(aes(label = round(annual_mean_lai,1), vjust = -0.8, hjust = 0.5, color = Year), 
            show.legend = TRUE)

Russia %>% 
  gather("year","lai") %>% 
  mutate(Year = substr(year,2,5)) %>% 
  group_by(Year) %>% 
  summarise(annual_mean_lai = mean(lai,na.rm = T),
            annual_varience_lai = var(lai,na.rm = T)) %>% 
  ggplot(aes(Year,annual_mean_lai,fill=Year,group=factor(1)))+
  geom_bar(stat="identity",width=0.5)+
  geom_text(aes(label = round(annual_mean_lai,1), vjust = -0.8, hjust = 0.5, color = Year), 
            show.legend = TRUE)

SouthAfrica %>% 
  gather("year","lai") %>% 
  mutate(Year = substr(year,2,5)) %>% 
  group_by(Year) %>% 
  summarise(annual_mean_lai = mean(lai,na.rm = T),
            annual_varience_lai = var(lai,na.rm = T)) %>% 
  ggplot(aes(Year,annual_mean_lai,fill=Year,group=factor(1)))+
  geom_bar(stat="identity",width=0.5)+
  geom_text(aes(label = round(annual_mean_lai,1), vjust = -0.8, hjust = 0.5, color = Year), 
            show.legend = TRUE)



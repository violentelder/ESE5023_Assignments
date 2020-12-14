#======将金砖五国对应的栅格数据切割出去并进行保存==============
library("sp")
library("rgdal")
library("sf")
library("raster")
library(pacman)
library(tidyverse)
setwd('D:/ESE5023/Final Project/Data/Map/')

#遍历的时间跨度
year <- seq(1982,2011)
year <- as.character(year)
#遍历的国家
country <- c("China", "Brazil", "India", "Russia", "SouthAfrica")



#==读取金砖五国的地图图层==
#=读取中国特定图层=
#中国大陆
Chinaline <- st_read("gadm36_CHN.gpkg", layer='gadm36_CHN_0')
#台湾
Taiwan <- st_read("gadm36_TWN.gpkg",layer = 'gadm36_TWN_0')
#澳门
Marco <- st_read("gadm36_MAC.gpkg",layer = 'gadm36_MAC_0')
#香港
Hongkong <- st_read("gadm36_HKG.gpkg",layer = 'gadm36_HKG_0')
#南沙群岛
xsp <- st_read("gadm36_XSP.gpkg",layer = 'gadm36_XSP_0')
China <- list(Chinaline,Taiwan,Marco,Hongkong,xsp)
China <- do.call(rbind,China)
#删除中间变量，减少内存占用
rm(Chinaline, Taiwan, Marco, Hongkong, xsp)

#=读取巴西特定图层=
Brazil <- st_read("gadm36_BRA.gpkg", layer='gadm36_BRA_0')

#=读取印度特定图层=
India <- st_read("gadm36_IND.gpkg", layer='gadm36_IND_0')

#=读取俄罗斯特定图层=
Russia <- st_read("gadm36_RUS.gpkg", layer='gadm36_RUS_0')

#=读取南非特定图层=
SouthAfrica <- st_read("gadm36_ZAF.gpkg", layer='gadm36_ZAF_0')

#==读取所有年份的年平均LAI栅格数据
#读取文件夹中所有的GeoTiff数据
path <- "D:/ESE5023/Final Project/Data/Year_mean/" 
#获得当前路径下的文件列表及数
a <- list.files(path,pattern = "*.tif$", full.names = TRUE)
n = length(a)
dir(path,full.names = TRUE)%>%
  stack()->LAI_YEAR_mean

#==对所有年平均LAI栅格数据进行分割，并分别保存到不同国家的文件夹路径下
#设置总的输出路径
output <- "D:/ESE5023/Final Project/Output/COUNTRY_LAY_YEAR_MEAN/"
#分别创建各个国家的文件夹
for (i in 1:length(country)) {
  outputpath <- paste(output,country[i],sep = "")
  dir.create(outputpath)
}
for (i in 1:n) {
  for (j in 1:length(country)) {
    country_lai <- eval(parse(text = country[j])) %>% 
      crop(LAI_YEAR_mean[[i]],.)
    exactLai <- country_lai %>%
      mask(eval(parse(text = country[j])), na.rm=TRUE)
    #每个国家的年平均lai放到对应国家文件夹下
    name = paste(output,country[j],
                 "/",
                 "LAI_", 
                 country[j],
                 "_",
                 year[i],
                 ".tif",
                 sep = "")
    writeRaster(exactLai,name)
  }
}




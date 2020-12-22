#=====处理原始数据，获得1982-2011间每年平均LAI的栅格数据信息=====
library("sp")
library("rgdal")
library("sf")
library("raster")
library(pacman)
library(tidyverse)
setwd('D:/ESE5023/Final Project/Data/glass LAI_MODIS/globalLAI2000-2011/')

#=====可修改区域=====
#遍历的时间跨度
year <- seq(2000,2011)
year <- as.character(year)

#分别读取不同年份对应的文件夹
for (j in 1:length(year)) {
  #读取文件夹中所有的GeoTiff数据
  path = paste("./", year[j],sep = "") 
  #获得当前路径下的文件列表及数目
  a <- list.files(path,pattern = "*.tif$", full.names = TRUE)
  n = length(a)
  dir(path,full.names = TRUE)%>%
    stack()->LAI_YEAR
  
  #读取该年度栅格数据中的value，并求均值
  lai <- LAI_YEAR[[1]]
  s <- as.vector(lai,mode='any')
  for (i in 2:n) {
    s1 <- as.vector(LAI_YEAR[[i]],mode='any')
    s = s+s1
  }
  rm(s1)
  s = s / n
  #将当年度平均value赋给一个新的GeoTiff数据
  mean <- setValues(lai, s)
  plot(mean, main="mean")
  #将该年度平均LAI栅格数据保存到本地，并以年份命名
  name = paste("LAI_", year[j],".tif",sep = "") 
  writeRaster(mean,name)
}


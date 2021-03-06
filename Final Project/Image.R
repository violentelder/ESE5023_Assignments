#======将处理好的金砖五国年平均LAI栅格数据转换成图像==============
library("sp")
library("rgdal")
library("sf")
library("raster")
library(pacman)
library(tidyverse)
setwd("D:/ESE5023/ESE5023_Assignments/Final Project/Converted Data/COUNTRY_LAY_YEAR_MEAN/")

#遍历的时间跨度
year <- seq(1982,2011)
year <- as.character(year)
#遍历的国家
country <- c("China", "Brazil", "India", "Russia", "SouthAfrica")
outDir <- "../COUNTRY_LAY_YEAR_MEAN_IMAGE/"

for (i in 1:length(country)) {
  outputpath <- paste(outDir,country[i],sep = "")
  dir.create(outputpath)
  a <- list.files(paste("./",country[i],sep = ""),
                  pattern = "*.tif$", 
                  full.names = TRUE)
  n = length(a)
  dir(paste("./",country[i],sep = ""),full.names = TRUE)%>%
    stack()->LAI_COUNTRY
  for (j in 1:n) {
    name <- paste(outputpath,
                  "/",
                  "LAI_YEAR_MEAN_", 
                  country[i],
                  "_",
                  year[j],
                  ".png", 
                  sep = "")
    #保存图片
    # Set the png format
    png(name, 
        width=12.75, 
        height=9, 
        units="in", res=600)
    # Plot
    col <- rev(topo.colors(30))
    plot(LAI_COUNTRY[[j]],
         col = col,
         axis.args=list(cex.axis = 1),
         zlim = c(0,100),
         horizontal=T, useRaster=T,
         legend.shrink=1, 
         legend.width=1, 
         legend.mar=2,
         legend.args=
           list(text=expression("Leaf Area Index "*"["*m^{2}*"/"*m^{2}*"]"),
                          cex=1.25))
    title(xlab="Longitude",cex.lab=1.25,line = 2)
    title(ylab="Latitude",cex.lab=1.25,line = 2)
    title(main=paste("Annual Mean LAI of", country[i], "in", year[j]),
          cex.main=1.5,font.main=2)
    # Close the png file to save the file
    dev.off()
  }
}



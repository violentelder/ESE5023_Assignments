#===========Potential Renewable Energy Spots in China===========
library("sp")
library("rgdal")
library("sf")
library("raster")
library(pacman)
library(tidyverse)
setwd('D:/ESE5023/ESE5023_Assignments/Data')


#1.1
Solor_June <- raster('wc2.1_2.5m_srad_06.tif')
Prec_June <- raster('wc2.1_2.5m_prec_06.tif')
Wind_June <- raster('wc2.1_2.5m_wind_06.tif')

#1.2
# 读取中国特定图层
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
plot(China)

#太阳辐射情况
#切割出包含中国领土的热力图
Chinasolor<- China %>%
  # now crop our temp data to the extent
  crop(Solor_June,.)
#掩膜覆盖只保留中国国土的热力图
exactChnS <- Chinasolor %>%
  mask(China, na.rm=TRUE)

col <- heat.colors(30)
plot(exactChnS,
     col =col,
     axis.args=list(cex.axis = 1),
     horizontal=T, useRaster=T,
     legend.shrink=1, 
     legend.width=1, 
     legend.mar=2,
     legend.args=list(text="Solar radiation [kJ m-2 day-1]",cex=1.25, col = 'orange'))
title(xlab="Longitude",cex.lab=1.25,line = 2)
title(ylab="Latitude",cex.lab=1.25,line = 2)
title(main=paste("Solar radiation in June (China)"),
      cex.main=2,font.main=2)

#降水量
#切割出包含中国领土的热力图
ChinaPrec<- China %>%
  # now crop our temp data to the extent
  crop(Prec_June,.)
#掩膜覆盖只保留中国国土的热力图
exactChnP <- ChinaPrec %>%
  mask(China, na.rm=TRUE)

col <- rev(topo.colors(30))
plot(exactChnP,
     col = col,
     axis.args=list(cex.axis = 1),
     horizontal=T, useRaster=T,
     legend.shrink=1, 
     legend.width=1, 
     legend.mar=2,
     legend.args=list(text="Precipitation [mm]",cex=1.25, col = 'blue'))
title(xlab="Longitude",cex.lab=1.25,line = 2)
title(ylab="Latitude",cex.lab=1.25,line = 2)
title(main=paste("Precipitation in June (China)"),
      cex.main=2,font.main=2)


#风速
#切割出包含中国领土的热力图
ChinaWind<- China %>%
  # now crop our temp data to the extent
  crop(Wind_June,.)
#掩膜覆盖只保留中国国土的热力图
exactChnW <- ChinaWind %>%
  mask(China, na.rm=TRUE)

col <- rev(cm.colors(30))
plot(exactChnW,
     col = col,
     axis.args=list(cex.axis = 1),
     horizontal=T, useRaster=T,
     legend.shrink=1, 
     legend.width=1, 
     legend.mar=2,
     legend.args=list(text="Wind Speed [m/s]",cex=1.25, col = 'purple'))
title(xlab="Longitude",cex.lab=1.25,line = 2)
title(ylab="Latitude",cex.lab=1.25,line = 2)
title(main=paste("Wind Speed in June (China)"),
      cex.main=2,font.main=2)

#1.3
exactChnWt <- exactChnW
mean_wind <- mean(exactChnWt@data@values, na.rm = T)
exactChnWt@data@values[exactChnWt@data@values <= 1.3*mean_wind] <- 0
exactChnWt@data@values[exactChnWt@data@values > 1.3*mean_wind] <- 1

col = c('grey','blue')
plot(exactChnWt,
     col = col,
     legend = F)
title(xlab="Longitude",cex.lab=1.25,line = 2)
title(ylab="Latitude",cex.lab=1.25,line = 2)
title(main=paste("Potential Locations of Wind Farms"),
      cex.main=2,font.main=2)
legend("right",legend = c('Bad','Good'),fill = col)

#1.4
exactChnPT <- exactChnP
exactChnST <- exactChnS

mean_solar <- mean(exactChnST@data@values,na.rm = T)
mean_Per <- mean(exactChnPT@data@values,na.rm = T)

exactChnPT@data@values[(exactChnST@data@values <= 1.1*mean_solar) |
                         (exactChnPT@data@values >= 0.9*mean_Per)] <- 0

exactChnPT@data@values[(exactChnST@data@values > 1.1*mean_solar) &
                         (exactChnPT@data@values < 0.9*mean_Per)] <- 1

col = c('grey','green')
plot(exactChnPT,
     col = col,
     legend = F)
title(xlab="Longitude",cex.lab=1.25,line = 2)
title(ylab="Latitude",cex.lab=1.25,line = 2)
title(main=paste("Potential Locations of PV Farms"),
      cex.main=2,font.main=2)
legend("right",legend = c('Bad','Good'),fill = col)









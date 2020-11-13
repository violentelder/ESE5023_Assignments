#===========Plotting with ggplot2=======
#由于所研究方向为无人机设计，没有观测数据，故采用R语言自带数据集以及NADA数据集
library(dplyr)
library(ggplot2)
library(NADA)
library(tidyr)
library(reshape2)
library(ggthemes)
library(fields) 
library(maps)
library(RNetCDF)

#=======Boxplot===
data(Thames)
Thames_tb <- as_tibble(Thames)
Thames_tb %>% 
  group_by(River) %>% 
  ggplot(aes(River, Dieldrin, fill=River))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge())+
  labs(title="Dieldrin in fish of the river in UK") +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))+
  facet_wrap( ~ Species)

#=======Time series===
class(Seatbelts)
Seatbelts_DF <- as.data.frame(Seatbelts)

#提取两组时间序列数据
Front <- ts(Seatbelts_DF$front , start=c(1969,1), frequency=12)
Rear <- ts(Seatbelts_DF$rear,start=c(1969,1), frequency=12)
Date_df <- data.frame(Time=c(time(Front)),
                               Front=c(Front),
                               Rear = c(Rear))
#数据变形
Date_df <- melt(Date_df,id="Time")
#更改列名
colnames(Date_df) <- c("Time","Position","Number")

ggplot(Date_df,aes(x = Time, y = Number, color = Position))+
  geom_line(size = 0.8)+
  ylab("Number of people")+
  labs(title="Passengers killed or seriously injured in UK from 1969 to 1984") +
  theme(plot.title=element_text(size=19, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))+
  theme_stata()+
  scale_fill_stata()


#=======Histogram===
#beaver1 是某只海狸每10分钟的体温数据
ggplot(beaver1,aes(x = temp)) +
  geom_histogram(fill="steelblue",colour = "black",binwidth = 0.05)+
  geom_density(color = "red")+
  xlab("Temperature")+
  ylab("Count") +
  labs(title="Temperature of beaver measured by every 10 minutes") +
  theme(plot.title=element_text(size=19, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))+
  theme_solarized()+
  scale_fill_solarized()


#====Scatter plot=====
AirPassengers_df <- data.frame(Time = time(AirPassengers),
                             Number = c(AirPassengers))
ggplot(AirPassengers_df, aes(x = Time, y = Number))+
  geom_point(color = "blue")+
  scale_colour_brewer(palette = "Set1")+
  geom_smooth(color = "red")+
  geom_rug()+
  ylab("Number of AirPassengers") +
  labs(title=" Monthly totals of international passengers from 1949 to 1960") +
  theme(plot.title=element_text(size=19, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))+
  theme_solarized()+
  scale_fill_solarized()

#======Image plot========
setwd("D://ESE5023/ESE5023_Assignments/Data")
ex.nc <- open.nc("wspd.mon.ltm.nc")

# Print the variables and attributes
print.nc(ex.nc)

# Read the variables
# Lat
Lat       <- var.get.nc(ex.nc, "lat")
# Lon
Lon       <- var.get.nc(ex.nc, "lon")
# Monthly long term mean, surface temperature [K]
wspd     <- var.get.nc(ex.nc, "wspd") 
# Close the NetCDF file
close.nc(ex.nc)

# Original Lat is in decreasing order, we need to reverse it
Lat <- rev(Lat) 
Lon <- Lon - 180


# Data transformation of wspd_T_Jan
wspd_T_Jan <- wspd[,,1,1]
for(row in 1:length(Lat)){
  wspd_T_Jan[,row] <- wspd_T_Jan[, (length(Lat)+1-row) ]
}

# Set margins on bottom, left, top, right
par(mar=c(4.5,3,2,1))

# Plot
image.plot(Lon, Lat, wspd_T_Jan,
           horizontal=T, useRaster=T,
           legend.shrink=1, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Wind Speed [m/s]",cex=1.25, col = 'gray'),           
           xlab='Longitude',ylab='latitude',midpoint=T, axes=F, ann=F
)
title(xlab="Longitude",cex.lab=1.25, col.lab = 'blue')
axis(1,at=pretty(Lat,5),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="Latitude",cex.lab=1.25,col.lab = 'blue')
axis(2,at=pretty(Lat,5),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("Long term (1800-2020) Mean Wind Speed(1000 hPa) in Jan."),
      cex.main=1.5,font.main=2)

# Add map
map('world',add=T,lwd=0.5,col="black")

# Add a box
box(lwd=2)


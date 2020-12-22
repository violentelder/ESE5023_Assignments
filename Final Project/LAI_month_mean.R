#=====将NetCdf拆解成各个月份对应的热力图像=====
library(fields) 
library(maps)
library(RNetCDF)
setwd("D:/ESE5023/ESE5023_Assignments/Final Project")

outDir <- file.path("Converted Data/LAI_MONTHLY_MEAN/")
Num <- c("08", "09", "10", "11", "12", "01", 
         "02", "03", "04", "05", "06", "07")
month <- c("August", "September", "October", "November", 
           "December", "January", "February", "March", 
           "April", "May", "June", "July")

# Open the NetCDF file
ex.nc <- open.nc("Data/LAI_mean_monthly_1981-2015.nc4")
# Print the variables and attributes
print.nc(ex.nc)
# Read the variables
# Lat
Lat       <- var.get.nc(ex.nc, "lat")
# Lon
Lon       <- var.get.nc(ex.nc, "lon")
# Monthly long term mean, Leaf Area Index [m^{2}/m^{2}]
LAI_T     <- var.get.nc(ex.nc, "LAI") 
# Close the NetCDF file
close.nc(ex.nc)

col <- rev(topo.colors(30))
for (i in 1:12) {
  LAI_T_Jan <- LAI_T[,,i]
  #文件命名
  name <- paste(outDir,
                "/",
                "LAI_mean_monthly_1981-2015_"
                ,Num[i], 
                "_",
                month[i],
                ".png", 
                sep = "")
  #保存图片
  # Set the png format
  png(name, 
      width=12.75, 
      height=9, 
      units="in", res=400) 
  
  # Set margins on bottom, left, top, right
  par(mar=c(4.5,3,2,1))
  
  # Plot
  image.plot(Lon, Lat, LAI_T_Jan,
             zlim = c(0,7),
             col = col,
             horizontal=T, useRaster=T,
             legend.shrink=1, axis.args=list(cex.axis = 1), 
             legend.width=1, legend.mar=2,
             legend.args=
               list(text=expression("Leaf Area Index "*"["*m^{2}*"/"*m^{2}*"]"),
                              cex=1.25),           
             xlab='',ylab='',midpoint=T, axes=F, ann=F)
  title(xlab="Longitude",cex.lab=1.25,line = 2)
  axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
  title(ylab="Latitude",cex.lab=1.25,line = 2)
  axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
  title(main=paste("Climatological Mean", 
                   month[i], 
                   "LAI for the 1981-2015"),
        cex.main=1.5,font.main=2)
  
  # Add map
  map('world',add=T,lwd=0.75,col="black")
  
  # Add a box
  box(lwd=2)
  
  # Close the png file to save the file
  dev.off()
}




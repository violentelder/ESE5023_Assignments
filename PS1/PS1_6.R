#============6========
#======Visibility in Shenzhen during the past 10 years======
setwd('D:/ESE5023/ESE5023_Assignments/Data')
Weather <- read.csv("2281305.csv",header = T)
Obs_Time <- as.Date(Weather$DATE)
obs_VIS <- Weather$VIS

#VISIBILITY-OBSERVATION distance quality code
Disqualcode <- substr(obs_VIS,8,8)

#VISIBILITY-OBSERVATION variability code
VarCode <- substr(obs_VIS,10,10)

#VISIBILITY-OBSERVATION quality variability code
Qualvarcode <- substr(obs_VIS,12,12)

#VISIBILITY-OBSERVATION Distance
distance <- as.numeric(substr(obs_VIS,1,6))
distance[which(distance > 160000 | Disqualcode != 1
               | VarCode != 'N' | Qualvarcode != 1)] <- NA

plot(Obs_Time,distance,type = 'l',col='BLUE',lwd = 0.5)

#=====6.2======
library(ggplot2)
distance2 <- as.numeric(substr(obs_VIS,1,6))
distance2[which(distance2 > 160000)] <- NA
max(distance2,na.rm = T)
daycode <- as.numeric(Obs_Time)
year <- substr(Obs_Time,1,4)
year_days <- c()

#计算能见度在a_b区间上的天数
sumdays <- function(a,b=Inf){
  year_days <<- c()
  for (i in unique(year)) {
    temp <- 0
    daycode1 <- daycode[which(year == i)] 
    for (j in unique(daycode1)) {
      m1 <- max(distance2[which(daycode == j)],na.rm = T)
      #计算当日能见度最大值时，可能会出现NA情况导致判断异常，故此处需要进行重新赋值
      if(is.na(m1))
        m1 <- 0
      if(m1 >= a && m1 < b){
        temp = temp + 1
      }
    }
    year_days <<- c(year_days, temp)
  }
  return(year_days)
}


a <- 0
b <- 5000
#分多个区域进行绘图并合为一张
split.screen(c(3,2))
for (i in 1:6) {
  screen(i)
  sday <- sumdays(a,b)
  x <- barplot(sday,names.arg=unique(year),xlab="year",ylab="Days",
               main=paste("Visibility chart in" , a,"to" ,b ,'(m)'))
  #将数值填入柱状图的每组数据中，可视化
  lbls<-paste(" ",sday)
  text(x,sday,labels=lbls,cex=0.5,pos=1)
  a <- a + 5000
  b <- b + 5000
}
#结束绘图
dev.off()
#绘制最后一幅图像
sday <- sumdays(a)
x <- barplot(sday,names.arg=unique(year),xlab="year",ylab="Days",
             main=paste("Visibility chart >" , a,'(m)'))
lbls<-paste(" ",sday)
text(x,sday,labels=lbls,cex=0.5,pos=1)
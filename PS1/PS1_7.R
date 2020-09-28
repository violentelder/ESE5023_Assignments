#============7=========
#=========Explore a data set============
setwd('D:/ESE5023/ESE5023_Assignments/Data')
Rice_export <- read.csv("Rice_China_export_Value.csv",header = T)
#=====检查数据中不存在无效点，故不进行过滤
PartnerCountry <- Rice_export$Reporter.Countries
Countrycode <- Rice_export$Partner.Country.Code
exp_year <- Rice_export$Year.Code
value <- Rice_export$Value
exp_year_hongkong <- c()
value_hongkong <- c()

#====Plot the time series of a certain variable
exp_year_hongkong <- c(exp_year_hongkong,exp_year[which(Countrycode == "96")])
value_hongkong <- c(value_hongkong, value[which(Countrycode == "96")] * 1000)
plot(exp_year_hongkong,value_hongkong,type = 'l',
     col='BLUE',lwd = 0.5,main = 'Rice export to Hongkong in 1987---2016',
     xlab = "Year",ylab = 'value($)')

#=====5 simple statistical checks with the variable
sum_hongkong <- sum(value_hongkong)
avr_hongkong <- mean(value_hongkong)
range(exp_year_hongkong)
max(value_hongkong)
min(value_hongkong)
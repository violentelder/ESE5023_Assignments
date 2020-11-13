#Analysis of the time series of monthly temperature
library(lubridate)
library(forecast)
library(tidyr)
library(dplyr)
library(ggplot2)
library(astsa)
setwd("D://ESE5023/ESE5023_Assignments/Data")

#2.1
Weather  <- read.csv(file = "2281305.csv", header = T)
Weather_tb <- as_tibble(Weather)
Temp_mean <- Weather_tb %>% 
  mutate(temp = ifelse((substr(TMP,7,7) == 2 | substr(TMP,7,7) == 3 |
                          substr(TMP,7,7) == 6 | substr(TMP,7,7) == 7 |
                          substr(TMP,1,5) == '+9999'),
                       NA,as.numeric(substr(TMP,1,5)) / 10)) %>% 
  mutate(date2 = as.numeric(paste(substr(DATE,1,4),
                                  substr(DATE,6,7),
                                  sep = ""))) %>% 
  filter(date2 >= 201001 & date2 <= 202008) %>% 
  group_by(date2) %>% 
  summarise(Temp_mean = mean(temp,na.rm = T)) %>%
  pull(Temp_mean)
Temp_mean_ts <- ts(Temp_mean, start=c(2010,1), frequency=12)
plot(Temp_mean_ts)

#2.2
Temp_mean_dts <- decompose(Temp_mean_ts)
plot(Temp_mean_dts)
# Plot hist
hist(Temp_mean_dts$random, prob=TRUE,ylim = range(0:1))
# Add pdf
curve(dnorm(x, 
            mean=mean(Temp_mean_dts$random,na.rm=T),
            sd=sd(Temp_mean_dts$random,na.rm=T)),
      add=TRUE, 
      col="red")
#遵循白噪声分布

#2.3
acf(Temp_mean_ts)
pacf(Temp_mean_ts)
model <- auto.arima(Temp_mean_ts)

#进行季节性arima分析
sarima(Temp_mean_ts,0,0,2,1,1,1,12)
sarima.for(Temp_mean_ts,24,0,0,2,1,1,1,12)

#2.4
#=====预测值=====
forecast_temp <- forecast(model, 10)
plot(forecast_temp)
#2020年9月
forecast_temp$mean[1]
#80%置信区间
forecast_temp$lower[1,1]
forecast_temp$upper[1,1]

#2020年10月
forecast_temp$mean[2]
#80%置信区间
forecast_temp$lower[2,1]
forecast_temp$upper[2,1]


#=====实际值=====
Weather_tb %>% 
  mutate(temp = ifelse((substr(TMP,7,7) == 2 | substr(TMP,7,7) == 3 |
                          substr(TMP,7,7) == 6 | substr(TMP,7,7) == 7 |
                          substr(TMP,1,5) == '+9999'),
                       NA,as.numeric(substr(TMP,1,5)) / 10)) %>% 
  mutate(date2 = as.numeric(paste(substr(DATE,1,4),
                                  substr(DATE,6,7),
                                  sep = ""))) %>% 
  filter(date2 >= 202009 & date2 <= 202010) %>% 
  group_by(date2) %>% 
  summarise(Temp_mean = mean(temp,na.rm = T))




  
  

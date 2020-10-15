#========1======
#======Significant earthquakes since 2150 B.C.========

library(tidyr)
library(dplyr)
library(ggplot2)
setwd('D:/ESE5023/ESE5023_Assignments/Data')

#1.1
signifdata <- read.csv("signif.csv",header = TRUE)
Sig_Eqs <- as_tibble(signifdata)

#1.2
Sig_Eqs %>% 
  group_by(COUNTRY) %>% 
  summarize(Death_sum = sum(DEATHS,na.rm = T)) %>% 
  arrange(desc(Death_sum))

#1.3
Sig_Eqs %>% 
  mutate(flag6.0 = ifelse(EQ_PRIMARY > 6.0, 1,NA)) %>% 
  group_by(YEAR) %>% 
  summarize(sum6.0 = sum(flag6.0,na.rm = T)) %>% 
  ggplot(aes(YEAR,sum6.0)) +
  geom_line()
#可以观察到自公元前2150年以来，震级在6.0级以上的地震数目逐渐增多，尤其自公元1500年开始，该数目开始陡增
#造成这种趋势的可能原因分析有两个：
#1.早期的地震记录少主要是由于科学技术不发达，导致的地震无法被准确记录，相关历史文献缺失导致
#2.造成地震数目不断升高可能表明当前地质运动活跃，造成地震数目升高

#1.4
CountEq_LargestEq <- function(country,tib = Sig_Eqs){
  CountEq <- tib %>% 
    filter(COUNTRY == country) %>% 
    nrow()
  LargestEq <- "Loss Data"
  LargestEq <- tib %>%
    filter(COUNTRY == country) %>% 
    filter(EQ_PRIMARY == max(EQ_PRIMARY,na.rm = T)) %>%
    mutate(DATE = paste(YEAR,MONTH,DAY,sep = "-")) %>% 
    select(DATE)
  LargestEq <- as.character(LargestEq)
  ans <- list(CountEq,LargestEq)
  return(ans)
}
CountEq_LargestEq('CHINA')[2]

Country <- Sig_Eqs %>% 
  pull(COUNTRY)
Country<- Country[!duplicated(Country)]
CountEq_LargestEq(Country[50])[2]

CountEq <- c()
LargestEq <- c()
for (i in 1:length(Country)) {
  CountEq <- c(CountEq,as.numeric(CountEq_LargestEq(Country[i])[1]))
  #在数据中会出现某个国家所有地震记录的震级不存在，导致MAX函数判断错误，使得LargestEq = 'character(0)'
  #这里需要进行筛选排除
  if(CountEq_LargestEq(Country[i])[2] != 'character(0)')
    LargestEq <- c(LargestEq,as.character(CountEq_LargestEq(Country[i])[2]))
  else
    LargestEq <- c(LargestEq,"Loss Data")
}

new_eqs <- data.frame(Country,CountEq,LargestEq)
new_eqs





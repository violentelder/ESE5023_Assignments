#=========Analysis of Data Sets from Your Group======
#由于所研究方向为无人机设计，没有观测数据，故采用R语言自带数据集
#=====7.1======
library(MASS)
library(leaps)
library(tidyr)
library(ggplot2)
library(dplyr)
#beaver1 & beaver2是两只海狸每10分钟的体温数据
#使用直方图检查体温数据是否满足正态分布
hist(beaver1$temp,main="Temperature of beaver No1",xlab = "Temp")
hist(beaver2$temp,main="Temperature of beaver No2",xlab = "Temp")
t.test(beaver1$temp, beaver2$temp)

#=====7.2====
#iris是3种鸢尾花形态数据，包含数据列有
#Sepal萼片的长度和宽度
#Petal的长度集合宽度
iris_tb <- as_tibble(iris)
#检验是否符合正态分布情况
iris_tb %>% 
  filter(Species == "setosa") %>% 
  ggplot(aes(Sepal.Length))+
  geom_histogram()
#绘制萼片长度的箱型图
iris_tb %>% 
  ggplot(aes(Species, Sepal.Length,col = Species))+
  geom_boxplot()

#对三种鸢尾花的萼片长度进行One-way ANOVA
anova_one_way <- aov(Sepal.Length~Species, data = iris_tb)
summary(anova_one_way)

#=====7.3=====
#swiss瑞士生育率和社会经济指标
subset_result <- regsubsets(Fertility~ ., data=swiss, nbest= 2)
plot(subset_result, scale="bic")
summary(subset_result)
fit<-lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality, data=swiss)
pre <- predict(fit,swiss)
#绘制预测值与实际值对比
##====实际值===
plot(swiss$Fertility,col = "red",xlab = "City",ylab = "Fertility")
lines(swiss$Fertility,col = "red",type ='c')
#====预测值===
points(pre, col = 'blue')
lines(pre, col = 'blue',type ='c')

#计算平均偏差
mean(swiss$Fertility - pre)


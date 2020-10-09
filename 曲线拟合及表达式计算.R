library(ggplot2)
library(ggpubr)

waterdata <- read.csv('E:/test/water_quality(1).csv', header=T)
head(waterdata)
ggplot(waterdata, 
       mapping= aes(x = num,y = E..coli..cfu.100mL.))+
  geom_point(shape=1)+
  xlab('Time(days)')+
  ylab('E. coli (cfu/100mL)')+
  #对数据进行线性回归拟合
  geom_smooth(method = "lm",formula ='y ~ x')+
  #对数据进行线性回归分析
  stat_cor(method = "pearson", 
           label.x = 1, 
           label.y = 50000)

coli <- waterdata$E..coli..cfu.100mL.
Num <- waterdata$num
z <- lm(coli~Num)
#得出线性回归相关数据
summary(z)


#==================
x1 <- c(0.2,0.4,0.6,0.8,1.0,1.2)
y1 <- c(1.5,1.7,2.5,3.6,3.7,3.8)
data1 <- data.frame(x1,y1)
#======进行三次多项式拟合
z1 <- lm(y1 ~ x1+I(x1^2)+I(x1^3))
#======获得多项式的系数
co <- coefficients(z1)

#建立f（x）函数
fx <- function(x){
  co[2]*x + co[3]*x^2 + co[4]*x^3 + co[1] 
}
xfx <- function(x){
  x*fx(x)
}

ggplot(data1, 
       mapping= aes(x = x1,y = y1))+
  geom_point(shape=1)+
  #对数据进行线性回归拟合
  geom_smooth(method = 'lm',formula = 'y ~ x+I(x^2)+I(x^3)')

xu <- max(x1)
xl <- min(x1)
ifx <- integrate(fx,xl,xu)

#=======计算ab的值
b <-(integrate(xfx,xl,xu)$value*(xu-xl) - 
        0.5*(xu^2 - xl^2)*integrate(fx,xl,xu)$value) / ((1/3)*(xu-xl)*(xu^2 - xl^2)
                                                  -0.25*(xu^2 - xl^2)^2)
a <- (integrate(fx,xl,xu)$value - 0.5*b*(xu^2 - xl^2)) / (xu - xl)






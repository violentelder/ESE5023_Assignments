#=========The Big Bang Theory ======
setwd('D:/ESE5023/ESE5023_Assignments/Data')
BigBang <- read.csv("The Big Bang Theory.csv",header = T)

Velocity <- BigBang$Velocity
Distance <- BigBang$Distance
Distance <- Distance * 1e6 * 1e12 * 30.9

#======5.1====
plot(Velocity,Distance,xlab = "Velocity(km per second)", ylab = "Distance(km)")

#======5.2====
fit1 <- lm(Distance~Velocity)
abline(fit1, lwd = 2,col = 'red')
text(-200,5.0e17,"修正前",col = 'red')
summary(fit1)

#=====5.3====
fit2 <- lm(Distance~Velocity-1)
abline(fit2, lwd = 2,col = 'blue')
text(200,2.0e17,"修正后",col = 'blue')
summary(fit2)
#====宇宙的年龄为
coefficients(fit2)/ 60 / 60 / 24/ 365.2

#==============CPU Performance=============
library(MASS)
library(leaps)
library(tidyr)
data(cpus)
str(cpus)

sample_index <- sample(nrow(cpus),nrow(cpus)*0.80)
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]

#=====6.1=======
subset_result <- regsubsets(perf~syct+mmin+mmax+cach+chmin+chmax, 
                            data=cpus_train, nbest= 2)
plot(subset_result, scale="bic")
summary(subset_result)

#======6.2=====
model1 <- lm(perf~syct+mmin+mmax+cach+chmax, data = cpus_train)
summary(model1)
pre <- predict(model1, cpus_test)
a <- seq(1,42,1)
cpus_test_new <- as.data.frame(cpus_test)
#====实际值===
plot(a,cpus_test_new$perf,col = 'red')
lines(a,cpus_test_new$perf, col = 'red')
#===预测值===
points(pre,col = 'blue')
lines(pre,col = 'blue')

#===计算平均偏差==
mean(cpus_test_new$perf - pre)



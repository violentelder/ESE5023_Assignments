#==========1=========
#=======Flowchart====
Print_values <- function(){
  a = runif(1,-9999,9999)
  b = runif(1,-9999,9999)
  c = runif(1,-9999,9999)
  array1 <- c()
  if(a > b){
    if(b > c){
      array1 <- c(a, b ,c)
    }
    else{
      if(a > c){
        array1 <- c(a, c, b)
      }
      else{
        array1 <- c(c, a, b)
      }
    }
  }
  else{
    if(b > c){
      if(a > c){
        array1 <- c(a, c, b)
      }
      else{
        array1 <- c(c, a, b)
      }
    }
    else
      array1 <- c(c, b, a)
  }
  return(array1)
}

Print_values()

#=========2=======
#=====Matrix multiplication=====
m1 <- matrix(sample(1:50, 50),nrow = 5,ncol = 10,byrow = T)
m2 <- matrix(sample(1:50, 50),nrow = 10,ncol = 5,byrow = T)
Matrix_multip <- function(M1, M2){
  matrix_mul <- c()
  count <- 0
  for (i in 1:5) {
    for (k in 1:5) {
      sum <- 0
      for (j in 1:10) {
        temp <- M1[i,j] * M2[j,k]
        sum = sum + temp
      }
      count = count + 1
      matrix_mul[count] <- sum
    }
  }
  M3 <- matrix(matrix_mul,nrow = 5, ncol = 5, byrow = T)
  return(M3)
}
Matrix_multip(m1,m2)
#确认正确性
m3 <- m1 %*% m2
m3

#============3=========
#=======Pascal triangle=======
Pascal_triangle <- function(n){
  Pascal <- c()
  for (i in 0:n) {
    temp <- factorial(n - 1)/(factorial(i - 1) * factorial(n - i))
    Pascal[i] <- temp
  }
  return(Pascal)
}
Pascal_triangle(10)

#=======4=======
#======Add or double======
Least_moves <- function(n, moves = 0){
  if(n == 1){
    return(moves)
  }
  if(n %% 2 == 0){
    moves = moves + 1
    n = n / 2
    return(Least_moves(n,moves))
  }
  else{
    moves = moves + 1
    n = n - 1
    return(Least_moves(n,moves))
  }
}
Least_moves(5)

#=======5==========
#======Dynamic programming======
symbol <- c('+', '-', '')
num1 <<- c('1','2','3','4','5','6','7','8','9')
num2 <<- c('9','8','7','6','5','4','3','2','1')
strvectot <<- c()

#枚举出所有的表达式
assemble <- function(prevStr, leng){
  if(leng <= length(num1)){
    prevStr = paste(prevStr, num1[leng],sep = "") 
  }
  for (i in 1:length(symbol)) {
    #如果还没到第九个数字，则继续递归
    if(leng < length(num1)){
      nextstr <- paste(prevStr,symbol[i],sep = "")
      assemble(nextstr, leng + 1)
    }
    #如果到了则添加进strvectot中
    else{
      strvectot <<- c(strvectot, prevStr)
      break
    }
  }
  return(strvectot)
}
assemble("", 1)

#找出等于n的表达式集合
Find_expression <- function(n){
  count <- 0
  answer <- c()
  for (i in 1:length(strvectot)) {
    #eval(parse(text))可以将字符串转为命令执行，需要牢记
    if(eval(parse(text = strvectot[i])) == n){
      count = count + 1
      answer[count] <- paste("解",count,":",strvectot[i],"=",n,sep = "")
    }
  }
  answer[count + 1] <- paste("符合条件的等式一共有：",count, "个")
  return(answer)
}

Find_expression(100)

#=======5.2====
Find_expression_count <- function(n){
  count <- 0
  for (i in 1:length(strvectot)) {
    #eval(parse(text))可以将字符串转为命令执行，需要牢记
    if(eval(parse(text = strvectot[i])) == n){
      count = count + 1
    }
  }
  return(count)
}

Total_solutions <- c()
for (i in 1:100) {
  Total_solutions <- c(Total_solutions,Find_expression_count(i))
}

#绘制图像
plot(seq(1,100,1),Total_solutions,type = "l",
     xlab = "Num",ylab = "Expression_Count")
#标出最大值点及其横纵坐标
abline(h=max(Total_solutions),col="red",lty=2)
abline(v=which(Total_solutions==max(Total_solutions)),col="red",lty=2)
text(20,max(Total_solutions),max(Total_solutions),
     cex=2,col="blue")
text(which(Total_solutions==max(Total_solutions)),
     8,which(Total_solutions==max(Total_solutions)),
     cex=2,col="red")

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
      m1 <- mean(distance2[which(daycode == j)],na.rm = T)
      #计算当日能见度平均值时，可能会出现NA情况导致判断异常，故此处需要进行重新赋值
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
#结论：从图像中可以看出从2010年到2020年间，高能见度天数逐年增加，
#在2018年达到峰值，但在2019年开始出现了下降趋势



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
















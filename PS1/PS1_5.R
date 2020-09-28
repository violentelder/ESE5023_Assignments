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
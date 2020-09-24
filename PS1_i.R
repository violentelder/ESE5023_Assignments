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









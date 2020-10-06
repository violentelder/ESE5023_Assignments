#============3=========
#=======Pascal triangle=======
Pascal_triangle <- function(n){
  Pascal <- c()
  for (i in 0:n) {
    if(i == 0)
      temp<- 1
    else
      temp <- factorial(n - 1)/(factorial(i - 1) * factorial(n - i))
    Pascal[i] <- temp
  }
  return(Pascal)
}
Pascal_triangle(5)
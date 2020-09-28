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
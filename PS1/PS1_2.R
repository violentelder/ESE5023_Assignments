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
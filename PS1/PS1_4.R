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
Least_moves(100)
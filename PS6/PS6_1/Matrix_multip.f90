subroutine Matrix_multip(x, y, c, n)
implicit none
  integer                  :: n, i, j
  real(4), dimension(n, n) :: x, y
  real(8), dimension(n, n) :: c
  c = matmul(x,y)
  return
end


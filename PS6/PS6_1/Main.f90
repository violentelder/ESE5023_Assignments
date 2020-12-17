program Main
implicit none

integer                              :: u, v, n, i, j
real(4), dimension(:,:), allocatable :: a, b
real(8), dimension(:,:), allocatable :: c


! File unit
u = 50
v = 80

! Open the file
open(unit=u, file='M.dat', status='old')
open(unit=v, file='N.dat',status = 'old')

! The first line of the file has the number of values for arrays a and b
! 已提前知道M/N.dat均为方阵，且维数相同，此处代码简化只获取其中一个的行数即可
n = 3

! Allocate the arrays
allocate(a(n,n), b(n,n),c(n,n))

j = 1
do i = 1,n
  read(u, *) a(i,j), a(i,j+1), a(i,j+2)
  read(v, *) b(i,j), b(i,j+1), b(i,j+2)
enddo

! Close the file
close(v)
close(u)

call Matrix_multip(a,b,c,n)

! Write the values to a new file
open(unit=u, file='MN.dat', status='replace')

do i = 1,n
  write(u,'(f8.1,f8.1,f8.1)') c(i,j), c(i,j+1), c(i,j+2)
enddo
close(u)

! Display the values
do i = 1,n
  write(*,*) "The Answer:"
  write(*,*) "Line ", i, " : ", c(i,j), c(i,j+1), c(i,j+2)
enddo


! Deallocate the arrays
deallocate( a, b, c )

End Program Main

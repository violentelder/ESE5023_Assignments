program FunctionTest
use Declination_angle
use AST
implicit none

logical :: direct
Integer :: DMT, year, month, date, hour, min, day
real(8) :: DA, Long, Lat, H, SAA, SZA
Integer :: ApparentST(2)
real    :: p

p = 3.1415926536

write(*,*) 'Please input the year:'
read(*,*) year

write(*,*) 'Please input the month:'
read(*,*) month

write(*,*) 'Please input the date:'
read(*,*) date

write(*,*) 'Please input the hour:'
read(*,*) hour

write(*,*) 'Please input the minute:'
read(*,*) min

write(*,*) 'Please input the Time Zone(West -12 ---- 12 East):'
read(*,*) DMT

write(*,*) 'In the western longitudes? (please input ".true." or ".false.")'
read(*,*) direct

write(*,*) 'Please input the Longitude:'
read(*,*) Long

write(*,*) 'Please input the Latitude:'
read(*,*) Lat


day = DaysInYear(year, month, date)
write(*,*) 'The day in this year is: ', day

DA = DecAngle(year, day)
write(*,*) 'The declination angle is: ', DA, 'Deg'

ApparentST = ASTIME(Long, direct, DMT, year, hour, min, day)
write(*,*) 'The apparent solar time(AST) is: ', ApparentST(1), ':', ApparentST(2)

H = ((60 * dble(ApparentST(1)) + dble(ApparentST(2))) - 720)/4
write(*,*) 'The hour angle(H) is: ', H

SAA = asin(cos(Lat*p/180)*cos(DA*p/180)*cos(H*p/180) + sin(Lat*p/180) * sin(DA*p/180))
SAA = SAA * 180 / p
write(*,*) 'The  altitude angle is: ', SAA

SZA = 90 - SAA
write(*,*) 'The zenith angle(SZA) is: ', SZA


end program FunctionTest

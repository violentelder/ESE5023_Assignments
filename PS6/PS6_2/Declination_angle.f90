module Declination_angle  

implicit none 

   real, parameter :: pi = 3.1415926536  
   
contains 
   ! 计算当前日期在当年的天数
   Integer Function DaysInYear(year, mon, day)
      Integer :: year, mon , day
      Integer :: DaysInMonth(12) = [31,28,31,30,31,30,31,31,30,31,30,31]
      if ( ( (MOD(year,4)==0).and.(MOD(year,100)/=0) ) .or. (mod(year,400)==0) ) then
         DaysInMonth(2) = 29
      else
         DaysInMonth(2) = 28
      end if
      DaysInYear = sum( DaysInMonth(:mon-1) ) + day
   End Function DaysInYear

   ! 计算declination angle     
   real(8) Function DecAngle(year, n)          
      Integer :: year, n
      real    :: temp
      ! 转化为弧度制
      if ( ( (MOD(year,4)==0).and.(MOD(year,100)/=0) ) .or. (mod(year,400)==0) ) then
         temp = (n + 284) * 360 * pi / (366 * 180)
      else
         temp = (n + 284) * 360 * pi / (365 * 180)
      end if
      DecAngle = 23.45 *  sin(temp)       
   end Function DecAngle 
   
end module Declination_angle 

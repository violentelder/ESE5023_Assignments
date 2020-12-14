module AST

implicit none

   real, parameter :: pi = 3.1415926536

contains
   ! 计算当前时刻的Local Solar Time
   Function ASTIME(Long, direct, DMT, year, hour, min, day)
      logical :: direct
      Integer :: ASTIME(2)
      Integer :: DMT, year, hour, min, day, LSTM, temp
      real(8) :: D, ET, Long, tt
      ! 根据是否是闰年修改公式
      if ( ( (MOD(year,4)==0).and.(MOD(year,100)/=0) ) .or. (mod(year,400)==0) ) then
         D =360 * (dble(day) - 81) / 366
      else
         D =360 * (dble(day) - 81) / 365
      end if
      ET = 9.87 * sin(2*D*pi/180) - 7.53 * cos(D*pi/180) - 1.5 * sin(D*pi/180)
      if(direct .eqv. .true.) then
	Long = -Long
      else
	Long = Long
      endif
      LSTM = 15 * DMT
      temp = 4 * (Long - LSTM) + ET
      ASTIME(1) = hour + INT(temp/60)
      ASTIME(2) = min + MOD(temp, 60)

      if(ASTIME(2) > 60) then
	ASTIME(1) = ASTIME(1) + 1
	ASTIME(2) = ASTIME(2) - 60
      elseif(ASTIME(2) < 0) then
	ASTIME(1) = ASTIME(1) - 1
	ASTIME(2) = 60 + ASTIME(2)
      else
	ASTIME(1) = ASTIME(1) 
	ASTIME(2) = ASTIME(2)
      endif
      if(ASTIME(1) >= 24)then
	ASTIME(1) = ASTIME(1)  - 24
      else
	ASTIME(1) = ASTIME(1)
      endif
      print*, "D = ", D          
      print*,  "ET = ", ET  
      print*, "LSTM = ", LSTM          
   End Function ASTIME
end module AST

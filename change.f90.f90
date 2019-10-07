INTEGER FUNCTION Lunker(valueGiven)
       IMPLICIT NONE
       integer, intent(in) :: valueGiven
      integer :: nuValueGiven

      if((valueGiven/30) > 0) then
         write(*, fmt='(1x,i2,a)', advance = 'no') valueGiven/30, ' Lunker'
      end if
      nuValueGiven = mod(valueGiven, 30)
      lunker = nuValueGiven
END FUNCTION Lunker

INTEGER FUNCTION Loonter(valueGiven)
      IMPLICIT NONE
      integer, intent(in) :: valueGiven
      integer :: nuValueGiven

      if ((valueGiven/15) > 0) then
       write(*, fmt ='(1x,i2,a)', advance = 'no') valueGiven/15, ' Loonter'
      end if
      nuValueGiven = mod(valueGiven, 15)
      Loonter = nuValueGiven

END FUNCTION Loonter

INTEGER FUNCTION Little(valueGiven)
        IMPLICIT NONE
        integer, intent(in) :: valueGiven
        integer :: nuValueGiven

        if((valueGiven/5) > 0) then
          write(*, fmt='(1x,i2,a)', advance='no') valueGiven/5, ' Little'
       end if
       nuValueGiven = mod(valueGiven, 5)
       Little = nuValueGiven
END FUNCTION Little

INTEGER FUNCTION Pooney(valueGiven)
        IMPLICIT NONE
        integer, intent(in) :: valueGiven
        integer :: nuVal

        if(valueGiven > 0) then
          write(*, fmt='(1x,i2,a)', advance='no') valueGiven, ' Pooney'
        end if
        print*, ' '
END FUNCTION

SUBROUTINE PART1(valueGiven)
        IMPLICIT NONE
        integer :: Lunker, Loonter, Little, Pooney, Temp
        integer, intent(inout) :: valueGiven

        Temp = valueGiven
        valueGiven = Lunker(valueGiven)
        if((Temp/30) > 0) then
                WRITE(*,*)       
        end if
       
        Temp = valueGiven
        valueGiven = Loonter(valueGiven)
        if((Temp/15)>0) then
                WRITE(*,*)
        end if

        Temp = valueGiven
        valueGiven = Little(valueGiven)
        if((Temp/5)>0) then
                 WRITE(*,*)
        end if
        
        Temp = valueGiven
        valueGiven = Pooney(valueGiven)
        if(Temp>0) then
                 WRITE(*,*)
        end if
END SUBROUTINE

SUBROUTINE PART2(valueGiven)
        IMPLICIT NONE
        integer :: Lunker, Loonter, Little, Pooney, Temp
        integer, intent(inout) :: valueGiven                                              
        Temp = valueGiven
        valueGiven = Lunker(valueGiven)
        if((Temp/30) > 0) then
             WRITE(*, fmt='(1x,a)', advance ='no')','
        end if                                                             
     
        Temp = valueGiven
        valueGiven = Loonter(valueGiven)
        if((Temp/15) > 0) then
              WRITE(*, fmt='(1x,a)', advance='no')','
        end if

        Temp = valueGiven
        valueGiven = Little(valueGiven) 
        if((Temp/5)>0) then
             WRITE(*, fmt='(1x,a)', advance='no')',' 
        end if  
        
        valueGiven = Pooney(valueGiven)

END SUBROUTINE

SUBROUTINE handleErrors(checkErrors, valueGiven)
        IMPLICIT NONE
          integer :: checkErrors, valueGiven
          integer :: x=0  

           DO while(x==0)
             WRITE(*,*) "Enter an Integer between 1 and 99." 
             READ(*,'(i3)', iostat = checkErrors) valueGiven

             IF (valueGiven > 99) then
                WRITE(*,*)'An error has occured, please enter another value'
                WRITE(*,*)
             ELSE IF(valueGiven < 1) then
                WRITE(*,*) 'An error has occured, please enter another value'
                WRITE(*,*)
             ELSE
                x = 1
             END IF 
           END DO

END SUBROUTINE handleErrors


PROGRAM HWOne
        IMPLICIT NONE
                integer, external :: Lunker, Loonter, Little, Pooney
                integer :: valueGiven  = 0
                integer :: valueGiven2 = 0
                integer :: nuValueGiven = 0
                integer :: x = 0
                
                integer checkErrors

                CHARACTER(LEN = 8) :: DateINFO
                CHARACTER(LEN = 4) :: Year, Month*2, Day*2
                CHARACTER(LEN = 10) :: TimeINFO
                CHARACTER(LEN = 2) :: Hour, Minute
                
                !Calls the initial question 
               
                CALL DATE_AND_TIME(DateINFO, TimeINFO)
                    Month = TimeINFO(5:6)
                    Day = DateINFO(7:8)
                    Year = DateINFO(1:4)
                    Hour = TimeINFO(1:2)
                    Minute = TimeINFO(3:4)

                    WRITE(*,*)'mo-dy-year, Hr:mi'
                    WRITE(*,*) Month,'-',Day,'-',Year,', ',Hour,':',Minute
                    WRITE(*,*)

                CALL handleErrors(checkErrors, valueGiven)
                !Calls and prints the first and second part               
                valueGiven2 = valueGiven
                CALL PART1(valueGiven)
                CALL PART2(valueGiven2)
                !Calls and prints the third part
 END PROGRAM HWOne

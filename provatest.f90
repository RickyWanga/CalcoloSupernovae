PROGRAM provaa
    IMPLICIT NONE
    CHARACTER:: B
    INTEGER:: nd,i,j
    REAL*8, ALLOCATABLE:: a(:,:)
    real*8 :: var


    var = -42
   nd=5
    ALLOCATE (a(nd,nd) )


   OPEN(12,file='fileprovaa.dat')
  DO i=1,nd
     READ(12,*) a(i,1), a(i,2), a(i,3), a(i,4), a(i,5)
  END DO

  DO I=1, nd
    do j=1, nd
        if(a(i,j)==var) then
            a(i,j) = 3
        end if
    end do
  END DO



  DO i=1,nd
     WRITE(*,*) (a(i,j),j=1,nd)
  END DO







END PROGRAM provaa
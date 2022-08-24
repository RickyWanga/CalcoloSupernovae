PROGRAM sorting13
    IMPLICIT NONE
    REAL*8, ALLOCATABLE:: x(:), f(:), newcolumn1(:), newcolumn2(:)
    REAL*8 minimum, substitution
    INTEGER::ndata, i, position_minimum, j
    
    OPEN(44, file="dati_per_sorting.dat")

      ndata=0
      DO
        READ(44,*, END=77)
        ndata=ndata+1
      END DO

    77 CONTINUE
    
     PRINT *, "The file contains", ndata, "data."
     ALLOCATE(x(ndata), f(ndata))
   
     REWIND(44)
     
     !Sto creando due vettori
     DO i=1,ndata
        READ(44,*) x(i), f(i)
     END DO
     
    DO i=1, ndata
      PRINT *, x(i), f(i)
    END DO
    CLOSE(44)
      
      
      newcolumn1=x
      newcolumn2=f
  
  
      DO i=1,ndata-1
        minimum=newcolumn1(i)
        position_minimum=i
        DO j=i+1, ndata
          IF(newcolumn1(j)<minimum) THEN
            minimum=newcolumn1(j)
            position_minimum=j
          END IF
        END DO
        newcolumn1(position_minimum)=newcolumn1(i)
        newcolumn1(i)=minimum
        substitution=newcolumn2(position_minimum)
        newcolumn2(position_minimum)=newcolumn2(i)
        newcolumn2(i)=substitution
      END DO 

      print *, "Dati ordinati"
    DO i=1,ndata
      print *, newcolumn1(i), newcolumn2(i)
   END DO

END PROGRAM sorting13
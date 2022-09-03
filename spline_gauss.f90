!Nota per te: le x sono i giorni, le f(x) sono i valori registrati. x=giorno, f(x)=valore calcolato in corrispondenza di x giorno

PROGRAM spline
  IMPLICIT NONE
  REAL*8, ALLOCATABLE:: x(:), f(:), newcolumn1(:), newcolumn2(:)
  REAL*8, ALLOCATABLE :: mat(:,:), c(:)
  REAL*8, ALLOCATABLE :: der(:), acopy(:,:), ccopy(:)
  REAL*8 :: minimum, substitution, summa, diff
  INTEGER::ndata, i, position_minimum, j
  
  OPEN(44, file="dati_per_sorting.dat")

    ndata=0
    DO
      READ(44,*, END=77)
      ndata=ndata+1
    END DO

  77 CONTINUE
  
   PRINT *, "Il file contiene", ndata, "data."
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
 
 !Costruisco la matrice rappresentativa
 
 ALLOCATE(mat(ndata,ndata), c(ndata))
 mat(:,:)=0.d0
 c(:)=0.d0

 DO i=1, ndata
  IF(i .eq. 1) THEN
    mat(i,1) = 1.0d0
  ELSE IF(i .eq. ndata) THEN
    mat(i,ndata) = 1.0d0
  ELSE 
    DO j=i-1, i+1
      IF(j .lt. i) THEN
        mat(i,j) = x(i) - x(i-1)
      ELSE IF (j .gt. i) THEN
        mat(i,j) = x(i+1) - x(i)
      ELSE
        mat(i,j) = 2.0d0*(x(i+1) - x(i-1))
      END IF
    END DO
  ENDIF

  IF((i .gt. 1) .and. (i .lt. ndata)) THEN
    c(i) = 6.0d0 * (((f(i+1) - f(i)) / (x(i+1) - x(i))) + (f(i-1) - f(i)) / (x(i) - x(i-1)))
  END IF
END DO

 !Nota per te: le x sono i giorni, le f(x) sono i valori registrati. x=giorno, f(x)=valore calcolato in corrispondenza di x giorno
  
 PRINT *, "Matrice rappresentativa:"
 WRITE(*,*)

  DO i=1,ndata
     WRITE(*,*) (mat(i,j), j=1,ndata)
  END DO

  WRITE(*,*)

  PRINT *, "Termine noto:"

  DO i=1,ndata
     WRITE(*,*) c(i)
  END DO

  WRITE(*,*)


 !Ora inizia la parte di programma che risolve il sistema lineare

 PRINT *, "Ora mi appresto a ridervere il sistema lineare algebrico."    !!!Sei qui

  ALLOCATE(der(ndata))  

    acopy=mat
    ccopy=c

    CALL gauss(acopy,ccopy,ndata,der)
   
    PRINT *, "Derivate seconde:"
    DO i=1,ndata
      print *, der(i)
    END DO
 
    PRINT *, "Procedo a verificare se i risultati ottenuti tramite il metodo di Gauss sono corretti."

    DO i=1,ndata
        summa=0.d0
        DO j=1,ndata
           summa=summa+mat(i,j)*der(j)
        END DO
        diff=ABS(summa-c(i))
        IF(diff>=1.d0)THEN
          PRINT *, "Procedura non eseguita correttamente."
        ELSE IF(diff<1.d0)THEN
          PRINT *, "Procedura eseguita correttamente."
        END IF
    END DO

END PROGRAM spline





SUBROUTINE gauss(mat,c,n,x)
  IMPLICIT NONE
  INTEGER, INTENT(IN) ::n
  REAL*8, INTENT(OUT):: mat(n,n)
  REAL*8, INTENT(INOUT):: c(n) 
  REAL*8, INTENT(OUT):: x(n)
  INTEGER:: i,j,k
  REAL*8:: fakt, summa

  PRINT *, "Eseguo la forward elimination."

  DO i=1,n-1   !sceglie la variabile da eliminare
     DO j=i+1,n  !sceglie la riga su cui eliminare
          fakt=mat(j,i)/mat(i,i)
        DO k=1,n
           mat(j,k)=mat(j,k)-mat(i,k)*fakt
        END DO
        c(j)=c(j)-c(i)*fakt
     END DO
  END DO
  
  PRINT *, "Eseguo la backward substitution."

  x(n)=c(n)/mat(n,n)

   DO i=n-1,1,-1
       summa=c(i)
       DO j=i+1,n
          summa=summa-mat(i,j)*x(j)
       END DO
       x(i)=summa/mat(i,i)
    END DO
  
END SUBROUTINE gauss
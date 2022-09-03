!Nota: devi creare un file con tutti i valori del massimo per le supernovae
!Nota: stai interpolando le x. Le x per cui devono essere i valori in banda B, mentre le f i giorni. Tu devi calcolare il valore in corrispondenza del quale si ha il massimo, cioe devi trovare f(val_massimo).

PROGRAM spline
    IMPLICIT NONE
    REAL*8, ALLOCATABLE:: x(:), f(:), newcolumn1(:), newcolumn2(:)
    REAL*8, ALLOCATABLE :: mat(:,:), c(:)
    REAL*8, ALLOCATABLE :: der(:), acopy(:,:), ccopy(:)
    REAL*8 :: summa, diff, gg_massimo, val_massimo
    INTEGER::ndata, i, j
    
    OPEN(44, file="dati_per_sorting.dat")
  
      ndata=0
      DO
        READ(44,*, END=77)
        ndata=ndata+1
      END DO
  
    77 CONTINUE
    
     PRINT *, "Il file contiene", ndata, "dati."
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

  CALL sorting(newcolumn1, newcolumn2, ndata)

   DO i=1,ndata
    x(i)=newcolumn1(i)
    f(i)=newcolumn2(i)
   END DO

   PRINT *, "Dati ordinati"
      DO i=1,ndata
      print *, "Valori registrati:", x(i), "Giorno di osservazione:", f(i)
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
  
   PRINT *, "Ora mi appresto a risolvere il sistema lineare algebrico."    
  
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
            PRINT *, "Derivata seconda con pedice",i,": ottenuta non correttamente."
          ELSE IF(diff<1.d0)THEN
            PRINT *, "Derivata seconda con pedice",i,": ottenuta correttamente."
          END IF
      END DO
  
      PRINT *, "Procedo a calcolare l'intervallo di tempo entro il quale e' stato misurato il massimo."
      
      !val_massimo è diverso per ogni supernovae
      PRINT *, "Immetti il valore da interpolare:"
      READ(*,*) val_massimo


      CALL interpolation(ndata, x, f, der, val_massimo, gg_massimo) 
      WRITE(*,*) val_massimo, gg_massimo

  END PROGRAM spline
  
  

  SUBROUTINE sorting(newcolumn1, newcolumn2, ndata)
    IMPLICIT NONE
    REAL*8, INTENT(INOUT):: newcolumn1(ndata), newcolumn2(ndata)
    INTEGER:: i, j, ndata, position_minimum
    REAL*8:: minimum, substitution
    
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
  END SUBROUTINE sorting


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


  SUBROUTINE interpolation(ndata, x, f, der, val_massimo, gg_massimo)
    IMPLICIT NONE
    INTEGER:: interval,i
    INTEGER, INTENT(IN):: ndata 
    REAL*8, INTENT(IN):: x(ndata), f(ndata), der(ndata), val_massimo
    REAL*8, INTENT(OUT):: gg_massimo
     
    !Cerco l'intervallo al quale appartiene val_massimo
    interval=0.d0

    IF(val_massimo<x(1).OR.val_massimo>x(ndata)) THEN
        PRINT *, "L'interpolazione non e' possibile."
    ELSE 
        DO i=1,ndata-1
            IF(val_massimo<=x(i+1).AND.val_massimo>=x(i)) THEN
                interval = i+1 
                EXIT
            END IF
        END DO
    END IF

    gg_massimo=(der(interval-1) * ((x(interval) - val_massimo)**3) + der(interval) * ((val_massimo - x(interval-1))**3)) / & 
         (6.0d0*(x(interval)-x(interval-1))) + &
         (f(interval-1) / (x(interval)-x(interval-1)) - &
         der(interval-1)*(x(interval)-x(interval-1)) / 6.0d0)*(x(interval)-val_massimo) + &
         (f(interval) / (x(interval)-x(interval-1)) - &
         der(interval)*(x(interval)-x(interval-1)) / 6.0d0)*(val_massimo - x(interval-1))

  END SUBROUTINE interpolation
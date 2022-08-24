PROGRAM elim
    IMPLICIT NONE
    INTEGER:: i,j,ndata,nd
    REAL*8 :: summa, diff
    REAL*8, ALLOCATABLE:: a(:,:), c(:), matrix(:,:), x(:)
    REAL*8, ALLOCATABLE:: acopy(:,:), ccopy(:)

    OPEN(12, file="matrice.dat")
    ndata=0
    DO
        READ(12,*, end=100)
        ndata=ndata+1
    END DO

100 CONTINUE

    PRINT *, 'Ho letto', ndata, 'dati.'

    ALLOCATE(a(ndata,ndata+1), c(ndata), matrix(ndata,ndata),x(ndata))
    
    REWIND(12)

    DO i=1,ndata
       READ(12,*,end=101) (a(i,j), j=1,ndata+1)
    END DO

101 CONTINUE

    DO j=1,ndata
        c(j)=a(j,ndata+1)
    END DO

    PRINT *, "Termini noti:" 
    DO j=1,ndata
        PRINT *, c(j)
    END DO
        
    DO i=1,ndata
        DO j=1,ndata
            matrix(i,j)=a(i,j)
        END DO
    END DO

    PRINT *, "Matrice rappresentativa:"
    DO j=1,ndata
        PRINT *, matrix(1,j), matrix(2,j), matrix(3,j), matrix(4,j), matrix(5,j)
    END DO

    CLOSE(12)

    acopy=matrix
    ccopy=c
    nd=ndata

    CALL gauss(acopy,ccopy,x,nd)
  
    PRINT *, "le soluzioni sono"
    DO i=1,nd
       PRINT *, x(i)
    END DO

    DO i=1,nd
         summa=0.d0
         DO j=1,nd
            summa=summa+a(i,j)*x(j)
         END DO
         PRINT *, summa, c(i)
    END DO
        
    PRINT *, "Mi accingo a verificare i risultati ottenuti tramite metodo di Gauss."

    DO i=1,nd
        summa=0.
        DO j=1,nd
           summa=summa+a(i,j)*x(j)
        END DO
        diff=ABS(summa-c(i))
        PRINT *, i, summa, c(i), diff
        IF(diff>1.d-10) PRINT *, "attenzione: problema"
     END DO

END PROGRAM elim

SUBROUTINE gauss(a,c,x,n)
  IMPLICIT NONE
  INTEGER::n,i,j,k
  REAL*8:: a(n,n),c(n),x(n)
  REAL*8:: fakt,summa
  
  DO i=1,n-1 ! sceglie la variabile da eliminare
     DO j=i+1,n ! sceglie la riga su cui eliminare
        fakt=a(j,i)/a(i,i)
        DO k=1,n
           a(j,k)=a(j,k)-a(i,k)*fakt
        END DO
        c(j)=c(j)-c(i)*fakt
     END DO
     DO j=1,n
       WRITE(*,*) (a(j,k),k=1,n), c(j)
     END DO
  END DO
 
   x(n)=c(n)/a(n,n)
   DO i=n-1,1,-1
       summa=0.d0
       DO j=i+1,n
          summa=summa+a(i,j)*x(j)
       END DO
       x(i)=(c(i)-summa)/a(i,i)
    END DO
  
END SUBROUTINE gauss

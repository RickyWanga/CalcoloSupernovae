PROGRAM pol_newton
    IMPLICIT NONE
    REAL*8, EXTERNAL:: fun
    REAL*8, ALLOCATABLE:: x(:), f(:), mat(:,:)
    INTEGER::ndati,i, irighe, icol, j
    REAL*8::x_int, esatto, summa, prod
  
    PRINT *, 'quanti dati vuoi considerare?'
    READ(*,*) ndati
    ALLOCATE(x(0:ndati-1), f(0:ndati-1))
    ALLOCATE(mat(0:ndati-1,0:ndati-1))
  
    mat=0.d0
    
    DO i=0,ndati-1
       x(i)=0.5*i
       f(i)=fun(x(i))
       PRINT *, i, x(i), f(i)
    END DO
    
    DO i=0,ndati-1
       mat(i,0)=f(i)
    END DO
    
  !  mat(0:ndati-1,0)=f(0:ndati-1)
  !  mat(:,0)=f(:)
  
    DO icol=1,ndati-1
       DO irighe=0, ndati-icol-1
          mat(irighe,icol)=(mat(irighe+1,icol-1)-mat(irighe,icol-1))/&
               (x(irighe+icol)-x(irighe))
       END DO
    END DO
  
    PRINT *, "dove vuoi interpolare?"
    READ(*,*) x_int
    esatto=fun(x_int)
  
    summa=0.
    DO i=0,ndati-1
       prod=1.
       DO j=0,i-1
          prod=prod*(x_int-x(j))
       END DO
       summa=summa+prod*mat(0,i)
       PRINT *, i, summa, esatto
       READ(*,*)
    END DO
  
    PRINT *, "faccio interpolazione con lagrange"
  
    summa=0.
    DO i=0,ndati-1
       prod=1.
       DO j=0,ndati-1
          IF(i/=j) prod=prod*(x_int-x(j))/(x(i)-x(j))
       END DO
       summa=summa+prod*f(i)
       WRITE(*,*) i, summa, esatto
    END DO
    
  END PROGRAM pol_newton
  
  REAL*8 FUNCTION fun(x)
    IMPLICIT NONE
    REAL*8::x
    fun=x**6+4.*(x**5)-32.*(x**4)-18.*(x**3)-2.*(x**2)+4.*x-20.
  END FUNCTION fun
  
REAL*8 FUNCTION integranda(z)
 IMPLICIT none
 REAL*8 :: z 

 integranda = 1.0d0 / sqrt((0.3 * ((1+z)**3)) + 0.7)

END FUNCTION integranda


PROGRAM newtoncotes
    IMPLICIT NONE
    REAL*8:: integrale, a, b
    INTEGER::n, ordine
    REAL*8, EXTERNAL:: integranda
  
    a = 0.d0
    b = 2.0d0
    n = 1000
    PRINT *, "quale ordine di newton-cotes vuoi usare?"
    READ(*,*) ordine
     
    CALL newton(integranda, a, b, n, ordine, integrale)
    
    PRINT *, n, integrale

        
  END PROGRAM newtoncotes
  
  SUBROUTINE newton(fun,a,b,n,ordine,risultato)
    IMPLICIT NONE
    REAL*8, EXTERNAL:: fun
    REAL*8::a,b,risultato,h,x1,x2, area, xm, delta
    INTEGER::n,i, ordine
    h=(b-a)/n
    risultato=0.d0
    DO i=1,n
       x1=a+(i-1)*h
       x2=a+i*h
       SELECT CASE(ordine)
       CASE(1)
          area=h*0.5*(fun(x1)+fun(x2))
       CASE(2)
          xm=0.5*(x1+x2)
          area=(h/6.)*(fun(x1)+4.*fun(xm)+fun(x2))
       CASE(3)
          delta=(x2-x1)/3.
          area=(h/8.)*(fun(x1)+3.*fun(x1+delta)+3.*fun(x1+2.*delta)+fun(x2))
       CASE(4)
          delta=(x2-x1)/4.
          area=(h/90.)*(7.*fun(x1)+32.*fun(x1+delta)+12.*fun(x1+2.*delta)+&
               32.*fun(x1+3.*delta)+ 7.*fun(x2))
       CASE DEFAULT
          PRINT *, "ordine non implementato"
          STOP
       END SELECT
       risultato=risultato+area
    END DO
  END SUBROUTINE newton
  
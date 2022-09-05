REAL*8 FUNCTION integranda(z)
 IMPLICIT none
 REAL*8 :: z 

 integranda = 1.0d0 / sqrt((0.3 * ((1+z)**3)) + 0.7)

END FUNCTION integranda

SUBROUTINE simpson1_3(variab, a, b, n, res)
    IMPLICIT NONE
    INTEGER:: i 
    REAL*8:: xup, xdown, xmed, h
    REAL*8, INTENT(IN):: a, b
    INTEGER, INTENT(IN):: n
    REAL*8, INTENT(OUT):: res
    REAL*8, EXTERNAL:: variab
  
    h = (b-a)/n
    res = 0.0d0
    
    DO i=1, n
        xup = a + h*i
        xdown = a + h*(i-1)
        xmed = 0.5d0*(xup + xdown)
        res = res + variab(xdown) + variab(xup) + 4.0d0*variab(xmed)
    END DO

      res = h*res/6.0d0
      
END SUBROUTINE simpson1_3

PROGRAM integrale 
    IMPLICIT NONE
    REAL*8, EXTERNAL :: integranda
    REAL*8:: a, b, res
    INTEGER:: n

    a = 0.d0
    !b = Z_CMB
    b = 2.0d0
    n = 1000

    
    CALL simpson1_3(integranda, a, b, n, res)

    PRINT * , "Risultato:", res


END PROGRAM 
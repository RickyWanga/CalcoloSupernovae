PROGRAM prova1
    IMPLICIT NONE
    INTEGER:: ndati
    
    OPEN(12, file="dati_monotonia.dat")
    ndati=0

    DO 
        READ(12,*)
        ndati=ndati+1
        IF(ndati==5) THEN
            EXIT
        END IF
    END DO

    PRINT * ,"Ciao"

END PROGRAM prova1
        
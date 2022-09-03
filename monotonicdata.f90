PROGRAM monotonic
    IMPLICIT NONE
    INTEGER:: ndati, n, i, j, k
    REAL*8, ALLOCATABLE:: obs(:), magn(:), maxx(:)
    REAL*8, ALLOCATABLE:: obs_ordinate(:), magn_ordinate(:)

    OPEN(12, file="dati_monotonia.dat")
    ndati=0
    DO 
        READ(12,*,end=111)
        ndati=ndati+1
    END DO

111 CONTINUE
    
    PRINT *, "Ho letto", ndati, "dati"

    REWIND(12)

    ALLOCATE(obs(ndati), magn(ndati))
    DO n = 1, ndati
        READ(12,*) obs(n), magn(n) 
    END DO

    PRINT *, "Verifico se i giorni di osservazione sono in ordine crescente."

    DO i=1,ndati-1
        IF(obs(i)<obs(i+1)) THEN
            IF(i==ndati-1) THEN
                PRINT *, "I dati in partenza erano ordinati." 
                DO k=1,ndati
                    PRINT *, obs(k), magn(k)
                END DO
            END IF
        ELSE IF(obs(i)>obs(i+1)) THEN
            PRINT *, "I dati non sono ordinati, procedo a ordinarli."
            obs_ordinate=obs
            magn_ordinate=magn
            CALL sorting_monotonia(obs_ordinate, magn_ordinate, ndati)
            DO j=1,ndati
                IF(j==1) THEN 
                    PRINT *, "Dati ordinati:"
                END IF
                obs_ordinate(j)=obs(j)
                magn_ordinate(j)=magn(j)
                PRINT *, obs(j), magn(j)
            END DO
            EXIT
        END IF
    END DO

    PRINT *, "Procedo a verificare se i dati sono non monotoni decrescenti."
    
    CALL monotonia(magn, ndati, max)

END PROGRAM monotonic



SUBROUTINE monotonia(x, n, max)
    IMPLICIT NONE
    REAL*8, INTENT(IN):: x(n)
    REAL*8, INTENT(OUT):: max
    INTEGER:: n, i

    max=maxval(x)
    PRINT *, "Massimo assoluto:", max

    IF(max==x(i).OR.max==x(n)) THEN
        PRINT *, "Dati non accettabili."
    ELSE IF(max/=x(i).AND.max/=x(i)) THEN
        PRINT *, "Dati accettabili."
    END IF 

END SUBROUTINE monotonia


SUBROUTINE sorting_monotonia(obs_ordinate, magn_ordinate, ndati)
    IMPLICIT NONE
    INTEGER :: posizione_minimo, ndati, i, j
    REAL*8, INTENT(INOUT) :: obs_ordinate(ndati:ndati), magn_ordinate(ndati:ndati)
    REAL*8 :: minimo, variab

    DO i=1,ndati-1
        minimo=obs_ordinate(i)
        posizione_minimo=i
        DO j=i+1, ndati
          IF(obs_ordinate(j)<minimo) THEN
            minimo=obs_ordinate(j)
            posizione_minimo=j
          END IF
        END DO
        obs_ordinate(posizione_minimo)=obs_ordinate(i)
        obs_ordinate(i)=minimo
        variab=magn_ordinate(posizione_minimo)
        magn_ordinate(posizione_minimo)=magn_ordinate(i)
        magn_ordinate(i)=variab
    END DO 

    END SUBROUTINE sorting_monotonia

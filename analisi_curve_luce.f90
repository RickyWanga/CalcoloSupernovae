PROGRAM analisicurve
    IMPLICIT none
    INTEGER :: ndati, j, k, n, s, m, select1, select2
    REAL*8, ALLOCATABLE:: observation_day(:), error_B(:), magnitude_B(:), minimum(:), maximum1(:), maximum2(:)
    
    !Leggo il numero di colonne
    OPEN(44, file="test_riga.dat")
    ndati=0
    DO 
        READ(44,*,end=100)
        ndati=ndati+1
    END DO

100 CONTINUE
 
    PRINT *, "Ho letto", ndati, "dati"

    PRINT *, "Procedo a verificare se i dati sono monotoni decrescenti."
    !Creo una matrice 'dati': nella prima colonna pongo i nomi delle supernovae, nella seconda colonna e terza colonna rispettivamente i valori nella banda B e i relativi errori   

    REWIND(44)
    ALLOCATE(observation_day(ndati),magnitude_B(ndati),error_B(ndati), minimum(ndati))
    DO n = 1, ndati
      READ(44,*) observation_day(n), magnitude_B(n), error_B(n)    
      PRINT *, observation_day(n), magnitude_B(n), error_B(n)
    END DO
    
    CLOSE(44)

    !Ora controllo che i dati siano non monotoni e decrescenti
    
    !Monotona(?) decrescente
    IF(magnitude_B(1)>magnitude_B(2)) THEN   
        PRINT *, "Il primo valore registrato in banda B e', in modulo, maggiore del secondo"
        DO j=2,ndati-1
            IF(j == ndati-1) THEN
                PRINT *, "I dati non sono analizzabili."  
            END IF 
            IF(select1 == 1) THEN
                EXIT  
            ELSE IF(magnitude_B(j)<magnitude_B(j+1)) THEN
                minimum(1)=magnitude_B(j)
                DO k=j+1,ndati-2
                    IF(magnitude_B(k)>magnitude_B(k+1)) THEN 
                        maximum1(1)=magnitude_B(k)
                        PRINT *, "La funzione ammette un massimo:",magnitude_B(k),"I dati sono pertanto analizzabili."
                        select1 = 1
                        EXIT
                    END IF
                END DO 
            END IF   
        END DO
    END IF
    !Monotona(?) crescente
    IF(magnitude_B(1)<magnitude_B(2)) THEN 
        PRINT *, "Il primo valore registrato in banda B e', in modulo, minore del secondo"
        DO m=2,ndati-1
            IF(m == ndati-1) THEN
                PRINT *, "I dati non sono analizzabili."
            END IF 
            IF(select2 == 1) THEN
                EXIT 
            ELSE IF(magnitude_B(m)>magnitude_B(m+1)) THEN
                maximum2(1)=magnitude_B(m)
                DO s=m+1,ndati-2
                    IF(magnitude_B(s)<magnitude_B(s+1)) THEN 
                        maximum2(1)=magnitude_B(s)
                        PRINT *, "La funzione ammette un minimo:",magnitude_B(s),"I dati sono pertanto analizzabili."
                        select2 = 1
                        EXIT
                    END IF
                END DO
            END IF
        END DO
    END IF   

END PROGRAM analisicurve

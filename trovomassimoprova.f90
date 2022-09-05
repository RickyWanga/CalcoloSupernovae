PROGRAM tr
    IMPLICIT NONE
    


IF(gg_max<x(1).OR.gg_max>x(ndata)) THEN
    PRINT *, "L'interpolazione non e' possibile."
ELSE IF(gg_max>x(1).OR.gg_max<x(ndata)) THEN
    DO i=1,ndata-1
        IF(gg_max<=x(i+1).AND.gg_max>=x(i)) THEN
            DO g=0.0001
            interval = i+1 
            EXIT
        END IF
    END DO
END IF

END PROGRAM tr
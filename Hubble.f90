!Commento: alla riga 231 devi specificare quale supernovae nel file prendere per calcolare redshift...!
REAL*8 FUNCTION integranda(z)
 IMPLICIT none
 REAL*8 :: z 

 integranda = 1.0d0 / sqrt((0.3 * ((1+z)**3)) + 0.7)

END FUNCTION integranda


REAL*8 FUNCTION integr_aperto(z)
 IMPLICIT NONE
 REAL*8 :: z

 integr_aperto = 1.0d0 / sqrt(0.3*((1+z)**3) + 0.7*((1+z)**2))
END FUNCTION integr_aperto


REAL*8 FUNCTION integr_piatto(z)
 IMPLICIT NONE
 REAL*8 :: z

 integr_piatto = 1.0d0 / sqrt((1+z)**3)

END FUNCTION integr_piatto


REAL*8 FUNCTION integr_chiuso(z)
 IMPLICIT NONE
 REAL*8 :: z
 
 integr_chiuso = 1.0d0 / sqrt(2*((1+z)**3) - 1*((1+z)**2))

END FUNCTION integr_chiuso


PROGRAM Costante_di_Hubble
  USE funzioni
  IMPLICIT NONE
  REAL*8, ALLOCATABLE :: MJD(:), BandaB(:), e_BandaB(:), MJDtemp(:), BandaBtemp(:), e_BandaBtemp(:)
  REAL(kind=8) :: xx = 0, y = 0
  CHARACTER*6, ALLOCATABLE :: nomiSupernovae(:)
  CHARACTER*20, ALLOCATABLE :: FileNomiSupernovae(:)
  CHARACTER*20, ALLOCATABLE :: PathSupernovae(:)
  CHARACTER*20 testnome
  INTEGER :: NUM_RIGHE = 0, FF = 1, NUM = 1, NUM_SUP = 0, M = 0, Max = 0, Scartati = 0, G = 0
  INTEGER :: BB = 0, Z = 0, test = 0, P = 0, IERR = 0, NUM_LINES = 0
  CHARACTER*256 :: CTMP
  REAL*8, ALLOCATABLE:: x(:), f(:), newcolumn1(:), newcolumn2(:), newcolumn3(:)
  REAL*8, ALLOCATABLE :: mat(:,:), c(:), supernovaedat(:), valori_costHubble(:), simulated(:)
  REAL*8, ALLOCATABLE :: der(:), acopy(:,:), ccopy(:), errorB(:), copyf(:), copyerrorB(:)
  REAL*8, EXTERNAL:: integranda, integr_aperto, integr_chiuso, integr_piatto
  REAL*8 :: summa, diff, gg_max, valmax_B, g_15, bmax, b15, delta_m15, B_max_ass
  REAL*8 :: mass, tempvar, toll1, toll2, up, mu, distanza, meanvalue, dev_standard, varianza
  REAL*8 :: EB_V, Rv, b_maxcorr, Z_CMB, x1, x2, y1, y2, media, sigma
  REAL*8 :: b, a, hub, c_luce, toll_Hubble1, toll_Hubble2
  REAL*8 :: Hubble, Hubble_piatto, Hubble_aperto, Hubble_chiuso
  INTEGER :: ndata, i, j, pos_massimo, l, npoints1, npoints2
  INTEGER :: contator, variab, n, jran, k
  INTEGER, PARAMETER :: a1=7141, c1=54773, m1=259200
  CHARACTER*6 :: Stringa
  
  !Creo filetemp
  NUM_SUP = contaRighe("NomiFileSupernovae.dat")
  PRINT *, "Numero Supernovae : ", NUM_SUP
    
  ALLOCATE(nomiSupernovae(NUM_SUP), PathSupernovae(NUM_SUP), FileNomiSupernovae(NUM_SUP))

  OPEN(unit=42,file="NomiFileSupernovae.dat")
  DO NUM = 1, NUM_SUP
    READ(42,*) nomiSupernovae(NUM)
    FileNomiSupernovae(NUM) = 'SN_temp/SN' // (nomiSupernovae(NUM)) // '.dat'
    PathSupernovae(NUM) = 'SN_data/SN' // (nomiSupernovae(NUM)) // '.dat'
  END DO
  CLOSE(42)

  DO NUM = 10, NUM_SUP + 9
    FF = NUM - 9
    OPEN(unit=NUM, file=PathSupernovae(FF))
    PRINT *, "File pulito : ", PathSupernovae(FF)
    NUM_LINES = 0
    IERR = 0
    CTMP = ""
    DO WHILE (IERR == 0)
      NUM_LINES = NUM_LINES + 1
      READ(NUM,*,iostat=IERR) CTMP
    END DO
    NUM_RIGHE = NUM_LINES - 1
    testnome = PathSupernovae(FF)
    test = contaRighe(testnome)
    PRINT *, "Lunghezza file : ", test
    Max = NUM_RIGHE - 5
    ALLOCATE(MJD(Max),BandaB(Max),e_BandaB(Max))
    REWIND(NUM)
    DO P = 1, NUM_RIGHE
      IF(P>=6) THEN
        M = P - 5
        READ(NUM,*) MJD(M), xx, y, BandaB(M), e_BandaB(M)
      ELSE
        READ(NUM,*)
      END IF
    END DO
    CLOSE(NUM)

    Scartati = 0

    P = 1
    DO
      IF(P > Max) THEN
        EXIT
        !Controllo che i dati dell'osservazione siano validi
      ELSE IF (.NOT.((BandaB(P) >= 99.0).AND.(BandaB(P)<= 99.99))) THEN
        P = P + 1
      ELSE
        Scartati = Scartati + 1
        P = P + 1
      END IF
    END DO
        
    ALLOCATE(MJDtemp(Max-Scartati), BandaBtemp(Max-Scartati), e_BandaBtemp(Max-Scartati))
        
    BB = 1
    Z = 1
        
    DO WHILE(BB<=Max)
      IF (.NOT.((BandaB(BB) >= 99.0).AND.(BandaB(BB)<= 99.99))) THEN
        MJDtemp(Z) = MJD(BB)
        BandaBtemp(Z) = BandaB(BB)
        e_BandaBtemp(Z) = e_BandaB(BB)
        BB = BB + 1
        Z = Z + 1
      ELSE 
        BB = BB + 1
      END IF
    END DO

    OPEN(NUM, file=FileNomiSupernovae(FF))
    DO P=1, Z-1
      WRITE(NUM, *) MJDtemp(P), BandaBtemp(P), e_BandaBtemp(P)
    END DO
    CLOSE(NUM)

    DEALLOCATE(MJD, BandaB, e_BandaB)
    DEALLOCATE(MJDtemp, BandaBtemp, e_BandaBtemp)

  END DO

  DEALLOCATE(PathSupernovae, FileNomiSupernovae)

  !Fine creazione file

  DO G=1, NUM_SUP

  OPEN(44, file=FileNomiSupernovae(G))
  ndata=0
  DO
    READ(44,*, END=100)
    ndata=ndata+1
  END DO

  100 CONTINUE
  
  PRINT *, "Il file contiene", ndata, "dati."

  ALLOCATE(x(ndata), f(ndata), errorB(ndata))
 
  REWIND(44)
   
  !Sto creando due vettori
  DO i=1,ndata
    READ(44,*) x(i), f(i), errorB(i)
  END DO
  
  CLOSE(44)

  newcolumn1=x
  newcolumn2=f
  newcolumn3=errorB

  !Ordino i dati

  CALL sorting(newcolumn1, newcolumn2, newcolumn3, ndata)
 
  contator=0
  !Confronto i dati
  DO i=1,ndata
    IF(x(i)==newcolumn1(i)) THEN
      contator=contator + 1
      IF(contator==ndata) THEN
      PRINT *, "I dati del file sono ordinati, non era necessario il sorting."
      EXIT
      END IF
    ELSE IF(x(i)/=newcolumn1(i)) THEN
      PRINT *, "I dati del file non erano ordinati."
      DO j=1,ndata
        x(i)=newcolumn1(i)
        f(i)=newcolumn2(i)
        errorB(i)=newcolumn3(i)
      END DO
      EXIT
    END IF
  END DO

  DO i=1,ndata
    PRINT *, "Giorno di osservazione:", x(i), "Dato registrato:", f(i)
  END DO

  PRINT *, "Procedo a verificare se i dati sono analizzabili."
  CALL monotonia(f, ndata, mass, pos_massimo,variab)

  IF(variab==0) THEN
    GO TO 118
  END IF

  copyf=f
  copyerrorB=errorB
  ALLOCATE(mat(ndata,ndata), c(ndata))
  ALLOCATE(der(ndata))
  ALLOCATE(supernovaedat(4))
  ALLOCATE(valori_costHubble(ndata+1))

  DO k=1,ndata+1
    IF(k==1) THEN
      GO TO 165
    END IF
    PRINT *, "Procedo realizzando simulazioni Monte Carlo dei dati di partenza."
    media=copyf(k-1)
    sigma=copyerrorB(k-1)

    jran=42363331
    DO j=1,ndata
      jran=MOD(jran*a1+c1,m1)
      x1=FLOAT(jran)/FLOAT(m1)
      jran=MOD(jran*a1+c1,m1)
      x2=FLOAT(jran)/FLOAT(m1)
      y1=SQRT(-2.d0*LOG(x1))*COS(2.*ACOS(-1.d0)*x2)
      y2=SQRT(-2.d0*LOG(x1))*SIN(2.*ACOS(-1.d0)*x2)
      IF(j/=ndata) THEN
        f(j)=y1*sigma+media
        f(j+1)=y2*sigma+media
      ELSE IF(j==ndata) THEN
        f(j)=y2*sigma+media
      END IF
    END DO
  
    165 CONTINUE

    !Costruisco la matrice rappresentativa
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
      END IF

      IF((i .gt. 1) .and. (i .lt. ndata)) THEN
        c(i) = 6.0d0 * (((f(i+1) - f(i)) / (x(i+1) - x(i))) + (f(i-1) - f(i)) / (x(i) - x(i-1)))
      END IF
    END DO

    PRINT *, "Ora mi appresto a risolvere il sistema lineare algebrico."      

    acopy=mat
    ccopy=c

    CALL gauss(acopy,ccopy,ndata,der)
   
    PRINT *, "Derivate seconde:"
    DO i=1,ndata
      PRINT *, der(i)
    END DO
 
    PRINT *, "Procedo a verificare se i risultati ottenuti tramite il metodo di Gauss sono corretti."
    
    DO i=1,ndata
      summa=0.d0
      DO j=1,ndata
        summa=summa+mat(i,j)*der(j)
      END DO
      diff=ABS(summa-c(i))
      IF(diff>=1.d0)THEN
        PRINT *, "Derivata seconda con pedice",i,":ottenuta non correttamente."
      ELSE IF(diff<1.d0)THEN
        PRINT *, "Derivata seconda con pedice",i,":ottenuta correttamente."
      END IF
    END DO
    
    IF(k==1) THEN
      PRINT *, ""
      PRINT *, "Procedo a calcolare il giorno in cui e' stato registrato il massimo."
    END IF

    npoints1=1000
    npoints2=5000
    
    DO l=1,npoints2
      up = x(pos_massimo)
      toll1 = up - 0.01
      toll2 = up + 0.01
      tempvar=x(1)
      gg_max = tempvar + (100.d0 - 90.d0) / npoints1 * l
      IF(gg_max<=toll2.AND.gg_max>=toll1) THEN
        CALL interpolation(ndata, x, f, der, gg_max, valmax_B)
        IF(k==1) THEN  
          PRINT *, "Giorno in cui e' stato registrato il massimo:", gg_max
          PRINT *, "Valore massimo registrato:", valmax_B
          EXIT
        END IF
      END IF
    END DO

    !Magnitudine apparente al massimo
    bmax=valmax_B 
    
    g_15 = gg_max + 15.0d0
    
    IF(k==1) THEN
      PRINT *, ""
      PRINT *, "Procedo a calcolare il valore di magnitudine registrato 15 giorni dopo il massimo."
    END IF

    CALL interpolation(ndata, x, f, der, g_15, valmax_B)
    
    IF(k==1) THEN
      PRINT *, "Giorno di osservazione:", g_15
      PRINT *, "Valore registrato:", valmax_B
    END IF

    !Magnitudine apparente 15 gg dopo il massimo
    b15 = valmax_B
    delta_m15 = b15 - bmax
    
    IF(k==1) THEN
      PRINT *, "Differenza in magnitudine tra il valore massimo e il valore calcolato 15 giorni dopo il massimo:", delta_m15
    END IF

    !Leggo E(B-V) e Rv 
    OPEN(57, file="datiSupernovae.dat")

    READ(57,*) Stringa, supernovaedat(2), supernovaedat(3), supernovaedat(4)
    CLOSE(57)

    Z_CMB = supernovaedat(2)
    EB_V = supernovaedat(3)
    Rv = supernovaedat(4)
    
    IF(k==1) THEN
      PRINT *, ""
      PRINT *, "Redshift:", Z_CMB
      PRINT *, "Eccesso di colore E(B-V):", EB_V
      PRINT *, "Pendenza di arrossamento:", Rv
      PRINT *, ""
    END IF
    b_maxcorr = b15 - (Rv+1)*(EB_V)
   
    IF(k==1) THEN
      PRINT *, "Valore di magnitudine corretto:", b_maxcorr
    END IF

    !Procedo ad utilizzare la formula di Hamuy et al. (1996) per stimare la magnitudine assoluta al massimo in banda B
    B_max_ass = -19.258 + 0.784 * (delta_m15 - 1.1)
    IF(k==1) THEN
      PRINT *, "Magnitudine assoluta al massimo:", B_max_ass
    END IF

    mu = bmax - B_max_ass
    IF(k==1) THEN
      PRINT *, "Modulo di distanza mu:", mu
    END IF

    distanza = (10.d0 ** ((mu + 5) / 5))/(10**6)
    IF(k==1) THEN
      PRINT *, "Distanza in Mpc:", distanza
    END IF
    
    IF(k==1) THEN 
      PRINT *, ""
      PRINT *, "Procedo ad ottenere una stima della costante di Hubble usando il modello Lambda-CDM."
    END IF

    a = 0.d0
    b = Z_CMB
    n = 1000

    CALL simpson1_3(integranda, a, b, n, hub)
    
    c_luce = 299.792
    Hubble = hub * (((1.0d0 + Z_CMB)*c_luce)/distanza)
    
   !Unità di misura della costante di Hubble: kilometri/sec*Mpc
    
   !Parte facoltativa
    IF(k>1) THEN
      GO TO 120
    ELSE IF(k==1) THEN
      PRINT *, "Costante di Hubble per il modello Lambda-CDM:", Hubble
      PRINT *, ""
      PRINT *, "Procedo ad ottenere una stima della costante di Hubble usando il modello aperto."

      CALL simpson1_3(integr_aperto, a, b, n, hub)

      Hubble_aperto = hub * (((1.0d0 + Z_CMB)*c_luce)/distanza)

      PRINT *, "Costante di Hubble per il modello aperto:", Hubble_aperto

      PRINT *, ""
      PRINT *, "Procedo ad ottenere una stima della costante di Hubble usando il modello piatto."
    
      CALL simpson1_3(integr_piatto, a, b, n, hub)

      Hubble_piatto = hub * (((1.0d0 + Z_CMB)*c_luce)/distanza)

      PRINT *, "Costante di Hubble per il modello piatto:", Hubble_piatto

      PRINT *, ""
      PRINT *, "Procedo ad ottenere una stima della costante di Hubble usando il modello chiuso."

      CALL simpson1_3(integr_chiuso, a, b, n, hub)

      Hubble_chiuso = hub * (((1.0d0 + Z_CMB)*c_luce)/distanza)

      PRINT *, "Costante di Hubble per il modello chiuso:", Hubble_chiuso

      !Confronto i tre valori della costante di Hubble ottenuti

      PRINT *, ""
      PRINT *, "Procedo a verificare la bassa dipendenza dalla cosmologia."
      toll_Hubble1 = Hubble + 0.01
      toll_Hubble2 = Hubble - 0.01
      IF(Hubble_aperto>=toll_Hubble2.AND.Hubble_aperto<=toll_Hubble1) THEN
        PRINT *, "Modello cosmologico aperto e modello cosmologico Lambda-CDM prevedono la stessa costante di Hubble."
      ELSE 
        PRINT *, "Modello cosmologico aperto e modello cosmologico Lambda-CDM non prevedono la stessa costante di Hubble."
      END IF
      IF(Hubble_piatto>=toll_Hubble2.AND.Hubble_piatto<=toll_Hubble1) THEN
        PRINT *, "Modello cosmologico piatto e modello cosmologico Lambda-CDM prevedono la stessa costante di Hubble."    
      ELSE
        PRINT *, "Modello cosmologico piatto e modello cosmologico Lambda-CDM non prevedono la stessa costante di Hubble."
      END IF
      IF(Hubble_chiuso>=toll_Hubble2.AND.Hubble_chiuso<=toll_Hubble1) THEN
        PRINT *, "Modello cosmologico chiuso e modello cosmologico Lambda-CDM prevedono la stessa costante di Hubble."
      ELSE
        PRINT *, "Modello cosmologico chiuso e modello cosmologico Lambda-CDM non prevedono la stessa costante di Hubble."
      END IF
    END IF
  
    120 CONTINUE 
    
    valori_costHubble(k)=Hubble

  END DO

  DO i=2,ndata+1
    PRINT *, "Valore", i, "simulato della costante di Hubble:", valori_costHubble(i)
  END DO
  
  ALLOCATE(simulated(ndata))
  DO i=1,ndata
    simulated(i)=valori_costHubble(i+1)
  END DO
  118 CONTINUE

  CALL deviazione_standard(simulated, ndata, meanvalue, varianza, dev_standard)

  PRINT *, "Errore associato a H0:", dev_standard 
  
END DO

END PROGRAM Costante_di_Hubble


SUBROUTINE monotonia(x, n, min, pos_min, variab)
  IMPLICIT NONE
  REAL*8, INTENT(IN):: x(n)
  REAL*8, INTENT(OUT):: min
  INTEGER:: n, i
  INTEGER, INTENT(OUT):: pos_min, variab

  min=minval(x)
  
  DO i=1,n
    IF(x(i)==min) THEN
      pos_min=i
    END IF
  END DO
  IF(min==x(1).OR.min==x(n)) THEN
    PRINT *, "Dati non analizzabili."
    variab=0
  ELSE IF(min/=x(1).AND.min/=x(n)) THEN
    PRINT *, "Dati analizzabili."
    variab=1
  END IF 
END SUBROUTINE monotonia

SUBROUTINE sorting(newcolumn1, newcolumn2, newcolumn3, ndata)
  IMPLICIT NONE
  REAL*8, INTENT(INOUT):: newcolumn1(ndata), newcolumn2(ndata), newcolumn3(ndata)
  INTEGER:: i, j, ndata, position_minimum
  REAL*8:: minimum, substitution1, substitution2
  
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
    substitution1=newcolumn2(position_minimum)
    substitution2=newcolumn3(position_minimum)
    newcolumn2(position_minimum)=newcolumn2(i)
    newcolumn3(position_minimum)=newcolumn3(i)
    newcolumn2(i)=substitution1
    newcolumn3(i)=substitution2
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

SUBROUTINE interpolation(ndata, x, f, der, gg_max, valmax_B)
  IMPLICIT NONE
  INTEGER:: interval, i
  INTEGER, INTENT(IN):: ndata 
  REAL*8, INTENT(IN):: x(ndata), f(ndata), der(ndata)
  REAL*8, INTENT(OUT):: valmax_B
  REAL*8, INTENT(INOUT):: gg_max
   
  !Cerco l'intervallo al quale appartiene gg_max
  interval=0.d0

 IF(gg_max<x(1)) THEN
  PRINT *, "L'interpolazione non e' possibile."
 ELSE IF(gg_max > x(ndata)) THEN
  PRINT *, "L'interpolazione non e' possibile."
 ELSE
  DO i=1,ndata-1
    IF(gg_max <= x(i+1).AND.gg_max >= x(i)) THEN 
      interval = i+1 
    EXIT
    END IF
  END DO
 END IF

  valmax_B = (der(interval-1) * ((x(interval) - gg_max)**3) + der(interval) * ((gg_max - x(interval-1))**3)) / & 
       (6.0d0*(x(interval)-x(interval-1))) + &
       (f(interval-1) / (x(interval)-x(interval-1)) - &
       der(interval-1)*(x(interval)-x(interval-1)) / 6.0d0)*(x(interval)-gg_max) + &
       (f(interval) / (x(interval)-x(interval-1)) - &
       der(interval)*(x(interval)-x(interval-1)) / 6.0d0)*(gg_max - x(interval-1))
END SUBROUTINE interpolation

SUBROUTINE simpson1_3(variab, a, b, n, cost_hubble)
  IMPLICIT NONE
  INTEGER:: i 
  REAL*8:: xup, xdown, xmed, h
  REAL*8, INTENT(IN):: a, b
  INTEGER, INTENT(IN):: n
  REAL*8, INTENT(OUT):: cost_hubble
  REAL*8, EXTERNAL:: variab

  h = (b-a)/n
  cost_hubble = 0.0d0
  
  DO i=1, n
    xup = a + h*i
    xdown = a + h*(i-1)
    xmed = 0.5d0*(xup + xdown)
    cost_hubble = cost_hubble + variab(xdown) + variab(xup) + 4.0d0*variab(xmed)
  END DO

  cost_hubble = h*cost_hubble/6.0d0  
END SUBROUTINE simpson1_3

SUBROUTINE deviazione_standard(simulated, ndata, media, varianza, dev_standard)
  IMPLICIT NONE
  INTEGER :: i
  INTEGER, INTENT(IN) :: ndata
  REAL*8, INTENT(IN) :: simulated(ndata)
  REAL*8, INTENT(OUT) :: media, varianza, dev_standard

  media=0.d0
  DO i=1,ndata
    media = media + simulated(i)
  END DO
  media = media/ndata

  varianza=0.d0
  DO i=1,ndata
    varianza=varianza+(simulated(i)-media)**2
  END DO
  varianza=varianza/(ndata-1)
  dev_standard=SQRT(varianza)

END SUBROUTINE deviazione_standard
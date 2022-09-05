PROGRAM montecarlo
   IMPLICIT NONE
   INTEGER :: conta, i, j, jran
   INTEGER, PARAMETER :: m=259200, c=54773, a=7141
   REAL*8 :: x1, x2, y1, y2, media, sigma
   REAL*8, ALLOCATABLE :: setsigma(:), setmedia(:), giorno(:), newdataset(:)

   OPEN(32, file="SN_temp/SN2004eo.dat")
   conta=0.
   DO 
      READ(32, *, END=113)
      conta=conta+1
   END DO

113 CONTINUE

   ALLOCATE(giorno(conta), setsigma(conta), setmedia(conta))

   REWIND(32)
   DO i=1,conta
      READ(32,*) giorno(i), setmedia(i), setsigma(i)
      PRINT *, giorno(i), setmedia(i), setsigma(i)
   END DO

   ALLOCATE(newdataset(conta))
   jran=42363331
   DO i=1,conta
      media=setmedia(i)
      sigma=setsigma(i) 
      DO j=1,conta
         jran=MOD(jran*a+c,m)
         x1=FLOAT(jran)/FLOAT(m)
         jran=MOD(jran*a+c,m)
         x2=FLOAT(jran)/FLOAT(m)
         y1=SQRT(-2.d0*LOG(x1))*COS(2.*ACOS(-1.d0)*x2)
         y2=SQRT(-2.d0*LOG(x1))*SIN(2.*ACOS(-1.d0)*x2)
         newdataset(2*j-1)=y1*sigma+media
         newdataset(2*j)=y2*sigma+media
      END DO
   END DO


END PROGRAM montecarlo
  




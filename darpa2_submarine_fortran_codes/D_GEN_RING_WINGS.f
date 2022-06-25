C
C *******************************************************
C
C                    PROGRAM DARPA2WINGS
C
C *******************************************************
C
C    THIS PROGRAM DEFINES THE DARPA2 RING WINGS
C
C    THE DARPA2 WINGS USE THE NACA66 (DTNSRDC MOD)
C             THICKNESS DISTRIBUTION
C                      AND
C             THE NACA A=0.4 MEANLINE
C
      DIMENSION XC(26), YC(26), YCP(26)
      DIMENSION B(17), YT(26)
      DIMENSION XU(26), YU(26), XL(26), YL(26)
      DIMENSION XDLE(2), YDLE(2), XDTE(2), YDTE(2)
C
C  XC ARRAY ARE THE X/C VALUES CURRENTLY USED TO DEFINE WING.
C
C      DATA XC/0.0, 0.005, 0.0075, 0.0125, 0.025, 0.05, 0.075, 0.10,
C1             0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55,
C2             0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.0/
      XC(1) = 0.0
      XC(2) = 0.005
      XC(3) = 0.0075
      XC(4) = 0.0125
      XC(5) = 0.025
      XC(6) = 0.05
      XC(7) = 0.075
      XC(8) = 0.10
      XC(9) = 0.15
      XC(10) = 0.20
      XC(11) = 0.25
      XC(12) = 0.30
      XC(13) = 0.35
      XC(14) = 0.40
      XC(15) = 0.45
      XC(16) = 0.50
      XC(17) = 0.55
      XC(18) = 0.60
      XC(19) = 0.65
      XC(20) = 0.70
      XC(21) = 0.75
      XC(22) = 0.80
      XC(23) = 0.85
      XC(24) = 0.90
      XC(25) = 0.95
      XC(26) = 1.0
C HAVE WROTE ABOVE AS DATA INITIALIZATION NOT WORKING IN GFORTRAN
C
C  B ARRAY CONTAINS COEFFICIENTS FOR CALCULATION OF THICKNESS DISTR.
C
      B(1) = 0.43756
      B(2) = -0.08136
      B(3) = -0.06496
      B(4) = -0.01926
      B(5) = -0.00185
      B(6) = 0.00348
      B(7) = 0.00156
      B(8) = -0.00113
      B(9) = -0.00058
      B(10) = 0.00027
      B(11) = 0.00080
      B(12) = 0.00006
      B(13) = -0.00027
      B(14) = -0.00033
      B(15) = 0.00005
      B(16) = 0.00014
      B(17) = 0.00008
C
C  XDLE, YDLE ARE LEADING EDGE X, R OF WING
C  XDTE, YDTE ARE TRAILING EDGE X, R OF WING
C
      XDLE(1) = 13.46990
      XDLE(2) = 13.46990
      YDLE(1) = 0.43004
      YDLE(2) = 0.47681
      XDTE(1) = 14.21661
      XDTE(2) = 14.2074
      YDTE(1) = 0.35659
      YDTE(2) = 0.33856
C
C  THE ENTIRE PROGRAM IS EXERCISED TWO TIMES.
C    THE FIRST TIME, WING 1 IS DEFINED.
C      WING 1 HAS LEADING EDGE AT (X=13.46990, Y=0.43004)
C             AND TRAILING EDGE AT (X=14.23, Y=0.3558)
C    THE SECOND TIME, WING 2 IS DEFINED.
C      WING 2 HAS LEADING EDGE AT (X=13.46990, Y=0.3558)
C             AND TRAILING EDGE AT (X=14.23, Y=0.33628)
C
      NXC = 26
      DO 1000 KK = 1, 2
      WRITE(6, 2)
C
C ***************************
C
C      DEFINE MEAN LINE
C
C ***************************
C
      DO 100 I = 1, NXC
      X = XC(I)
      D = 0.4 - X
      E = 1.0 - X
      IF(ABS(X - 0.0) .LE. 1.0E-20) X = 1.0E-30
      IF(ABS(D) .LE. 1.0E-20) D = 1.0E-30
      IF(ABS(E) .LE. 1.0E-20) E = 1.0E-30
      YC(I) = -0.049921*(0.5*D*D*ALOG(ABS(D)) - 0.5*E*E*ALOG(E) +
     +        0.25*E*E - 0.25*D*D)
      YC(I) = YC(I) + 0.029953*(X*ALOG(X) + 0.227828 - 0.531076*X)
      YCP(I) = -0.049921*(E*ALOG(E) - D*ALOG(ABS(D))) +
     +          0.02995253*(ALOG(X) + 0.4689244)
C      WRITE(6, 1) I, 100.0*XC(I), 100.0*YC(I), YCP(I)
1     FORMAT(I5, 2F10.3, F10.5)
100   CONTINUE
C      WRITE(6, 2)
2     FORMAT(//)
C
C *******************************
C
C  DEFINE THICKNESS DISTRIBUTION
C
C *******************************
C
      NSER = 17
      DO 200 I = 1, NXC
      X = XC(I)
      IF(I .GE. 16) GOTO 150
      OM = ACOS(2.0*X - 1.0)
      YY = 0.0
      DO 125 J = 1, NSER
      YY = YY + B(J)*SIN(J*OM)
125   CONTINUE
      YT(I) = YY
      GOTO 199
150   CONTINUE
      XC1 = 1.0 - XC(I)
      YT(I) = 0.033333 + 1.696969*XCl - 1.441945*XC1*XC1 -
     +        0.366363*XC1**3 + 0.333049*XC1**4
199   CONTINUE
      YT(I) = 0.1*YT(I)
C      WRITE(6, 3) I, X, YT(I)
3     FORMAT(I5, F10.3, F10.5)
200   CONTINUE
C      WRITE(6, 2)
C
C *******************************
C
C       DEFINE DARPA2 WING
C
C *******************************
C
      XLINIT = 0.9425
      YLINIT = 0.0258
      CHORD = 0.0525
      XU(1) = 0.0
      YU(1) = 0.0
      XL(1) = 0.0
      YL(1) = 0.0
      I = 1
C      WRITE(6, 6)
C6     FORMAT(2X,1HI,3X,4H XU,5X,4H YU,5X,4H XL,5X,4H YL,6X,3HX/C,7X,2HYT,7X,2HYC,4X,7HDYC/DXC/)
5     FORMAT(I3, 4F9.5, F9.4, 4F9.5)
C      WRITE(6,5) I,XU(I),YU(I),XL(I),YL(I),XC(I),YT(I),YC(I),YCP(I)
      DO 300 I = 2, NXC
      TH = ATAN(YCP(I))
      SINTH = SIN(TH)
      COSTH = COS(TH)
      XU(I) = XC(I) - YT(I)*SINTH
      YU(I) = YC(I) + YT(I)*COSTH
      XL(I) = XC(I) + YT(I)*SINTH
      YL(I) = YC(I) - YT(I)*COSTH
C      WRITE(6,5) I,XU(I),YU(I),XL(I),YL(I),XC(I),YT(I),YC(I),YCP(I)
300   CONTINUE
C
C *********************************
C
C  DEFINE PHYSICAL WING DIMENSIONS
C
C *********************************
C
      PHI = ATAN2((YDTE(KK) - YDLE(KK)), (XDTE(KK) - XDLE(KK)))
      CS = COS(PHI)
      SN = SIN(PHI)
      CHORD = SQRT((YDTE(KK) - YDLE(KK))**2 + (XDTE(KK) - XDLE(KK))**2)
C      WRITE(6,444) XDLE(KK),YDLE(KK),XDTE(KK),YDTE(KK)
444   FORMAT(2X,'(XDLE,YDLB) = ',F10.5/2X,'(XDTE,YDTE) = ',F10.5)
C      WRITE(6, 6)
      DO 400 I = 1, NXC
      XUU = XU(I)
      XU(I) = XDLE(KK) + CHORD*(XU(I)*CS - YU(I)*SN)
      YU(I) = YDLE(KK) + CHORD*(XUU*SN + YU(I)*CS)
      XLL = XL(I)
      XL(I) = XDLE(KK) + CHORD*(XL(I)*CS - YL(I)*SN)
      YL(I) = YDLE(KK) + CHORD*(XLL*SN + YL(I)*CS)
      WRITE(6,5) I,XU(I),YU(I),XL(I),YL(I),XC(I),YT(I),YC(I),YCP(I)
4     FORMAT(I5, 4F10.5)
400   CONTINUE
C
C ***************************************
C
C WRITE WING OFFSETS TO FILE 7 FOR IPLOT
C
C ***************************************
C
      IF(KK .EQ. 1) WRITE(7, 10)
10    FORMAT('S1')
      IF(KK .EQ. 2) WRITE(7, 11)
11    FORMAT('S2')
      IF(KK .EQ. 1) WRITE(7, 12)
12    FORMAT('DARPA2 RING WING 1 ')
      IF(KK .EQ. 2) WRITE(7, 15)
15    FORMAT('DARPA2 RING WING 2 ')
      NXC2 = 2*NXC
      WRITE(7, 13) NXC2
13    FORMAT(I5)
      DO 500 I = 1, NXC2
      IF(I .GT. NXC) GOTO 450
      WRITE(7, 14) XU(I), YU(I)
      GOTO 500
450   CONTINUE
      J = NXC2 - I + 1
      WRITE(7, 14) XL(J), YL(J)
500   CONTINUE
14    FORMAT(2F10.5)
C
C **********************************
C
C    PRINT OFFSETS IN AMI FORMAT
C           ONTO FILE 9.
C
C **********************************
C
      DO 600 I = 1, NXC2
      IF(I .GT. NXC) GOTO 550
      J = NXC - I + 1
      WRITE(9, 14) XL(J), YL(J)
      GOTO 600
550   CONTINUE
      K = I - NXC
      WRITE(9, 14) XU(K), YU(K)
600   CONTINUE
1000  CONTINUE
      STOP
      END

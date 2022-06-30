C ***********************************************************************
C                              DARPA2GEN.FOR
C ***********************************************************************
C              THIS PROGRAM CONTAINS FOLLOWING EQUATIONS FOR
C              GENERATING OFFSETS IN FEET FOR DARPA2 MODEL
C              WITH (FULL/MODEL) SCALE RATIO = 24.
C
C     INCLUDED ARE:
C         BOW EQ.                FOR       0.0 FT <= X <=  3.333333 FT,
C         PARALLEL MID-BODY EQ.  FOR  3.333333 FT <= X <= 10.645833 FT,
C         AFTERBODY EQ.          FOR 10.645833 FT <= X <= 13.979167 FT,
C         AFTERBODY CAP EQ.      FOR 13.979167 FT <= X <= 14.291667 FT.
C  AS SET UP HERE, OFFSETS ARE COMPUTED EVERY 0.1 FT.
C  (EXCEPT IN FIRST 0.5 FT, WHERE THEY ARE EVERY 0.01 FT)
      DIMENSION X(300), Y(300)
      REAL K0, K1
C
C **********************************
C
C         DEFINE CONSTANTS
C
C **********************************
      RMAX = 0.8333333
      XB = 3.333333
      XM = 10.645833
      XA = 13.979167
      XC = 14.291667
      CB1 = 1.126395101
      CB2 = 0.442874707
      CB3 = 1.0/2.1
      RH = 0.1175
      K0 = 10.0
      K1 = 44.6244
C
      XX = -0.01
      DX = 0.01
      DO 1000 I=1,300
      NP = I
      XX = XX + DX
      IF(XX.GE.0.5) DX = 0.1
      IF(XX.GE.XA) DX = 0.01
      IF(XX.GE.XB) GO TO 200
C
C **********************************
C
C           BOW EQUATION
C
C **********************************
C
      A = 0.3*XX - 1.0
      A3 = A**3
      A4 = A**4
      B = 1.2*XX + 1.0
      R = CB1*XX*A4 + CB2*XX*XX*A3 + 1.0 - A4*B
      X(I) = XX
      Y(I) = R
      GO TO 1000
C
200   CONTINUE
      IF(XX.GE.XM) GO TO 400
C
C **********************************
C
C     PARALLEL MID-BODY EQUATION
C
C **********************************
C
      X(I) = XX
      Y(I) = RMAX
      GO TO 1000
C
400   CONTINUE
      IF(XX.GE.XA) GO TO 600
C
C **********************************
C
C        AFTERBODY EQUATION
C
C **********************************
C
      XI = (13.979167 - XX)/3.333333
      C1 =               RH*RH
      C2 =                           RH*K0               *XI*XI
      C3 = ( 20.0 - 20.0*RH*RH - 4.0*RH*K0 - 0.333333*K1)*XI**3
      C4 = (-45.0 + 45.0*RH*RH + 6.0*RH*K0 +          K1)*XI**4
      C5 = ( 36.0 - 36.0*RH*RH - 4.0*RH*K0 -          K1)*XI**5
      C6 = (-10.0 + 10.0*RH*RH +     RH*K0 + 0.333333*K1)*XI**6
      R = RMAX*(C1 + C2 + C3 + C4 + C5 + C6)**0.5
      X(I) = XX
      Y(I) = R
      GO TO 1000
C
600   CONTINUE
      IF(XX.GE.XC) GO TO 1100
C
C **********************************
C
C      AFTERBODY CAP EQUATION
C
C **********************************
C
      R = 1.0 - (3.2*XX - 44.7333333)**2
      R = RH*RMAX*(R**0.5)
      X(I) = XX
      Y(I) = R
1000  CONTINUE
1100  CONTINUE
      X(NP) = XC
      X(NP) = 0.0
C
C **********************************
C
C      WRITE OFFSETS TO TAPE 6
C
C **********************************
C
C      WRITE(6, 1)
C1     FORMAT('DARPA2')
C      WRITE(6, 2)
C2     FORMAT('MODEL WITH (MODEL/FULL) = 24')
C      WRITE(6, 3) NP
C3     FORMAT(I5)
C      WRITE(6, 4) (X(I), Y(I), I=1, NP)
C4     FORMAT(2F10.5, 10X, 2F10.5, 10X, 2F10.5)
C4     FORMAT(F10.5, 3X, F10.5)
      OPEN(6, STATUS='NEW', FORM='FORMATTED', FILE='RhinoDARPA2Hull')
      WRITE(6, 1)
1     FORMAT('InterpCrv')
      WRITE(6, 2) (X(I), Y(I), I=1, NP-1)
2     FORMAT(F0.5, ',', F0.5, ',0.0')
      WRITE(6, 3)
3     FORMAT('enter')
      WRITE(6, 4)
4     FORMAT('Revolve')
      WRITE(6, 5)
5     FORMAT('SelAll')
      WRITE(6, 6)
6     FORMAT('enter')
      WRITE(6, 7) X(1), Y(1)
7     FORMAT(F0.5, ',', F0.5, ',0.0')
      WRITE(6, 8) X(NP-1), Y(NP-1)
8     FORMAT(F0.5, ',', F0.5, ',0.0')
      WRITE(6, 9)
9     FORMAT('0')
      WRITE(6, 10)
10    FORMAT('360')
      WRITE(6, 11)
11    FORMAT('enter')
C
C  ALL DONE, PROGRAM ENDS
C
      STOP
      END

C
C  REVISED 11-JANUARY-1989
C
C ***********************************************************
C
C                        DARPA2GEN.FOR
C
C ***********************************************************
C
C  THIS PROGRAM CONTAINS FOLLOWING EQUATIONS FOR
C  GENERATING OFFSETS IN FEET FOR THE SAIL OF
C  THE DARPA2 MODEL WITH (FULL/MODEL) SCALE
C  RATIO = 24.
C
C  INCLUDED ARE:
C  SAIL FOREBODY EQ.   FOR  3.032986 FT <= X <= 3.358507 FT
C                           0.833333 FT <= X <= 1.507813 FT
C  SAIL MID-BODY EQ.   FOR  3.358507 FT <= X <= 3.559028 FT
C                           0.833333 FT <= X <= 1.507813 FT
C  SAIL AFTERBODY EQ.  FOR  3.559028 FT <= X <= 4.241319 FT
C                           0.833333 FT <= X <= 1.507813 FT
C  SAIL CAP EQ.        FOR  3.032986 FT <= X <= 4.241319 FT
C                           1.507813 FT <= X <= 1.562501 FT
C
C  OFFSETS ARE COMPUTED EVERY 0.005 FT.
C
C
      DIMENSION NP(300), X(300, 50, 3)
C
C *****************************************
C
C             DEFINE CONSTANTS
C
C *****************************************
C
      A1 = 2.094759
      B1 = 0.207178
      A3 = 2.908891
      B3 = 1.234491
      C3 = 3.444817
      D3 = 3.850435
      E3 = 2.080019
      HMAX = 0.109375
      DX = 0.005
      DX0 = 0.005
      XXCST = 3.032986
      XXAFN = 4.241319
      XXFFN = 3.358507
      XXMFN = 3.559028
      XZST = 1.507813
C
C *****************************************
C
C                 CALCULATE
C
C *****************************************
C
      XX = XXCST - DX
      DO 1000 I = 1, 300
      XZ = XZST
      X(I, 1, 3) = XZ
      J = 1
      XX = XX + DX
      X(I, 1, 1) = XX
      IF(XX .GT. XXAFN) THEN
            NI = I - 1
            GOTO 1014
      ENDIF
      IF(XX .GT. XXFFN) GOTO 1002
C
C *****************************************
C
C          SAIL FOREBODY EQUATION
C
C *****************************************
C
      D = 3.072*(XX - 3.032986)
      DM1 = D - 1
      A = 2*D*(DM1**4)
      B = D*D*(DM1**3)/3
      C = 1 - ((DM1**4)*(4*D + 1))
      X(I, 1, 2) = HMAX*(SQRT(A1*A + B1*B + C))
      GOTO 1004
C
C *****************************************
C
C         SAIL MID-BODY EQUATION
C
C *****************************************
C
1002  CONTINUE
      IF(XX .GT. XXMFN) GOTO 1003
      X(I, 1, 2) = HMAX
      GOTO 1004
C
C *****************************************
C
C        SAIL AFTER BODY EQUATION
C
C *****************************************
C
1003  CONTINUE
      E = (4.241319 - XX)/0.6822917
      F = E - 1
      G = 2.238361*E*F**4
      H = 3.106529*(E**2)*(F**3)
      P = 1 - (F**4)*(4*E + 1)
      X(I, 1, 2) = 0.109375*(G + H + P)
C
C *****************************************
C
C           SAIL CAP EQUATION
C
C *****************************************
C
1004  CONTINUE
      XZEND = (X(I, 1, 2)/2) + 1.507813
      NP(I) = 1
      DO 1008 J = 2, 50
      ICON1 = 0
1005  XZ = XZ + DX
      X(I, J, 3) = XZ
      IF(XZ .GT. XZEND) THEN
            ICON1 = ICON1 + 1
            IF(ICON1 .EQ. 1) THEN
                  XZ = XZ - DX
                  DX = 0.0005
                  GOTO 1005
            ENDIF
            IF(ICON1 .EQ. 2) THEN
                  X(I, J, 2) = 0.0
                  X(I, J, 3) = XZEND
                  NP(I) = J
                  ICON1 = 0
                  DX = DX0
                  GOTO 1000
            ENDIF
      ENDIF
      ADUM = (X(I, 1, 2)**2) - ((2*(XZ - XZST))**2)
      X(I, J, 2) = SQRT(ADUM)
1008  CONTINUE
1000  CONTINUE
C
C *****************************************
C
C         WRITE OFFSETS TO TAPE6
C            IN IPLOT FORMAT
C
C *****************************************
C
1014  OPEN(6, STATUS='NEW', FORM='FORMATTED', FILE='TP6')
      WRITE(6, 1015)
1015  FORMAT('DARPA2 SAIL')
      WRITE(6, 1016)
1016  FORMAT('MODEL WITH (MODEL/FULL) = 24')
      WRITE(6, 1017) NI
1017  FORMAT(I5)
      WRITE(6, 1018) (X(I, 1, 1), X(I, 1, 2), I=1, NI)
1018  FORMAT(2F10.5, 3X, 2F10.5, 3X, 2F10.5)
      DO 1013 I = 1, NI, 8
      WRITE(6, 1009) I
1009  FORMAT(I3)
      WRITE(6, 1010) X(I, 1, 1)
1010  FORMAT(' X=', F7.3, ' FEET')
      WRITE(6, 1011) (NP(I) + 1)
1011  FORMAT(I5)
      WRITE(6, 1012) X(I, 1, 2), 1.5
      WRITE(6, 1012) (X(I, J, 2), X(I, J, 3), J=1, NP(I))
1012  FORMAT(2F10.5, 3X, 2F10.5, 3X, 2F10.5)
1013  CONTINUE
666   STOP
      END

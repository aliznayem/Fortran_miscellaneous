C *********************************************************
C
C                    DARPA2STERNAPP.FOR
C
C *********************************************************
C
C
C    THIS PROGRAM DEFINES THREE-DIMENSIONAL (X, Y, Z)
C    OFFSETS FOR DARPA2 STERN APPENDAGES WITH TRAILING
C    EDGE LOCATED AT THREE DIFFERENT VALUES OF AXIAL
C    LENGTH X. FOR EACH AXIAL POSITION, FOUR IDENTICAL
C    STERN APPENDAGES ARE MOUNTED ON THE AXISYMMETRIC
C    HULL SURFACE AT TOP-DEAD-CENTER, 90 DEG, 180 DEG,
C    AND 270 DEG AZIMUTHALLY.
C
C
C  (X, RR, Z) = CARTESIAN COORDINATES IN FEET
C  H = X COORDINATE OF STERN APPENDAGES TRAILING EDGE.
C         H(1) = 12.729617
C         H(2) = 13.146284 = BASELINE
C         H(3) = 13.562950
C  CY = CHORD LENGTH = -0.466308*RR + 0.88859
C
      DIMENSION XXI(19), H(3)
C
      PARAMETER NP = 19, RMAX = 0.833333
C
C      DATA XXI/0.0,   0.005, 0.0125, 0.025, 0.050, 0.075, 0.100,
C1              0.150, 0.200, 0.2500, 0.300, 0.400, 0.500, 0.600,
C2              0.700, 0.800, 0.9000, 0.950, 1.000/
      XXI(1) = 0.0
      XXI(2) = 0.005
      XXI(3) = 0.0125
      XXI(4) = 0.025
      XXI(5) = 0.050
      XXI(6) = 0.075
      XXI(7) = 0.100
      XXI(8) = 0.150
      XXI(9) = 0.200
      XXI(10) = 0.250
      XXI(11) = 0.300
      XXI(12) = 0.400
      XXI(13) = 0.500
      XXI(14) = 0.600
      XXI(15) = 0.700
      XXI(16) = 0.800
      XXI(17) = 0.900
      XXI(18) = 0.950
      XXI(19) = 1.000
C DOING ABOVE AS DATA INITIALIZATION IS NOT WORKING IN GFORTRAN
C
C      DATA H/12.729617, 13.146284, 13.562950/
      H(1) = 12.729617
      H(2) = 13.146284
      H(3) = 13.562950
C
C ********************************************
C
C        LOOP ON THE LOCATION OF STERN
C           APPENDAGE TRAILING EDGE
C
C ********************************************
C
      DO 900 K = 1, 3
      HH = H(K)
      WRITE(6, 1) HH
1     FORMAT(//2X, 'STERN APPENDAGE TRAILING EDGE LOCATED AT X=', F10.5)
      DX = 0.05
      X = HH + DX
C
C ********************************************
C
C        LOOP ON THE AXIAL POSITION X.
C      BEGIN AT STERN APPENDAGE TRAILING
C         EDGE AND MOVE FORWARD IN X.
C
C ********************************************
C
      DO 800 J = 1, 32
      X = X - DX
      IF(X .GT. HH) GOTO 800
C
C ********************************************
C
C      DEFINE HULL RADIUS AT VALUE OF X
C
C ********************************************
C
      XIB = (13.979167 - X)/3.333333
      A =               RH*RH +     RH*AK0                *XIB*XIB
      B = ( 20.0 - 20.0*RH*RH - 4.0*RH*AK0 - 0.333333*AK1)*XIB**3
      C = (-45.0 + 45.0*RH*RH + 6.0*RH*AK0 +          AK1)*XIB**4
      D = ( 36.0 - 36.0*RH*RH - 4.0*RH*AK0 -          AK1)*XIB**5
      E = (-10.0 + 10.0*RH*RH +     RH*AK0 + 0.333333*AK1)*XIB**6
      RHA = A + B + C + D + E
      RHA = RMAX*SQRT(RHA)
      RHAS = RHA*RHA
      DELR = 0.025
      RR = 0.075
      ITR = 0
C
C ********************************************
C
C              LOOP ON RADIUS.
C            BEGIN WITH R = 0.1
C
C ********************************************
C
      DO 700 I = 1, 31
      RR = RR + DELR
620   CONTINUE
      CY = -0.466308*RR + 0.88859
      XI = (X - HH)/CY + 1.0
      IF(XI .LT. 0.0 .OR. XI .GT. 1.0) GOTO 700
C
C ********************************************
C
C           DEFINE STERN APPENDAGE
C
C ********************************************
C
      Z = 0.29690*SQRT(XI) - 0.12600*XI - 0.35160*XI*XI +
     +    0.28520*XI**3 - 0.10450*XI**4
C DOING ABOVE AS MULTILINE CONTINUATION IS NOT WORKING IN GFORTRAN
      Z = CY*Z
      SRS = RR*RR + Z*Z
C
C ********************************************
C
C         IF STERN APPENDAGE LOCATED
C           INSIDE BODY, INCREASE R
C
C ********************************************
C
      IF(STS .LT. RHAS .AND. ITR .EQ. 0) GOTO 700
C
C ********************************************
C
C        IF STERN APPENDAGE LOCATED
C             ON BODY SURFACE,
C            GOTO 710 TO DEFINE
C          STERN APENDAGE SECTION.
C
C ********************************************
C
      IF(ABS(SRS - RHAS) .LE. 0.00001) GOTO 710
C
C ********************************************
C
C         STERN APPENDAGE IS "CLOSE"
C         TO HULL RADIUS, GET CLOSER.
C
C ********************************************
C
      ITR = ITR + 1
      IF(ITR .GT. 20) STOP 1
      DELR = 0.5*DELR
      IF(SRS .GT. RHAS) RR = RR - DELR
      IF(SRS .LE. RHAS) RR = RR + DELR
      GOTO 620
700   CONTINUE
      GOTO 800
710   CONTINUE
C
C ********************************************
C
C      SOLVE FOR STERN APPENDAGE SECTION
C              AT GIVEN RADIUS
C
C ********************************************
C
      CY = -0.466308*RR + 0.88859
      I750 = 0
      XINIT = (X - HH)/CY + 1.0
C
C ********************************************
C
C                 LOOP ON XI
C
C ********************************************
C
      DO 750 I = 1, NP
      XI = XXI(I)
      IF(XI .LT. XINIT) GOTO 750
740   CONTINUE
      XI = XXI(I)
      IF(I750 .EQ. 0) XI = XINIT
      XXX = (XI - 1.0)*CY + HH
      IF(XI .LT. 0.0 .OR. XI .GT. 1.0) GOTO 750
      Z = 0.29690*SQRT(XI) - 0.12600*XI - 0.35160*XI*XI +
     +    0.28520*XI**3 - 0.10450*XI**4
      Z = CY*Z

C
C ********************************************
C
C         PRINT X, Y, (+/-)Z VALUES
C             TO PRINTER FILE 6
C
C ********************************************
C
      IF(I750 .EQ. 0) WRITE(6, 2)
2     FORMAT(/6X, 1HX, 9X, 1HY, 6X, 6H(+/-)Z)
      WRITE(6, 3) XXX, RR, Z
3     FORMAT(3F10.5)
      I750 = I750 + 1
      RBSMAX = RR
      IF(I750 .EQ. 1) GOTO 740
750   CONTINUE
800   CONTINUE
C
C ********************************************
C
C    COMPUTED ALL STERN APPENDAGE SECTIONS
C            WHICH INTERSECT HULL.
C         NOW COMPUTE STERN SECTIONS
C      WITH RADIUS LARGER THAN HULL RADIUS.
C
C ********************************************
C
      DELR = 0.05
      DO 850 I = 1, NP
      RO = RR
      RR = RBSMAX + I*DELR
      IF(RR .GT. RMAX) RR = RMAX
      IF(RR .EQ. RO) GOTO 900
      CY = -0.466308*RR + 0.88859
      WRITE(6, 2)
      DO 840 J = 1, NP
      XI = XXI(J)
      XXX = (XI - 1.0)*CY + HH
      Z = 0.29690*SQRT(XI) - 0.12600*XI - 0.35160*XI*XI +
     +    0.28520*XI**3 - 0.10450*XI**4
      Z = CY*Z
C
C ********************************************
C         PRINT X, Y, (+/-)Z VALUES
C             TO PRINTER FILE 6
C
C ********************************************
C
      WRITE(6, 3) XX, RR, Z
840   CONTINUE
850   CONTINUE
900   CONTINUE
      STOP
      END

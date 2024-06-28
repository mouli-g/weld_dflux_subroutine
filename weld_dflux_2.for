      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME

      X = COORDS(1)
      Y = COORDS(2)
      Z = COORDS(3)
      T = TIME(2)
      X0 = 0.000
      Y0 = 0.000
      Z0 = 0.000
      V = 15.000
      I = 60.000
      ETTA = 0.800
      Q = V * I * ETTA
      RO = 0.003
      RO = RO**2
      PI = 3.141593

      X_END = 0.1  ! End position in the X direction
      TOTAL_TIME = X_END / V  ! Total time to reach the end position

      IF (T > TOTAL_TIME) THEN
      XC = X_END  ! Stop at the end position
      ELSE
      XC = X0 + V * T  ! X-coordinate of the heat source moving in the X direction
      END IF

      YC = Y0
      ZC = Z0

      FLUX(1) = 0
      Q0 = ((3 * Q) / (PI * RO)) * EXP((-3 * ((X - XC)**2 + (Y - YC)**2)) / RO)
 
      IF (((X - XC)**2 + (Y - YC)**2) < RO) THEN
      FLUX(1) = Q0
      END IF
      FLUX(2) = 0.0

      RETURN
      END

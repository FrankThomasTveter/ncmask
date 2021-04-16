      SUBROUTINE DJ2000X(DAY,YY,MM,DD,HH,MI,SEC)
!P  COMPUTES CALENDER DATE FROM MODIFIED JULIAN DAY 2000
!   VALID FOR DATES BETWEEN 1950/JAN/1 AND 2099/DEC/31.
!
!   MJD(2000) = MJD(1950) - 18262.0 IS = 0 ON 2000/01/01 AT 00:00:00.
!
!I  (REAL) DAY = MOD. JUL. DAY, REFERRED TO 2000.
!O  (INT*4) YY = YEAR WITH 4 DIGITS
!O  (INT*4) MM = MONTH
!O  (INT*4) DD = DAY
!O  (INT*4) HH = HOUR
!O  (INT*4) MI = MINUTE
!O  (REAL) SEC = SECOND.
!
      IMPLICIT NONE
      REAL DAY, SEC
      INTEGER YY,MM,DD,HH,MI
      REAL Z,A,ALPHA,B,rh,rm
      INTEGER IC,ID,IE
!  MAKE SURE TO ROUND-OFF ONLY DOWN, ALSO FOR NEGATIVE MJD:
      Z = NINT(-0.5D0+DAY + 2451545.0) ! true julian date
      IF (Z .LT. 2299161D0) THEN
         A = Z
      ELSE
         ALPHA = INT((Z - 1867216.25D0) / 36524.25D0)
         A = Z + 1.0D0 + ALPHA - INT(ALPHA / 4.0D0)
      END IF
      B = A + 1524.0D0
      IC = INT((B - 122.1D0) / 365.25D0)
      ID = INT(365.25D0 * IC)
      IE = INT((B - ID) / 30.6001D0)
      DD = B - ID - INT(30.6001D0 * IE)
      IF (IE .LT. 14) THEN
         MM = IE - 1
      ELSE
         MM = IE - 13
      END IF
      IF (MM .GT. 2) THEN
         YY = IC - 4716
      ELSE
         YY = IC - 4715
      END IF
      SEC = real(nint((DAY - NINT(-0.5D0+DAY))*86400.D4))*1.0D-4
      HH=int(sec/3600.0D0)
      sec=sec-real(hh)*3600.0D0
      MI=int(sec/60.0D0)
      sec=sec-real(MI)*60.0D0
      RETURN
      END

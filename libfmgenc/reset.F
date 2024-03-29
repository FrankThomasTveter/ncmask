      SUBROUTINE RESET(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     ERASES CONTENTS OF STRING                                             *
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE   |                 REASON                  | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 18/07/95 | NEW                                     | *
C     |              |          |                                         | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
      IMPLICIT NONE
      SAVE
C
      INTEGER N
      CHARACTER*(*) STRING
      CHARACTER*1 C1
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      INTEGER II
C
      IF (FIRST) THEN
         C1=CHAR(0)
         FIRST=.FALSE.
      ENDIF
      DO II=1,N
         STRING(II:II)=C1
      ENDDO
      RETURN
      END

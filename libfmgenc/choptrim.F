      SUBROUTINE CHOPTRIM(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES SURPLUS BLANKS FROM A STRING                                  *
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
C
      INTEGER N
      CHARACTER*(*) STRING
      CHARACTER*1 C1
      INTEGER II,JJ
      LOGICAL LASTBL
C
      CALL CHOP0(STRING,N)
C
      RETURN
      END

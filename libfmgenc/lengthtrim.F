      INTEGER FUNCTION LENGTHTRIM(STRING,N,GUESS)
C     ***********************************************************************
C     +                                                                     *
C     GIVES THE LENGTH OF THE STRING                                        *
C     +                                                                     *
CI    STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CI    GUESS (I*4) = FIRST GUESS FOR LENGTH                                  *
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE   |                 REASON                  | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 18/07/95 | NEW                                     | *
C     | FTT, OV      | 22/06/00 | removed save + 0:0-bug
C     |              |          |                                         | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER N,LL,LENS,GUESS
      CHARACTER*(*) STRING
      CHARACTER*1 C1
      LOGICAL BL,FORW,BDONE
C
      LENGTHTRIM=LEN_trim(string)
C
      RETURN
      END

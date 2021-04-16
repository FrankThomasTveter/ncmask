      CHARACTER*250 FUNCTION NUKEHEAD(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     FUNCTION RETURNS THE FIRST ITEM IN STRING,                            *
C     AND REMOVES IT FROM STRING                                            *
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CO    NUKEHEAD (C*250) = HEAD OF STRING                                     *
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
      CHARACTER*1 STRING(N),C1
C
      CHARACTER*250 OUT
      INTEGER CRR,II,JJ
      LOGICAL BLNK,FIRST
      DATA FIRST /.TRUE./
      CHARACTER*8 MYNAME
      DATA MYNAME /'NUKEHEAD'/
C
      IF (FIRST) THEN
         C1=CHAR(0)
         FIRST=.FALSE.
      ENDIF
C
C     CRR = CURRENT POINTER POSITION IN STRING
      CRR=0
C
      BLNK=.FALSE.
C
      DO WHILE (.NOT.BLNK .AND. CRR.LT.N)
         CRR = CRR+1
         BLNK= (STRING(CRR) .EQ. ' ')
      ENDDO
C
      CALL RESET(OUT,250)
      IF (BLNK) THEN
C     SEVERAL ITEMS IN STRING
         JJ=MIN(250,CRR)
         DO II=1,JJ
            OUT(II:II)=STRING(II)
         ENDDO
         JJ=N-CRR
         DO II=1,JJ
            STRING(II)=STRING(II+CRR)
         ENDDO
         DO II=JJ+1,N
            STRING(II)=C1
         ENDDO
      ELSE
C     JUST ONE ITEM IN STRING
         JJ=MIN(250,N)
         DO II=1,JJ
            OUT(II:II)=STRING(II)
         ENDDO
         DO II=1,N
            STRING(II)=C1
         ENDDO
      ENDIF
C
      CALL CHOP0(OUT,250)
      CALL CHOP0(STRING,N)
C
      NUKEHEAD=OUT
      RETURN
      END

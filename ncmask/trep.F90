      SUBROUTINE TREP(STR250,NN,SRC100,REP100,IRC)
      IMPLICIT NONE
      SAVE
!
!     search for, and replace, key words in string with
!     time information...
!
      CHARACTER*250 STR250
      INTEGER NN
      CHARACTER*100 SRC100(NN)
      CHARACTER*100 REP100(NN)
      INTEGER IRC
!
      LOGICAL BDONE
      CHARACTER*250 BUFF250
      CHARACTER*1000 BUFF1000
      INTEGER, EXTERNAL :: LENGTH
      INTEGER II,JJ,LENS,LENR,LENI
!
      CHARACTER*16 MYNAME
      DATA MYNAME /'TREP'/
!
      DO II=1,NN
         CALL CHOP0(SRC100(II),100)
         CALL CHOP0(REP100(II),100)
         LENI=LENGTH(STR250,250,10)
         LENS=LENGTH(SRC100(II),100,2)
         LENR=LENGTH(REP100(II),100,2)
         BDONE=(LENI.EQ.0)
         DO WHILE (.NOT.BDONE)
            BUFF250=STR250
            BDONE=.TRUE.
            DO JJ=1,LENI-LENS+1
!     write(*,*) myname,'"',STR250(JJ:JJ+LENS-1),'"',
!     &              SRC100(II)(1:LENS),'"'
               IF (STR250(JJ:JJ+LENS-1).EQ. &
     &              SRC100(II)(1:LENS)) THEN
                  BUFF1000=STR250(1:JJ-1)// &
     &                 REP100(II)(1:LENR)// &
     &                 STR250(JJ+LENS:LENI)
                  CALL CHOP0(BUFF1000,251)
                  STR250=BUFF1000(1:250)
                  LENI=LENGTH(STR250,250,10)
!     write(*,*) myname,str250(1:leni)
                  BDONE=(BUFF250.eq.STR250)
               END IF
            END DO
         END DO
      END DO
!     
      RETURN
      END




      PROGRAM NCMASK
!
!     NCMASK PROGRAM FOR DATA TEST READ
!
      IMPLICIT NONE
      SAVE
!
      INTEGER  UNITI,IRC
      CHARACTER*10 MYNAME
      DATA MYNAME /'NCMASK'/
      LOGICAL  BDEB,ACTIVE
      DATA BDEB /.FALSE./
      DATA ACTIVE /.FALSE./
!
      integer ii
      integer lenr, length
      external length
!
      IRC=0
      UNITI=5
!
!     Debug System.
!
!      IF(.NOT.ACTIVE)CALL DEBUG(MYNAME,BDEB,ACTIVE)
!
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Program starts.',IRC
!      CALL BLOBB

      CALL MNCMASK(UNITI,IRC)
      IF (IRC.NE.0) THEN
         WRITE(*,*) MYNAME,'Error return from MNCMASK.',IRC
      ENDIF
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Program ends.',IRC
!
      IF (IRC.EQ.0) THEN
         WRITE(*,*) MYNAME,'-------NORMAL END OF PROGRAM-------'
      ELSE
         WRITE(*,*) MYNAME,'--------ERROR WHILE RUNNING--------',IRC
      ENDIF
!
      CALL exit(IRC)
!
      END

      subroutine nice(in)
C
      implicit none
      character*(*) in
      integer length, leni,lenin,lenn, lenp,ii,jj
      external length
      logical bdone
      character*250 buff250
C
      lenn=len(in)
      call chop0(in,lenn)
      leni=length(in,lenn,min(lenn,10))

C      buff250=in
C     first check if there is a decimal stop in the string
      bdone=.false.
      ii=leni
      lenp=leni+1
      do while (.not.bdone)
         if (in(ii:ii).ne.".") THEN
            ii=ii-1
            if (ii.le.0) bdone=.true.
         ELSE
            lenp=ii
            bdone=.true.
         end if
      end do
      ii=leni
      lenin=leni
C     check "0" after decimal stop
      bdone=(lenp.ge.leni)
      do while (.not.bdone)
         if (ii.gt.1.and.in(ii:ii).eq."0") THEN
            ii=ii-1
            if (ii.le.0) bdone=.true.
         ELSE
            lenin=ii
            bdone=.true.
         end if
      end do
C     remove trailing "0" after decimal stop...
      do ii=lenin+1,leni
         in(ii:ii)=" "
      end do
      call chop0(in,lenn)
C      buff250=buff250(1:leni)//' -> '//in(1:lenin)
C     write(*,*) 'NICE '//buff250(1:leni+4+lenin)
      return
      end subroutine nice

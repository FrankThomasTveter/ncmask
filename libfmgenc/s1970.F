      real function s1970(yy,mm,dd,hh,mi,sec)
      implicit none
      integer YY
      integer mm
      integer dd
      integer hh
      integer mi
      real sec
C     returns "seconds since 1970-01-01 00:00:00 +00:00"
      real days
C     yy=1970
C     mm=01
C     dd=01
C     hh=00
C     mi=00
C     sec=0.0D0
C     call jd2000(days,yy,mm,dd,hh,mi,sec)
C     write(*,*) 'S1970 1970-reference in J2000:',days!  -10957.000000000000 
      call jd2000(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
      days = days + 10957.0D0   ! convert to days since reference
      S1970=days*86400.0D0      ! convert to seconds
      return
      end function s1970

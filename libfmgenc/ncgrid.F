      subroutine ncgrid(ogtype,og,
     &     nrlon,nrlat,gridlon,gridlat,
     &     gridx, gridy, dims, irc)
c
c****************************************************************
c
c     ncgrid - write grid projection x and y values to netcdf file
c
c  purpose:
c
c     Write grid projection x and y values for different grids; 
c     spherical, spherical rotated, polar stereographic or mercator.
c
c  input/output parameters:
c
c     ogtype  - output grid type
c     og      - output grid description
c     nrlon   - no. of positions in lon direction
c     nrlat   - no. of positions in lat direction
c     gridlon - longitude values
c     gridlat - latitude values
c     gridx   - x-values
c     gridy   - y-values
c     dims    - number of dimensions
c     irc     - output error status, 0=no error
c
c  description of g = og and gr (for igtype = ogtype):
c
c  for spherical (rotated) grid, igtype=2,3:
c
c     g(1) - western boundary (degrees)
c     g(2) - southern boundary (degrees)
c     g(3) - longitude increment (degrees)
c     g(4) - latitude increment (degrees)
c     g(5) - xcen: longitude position of rotated equator (degrees)
c     g(6) - ycen: latitude  position of rotated equator (degrees)
c            (lamda,theta)=(xcen,ycen) at (lamda',theta')=(0,0),
c            where (lamda,theta) are usual spherical coord. and
c            (lamda',theta') are rotated spherical coord.
c            xcen = ycen = 0 for usual spherical coord.
c
c     (note: if the coordinates are geographic longitude(x),latitude(y)
c            set igtype=2 and g(1:6) = 1.,1.,1.,1.,0.,0. )
c
c  for polar stereographic grid, igtype=1,4:
c
c     g(1) - x-position of north pole
c     g(2) - y-position of north pole
c     g(3) - number of grid distances between pole and equator
c     g(4) - rotation angle of the grid (degrees)
c     g(5) - projection latitude (degrees)
c            (60 degrees north for igtype=1)
c     g(6) - 0. (not used)
c
c  for mercator (unrotated) grid, igtype=5:
c
c     g(1) - western boundary (longitude for x=1) (degrees)
c     g(2) - southern boundary (latitude for y=1) (degrees)
c     g(3) - x (longitude) increment (km)
c     g(4) - y (latitude)  increment (km)
c     g(5) - reference (construction) latitude (degrees)
c     g(6) - 0.  (not used)
c
c  for lambert conformal conic grid, igtype=9:
c
c     g(1) - western boundary (longitude for x=1) (degrees)
c     g(2) - southern boundary (latitude for y=1) (degrees)
c     g(3) - standard_parallel 1
c     g(4) - standard_parallel 2
c     g(5) - longitude_of_central_meridian
c     g(6) - latitude_of_projection_origin
c
c  externals:
c
c     pol2sph - polar sterographic <-> spherical coordinates
c     sph2rot - spherical <-> spherical rotated coordinates
c     mer2sph - mercator (unrotated) <-> spherical coordinates
c
c  history:
c
c     j.e. haugen/dnmi      nov -94 ... grd2grd
c     a.   foss   dnmi   02.02.1995 ... no size limits
c     a.   foss   dnmi   25.08.1995 ... xyconvert
c     a.   foss   dnmi   15.05.1996 ... mercator (unrotated)
c
c****************************************************************
c
      implicit none
c
      integer nrlat,nrlon, ogtype, dims, irc
      real*4  gridlat(nrlon,nrlat)
      real*4  gridlon(nrlon,nrlat)
      real*4  gridx(nrlat,nrlon)
      real*4  gridy(nrlat,nrlon)
      real    og(6), x, y
c
      integer ii,jj
      real    zpir18, rearth,
     +        xwa, ysa, dxa, dya, xca, yca,
     +        xwr, ysr, dxr, dyr, xcr, ycr,
     +        xpa, ypa, ana, fia, fpa,
     +        xpr, ypr, anr, fir, fpr,
     +        xa, ya, zcrot, zsrot, zx1, zx2, zx3, zy1, zy2, zy3
      character*10 myname
      data myname /'NCGRID'/
c
      zpir18 = 2.0*asin(1.0)/180. ! pi/180.0
      call earthr(rearth)
c
      irc = 0
c
C      write(*,*)myname,'Ogtype:',ogtype

      dims=0
      if (ogtype.eq.2 .or. ogtype.eq.3) then
c
c..geographic->sph
c     
         xwr = og(1)*zpir18
         ysr = og(2)*zpir18
         dxr = og(3)*zpir18
         dyr = og(4)*zpir18
         xcr = og(5)*zpir18
         ycr = og(6)*zpir18
c     
         if(xcr.ne.0. .or. ycr.ne.0.) then
            do ii=1,nrlon
               do jj=1,nrlat
                  x=gridlon(ii,jj)*zpir18
                  y=gridlat(ii,jj)*zpir18
                  call sph2rot(+1,1,x,y,
     &                 xcr,ycr,irc)
                  gridx(ii,jj)=x/zpir18
                  gridy(ii,jj)=y/zpir18
               end do
            end do
         ELSE
            do ii=1,NRLON
               do jj=1,NRLAT
                  gridx(ii,jj)=gridlon(ii,jj)
                  gridy(ii,jj)=gridlat(ii,jj)
               end do
            end do
         endif
         dims=2
c     
      elseif (ogtype.eq.1 .or. ogtype.eq.4) then
c
c..geographic->pol (unless pol->pol with og(5)=og(5) is requested)
c
         xpr = og(1)
         ypr = og(2)
c         anr=og(3)
         fir = og(4)*zpir18
         fpr = og(5)*zpir18

         anr = rearth*(1.0D0+sin(fpr)) 
C*(1.0D0+sin(fpr))/cos(fpr) ! meters from pole to equator, og(3)
c
         do ii=1,nrlon
            do jj=1,nrlat
               x=gridlon(ii,jj)*zpir18
               y=gridlat(ii,jj)*zpir18

C               if (ii.eq.1.and.jj.eq.1) then
C                  write(*,*)'NCGRID(1,1)==',x,y,' latlon',ogtype
C               end IF

               call pol2sph(-1,1,x,y,
     &              fpr,xpr,ypr,anr,fir,irc)

C               if (ii.eq.1.and.jj.eq.1) then
C                  write(*,*)'NCGRID(1,1)->',x,y,' grid',ogtype
C               end IF

               gridx(ii,jj)=x
               gridy(ii,jj)=y
            end do
         end do
         dims=2
c
      elseif (ogtype.eq.5) then
c
c..geographic->mercator
c
         xwr = og(1)*zpir18
         ysr = og(2)*zpir18
         ycr = og(5)*zpir18
c
         do ii=1,nrlon
            do jj=1,nrlat
               x=gridlat(ii,jj)
               y=gridlon(ii,jj)
               call mer2sph(-1,1,x,y,
     &              xwr,ysr,ycr,irc)
               gridx(ii,jj)=x
               gridy(ii,jj)=y
            end do
         end do
         dims=2
c
      elseif (ogtype.eq.6) then ! lambert

         xwr = og(1)*zpir18
         ysr = og(2)*zpir18
         dxr = og(3)*1000.
         dyr = og(4)*1000.
         xcr = og(5)*zpir18
         ycr = og(6)*zpir18
c
         do ii=1,nrlon
            do jj=1,nrlat
               x=gridlon(ii,jj)*zpir18
               y=gridlat(ii,jj)*zpir18
               call lam2sph2(-1,1,x,y,xwr,ysr,dxr,dyr,xcr,ycr,ycr,
     +              irc)
               if (irc.ne.0) return
C               if (ii.eq.1.and.jj.eq.1) then
C                  write(*,*)'NCGRID(1,1)->',xpa,ypa,' grid',ogtype
C               end IF
               gridx(ii,jj)=x
               gridy(ii,jj)=y
            end DO
         end do
      elseif (ogtype.eq.9) then ! lambert
c..geographic->lambert conformal conic
         do ii=1,nrlon
            do jj=1,nrlat
               x=gridlat(ii,jj)!*zpir18
               y=gridlon(ii,jj)!*zpir18
C               if (ii.eq.1.and.jj.eq.1) then
C                  write(*,*)'NCGRID(1,1)->',x,y,' latlon',ogtype
C               end IF
               call mvGeod2LambConf(
     &              og(3),      ! latin1
     &              og(4),      ! latin2
     &              og(5),      ! lon0
     &              og(6),      ! lat0
     &              x,y,        ! lat/lon pos (deg)
     &              xpa, ypa,   ! output easting and northing (km) - reversed order
     &              irc)
               if (irc.ne.0) return
C               if (ii.eq.1.and.jj.eq.1) then
C                  write(*,*)'NCGRID(1,1)->',xpa,ypa,' grid',ogtype
C               end IF
               gridx(ii,jj)=xpa
               gridy(ii,jj)=ypa
            end do
         end do
         dims=2
      else
c
	irc = 1
c
      end if
c
      return
      end

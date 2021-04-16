      character*250 function ncproj(ncid,igtype,g,irc)
C
C     writes felt-projection parameters to the netcdf file, and returns the projection name
C     
      implicit none
      integer ncid
      integer igtype
      real g(6)
      integer irc
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
C     
#     include "netcdf.inc"
C     
      character*10 myname
      data myname /'NCPROJ'/
C
      character*50 gs(6), bs(6)
      integer gl(6), bl(6)
      real b(6)
      integer ii
C
      integer ndims
      PARAMETER (NDIMS=1)
      INTEGER DIMIDS(NDIMS)
      integer cvarid
C
      character*250 cname,proj250
      integer length, lenp, lenc
      external length
      real buff
C
      do ii=1,6
         write(gs(ii),'(F27.15)') g(ii)
         call nice(gs(ii))
         call chop0(gs(ii),50)
         gl(ii)=length(gs(ii),50,10)
      end do
C
      if (igtype.eq.1.or.igtype.eq.4) THEN ! polar stereographic *******************
         cname="stereographic"
         call chop0(cname,250)
         lenc=length(cname,250,20)
         ncproj=cname
         if (igtype.eq.1) THEN
            b(5)=60.0D0
         else
            b(5)=g(5)
         end IF
         write(gs(5),'(F27.15)') b(5)
         call nice(gs(5))
         call chop0(gs(5),50)
         gl(5)=length(gs(5),50,10)
C     define "proj"-string
         write(proj250,'("+proj=stere +lat_0=90 +lon_0=",A,'//
     &        '" +lat_ts=",A," +units=m")')
     &        gs(4)(1:gl(4)),
     &        gs(5)(1:gl(5))
         call chop0(proj250,250)
         lenp=length(proj250,250,30)
C     Define projection variable in nc-file
         irc = nf_def_var(ncid, cname(1:lenc), NF_INT, 
     +        0, dimids, cvarid)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_DEF_VAR1',
     &           nf_strerror(irc),irc
            return
         end if
C     "grid_mapping_name"                      -> "stereographic"
         irc = nf_put_att_TEXT(ncid, cvarid, 
     &        'grid_mapping_name', lenc, cname)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_TEXT2',
     &           nf_strerror(irc),irc
            return
         end if
C     "straight_vertical_longitude_from_pole"  -> +lon_0 (double), g(4)
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "straight_vertical_longitude_from_pole",
     &        NF_DOUBLE,1,g(4)); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE3',
     &           nf_strerror(irc),irc
            return
         end if
C     "latitude_of_projection_origin"  -> +lat_0 (double), 90.0
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "latitude_of_projection_origin",
     &        NF_DOUBLE,1,90.0D0); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE3',
     &           nf_strerror(irc),irc
            return
         end if
C     "standard_parallel"  -> +lat_ts (double), g(5) 
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "standard_parallel",
     &        NF_DOUBLE,1,b(5)); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE3',
     &           nf_strerror(irc),irc
            return
         end if
C     "false_easting"  -> 0.0f (double)
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "false_easting",
     &        NF_DOUBLE,1,0.0D0); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE3',
     &           nf_strerror(irc),irc
            return
         end if
C     "false_northing"  -> 0.0f (double)
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "false_northing",
     &        NF_DOUBLE,1,0.0D0); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE3',
     &           nf_strerror(irc),irc
            return
         end if
C     "proj4"                                  -> proj250
C         irc = nf_put_att_text(ncid, cvarid, 
C     &        'proj4', lenp, proj250)
C         if (irc .ne. nf_noerr) then
C            write(*,*) myname,'Error return from NF_PUT_ATT_TEXT4',
C     &           nf_strerror(irc),irc
C            return
C         end if
      else if (igtype.eq.2) THEN ! spherical *******************
         cname="latitude_longitude"
         call chop0(cname,250)
         lenc=length(cname,250,20)
         ncproj=cname
C     define "proj"-string
         write(proj250,'("+proj=longlat")')
         call chop0(proj250,250)
         lenp=length(proj250,250,30)
C     Define projection variable in nc-file
         irc = nf_def_var(ncid, cname(1:lenc), NF_INT, 
     +        0, dimids, cvarid)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_DEF_VAR5',
     &           nf_strerror(irc),irc
            return
         end if
C     "grid_mapping_name" -> "latitude_longitude"
         irc = nf_put_att_TEXT(ncid, cvarid, 
     &        'grid_mapping_name', lenc, cname)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_REAL6',
     &           nf_strerror(irc),irc
            return
         end if
C     "proj4" -> proj4Str (proj4GetEarthAttributes)
         call earthr(buff)      ! 
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        'earth_radius', NF_DOUBLE,1, buff)
         if (irc .ne. nf_noerr) then
            write(*,*) 'NCIMG Error return from NF_PUT_ATT_REAL8',
     &           nf_strerror(irc),irc
            return
         end if
C         irc = nf_put_att_text(ncid, cvarid, 
C     &        'proj4', lenp, proj250)
C         if (irc .ne. nf_noerr) then
C            write(*,*) myname,'Error return from NF_PUT_ATT_TEXT7',
C     &           nf_strerror(irc),irc
C            return
C         end if
      else if (igtype.eq.3) THEN ! spherical *******************
         cname="rotated_latitude_longitude"
         call chop0(cname,250)
         lenc=length(cname,250,20)
         ncproj=cname
C     define "proj"-string
         b(5)=mod(g(5)+180.0D0,360.0D0)
         b(6)=90.0D0-g(6)
         do ii=5,6
            write(bs(ii),'(F27.15)') b(ii)
            call nice(bs(ii))
            call chop0(bs(ii),50)
            bl(ii)=length(bs(ii),50,10)
         end do
         write(proj250,'("+proj=ob_tran +o_proj=longlat +lon_0=",A,'//
     &        '" +o_lat_p=",A)') ! o_lon_p=north_pole_grid_lon
     &        bs(5)(1:bl(5)),
     &        bs(6)(1:bl(6))
         call chop0(proj250,250)
         lenp=length(proj250,250,30)
C     Define projection variable in nc-file
         irc = nf_def_var(ncid, cname(1:lenc), NF_INT, 
     +        0, dimids, cvarid)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_DEF_VAR8',
     &           nf_strerror(irc),irc
            return
         end if
C     "grid_mapping_name" -> "rotated_latitude_longitude"
         irc = nf_put_att_TEXT(ncid, cvarid, 
     &        'grid_mapping_name', lenc, cname)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_REAL9',
     &           nf_strerror(irc),irc
            return
         end if
C         north_pole_grid_lon = 0.0D0
C         north_pole_lon = b(5)
C         north_pole_lat = b(6)
C     "grid_north_pole_longitude" -> +lon_0 (double), normalizeLongitude180(180+g(5))) north_pole_lon, b(5)
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "grid_north_pole_longitude", NF_DOUBLE,1,b(5)); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE10',
     &           nf_strerror(irc),irc
            return
         end if
C     "grid_north_pole_latitude" ->  +o_lat_p (double), north_pole_lat
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "grid_north_pole_latitude", NF_DOUBLE,1,b(6)); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE11',
     &           nf_strerror(irc),irc
            return
         end if
c$$$C     "north_pole_grid_longitude" -> +o_lon_p (double), north_pole_grid_lon
c$$$         irc = nf_put_att_DOUBLE(ncid, cvarid, 
c$$$     &        "north_pole_grid_longitude", NF_DOUBLE,1,0.0D0); 
c$$$         if (irc .ne. nf_noerr) then
c$$$            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE12',
c$$$     &           nf_strerror(irc),irc
c$$$            return
c$$$         end if
C     "proj4" -> proj250 (+proj4GetEarthAttributes)
         call earthr(buff)      ! 
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        'earth_radius', NF_DOUBLE,1, buff)
         if (irc .ne. nf_noerr) then
            write(*,*) 'NCIMG Error return from NF_PUT_ATT_REAL8',
     &           nf_strerror(irc),irc
            return
         end if
C         irc = nf_put_att_text(ncid, cvarid, 
C     &        'proj4', lenp, proj250)
C         if (irc .ne. nf_noerr) then
C            write(*,*) myname,'Error return from NF_PUT_ATT_TEXT13',
C     &           nf_strerror(irc),irc
C            return
C         end if
      else if (igtype.eq.5) THEN ! mercator (unrotated) *******************
         cname="mercator"
         call chop0(cname,250)
         lenc=length(cname,250,20)
         ncproj=cname
C     define "proj"-string
         write(proj250,'("+proj=merc +lat_ts=",A)') 
     &        gs(5)(1:gl(5))
         call chop0(proj250,250)
         lenp=length(proj250,250,30)
C     Define projection variable in nc-file
         irc = nf_def_var(ncid, cname(1:lenc), NF_INT, 
     +        0, dimids, cvarid)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_DEF_VAR14',
     &           nf_strerror(irc),irc
            return
         end if
C     "grid_mapping_name" -> "mercator"
         irc = nf_put_att_TEXT(ncid, cvarid, 
     &        'grid_mapping_name', lenc, cname)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_REAL15',
     &           nf_strerror(irc),irc
            return
         end if
C     lat_0!=0 not supported for merc, lon_0 and lat_0 part of startx
c$$$C     "longitude_of_projection_origin" -> lon_0, ?
c$$$         irc = nf_put_att_DOUBLE(ncid, cvarid, 
c$$$     &        "longitude_of_projection_origin", NF_DOUBLE,1,0.0D0); 
c$$$         if (irc .ne. nf_noerr) then
c$$$            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE16',
c$$$     &           nf_strerror(irc),irc
c$$$            return
c$$$         end if
C     "standard_parallel" -> lat_ts, g(5)
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "standard_parallel", NF_DOUBLE,1,g(5)); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE17',
     &           nf_strerror(irc),irc
            return
         end if
c$$$C     "scale_factor_at_projection_origin" -> +k=, ?
c$$$         irc = nf_put_att_DOUBLE(ncid, cvarid, 
c$$$     &        "scale_factor_at_projection_origin", NF_DOUBLE,1,0.0D0); 
c$$$         if (irc .ne. nf_noerr) then
c$$$            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE18',
c$$$     &           nf_strerror(irc),irc
c$$$            return
c$$$         end if
C     "proj4" -> proj250 (+proj4GetEarthAttributes)
         call earthr(buff)      ! 
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        'earth_radius', NF_DOUBLE,1, buff)
         if (irc .ne. nf_noerr) then
            write(*,*) 'NCIMG Error return from NF_PUT_ATT_REAL8',
     &           nf_strerror(irc),irc
            return
         end if
C         irc = nf_put_att_text(ncid, cvarid, 
C     &        'proj4', lenp, proj250)
C         if (irc .ne. nf_noerr) then
C            write(*,*) myname,'Error return from NF_PUT_ATT_TEXT19',
C     &           nf_strerror(irc),irc
C            return
C         end if
      else if (igtype.eq.6.or.igtype.eq.9) THEN ! lambert conformal conic *******************
         cname="lambert_conformal_conic"
         call chop0(cname,250)
         lenc=length(cname,250,20)
         ncproj=cname
C     define "proj"-string
         write(proj250,'("+proj=lcc +lon_0=",A," +lat_0=",A,'//
     &        '" +lat_1=",A," +lat_2=",A)')
     &        gs(5)(1:gl(5)),
     &        gs(6)(1:gl(6)),
     &        gs(6)(1:gl(6)),
     &        gs(6)(1:gl(6))
         call chop0(proj250,250)
         lenp=length(proj250,250,30)
C     Define projection variable in nc-file
         irc = nf_def_var(ncid, cname(1:lenc), NF_INT, 
     +        0, dimids, cvarid)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_DEF_VAR20',
     &           nf_strerror(irc),irc
            return
         end if
C     "grid_mapping_name" -> "lambert_conformal_conic"
         irc = nf_put_att_TEXT(ncid, cvarid, 
     &        'grid_mapping_name', lenc, cname)
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_REAL21',
     &           nf_strerror(irc),irc
            return
         end if
C     "standard_parallel" -> (+lat_1 (double) = g(6)) (+lat_2 (double) = g(6))
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "standard_parallel", NF_DOUBLE,1,g(6)); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE22',
     &           nf_strerror(irc),irc
            return
         end if
C     "longitude_of_central_meridian" -> +lon_0 (double) = g(5)
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "longitude_of_central_meridian", NF_DOUBLE,1,g(5)); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE23',
     &           nf_strerror(irc),irc
            return
         end if
C     "latitude_of_projection_origin" -> +lat_0 (double) = g(6)
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        "latitude_of_projection_origin", NF_DOUBLE,1,g(6)); 
         if (irc .ne. nf_noerr) then
            write(*,*) myname,'Error return from NF_PUT_ATT_DOUBLE24',
     &           nf_strerror(irc),irc
            return
         end if
C     "proj4" -> proj250 (+proj4GetEarthAttributes)
         call earthr(buff)      ! 
         irc = nf_put_att_DOUBLE(ncid, cvarid, 
     &        'earth_radius', NF_DOUBLE,1, buff)
         if (irc .ne. nf_noerr) then
            write(*,*) 'NCIMG Error return from NF_PUT_ATT_REAL8',
     &           nf_strerror(irc),irc
            return
         end if
c     "proj4"
C         irc = nf_put_att_text(ncid, cvarid, 
C     &        'proj4', lenp, proj250)
C         if (irc .ne. nf_noerr) then
C            write(*,*) myname,'Error return from NF_PUT_ATT_TEXT25',
C     &           nf_strerror(irc),irc
C            return
C         end if
      else                      ! *******************
         write(*,*)myname,'Unknown projection:',igtype
         irc=945
         return
      end IF
C
      return
      end function ncproj

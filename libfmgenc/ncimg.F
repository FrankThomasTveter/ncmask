      subroutine NCIMG(
     &     ogtype,              ! projection type
     &     og,                  ! projection parameters
     &     nrlon,               ! nr of x in cmat(x,y)
     &     nrlat,               ! nr of y in cmat(x,y)
     &     GRIDLON,             ! grid longitude
     &     GRIDLAT,             ! grid latitude
     &     npar,                ! number of parameters
     &     GRIDVAL,             ! grid value
     &     minval,maxval,       ! minimum, maximum values
     &     scale,               ! scale factor
     &     name250,             ! name of image
     &     maxatt,              ! maximum number of attributes
     &     natt,                ! number of attributes
     &     catt,                ! attribute name
     &     cval,                ! attribute value
     &     YY1,MM1,DD1,HH1,MI1, ! time of the image
     &     fn250,               ! output file
     &     irc)                 ! error return code
C     
      implicit none
      integer ogtype
      real og(6)
      integer nrlon
      integer nrlat
      real*4 gridlon(nrlon,nrlat)
      real*4 gridlat(nrlon,nrlat)
      integer npar
      real gridval(nrlon,nrlat,npar)
      real  minval(npar),maxval(npar),scale(npar)
      integer maxatt
      integer natt(npar)
      character*250 name250(npar)
      character*250 catt(maxatt,npar),cval(maxatt,npar)
      integer yy1,mm1,dd1,hh1,mi1
      character*250 fn250
      integer irc
C
C     
#     include "netcdf.inc"
C     
      real*4 gridx(nrlon,nrlat), vecx(nrlon)
      real*4 gridy(nrlon,nrlat), vecy(nrlat)
C
      integer length, leni, lens,lent,ii,pp,lenv,lena
      character*250 ncproj
      external length,ncproj
      character*100 type100
      real*4 standard_parallel(2), buff
      character*100 proj4
      real S1970, time1970, sec
      external s1970
      data sec/0.0D0/
C
C     This is the name of the data file we will create.

C     We are writing 2D data, a 6 x 12 lat-lon grid. We will need two
C     netCDF dimensions.
      INTEGER NDIMS, dims
      PARAMETER (NDIMS=3)
      INTEGER DIMIDS(NDIMS)
      INTEGER NCID
      INTEGER LON_DIMID, LAT_DIMID, time_dimid
      INTEGER LAM_VARID, LAT_VARID, LON_VARID, time_varid
      integer x_varid,y_varid,xx_varid,yy_varid,VARID(npar)
      character*250 pro250
      integer lenp

C     Loop indices.
      integer lat, lon
      character*8 myname
      data myname /'NCIMG'/
C
      time1970= S1970(YY1,MM1,DD1,HH1,MI1,sec)
C
C     Create the file. 
      irc = nf_create(fn250, nf_clobber, ncid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_CREATE0',
     &        nf_strerror(irc),irc
          return
       end if

C     Define the dimensions.
      irc = nf_def_dim(ncid, 'x', NRLON, lon_dimid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_DIM1',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_def_dim(ncid, 'y', NRLAT, lat_dimid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_DIM2',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_def_dim(ncid, 'time', nf_unlimited, time_dimid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_DIM2',
     &        nf_strerror(irc),irc
          return
       end if
C     
C     Define coordinate system
       pro250=ncproj(ncid,ogtype,og,irc)
       if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR3',
     &         nf_strerror(irc),irc
          return
       end if
       call chop0(pro250,250)
       lenp=length(pro250,250,20)
C     
C     Define the coordinate variables.
C
      dimids(1) = lon_dimid
      dimids(2) = lat_dimid
      dimids(3) = time_dimid
      irc = nf_def_var(ncid, 'latitude', NF_REAL, 
     +     2, dimids, lat_varid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR10',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_def_var(ncid, 'longitude', NF_REAL, 
     +     2, dimids, lon_varid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR12',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_def_var(ncid, 'time', NF_REAL, 
     +     1, time_dimid, time_varid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR12',
     &        nf_strerror(irc),irc
          return
       end if
C
C     define grid coordinate variables
C
       call ncgrid(ogtype,og,
     &      nrlon,nrlat,gridlon,gridlat,
     &      gridx, gridy, dims, irc)
       if (irc.ne.0) THEN
          write(*,*)myname,'Error return from NCGRID.',irc
          return
       end if
C
      irc = nf_def_var(ncid, 'x', NF_REAL, 
     +     1, lon_dimid, x_varid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR10',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_def_var(ncid, 'y', NF_REAL, 
     +     1, lat_dimid, y_varid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR10',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_def_var(ncid, 'xx', NF_REAL, 
     +     2, dimids, xx_varid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR10',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_def_var(ncid, 'yy', NF_REAL, 
     +     2, dimids, yy_varid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR10',
     &        nf_strerror(irc),irc
          return
       end if
C     
C     Assign attributes to coordinate var data.
C 
      irc = nf_put_att_text(ncid, lat_varid,
     &     'units',        12, "degree_north")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR13',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, lat_varid,
     &     'long_name',     8, "latitude")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT14',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, lat_varid,
     &     'standard_name', 8, "latitude")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT15',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, lon_varid,
     &     'units',        11, "degree_east")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT16',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, lon_varid,
     &     'long_name',     9, "longitude")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT17',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, lon_varid,
     &     'standard_name', 9, "longitude")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT18',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, lat_varid,
     &     'units',        12, "degree_north")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR19',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, time_varid,
     &     'long_name',4, "time")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR19',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, time_varid,
     &     'units',40, "seconds since 1970-01-01 00:00:00 +00:00")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR19',
     &        nf_strerror(irc),irc
          return
       end if
c
      irc = nf_put_att_text(ncid, x_varid,
     &     'long_name',32, "x-coordinate in Cartesian system")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT21',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, x_varid,
     &     'standard_name',23, "projection_x_coordinate")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT22',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, x_varid,
     &     'units', 1, "m")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR23',
     &        nf_strerror(irc),irc
          return
       end if
c
      irc = nf_put_att_text(ncid, y_varid,
     &     'long_name',32, "y-coordinate in Cartesian system")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT24',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, y_varid,
     &     'standard_name',23, "projection_y_coordinate")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT25',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_att_text(ncid, y_varid,
     &     'units', 1, "m")
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_DEF_VAR26',
     &        nf_strerror(irc),irc
          return
       end if
C
C     loop over variables
C
       do pp=1,npar
C
C     Define the netCDF variables. 
C
          type100=name250(pp)(1:100)
          call chop0(type100,100)
          lent=length(type100,100,10)
          irc = nf_def_var(ncid,type100(1:lent), NF_REAL, 
     &         NDIMS, dimids, varid(pp))
          if (irc .ne. nf_noerr) then
             write(*,*) 'NCIMG Error return from NF_DEF_VAR30',
     &            nf_strerror(irc),irc,type100(1:lent)
             return
          end if
          
C     Assign attributes to the netCDF variables.
          do ii=1,natt(pp)
             call chop0(catt(ii,pp),250)
             lena=length(catt(ii,pp),250,10)
             call chop0(cval(ii,pp),250)
             lenv=length(cval(ii,pp),250,10)
             irc = nf_put_att_text(ncid, varid(pp), catt(ii,pp)(1:lena), 
     &            lenv, cval(ii,pp)(1:lenv))
             if (irc .ne. nf_noerr) then
                write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT31',
     &               nf_strerror(irc),irc
                return
             end if
          end do
c$$$  C
          if (maxval(pp).gt.minval(pp).and.
     &         abs(scale(pp)-1.0D0).gt.1.0D-10) THEN
             irc = nf_put_att_real(ncid, varid(pp),"scale_factor", 
     &            nf_real,1,scale(pp))
             if (irc .ne. nf_noerr) then
                write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT32',
     &               nf_strerror(irc),irc
                return
             end if
c$$$  irc = nf_put_att_real(ncid, varid(pp),"valid_min", 
c$$$  &         nf_real,1,minval(pp))
c$$$  if (irc .ne. nf_noerr) then
c$$$  write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT33',
c$$$  &            nf_strerror(irc),irc
c$$$  return
c$$$  end if
c$$$  irc = nf_put_att_real(ncid, varid(pp),"valid_max", 
c$$$  &         nf_real,1,maxval(pp))
c$$$  if (irc .ne. nf_noerr) then
c$$$  write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT34',
c$$$  &            nf_strerror(irc),irc
c$$$  return
c$$$  end if
          end IF
C     define coordinate system
          irc = nf_put_att_text(ncid, varid(pp), 'grid_mapping', 
     &         lenp, pro250(1:lenp))
          if (irc .ne. nf_noerr) then
             write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT35',
     &            nf_strerror(irc),irc
             return
          end if
       end do
C     define coordinate system
       irc = nf_put_att_text(ncid, nf_global, 'Conventions', 
     &      6,'CF-1.6')
       if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_ATT_TEXT35',
     &         nf_strerror(irc),irc
          return
       end if
C     End define mode.
      irc = nf_enddef(ncid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_ENDEF36',
     &        nf_strerror(irc),irc
          return
       end if

C     Write the coordinate variable data. This will put the latitudes
C     and longitudes of our data grid into the netCDF file.
      irc = nf_put_var_real(ncid, lat_varid, gridlat)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_VAR_REAL37',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_var_real(ncid, lon_varid, gridlon)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_VAR_REAL38',
     &        nf_strerror(irc),irc
          return
       end if
       do ii=1,NRLON
          vecx(ii)=gridx(ii,1)
          if (abs (gridx(ii,1)-gridx(ii,nrlat)).gt.
     &         0.01*abs(gridx(min(ii+1,nrlon),1)-
     &         gridx(max(1,ii-1),nrlat))) THEN
             write(*,*) myname,'Irregular Xgrid:',
     *            ii,gridx(ii,1),gridx(ii,nrlat)
          end if
       end DO
      irc = nf_put_var_real(ncid, x_varid,vecx)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_VAR_REAL37',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_var_real(ncid, xx_varid,gridx)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_VAR_REAL37',
     &        nf_strerror(irc),irc
          return
       end if
       do ii=1,NRLAT
          vecy(ii)=gridy(1,ii)
          if (abs (gridy(1,ii)-gridy(nrlon,ii)).gt.
     &         0.01*abs(gridy(1,min(ii+1,nrlat))-
     &         gridy(nrlon,max(1,ii-1)))) then 
             write(*,*) myname,'Irregular Ygrid:',
     *            ii,gridy(1,ii),gridy(nrlon,ii)
          end if
       end DO
      irc = nf_put_var_real(ncid, y_varid, vecy)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_VAR_REAL38',
     &        nf_strerror(irc),irc
          return
       end if
      irc = nf_put_var_real(ncid, yy_varid, gridy)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_VAR_REAL38',
     &        nf_strerror(irc),irc
          return
       end if

      irc = nf_put_vara_double(ncid, time_varid,1,1,time1970)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_PUT_VAR_REAL38',
     &        nf_strerror(irc),irc
          return
       end if

       do pp=1,npar
C     Write the data.
          irc = nf_put_var_double(ncid, varid(pp), gridval(1,1,npar))
          if (irc .ne. nf_noerr) then
             write(*,*) 'NCIMG Error return from NF_PUT_VAR_REAL39',
     &            nf_strerror(irc),irc
             return
          end if
       end do
C     Close the file.
      irc = nf_close(ncid)
      if (irc .ne. nf_noerr) then
          write(*,*) 'NCIMG Error return from NF_CLOSE40',
     &        nf_strerror(irc),irc
          return
       end if
   
      return
      end subroutine ncimg

program inside
  !
  ! Check if polyline is inside polygon file list
  !
  use xmlparse
  use ncf
  use sim
  IMPLICIT NONE
  !
  ! types of filters
  !
  integer, parameter :: flt_slice = 1
  integer, parameter :: flt_cylinder = 2
  integer, parameter :: flt_polygon = 3
  integer, parameter :: flt_polyline = 4
  integer, parameter :: flt_duct = 5
  integer, parameter :: flt_value = 6
  integer, parameter :: flt_string = 7
  integer, parameter :: flt_dimension = 8
  !
  character*250 :: in250
  integer, parameter :: maxfn=100
  integer :: nrfn
  character*250 :: fn250(maxfn)
  INTEGER :: ii
  CHARACTER(len=250) :: arg250
  INTEGER IRC
  ! 
  CHARACTER*12 MYNAME
  DATA MYNAME /'MNCMASK'/
  ! 
  LOGICAL  BDEB
  DATA BDEB   /.false./ ! not set here...
  ! 
  integer ftunit
  external ftunit
  integer, external:: length
  !
  ! xml variables
  !
  type xmltype
     character*700 b700
     INTEGER  INTOUT
     LOGICAL  ENDOFF
     logical           :: mustread
     type(XML_PARSE)   :: info
     character(len=80)                      :: tag
     logical                                :: starttag
     logical                                :: endtag
     character(len=700), dimension(1:2,1:20) :: attribs
     integer                                :: no_attribs
     character(len=700), dimension(1:100)   :: data
     integer                                :: no_data
  end type xmltype
  !
  type node
     type(node), pointer :: next=>null()
     type(node), pointer :: prev=>null()
     logical :: split=.false.
     real :: lat,lon
  end type node
  !
  type filter ! filter definition
     integer               :: type = 0
     character*250         :: name250 = ""
     character*250         :: file250 = ""
     real                  :: eps=10.0D0
     real                  :: delta=0.0D0 ! in km
     logical               :: inside = .true.
     logical               :: simplify = .true.
     logical               :: ledge = .false.
     integer               :: mpar=0
     logical, allocatable  :: lpar(:)
     integer, allocatable  :: npar(:)
     real, allocatable     :: rpar(:)
     character*250         :: spar250
     integer               :: lens
     character*80          :: sdim80 = ""
     logical               :: ipar = .false.
     logical               :: active = .false.
     character*250         :: var250 = ""! variable to be loaded
     type(DimensionOrder), pointer :: sdo=>null()
     type(inventory), pointer :: i => null()
     type(variable), pointer :: v => null()
     real :: minval = 0.0D0
     real :: maxval = 0.0D0
     ! chain
     type(filter), pointer :: next=>null()
     type(filter), pointer :: prev=>null()
  end type filter
  !
  type report
     ! integer :: ntim
     ! integer nxml,nkey
     ! logical :: laux
     ! type(variable), pointer :: vaux=>null()
     ! real :: auxmin(mtim),auxmax(mtim)
     ! !
     ! character*700 xml700(2,mxml),aux700(2),key700(2,mkey)
     ! logical :: lxml=.false., ljson=.false.
     ! character*350 xmlo350,poly350 ! var350,
     ! logical :: lpoly=.false.  ! write polygon?
     ! logical :: lwrite=.false. ! write empty reports?
     ! ! targets
     ! logical :: ltrg=.false.
     ! integer :: mrest=0
     ! integer :: nrest=0
     ! integer :: arest=0
     ! real, allocatable :: key1(:,:)
     ! integer, allocatable :: nkey1(:),nkey0(:),keyind(:,:)
     ! integer :: grouptrg=0
     ! integer :: ngroups
     ! real, allocatable :: groups(:)
     ! ! macros...
     ! character*350 xmlvar350(mxmlvar,mtim)
     ! character*350 keyvar350(mkeyvar)
     ! ! statistics
     ! real valmin(mtim),valmax(mtim),valavg(mtim),valcnt(mtim)
     ! real llmin(2,mtim),llmax(2,mtim)
     ! integer ttmin(mtim),ttmax(mtim)
     ! logical lfirst(mtim),lmin,lmax
     ! real tj2000(mtim)
     ! ! targets
     ! integer ntrg
     ! real trgval(mtrg)
     ! character*700 :: trgvar700(mtrg)
     ! integer trgtyp(mtrg)
     ! character*10 trg10(mtrg)
     ! logical trgreq(mtrg)
     ! ! average values
     ! logical :: lavg=.false.
     ! character*10 :: avg10
     ! center position and radius..
     logical :: lcen = .false. ! does report have a center-distance?
     logical :: lcok = .true. ! does report have a valid center-distance?
     real :: clat     ! center latitude
     real :: clon     ! center longitude
     real :: cdist    ! max center distance
     real :: sn=0     ! sum count
     real :: sd=0.0D0 ! max distance in degrees
     real :: sx=0.0D0 ! sum x
     real :: sy=0.0D0 ! sum y
     real :: sz=0.0D0 ! sum z
     logical :: used=.false. ! is report ever used
     ! ! filter
     ! type(operation), pointer :: roperation => null() ! root operation
     ! type(operation), pointer :: coperation => null() ! current operation
     ! type(filter), pointer :: firstfilter=> null()
     ! type(filter), pointer :: lastfilter => null()
     ! ! report chain
     ! type(report), pointer :: next=>null()
     ! type(report), pointer :: prev=>null()
  end type report
  !
  nrfn=0
  DO ii = 1, iargc()
    CALL getarg(ii, arg250)
    if (ii.eq.1) then
       in250=arg250
    else
       nrfn=min(maxfn,nrfn+1)
       fn250(nrfn)=arg250
    end if
  END DO
  do ii=1,nrfn
     if (isinside(in250,fn250(ii))) then
        write(*,*) "$$$ OVERLAPS: "//trim(in250), " -> ",trim(fn250(ii))
     end if
  end do

contains
  logical function isinside(in250, trg250)
    implicit none
    character*250 in250, trg250
    type(filter), pointer :: nfilter => null()
    type(filter), pointer :: tfilter => null()
    type(report), pointer :: crep=>null()
    type(xmltype)  :: xml
    type(XML_PARSE) :: inc
    real :: ilat,ilon
    integer :: inout,ii
    ! read source
    write(*,*) "Processing: "//trim(in250)//" -> "//trim(trg250)
    allocate(nfilter,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate filter.',irc
       return
    end if
    call xml_open( inc, trim(in250), .true. ) ! read file name
    call xml_options(inc, report_lun = 21, report_details = .false. )
    xml%starttag=.true. ! "open" the xml%tag and read the include-file
    xml%endtag=.false. ! "open" the xml%tag and read the include-file
    call definePolyline(xml,inc,crep,nfilter,irc)
    if (irc.ne.0) then
       write(*,*) myname,'Error return from definePolygline.',irc
       call xml_close( inc )
       return
    end if
    call xml_close( inc )
    ! read target
    allocate(tfilter,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate filter.',irc
       return
    end if
    call xml_open( inc, trim(trg250), .true. ) ! read file name
    call xml_options(inc, report_lun = 21, report_details = .false. )
    xml%starttag=.true. ! "open" the xml%tag and read the include-file
    xml%endtag=.false. ! "open" the xml%tag and read the include-file
    call definePolygon(xml,inc,crep,tfilter,irc)
    if (irc.ne.0) then
       write(*,*) myname,'Error return from definePolygon.',irc
       call xml_close( inc )
       return
    end if
    call xml_close( inc )
    isinside=.false.
    ! loop over polyline
    do ii=1,nfilter%mpar
       ilat=nfilter%rpar(ii)
       ilon=nfilter%rpar(ii+nfilter%mpar)
       CALL PNPOLY (ilat, ilon, tfilter%mpar, tfilter%rpar(1), &
            & tfilter%rpar(1+tfilter%mpar), INOUT )
       if (inout.ge.0) then
          isinside=.true.
          return;
       end if
    end do
    return
  end function isinside
  subroutine definePolygon(xml,info,crep,nfilter,irc)
    implicit none
    type(xmltype)              :: xml
    type(XML_PARSE)            :: info
    type(report), pointer      :: crep
    type(filter), pointer      :: nfilter
    integer :: irc
    type(node), pointer :: first=>null()
    type(node), pointer :: last=>null()
    type(node), pointer :: cnode=>null(),nnode=>null()
    logical llat,llon
    real minlat,maxlat,minlon,maxlon,lat,lon
    integer, external :: length
    integer :: len2,ii,cnt
    character*700 :: buff700
    logical :: bok
    CHARACTER*16 MYNAME
    DATA MYNAME /'definePolygon'/
    nfilter%mpar=0
    if (allocated(nfilter%lpar)) deallocate(nfilter%lpar)
    if (allocated(nfilter%npar)) deallocate(nfilter%npar)
    allocate(nfilter%lpar(0:1),nfilter%npar(1),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    if (xml%starttag) then
       nfilter%type=flt_polygon ! polygon
       nfilter%lpar(0)=.true. ! need initialisation
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=132
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."volume") then
             if (buff700(1:len2).eq."inside") then
                nfilter%inside = .true. ! inside
             else if (buff700(1:len2).eq."outside") then
                nfilter%inside = .false. ! outside
             else
                write(*,*)myname,'Unknown setting:',trim(xml%tag)//':'//trim(xml%attribs(1,ii))
             end if
             !                       else if (trim(xml%attribs(1,ii)).eq."surface") then
             !                          nfilter%var250=buff700(1:len2)
             !                          nfilter%lpar(100)=.true.
          else if (trim(xml%attribs(1,ii)).eq."simplify") then
             if (buff700(1:len2).eq."never") then
                nfilter%simplify = .false. ! dont simplify
             end if
          else if (trim(xml%attribs(1,ii)).eq."tolerance") then
             read(buff700(1:len2),*,iostat=irc) nfilter%eps
             if (irc.ne.0) then
                write(*,*)myname,'Error reading attribute:',buff700
                return
             end if
             if (nfilter%eps <=0.0D0) then
                nfilter%simplify=.false.
             end if
          else
             write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=810
             return
          end if
       end do
       allocate(first, last)
       first%next => last
       last%prev => first
    end if
    ! read until xml%endtag is reached
    llat=.false.
    llon=.false.
    cnt=0
    POLYGON: do while(.not. (trim(xml%tag).eq."polygon".and.xml%endtag))
       call xml_get( info, xml%tag, xml%starttag, xml%endtag, xml%attribs, &
            & xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(info) ) exit POLYGON
       cnt=cnt+1
       if (bdeb.and.cnt.lt.3) write(*,*)myname,'Found DEFINE/POLYGON xml%tag:',&
            & trim(xml%tag),xml%starttag,xml%endtag
       if (bdeb.and.cnt.eq.3) write(*,*)myname,'Found DEFINE/POLYGON xml%tag...'
       if (trim(xml%tag).eq."node") then
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=133
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) lat
                if (irc.ne.0) then
                   write(*,*)myname,'Error reading attribute:',buff700
                   return
                end if
                if (.not.llat) then
                   llat=.true.
                   minlat=lat
                   maxlat=lat
                else
                   minlat=min(minlat,lat)
                   maxlat=max(maxlat,lat)
                end if
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lat:',&
                        & buff700(1:len2)
                   return
                end if
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) lon
                if (irc.ne.0) then
                   write(*,*)myname,'Error reading attribute:',buff700
                   return
                end if
                if (.not.llon) then
                   llon=.true.
                   minlon=lon
                   maxlon=lon
                else
                   minlon=min(minlon,lon)
                   maxlon=max(maxlon,lon)
                end if
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lon:',&
                        & buff700(1:len2)
                   return
                end if
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
                irc=812
                return
             end if
          end do
          if (llat.and.llon) then
             allocate(cnode,stat=irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from allocate (cnode).',irc
                return
             end if
             cnode%prev=>last%prev
             cnode%next=>last
             cnode%prev%next => cnode
             cnode%next%prev => cnode
             cnode%lat=lat
             cnode%lon=lon
             nfilter%mpar=nfilter%mpar+1
             ! if (crep%lcen) then
             !    crep%sx=crep%sx + cosdeg(lat)*cosdeg(lon)
             !    crep%sy=crep%sy + cosdeg(lat)*sindeg(lon)
             !    crep%sz=crep%sz + sindeg(lat)
             !    crep%sn=crep%sn + 1.0D0
             ! else
             !    crep%lcen=.true.
             !    crep%sx=cosdeg(lat)*cosdeg(lon)
             !    crep%sy=cosdeg(lat)*sindeg(lon)
             !    crep%sz=sindeg(lat)
             !    crep%sn=1.0D0
             ! end if
          else
             write(*,*)'Ignoring mask-node missing LAT/LON.'
          end if
       else if (trim(xml%tag).ne."polygon") then
          write(*,*)myname,'Unknown xml%tag E:',trim(xml%tag)
          irc=933
          return
       end if
    end do POLYGON
    if (nfilter%mpar .lt. 3) then
       write(*,*) myname,'Too few nodes in polygon.',nfilter%mpar
       irc=458
       return
    end if
    if (allocated(nfilter%rpar)) deallocate(nfilter%rpar)
    allocate(nfilter%rpar(nfilter%mpar*2+4),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    nfilter%rpar(:)=0.0D0
    nfilter%lpar(:)=.false.
    nfilter%npar(:)=0
    ii=0
    cnode => first%next
    do while (.not.associated(cnode, target=last))
       ii=ii+1
       nfilter%rpar(ii)=cnode%lat
       nfilter%rpar(ii+nfilter%mpar)=cnode%lon
       cnode => cnode%next
    end do
    !write(*,*)myname,' LATLON:',minlat,maxlat,minlon,maxlon
    nfilter%rpar(nfilter%mpar*2+1)=minlat
    nfilter%rpar(nfilter%mpar*2+2)=maxlat
    nfilter%rpar(nfilter%mpar*2+3)=minlon
    nfilter%rpar(nfilter%mpar*2+4)=maxlon
    ! simplify filter
    ii=nfilter%mpar
    if (nfilter%simplify) then
       call simplify(nfilter%mpar,nfilter%rpar,nfilter%eps)
       if (nfilter%mpar.gt.ii) then
          write(*,*)myname,'Corrupt simplification.',nfilter%mpar,ii
          irc=999
          return
       end if
    end if
    ! the next bit is done automatically by simplify...
    !nfilter%rpar(nfilter%mpar*2+1)=nfilter%rpar(ii*2+1)
    !nfilter%rpar(nfilter%mpar*2+2)=nfilter%rpar(ii*2+2)
    !nfilter%rpar(nfilter%mpar*2+3)=nfilter%rpar(ii*2+3)
    !nfilter%rpar(nfilter%mpar*2+4)=nfilter%rpar(ii*2+4)
    cnode => first%next
    do while (.not.associated(cnode, target=last))
       nnode=>cnode%next
       if (associated(cnode)) deallocate(cnode)
       cnode => nnode
    end do
    if (associated(first)) deallocate(first)
    if (associated(last)) deallocate(last)
    !if (associated(first)) deallocate(first)
    !if (associated(last)) deallocate(last)
  end subroutine definePolygon


  subroutine definePolyline(xml,info,crep,nfilter,irc)
    implicit none
    type(xmltype)              :: xml
    type(XML_PARSE)            :: info
    type(report), pointer      :: crep
    type(filter), pointer      :: nfilter
    integer :: irc
    type(node), pointer :: first=>null()
    type(node), pointer :: last=>null()
    type(node), pointer :: cnode=>null(),nnode=>null()
    logical llat,llon
    real xlat,xlon
    real minlat,maxlat,minlon,maxlon,lat,lon
    integer, external :: length
    integer :: len2,ii,cnt
    integer :: nsplit,isplit,fsplit,lsplit
    character*700 :: buff700
    logical :: bok
    CHARACTER*16 MYNAME
    DATA MYNAME /'definePolyline'/
    nfilter%mpar=0
    nsplit=1 ! always one split
    if (allocated(nfilter%lpar)) deallocate(nfilter%lpar)
    if (allocated(nfilter%npar)) deallocate(nfilter%npar)
    allocate(nfilter%lpar(0:1),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    if (xml%starttag) then
       nfilter%type=flt_polyline ! polyline
       nfilter%lpar(0)=.true. ! need initialisation
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=132
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."volume") then
             if (buff700(1:len2).eq."inside") then
                nfilter%inside = .true. ! inside
             else if (buff700(1:len2).eq."outside") then
                nfilter%inside = .false. ! outside
             else
                write(*,*)myname,'Unknown setting:',trim(xml%tag)//':'//trim(xml%attribs(1,ii))
             end if
             !                       else if (trim(xml%attribs(1,ii)).eq."surface") then
             !                          nfilter%var250=buff700(1:len2)
             !                          nfilter%lpar(100)=.true.
          else if (trim(xml%attribs(1,ii)).eq."simplify") then
             if (buff700(1:len2).eq."never") then
                nfilter%simplify = .false. ! dont simplify
             end if
          else if (trim(xml%attribs(1,ii)).eq."tolerance") then
             read(buff700(1:len2),*,iostat=irc) nfilter%eps
             if (irc.ne.0) then
                write(*,*)myname,'Error reading attribute:',buff700
                return
             end if
             if (nfilter%eps <=0.0D0) then
                nfilter%simplify=.false.
             end if
          else if (trim(xml%attribs(1,ii)).eq."delta") then
             read(buff700(1:len2),*,iostat=irc) nfilter%delta
             if (irc.ne.0) then
                write(*,*)myname,'Error reading attribute:',buff700
                return
             end if
             nfilter%ledge=(nfilter%delta>0.0D0)
          else
             write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=810
             return
          end if
       end do
       allocate(first, last)
       first%next => last
       last%prev => first
    end if
    ! read until xml%endtag is reached
    llat=.false.
    llon=.false.
    cnt=0
    POLYLINE: do while(.not. (trim(xml%tag).eq."polyline".and.xml%endtag))
       call xml_get( info, xml%tag, xml%starttag, xml%endtag, xml%attribs, &
            & xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(info) ) exit POLYLINE
       cnt=cnt+1
       if (bdeb.and.cnt.lt.3) write(*,*)myname,'Found DEFINE/POLYLINE xml%tag:',&
            & trim(xml%tag),xml%starttag,xml%endtag
       if (bdeb.and.cnt.eq.3) write(*,*)myname,'Found DEFINE/POLYLINE xml%tag...'
       if (trim(xml%tag).eq."split") then
             allocate(cnode,stat=irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from allocate (cnode).',irc
                return
             end if
             cnode%prev=>last%prev
             cnode%next=>last
             cnode%prev%next => cnode
             cnode%next%prev => cnode
             cnode%split=.true.
             nsplit=nsplit+1
       else if (trim(xml%tag).eq."node") then
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=133
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) lat
                if (irc.ne.0) then
                   write(*,*)myname,'Error reading attribute:',buff700
                   return
                end if
                if (.not.llat) then
                   llat=.true.
                   minlat=lat
                   maxlat=lat
                else
                   minlat=min(minlat,lat)
                   maxlat=max(maxlat,lat)
                end if
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lat:',&
                        & buff700(1:len2)
                   return
                end if
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) lon
                if (irc.ne.0) then
                   write(*,*)myname,'Error reading attribute:',buff700
                   return
                end if
                if (.not.llon) then
                   llon=.true.
                   minlon=lon
                   maxlon=lon
                else
                   minlon=min(minlon,lon)
                   maxlon=max(maxlon,lon)
                end if
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lon:',&
                        & buff700(1:len2)
                   return
                end if
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
                irc=812
                return
             end if
          end do
          if (llat.and.llon) then
             allocate(cnode,stat=irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from allocate (cnode).',irc
                return
             end if
             cnode%prev=>last%prev
             cnode%next=>last
             cnode%prev%next => cnode
             cnode%next%prev => cnode
             cnode%lat=lat
             cnode%lon=lon
             nfilter%mpar=nfilter%mpar+1
             ! if (crep%lcen) then
             !    crep%sx=crep%sx + cosdeg(lat)*cosdeg(lon)
             !    crep%sy=crep%sy + cosdeg(lat)*sindeg(lon)
             !    crep%sz=crep%sz + sindeg(lat)
             !    crep%sn=crep%sn + 1.0D0
             ! else
             !    crep%lcen=.true.
             !    crep%sx=cosdeg(lat)*cosdeg(lon)
             !    crep%sy=cosdeg(lat)*sindeg(lon)
             !    crep%sz=sindeg(lat)
             !    crep%sn=1.0D0
             ! end if
          else
             write(*,*)'Ignoring mask-node missing LAT/LON.'
          end if
       else if (trim(xml%tag).ne."polyline") then
          write(*,*)myname,'Unknown xml%tag E:',trim(xml%tag)
          irc=933
          return
       end if
    end do POLYLINE
    !if (nfilter%mpar .lt. 3) then
    !   write(*,*) myname,'Too few nodes in polyline.',nfilter%mpar
    !   irc=458
    !   return
    !end if
    if (allocated(nfilter%rpar)) deallocate(nfilter%rpar)
    allocate(nfilter%rpar(nfilter%mpar*2+4),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    allocate(nfilter%npar(1+2*nsplit),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    nfilter%rpar(:)=0.0D0
    nfilter%lpar(:)=.false.
    nfilter%npar(1)=nsplit
    isplit=1
    fsplit=0
    lsplit=0
    ii=0
    cnode => first%next
    do while (.not.associated(cnode, target=last))
       if (cnode%split) then
          nfilter%npar(isplit*2)=fsplit
          nfilter%npar(isplit*2+1)=lsplit
          isplit=isplit+1
          fsplit=0
          lsplit=0
       else
          ii=ii+1
          if (fsplit.eq.0) fsplit=ii
          lsplit=ii
          nfilter%rpar(ii)=cnode%lat
          nfilter%rpar(ii+nfilter%mpar)=cnode%lon
       end if
       cnode => cnode%next
    end do
    nfilter%npar(isplit*2)=fsplit
    nfilter%npar(isplit*2+1)=lsplit
    !write(*,*)myname,' LATLON:',minlat,maxlat,minlon,maxlon
    nfilter%rpar(nfilter%mpar*2+1)=minlat
    nfilter%rpar(nfilter%mpar*2+2)=maxlat
    nfilter%rpar(nfilter%mpar*2+3)=minlon
    nfilter%rpar(nfilter%mpar*2+4)=maxlon
    ! simplify filter
    ii=nfilter%mpar
    if (nfilter%simplify) then
       ! store last position, offset last to first, simplify, reset last to
       call simplifys(nfilter%mpar,nfilter%rpar,nfilter%npar,nfilter%eps)
       if (nfilter%mpar.gt.ii) then
          write(*,*)myname,'Corrupt simplification.',nfilter%mpar,ii
          irc=999
          return
       end if
    end if
    ! the next bit is done automatically by simplify...
    !nfilter%rpar(nfilter%mpar*2+1)=nfilter%rpar(ii*2+1)
    !nfilter%rpar(nfilter%mpar*2+2)=nfilter%rpar(ii*2+2)
    !nfilter%rpar(nfilter%mpar*2+3)=nfilter%rpar(ii*2+3)
    !nfilter%rpar(nfilter%mpar*2+4)=nfilter%rpar(ii*2+4)
    cnode => first%next
    do while (.not.associated(cnode, target=last))
       nnode=>cnode%next
       if (associated(cnode)) deallocate(cnode)
       cnode => nnode
    end do
    if (associated(first)) deallocate(first)
    if (associated(last)) deallocate(last)
    !if (associated(first)) deallocate(first)
    !if (associated(last)) deallocate(last)
  end subroutine definePolyline
  !
  subroutine replaceEnv(str,lens,bok,irc)
    implicit none
    character*(*) str
    integer lens
    logical bok
    integer irc
    character*700 :: i700,b700
    integer :: leno,opos,cpos,leni,leny
    integer, external :: length, lengthtrim
    character*50 :: bal50
    CHARACTER*12 MYNAME
    DATA MYNAME /'replaceENV'/
    bok=.true.
    leno=0
    opos=1
    cpos=1
    i700=str
    call chop0(i700,700)
    leni=length(i700,700,20)
    do while (cpos.le.leni)
       if (i700(cpos:cpos).eq."$") then ! first "$"
          b700=b700(1:leno)//i700(opos:cpos-1)
          leno=leno+max(0,cpos-opos)
          ! find next "$" if any
          cpos=cpos+1
          opos=cpos
          do while (cpos.lt.leni.and.i700(cpos:cpos).ne."$") ! second "$"
             cpos=cpos+1
          end do
          ! get environment-variable i700(opos:cpos-1)
          if (getVarEnv(i700(opos:cpos-1),bal50)) then
             leny=len_trim(bal50)
             b700=b700(1:leno)//bal50(1:leny)
             leno=leno+max(0,leny)
             opos=cpos+1 ! point to character after "$"
          else
             bok=.false.
             write(*,*)myname,'*** Undefined ENVIRONMENT variable: "'//&
                  & i700(opos:cpos-1)//'"'
             opos=cpos+1 ! reset opos to first "$"
          end if
       end if
       !write(*,*) myname,' BUFF:::',cpos,lenb,i700(cpos:cpos)
       cpos=cpos+1
    end do
    b700=b700(1:leno)//i700(opos:cpos-1) ! copy the rest
    leno=leno+max(0,cpos-opos)
    ! if(bdeb) write(*,*)myname,'Output: "'//b700(1:min(lens,leno))//'"',lens
    str=b700(1:min(lens,min(lens,leno)))
    call chop0(str,lens)
    return
  end subroutine replaceEnv
  !
  !
  logical function getVarEnv(var,val50)
    implicit none
    character(*) :: var
    character*50 :: val50
    integer, parameter :: menv=10
    integer :: nenv=0
    call getenv(var,val50)
    getVarEnv=(len_trim(val50).ne.0)
    return
  end function getVarEnv

end program inside

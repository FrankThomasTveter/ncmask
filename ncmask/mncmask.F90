SUBROUTINE MNCMASK(UNITI,IRC)
  ! 
  ! ***********************************************************************
  ! +                                                                     *
  ! +  UNITI = UNIT NUMBER FOR INPUT FILE                                 *
  ! +  IRC=ERROR RETURN CODE (0=OK)                                       *
  ! +                                                                     *
  ! +                                                                     *
  ! VERSION                      : 01/01/00                               *
  ! +                                                                     *
  ! WRITTEN/MODIFIED BY:                                                  *
  ! --------------------------------------------------------------------- *
  ! |    NAME      |   DATE   |                 REASON                  | *
  ! --------------------------------------------------------------------- *
  ! | F. TVETER    | 01/08/14 | NEW                                     | *
  ! |              |          |                                         | *
  ! --------------------------------------------------------------------- *
  ! ***********************************************************************
  !
  use xmlparse
  use ncf
  use sim
  use parse
  !use shape
  IMPLICIT NONE
  SAVE
  ! INTERFACE VARIABLES
  ! 
  INTEGER  UNITI
  INTEGER IRC
  ! 
  CHARACTER*12 MYNAME
  DATA MYNAME /'MNCMASK'/
  ! 
  INTEGER  IKODE,OKODE
  LOGICAL  OK,LFLDAT(100),BDEB,ACTIVE,found
  DATA BDEB   /.false./ ! not set here...
  DATA ACTIVE /.FALSE./
  ! 
  integer unitx,ftunit
  external ftunit
  integer lenx,leny,lenn,lenv,opos,cpos,lenp,len2
  INTEGER  KODE, lena, lend, lenb, lenh, lene, lenr
  integer, external:: length
  character*250 :: buff250
  ! 
  INTEGER  LINE
  INTEGER  NRHDR
  PARAMETER (NRHDR=100)
  !
  real :: tanfact = 1.0
  real :: timfact = 1.0
  integer ecnt
  !
  ! xml-macroes
  !
  integer, parameter :: xml_issued      = 1
  integer, parameter :: xml_issued_yy   = 2
  integer, parameter :: xml_issued_mm   = 3
  integer, parameter :: xml_issued_dd   = 4
  integer, parameter :: xml_issued_hh   = 5
  integer, parameter :: xml_issued_epoch= 6
  integer, parameter :: xml_start_epoch = 7
  integer, parameter :: xml_stop_epoch  = 8
  integer, parameter :: xml_start       = 9
  integer, parameter :: xml_stop        = 10
  integer, parameter :: xml_expires     = 11
  integer, parameter :: xml_minval      = 12
  integer, parameter :: xml_minval_lat  = 13
  integer, parameter :: xml_minval_lon  = 14
  integer, parameter :: xml_minval_time = 15
  integer, parameter :: xml_minval_dt   = 16
  integer, parameter :: xml_minval_dc   = 17
  integer, parameter :: xml_minval_yy   = 18
  integer, parameter :: xml_minval_mm   = 19
  integer, parameter :: xml_minval_dd   = 20
  integer, parameter :: xml_minval_hh   = 21
  integer, parameter :: xml_maxval      = 22
  integer, parameter :: xml_maxval_lat  = 23
  integer, parameter :: xml_maxval_lon  = 24
  integer, parameter :: xml_maxval_time = 25
  integer, parameter :: xml_maxval_dt   = 26
  integer, parameter :: xml_maxval_dc   = 27
  integer, parameter :: xml_maxval_yy   = 28
  integer, parameter :: xml_maxval_mm   = 29
  integer, parameter :: xml_maxval_dd   = 30
  integer, parameter :: xml_maxval_hh   = 31
  integer, parameter :: xml_acc_hh      = 32
  integer, parameter ::  mxmlvar =32
  integer, parameter :: key_issued      = 1
  integer, parameter :: key_issued_yy   = 2
  integer, parameter :: key_issued_mm   = 3
  integer, parameter :: key_issued_dd   = 4
  integer, parameter :: key_issued_hh   = 5
  integer, parameter :: key_issued_epoch= 6
  integer, parameter :: key_par_name    = 7
  integer, parameter :: key_iter        = 8
  integer, parameter ::  mkeyvar = 8
  !
  ! types of operations
  !
  integer, parameter :: opr_inter = 1
  integer, parameter :: opr_union = 2
  integer, parameter :: opr_filter = 3
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
  ! types of targets...
  !
  integer, parameter :: trg_fraction=1  ! fraction
  integer, parameter :: trg_area=2      ! area
  integer, parameter :: trg_count=3     ! count
  integer, parameter :: trg_aux=4       ! auxiliary
  integer, parameter :: trg_average=5   ! average
  !
  ! fail modes
  !
  integer, parameter :: fail_NoData=1
  integer, parameter :: fail_AnyData=2
  integer, parameter :: fail_None=3
  integer, parameter :: fail_Size=4
  !
  integer mxml,mkey,mtim,mtrg
  parameter(mxml=20,mkey=25,mtim=250,mtrg=100)
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
     integer :: lene=0
     character*700 :: exp700="";
     type(parse_session), pointer :: exp => null();
     ! chain
     type(filter), pointer :: next=>null()
     type(filter), pointer :: prev=>null()
  end type filter
  !
  type operation ! an operation may consist of other operations
     integer                  :: type = 0 ! mask,intersection,union
     character*250            :: name250 = "undefined"
     type(filter), pointer    :: f=>null()
     logical                  :: ignore
     ! logic
     type(operation), pointer :: first=>null()
     type(operation), pointer :: last=>null()
     type(operation), pointer :: parent=>null()
     ! chain
     type(operation), pointer :: next=>null()
     type(operation), pointer :: prev=>null()
  end type operation
  !
  type report
     integer :: ntim
     integer :: nxtr
     integer nxml,nkey
     logical :: laux
     integer :: extind(mtim) ! extreme index
     !
     character*700 xml700(2,mxml),aux700(2),key700(2,mkey)
     logical :: lxml=.false., ljson=.false.,lext=.false.
     character*350 xmlo350,poly350,ext350 ! var350,
     logical :: lpoly=.false.  ! write polygon?
     logical :: lwrite=.false. ! write empty reports?
     ! targets
     logical :: ltrg=.false.
     logical :: larea=.false.
     integer :: mrest=0
     integer :: nrest=0
     integer :: arest=0
     logical :: skip=.false.
     real, allocatable :: key1(:,:)
     integer, allocatable :: nkey1(:),nkey0(:),keyind(:,:)
     integer :: grouptrg=0
     integer :: ngroups
     real, allocatable :: groups(:)
     ! macros...
     character*350 xmlvar350(mxmlvar,mtim)
     character*350 keyvar350(mkeyvar)
     ! statistics
     logical :: wmin=.false.
     logical :: wmax=.false.
     real valmin(mtim),valmax(mtim),valavg(mtim),valcnt(mtim)
     real llmin(2,mtim),llmax(2,mtim)
     integer ttmin(mtim),ttmax(mtim)
     logical lfirst(mtim),lmin,lmax
     real tj2000(mtim)
     ! targets
     integer ntrg
     real trgval(mtrg)
     integer       :: trgtyp(mtrg)
     character*10  :: trg10(mtrg)
     logical       :: trgreq(mtrg)=.false.
     integer       :: trgext(mtrg)=0
     character*700 :: trgvar700(mtrg)
     integer       :: trglenv(mtrg)=0
     type(parse_pointer) :: trgexp(mtrg);
     real :: auxmin(mtim,mtrg),auxmax(mtim,mtrg)
     ! average values
     logical :: lavg=.false.
     character*10 :: avg10
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
     ! filter
     type(operation), pointer :: roperation => null() ! root operation
     type(operation), pointer :: coperation => null() ! current operation
     type(filter), pointer :: firstfilter=> null()
     type(filter), pointer :: lastfilter => null()
     ! report chain
     type(report), pointer :: next=>null()
     type(report), pointer :: prev=>null()
  end type report
  !
  type reportPtr
     type(report), pointer :: ptr=>null()
  end type reportPtr
  !
  !========= fast scan variables...=============
  !
  ! Define a tree node
  type node2d
     real*8 lon, lat, dist       ! Point coordinates
     logical*4 :: enabled=.true.           ! Indicates whether point is deleted or not
     type(report), pointer :: rep => null()
     type(node2d), pointer :: parent => null()
     type(node2d), pointer :: next => null()
     type(node2d), pointer :: l => null() ! Left child node
     type(node2d), pointer :: r => null() ! Right child node
  end type node2d

  ! Due to limitations in Fortran 90, we neeed this to make
  ! an array of pointers to tree nodes
  type node2dptr
     type(node2d), pointer :: p=>null()
  end type node2dptr

  ! Tree main data structure
  type tree2d
     integer*4 N                 ! Number of elements in tree
     type(node2dptr), pointer :: list(:)=> null() ! Data points, sorted cost-wise
     type(node2d), pointer :: root => null() ! Pointer to tree root node
  end type tree2d
  type(node2d), pointer :: searchfirst => null()
  type(node2d), pointer :: searchlast => null()
  ! ======================================================
  !
  type variablePtr
     type(variable), pointer :: ptr=>null()
  end type variablePtr
  !
  ! logical, allocatable          :: req(:)            ! list of required variables
  !  use parse
  !  use shape
  !  character*80, allocatable :: elem80(:) ! list of variable names
  !  real, allocatable         :: val(:)   ! list of values
  !  integer, allocatable      :: lenv(:)  ! list of target name length
  !  logical, allocatable      :: req(:)   ! list of required variables
  !  type(parse_session), pointer :: psf => null()
  !  call parse_open(css%psf,crc250,irc)
  !  call parse_parsef(css%psf,css%flt250(1:css%lenf),css%elem80,crc250,irc)
  !  val=parse_evalf(css%psf,css%val,crc250,irc)
  !  call parse_used(css%psf,css%req)
  !  call parse_close(css%psf,crc250,irc)
  !
  type parameter
     logical :: find=.false.
     logical :: fiad=.false.
     logical :: fino=.false.
     real :: scl=1.0D0
     logical :: hasvar = .false.
     character*350 :: par350
     ! label and expression (if any)
     character*80 :: elem80="";
     character*700 :: exp700="";
     integer :: lene=0
     type(parse_session), pointer :: exp => null();
     integer :: nrep=0
     ! variable
     real :: hrs=0.0D0            ! accumulation hours
     type(variable), pointer :: var=>null()
     ! search tree
     type(tree2d) :: tree
     ! report chain
     type(report), pointer :: firstreport=>null()
     type(report), pointer :: lastreport=>null() ! filter definition chain
     ! parameter chain
     type(parameter), pointer :: next=>null()
     type(parameter), pointer :: prev=>null()
  end type parameter
  !
  type parameterPtr
     type(parameter), pointer :: ptr=>null()
  end type parameterPtr
  !
  type filetype
     real i2000,a2000,tt2000
     real t2000, dt, mint2000,maxt2000
     character*24 i24,s24,e24,a24,t24
     integer :: tmin, tmax
     integer :: mtime,mrest=0,npst=0
     logical :: skip=.false.
     real :: keep=0.0D0, cutoff=0.0D0, maxdays=1000.0D0
     character*350 :: inp350,ref350,nco350 ! file names
     character*80 :: iter80
     logical :: ignorealt=.false.  ! ignore altitude, can cause problems...
     integer :: leni=0
     integer :: ilen1=-1,ilen2=-1
     integer :: extreme = 0
     character*700 :: ign700(2)
     logical :: nodata = .true.
     integer rok(10), rrm(10)
     real pst(10)
     real :: darea = 1.0D0
     logical :: lcrequest=.false.    ! request center-filter
     logical :: lcok=.false.         ! is center-filter available?
     integer :: npar=0
     logical :: firsttrg=.true.,firstt=.true.
     type(inventory), pointer  :: ref => null()
     type(inventory), pointer  :: g => null()
     type(inventory) :: out
     type(variable), pointer  :: v=>null(),i=>null()
     type(Dimension), pointer :: ix => null(),iy=>null()
     type(dimensionOrder), pointer :: refParDO => null()
     type(dimensionOrder), pointer  :: refLatDO=>null(),refLonDO=>null()
     type(dimensionOrder), pointer  :: refAltDO=>null(),refTimDO=>null()
     type(dimensionOrder), pointer  :: refAllDO=>null(),refIterDO=>null()
     type(dimensionOrder), pointer  :: refLatLonAltDO=>null(),refOut=>null()
     type(dimensionOrder), pointer  :: refInn=>null(),refStrDO=>null(),refFlt=>null()
     type(dimensionOrder), pointer  :: refDimDO=>null()
     type(dimensionOrder), pointer  :: refLatLonDO=>null()
     type(parameter), pointer :: firstparameter=> null()
     type(parameter), pointer :: lastparameter => null()
     ! expression stuff
     character*80, allocatable      :: elem80(:)          ! list of variable names
     integer, allocatable           :: lenv(:)           ! list of variable name length
     real, allocatable              :: val(:)            ! list of variable values
     type(variablePtr),allocatable  :: var(:)      ! pointer to variable
     type(parameterPtr),allocatable :: par(:)      ! pointer to variable
     logical    :: hasexp = .false.
     logical    :: hasfltexp = .false.
  end type filetype
  !
  ! global variables
  !
  type(xmltype) :: xml
  type(filetype) :: file
  !
  ! dynamic keys
  !
  type(filter), pointer :: nfilter => null()
  type(filter), pointer :: cfilter => null()
  type(operation), pointer :: noperation => null() ! new operation
  real :: hrs=0.0D0
  character*700 :: var700,b700
  !
  character*80,allocatable      :: flt_elem80(:)          ! filter variable name
  real, allocatable             :: flt_val(:)            ! filter variable value
  !
  real :: maxval,minval
  integer cnt,cntud
  !
  integer ee, tt, ii,jj,kk,ll
  real lat,lon,alt
  !
  logical bok, bbok
  character*250 :: sval250
  integer :: lens
  integer fexp
  type(attribute), pointer :: a=>null()
  integer lenk
  !
  ! polygon structure
  !
  logical :: ifirst=.true.
  integer :: npos=0
  real :: ilat, ilon
  type node
     type(node), pointer :: next=>null()
     type(node), pointer :: prev=>null()
     logical :: split=.false.
     real :: lat,lon
  end type node
  type(node), pointer :: firstnode=>null()
  type(node), pointer :: lastnode=>null()
  integer  :: nodecnt=0
  type(filter), pointer :: gfilter => null()
  !
  real re
  parameter (re = 6371.0D3)
  !parameter (eps = 10.0D0) ! allow 10 km error in polygon
  logical :: fail(4)
  logical :: exists
  !
  IRC=0
  ecnt=0
  ! 
  ! DEBUG SYSTEM
  ! 
  ! IF (.NOT.ACTIVE) CALL DEBUG(MYNAME,BDEB,ACTIVE)
  ! bdeb=.true.
  ! bdeb=.FALSE.
  ! 
  BDEB = .false. ! set here
  IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
  open( 20, file = 'xml.debug' )
  !
  ! initialise
  !
  call initFile(file,irc)
  if (irc.ne.0) then
     write(*,*)myname,'Error return from initfile.',irc
     return
  end if
  !
  file%inp350=""
  call readXmlFile(file,irc)
  if (irc.ne.0) then
     write(*,*)myname,'Error return from readXmlfile.',irc
     return
  end if
  !
  ! read input file
  !
  file%rok(:)=0
  file%rrm(:)=0
  allocate(flt_elem80(1),flt_val(1),stat=irc)
  if (irc.ne.0) then
     write(*,*)myname,'Unable to allocate flt-variables.',irc
     return
  end if
  ! 
  WRITE(*,*) MYNAME,'----------------------------------------'
  !
  ! open file and read inventory
  !
  lenr=length(file%ref350,350,10)
  write(*,*)myname,'Opening: "'//file%ref350(1:lenr)//'"'
  allocate(file%ref,stat=irc)
  if (irc.ne.0) then
     write(*,*)myname,'Unable to allocate REF.',irc
     return
  end if
  call ncf_openFile(file%ref350,file%ref,bok,irc)
  if (irc.ne.0) then
     write(*,*)myname,'Error return from READINVENTORY.',irc
     return
  end if
  !
  call readDataFile(file,bok,irc)
  if (irc.ne.0) then
     write(*,*)myname,'Error return from readDatafile.',irc
     return
  end if
  if (.not.bok) then
     lenr=length(file%ref350,350,10)
     write(*,*)myname,'Corrupt file:',file%ref350(1:lenr)
     irc=347
     return
  end if
  !
  ! check exit conditions
  !
  call checkExitCondition(file,fail,irc)
  if (irc.ne.0) then
     write(*,*) myname,'Error return from checkExitCondition.',irc
     return
  end if
  !
  ! get time dimensions
  !
  call extractTimes(file,bok,irc)
  if (irc.ne.0) then
     write(*,*)myname,'Error return from getTimeInfo.',irc
     return
  end if
  ! !
  ! call matchAuxs(file,irc)
  ! if (irc.ne.0) then
  !    write(*,*)myname,'Error return from matchAuxs.',irc
  !    return
  ! end if
  !
  ! loop over iter-dimension
  !
  call ncf_resetPos(file%refIterDO,irc)
  if (irc.ne.0) then
     write(*,*) myname,'Error return from resetPos (file%refIterDO).',irc
     return
  end if
  call ncf_printDimOrder(file%refIterDO)
  ITER:do while (ncf_increment(file%ref,file%refIterDO,irc))
     !
     call reinReports(file,irc)
     if (associated(file%i)) then
        ii=max(1,ncf_getLocation(file%i))
        jj=nint(ncf_valuePosition(file%i,irc))
        !write(*,'(X,A,A,I0,A,I0)')myname,"Iteration count: ",ii,":",file%i%lend
     else
        ii=1
        jj=0
     end if
     write(*,'(X,A,A,A,A,I0,A,I0,A)')myname,"Iteration '",&
          & file%iter80(1:file%leni),"'=",ii," (",jj,")"
     call processDataFile(file,fail,bok,irc)
     if (irc.ne.0) then
        write(*,*)myname,'Error return from processDataFile.',irc
        return
     end if
     !
     !write(*,*)myname,'Sorting:   ',file%npar,file%mtime
     ! sort filtered values so that we can find dynamic targets (a100, a95...)
     call sortTargetData(file,irc)
     if (irc.ne.0) then
        write(*,*)myname,'Error return from sortTargetData.',irc
        return
     end if
     !
     ! recover data...
     !
     if (bok) then  
        !
        ! write time information
        !
        call writeTimes(file,irc)
        if (irc.ne.0) then
           write(*,*) myname,'Error return from writeTimes.',irc
           return
        end if
        !
        ! make key information...
        !
        call processKeys(file,irc)
        if (irc.ne.0) then
           write(*,*) myname,'Error return from processKeys.',irc
           return
        end if
        !
        call writeReports(file,irc)
        if (irc.ne.0) then
           write(*,*)myname,'Error return from writeReports.',irc
           return
        end if
        !
     else
        call hintReports(file,irc)
        if (irc.ne.0) then
           write(*,*)myname,'Error return from hintReports.',irc
           return
        end if
     end if ! bok
     if (bdeb)call ncf_printDimOrder(file%refIterDO)
  end do ITER
  write(*,*)myname,'Wrapping up.'
  if (bok) then  
     !
     call ncf_closeFile(file%ref,irc) ! close file for reading
     if (irc.ne.0) then
        write(*,*)myname,'Error return from closeFile.',irc
        irc=0
        ! return
     end if
     ! ! print times...
     ! if (associated(file%ref%timid)) then
     !    call ncf_countField(file%ref%timid,cnt,cntud,minval,maxval,irc)
     !    if (irc.ne.0) then
     !       write(*,*)myname,'Error return from COUNTFIELD.',irc
     !       return
     !    end if
     ! end if
     call ncf_unmakeAllRealData(file%ref,irc)
     if (irc.ne.0) then
        write(*,*) myname,'Error return from unmakeAllRealData.',irc
        return
     end if
     !
     lenn=length(file%nco350,350,10)
     if (lenn.ne.0) then ! write to file
        !
        ! remove unused dimensions and write to file
        !
        call ncf_compressVariables(file%ref,irc) ! compress variables
        if (irc.ne.0) then
           write(*,*)myname,'Error return from compressVariables.',irc
           return
        end if
        call ncf_compressDimensions(file%ref)
        call ncf_writeNcOut(file%ref,file%nco350,irc)
        if (irc.ne.0) then
           write(*,*) myname,'Error return from writeNcOut.',irc
           return
        end if
     end if
  else
     !
     ! clean up
     call ncf_clearData(file%ref,irc)
     if (irc.ne.0) then
        write(*,*)myname,'Error return from cleanMemory.',irc
        return
     end if
     !
     call ncf_closeFile(file%ref,irc) ! close file for reading
     if (irc.ne.0) then
        write(*,*)myname,'Error return from closeFile.',irc
        irc=0
        ! return
     end if
  end if
  !
  call clearFile(file,irc)
  if (irc.ne.0) then
     write(*,*)myname,'Error return from clearFile.',irc
     return
  end if
  !
  if (allocated(flt_elem80)) deallocate(flt_elem80)
  if (allocated(flt_val)) deallocate(flt_val)
  !
  ! handle fail conditions
  !
  if (fail(fail_NoData).or.fail(fail_AnyData).or.fail(fail_size)) then
     write(*,*)myname,'FAIL condition encountered.'
     irc=100
  end if
  return
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine initFile(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    CHARACTER*12 MYNAME
    DATA MYNAME /'initfile'/
    allocate(file%firstparameter,file%lastparameter,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate Parameter-chain.',irc
       return
    end if
    file%firstparameter%next => file%lastparameter
    file%lastparameter%prev => file%firstparameter
    !
    file%nco350=""
    file%iter80=""
    file%leni=0
    return
  end subroutine initFile
  !
  subroutine initParameter(cpar,irc)
    !use parse
    !use shape
    implicit none
    type(parameter), pointer :: cpar
    integer :: irc
    type(report), pointer :: crep=>null()
    character*250 :: crc250
    integer :: lenc
    CHARACTER*12 MYNAME
    DATA MYNAME /'iniparameter'/
    cpar%par350=""
    cpar%hasvar=.false.
    if (associated(cpar%lastreport).and.associated(cpar%firstreport)) then
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          call clearReport(crep,irc)
          crep=>crep%next
       end do
       deallocate(cpar%firstreport,cpar%lastreport,stat=irc)
    end if
    allocate(cpar%firstreport,cpar%lastreport,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate first/last-report.',irc
       return
    end if
    cpar%firstreport%next => cpar%lastreport
    cpar%lastreport%prev => cpar%firstreport
    cpar%nrep=0
    call parse_open(cpar%exp,crc250,irc)
    if (irc.ne.0) then
       lenc=length(crc250,250,10)
       write(*,*) crc250(1:lenc)
       write(*,*)myname,'Unable to open expression.',irc
       return
    end if
    return
  end subroutine initParameter
  !
  subroutine initReport(rep,irc)
    implicit none
    type(report), pointer :: rep
    integer :: irc
    CHARACTER*12 MYNAME
    DATA MYNAME /'initreport'/
    !
    rep%nxml=0
    rep%laux=.false.
    rep%nkey=0
    rep%ntrg=0
    rep%valcnt(:)=0.0D0
    rep%lfirst(:)=.true.
    rep%xmlo350="" ! report
    rep%ext350="extreme.xml" ! report
    rep%lpoly=.false.
    rep%lwrite=.false.
    rep%poly350="polygon.json" ! report
    rep%ntim=0
    rep%mrest=0
    rep%nrest=0
    rep%arest=0
    allocate(rep%firstfilter, rep%lastfilter,stat = irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate first/last-filter.',irc
       return
    end if
    rep%firstfilter%next => rep%lastfilter
    rep%lastfilter%prev => rep%firstfilter
    return
  end subroutine initReport
  !
  subroutine reinReports(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(parameter), pointer :: cpar=>null()
    type(report), pointer :: crep=>null()
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       ! loop over reports
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          crep%used=.false.
          crep%keyvar350(key_iter)=""
          call choptrim(crep%keyvar350(key_iter),350)
          crep%lfirst(:)=.true.
          crep=>crep%next
       end do
       cpar=>cpar%next
    end do
    return
  end subroutine reinReports
  !
  subroutine initFileKeys(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(parameter), pointer :: cpar=>null()
    type(report), pointer :: crep=>null()
    CHARACTER*12 MYNAME
    DATA MYNAME /'initkeys'/
    file%lcok=.true.
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       ! loop over reports
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          call initKeys(file,crep,irc)
          crep=>crep%next
       end do
       cpar=>cpar%next
    end do
    if (file%lcrequest.and..not.file%lcok) then
       write(*,*)myname,'Unable to scan "fast".';
       irc=311
       return
    else if (file%lcrequest) then
       write(*,*)myname,'Request to scan "fast".';
       call initScan(file,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from initScan.',irc
          return
       end if
    else
       write(*,*)myname,'No scan request.';
    end if
    return
  end subroutine initFileKeys
  !
  subroutine initScan(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(parameter), pointer :: cpar=>null()
    type(report), pointer :: crep=>null()
    integer :: nrep
    real,allocatable            :: lat(:)
    real,allocatable            :: lon(:)
    integer,allocatable         :: ind(:)
    type(reportPtr),allocatable :: rep(:)
    CHARACTER*12 MYNAME
    DATA MYNAME /'initScan'/
    ! Make indexes..
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       if (allocated(lat)) deallocate(lat)
       if (allocated(lon)) deallocate(lon)
       if (allocated(ind)) deallocate(ind)
       if (allocated(rep)) deallocate(rep)
       allocate(lat(cpar%nrep),lon(cpar%nrep),ind(cpar%nrep),rep(cpar%nrep),stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate lat/lon/rep.',irc
          return
       end if
       ! loop over reports
       nrep=0
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          nrep=nrep+1
          lat(nrep)=crep%clat
          lon(nrep)=crep%clon
          ind(nrep)=nrep
          rep(nrep)%ptr=>crep
          crep=>crep%next
       end do
       ! Build a 2D search tree from the data
       call tree_build(cpar%tree, ind, lon, lat, rep, irc)    
       if (irc.ne.0) then
          write(*,*)myname,'Error return from tree_build.',irc
          return
       end if
       deallocate(lat)
       deallocate(lon)
       deallocate(ind)
       deallocate(rep)
       cpar=>cpar%next
    end do
    return
  end subroutine initScan
  !
  subroutine initKeys(file,crep,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    integer :: irc
    real :: ss,cc
    CHARACTER*12 MYNAME
    DATA MYNAME /'initkeys'/
    !write(*,*)myname,'Entering.'
    if (crep%ltrg) then
       if (crep%mrest.eq.0) crep%mrest=file%mrest
       if (allocated(crep%nkey1)) deallocate(crep%nkey1)
       if (allocated(crep%nkey0)) deallocate(crep%nkey0)
       if (allocated(crep%key1)) deallocate(crep%key1)
       if (allocated(crep%keyind)) deallocate(crep%keyind)
       allocate(crep%nkey1(file%mtime),&
            & crep%nkey0(file%mtime),&
            & crep%key1(crep%mrest,file%mtime),&
            & crep%keyind(crep%mrest,file%mtime),&
            & stat=irc)
       if (irc.ne.0) then
          write(*,'(X,A,3(A,I0))') myname,&
               & 'Unable to allocate dynamic key variables. N=', &
               & crep%mrest," T=",file%mtime," irc=",irc
          return
       end if
       do tt=1,file%mtime
          crep%nkey1(tt)=0
          crep%nkey0(tt)=0
       end do
    else
       if (allocated(crep%nkey0)) deallocate(crep%nkey0)
       allocate(crep%nkey0(file%mtime),&
            & stat=irc)
       if (irc.ne.0) then
          write(*,'(X,A,3(A,I0))') myname,&
               & 'Unable to allocate some dynamic key variables. N=', &
               & crep%mrest," T=",file%mtime," irc=",irc
          return
       end if
    end if
    ! get center...
    !write(*,*)myname,'Get center...'
    if (crep%lcen) then
       crep%sx=crep%sx/max(crep%sn,1.0D0)
       crep%sy=crep%sy/max(crep%sn,1.0D0)
       crep%sz=crep%sz/max(crep%sn,1.0D0)
       ss=dsqrt(crep%sx*crep%sx+crep%sy*crep%sy+crep%sz*crep%sz)
       if (ss.lt.1.0D-5) then
          write(*,*)myname,'Strange position:',crep%sn,crep%sx,crep%sy,crep%sz
          irc=347
          return
       end if
       crep%sx=crep%sx/ss
       crep%sy=crep%sy/ss
       crep%sz=crep%sz/ss
       cc=dsqrt(crep%sx*crep%sx+crep%sy*crep%sy)
       crep%clat=acosdeg(cc)
       crep%clon=atan2deg(crep%sy,crep%sx)
       ! calculate max distance from center...
       crep%lcok=.true.
       crep%cdist=crep%sd + &
            & centerDistance(crep,crep%roperation,crep%lcok,irc)
       !write(*,*) myname,'Center:',crep%clat,crep%clon,crep%cdist,crep%lcok,crep%sd
       if (.not.crep%lcok) then
          file%lcok=.false.
       end if
    end if
    !write(*,*)myname,'Done...'
    return
  end subroutine initKeys
  !
  recursive function centerDistance(crep,op,bok,irc) result (dist)
    implicit none
    real dist
    type(report), pointer :: crep
    type(operation), pointer :: op
    logical :: bok
    integer :: irc
    integer :: ii
    real :: ldist
    type(operation), pointer :: cop
    type(filter), pointer :: f
    real, parameter :: fact=360.0D0/40075.0D0 ! km -> deg
    CHARACTER*18 MYNAME
    DATA MYNAME /'centerDistance'/
    !write(*,*)myname,'Entering...',op%type
    dist=0.0D0
    select case (op%type)
    case (opr_inter) ! intersection or union
       cop => op%first%next
       do while (.not.associated(cop,target=op%last))
          ldist=centerDistance(crep,cop,bok,irc)
          dist=max(dist,ldist)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from centerDistance.',irc
             return
          end if
          cop=>cop%next
       end do
    case (opr_union) ! intersection or union
       cop => op%first%next
       do while (.not.associated(cop,target=op%last))
          ldist=centerDistance(crep,cop,bok,irc)
          dist=max(dist,ldist)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from centerDistance.',irc
             return
          end if
          cop=>cop%next
       end do
    case (opr_filter) ! filter
       f => op%f
       if (.not.associated(f)) then
          bok=.false.
          write(*,*) 'Missing filter:',trim(op%name250)
          irc=942
          return
       end if
       select case(f%type)
       case (flt_cylinder) ! cylinder
          if (f%inside) then
             dist=max(dist,getdist(f%rpar(1),f%rpar(2),crep%clat,crep%clon))
          else
             bok=.false.
          end if
       case (flt_polygon)  ! polygon
          if (f%inside) then
             do ii=1,f%mpar
                dist=max(dist,getdist(f%rpar(ii),f%rpar(f%mpar+ii),crep%clat,crep%clon))
                !write(*,*) myname,'Dist:',ii,dist,f%rpar(ii),f%rpar(f%mpar+ii),crep%clat,crep%clon
             end do
             dist = dist + fact*f%delta 
          else
             bok=.false.
          end if
       case (flt_polyline)  ! polyline
          if (f%inside) then
             do ii=1,f%mpar
                dist=max(dist,getdist(f%rpar(ii),f%rpar(f%mpar+ii),crep%clat,crep%clon))
                !write(*,*) myname,'Dist:',ii,dist,f%rpar(ii),f%rpar(f%mpar+ii),crep%clat,crep%clon
             end do
             dist = dist + fact*f%delta 
          else
             bok=.false.
          end if
       case(flt_slice) ! slice
          bok=.false.
       case (flt_duct) ! duct
          bok=.false. ! not implemented
       case (flt_value) ! parameter
          bok=.false. ! not available
       case (flt_string) ! string
          bok=.false. ! not available
       case (flt_dimension) ! dimension
          bok=.false. ! not implemented
       case DEFAULT
          bok=.false. ! not implemented
          write(*,*) 'Invalid filter type:',f%type,trim(op%name250)
          irc=941
          return
       end select
    case DEFAULT
       bok=.false.
       write(*,*) 'Unknown type:',op%type
       irc=931
       return
    end select
    !write(*,*)myname,'Done...',op%type
    return
  end function centerDistance
  !
  subroutine printReport(crep)
    implicit none
    type(report), pointer :: crep
    integer :: ii, lenk
    character*12 :: myname
    data myname /'printReport'/
    if (associated(crep)) then
       write(*,'(X,A,I0,A,I0)')"   Report  nkey=",crep%nkey," ntrg=",crep%ntrg
       do ii=1,crep%nkey
          write(*,'(X,A,I0,A)') "      key:",ii,&
               & ' "'//trim(crep%key700(1,ii))//'" -> "'//&
               & trim(crep%key700(2,ii))//'"'
       end do
       do ii=1,crep%ntrg
          write(*,'(X,A,I0,A,I0)') "      trg:",ii,&
               & ' "'//trim(crep%trg10(ii))//'" -> ',&
               & crep%trgtyp(ii)
       end do
    else
       write(*,*)myname,'Missing report...'
    end if
    return
  end subroutine printReport
  !
  subroutine printParameter(cpar)
    implicit none
    type(parameter), pointer :: cpar
    type(report), pointer    :: crep=>null()
    integer :: lenp
    character*16 :: myname
    data myname /'printParameter'/
    lenp=length(cpar%par350,350,10)
    write(*,'(X,A,A)')myname,'Parameter "'//cpar%par350(1:lenp)//'"',&
         & cpar%hasvar
    crep=>cpar%firstreport%next
    do while (.not.associated(crep,target=cpar%lastreport))
       call printReport(crep)
       crep => crep%next
    end do
    return
  end subroutine printParameter
  !
  subroutine printFile(file)
    implicit none
    type(filetype) :: file
    type(parameter), pointer :: cpar=>null()
    character*12 myname
    data myname /'printFile'/
    cpar => file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       call printParameter(cpar)
       cpar => cpar%next
    end do
    return
  end subroutine printFile
  !
  character*100 function strFilter(f)
    implicit none
    type(filter), pointer :: f
    character*100 :: buff100
    integer :: lenb,lenn
    if (associated(f)) then
       write(strFilter,'(A,I0,A)') "("//&
            & trim(f%name250)//"|"//&
            & trim(f%file250)//"|",&
            & f%mpar,")"       
    else
       strFilter="(NA)"
    end if
    return
  end function strFilter
  !
  recursive subroutine printMask(cop,lev)
    implicit none
    type(operation), pointer :: cop
    integer :: lev
    type(operation), pointer :: pop
    character*20 :: blank20="                    "
    integer :: ilev
    character*12 myname
    data myname /'printMask'/
    ilev=lev+1
    write(*,'(A,I0,A)') blank20(1:min(lev,20))//">"//trim(cop%name250)//" :",&
         & cop%type,":"//trim(strFilter(cop%f))
    pop => cop%first%next
    do while (.not.associated(pop,target=cop%last))
       call printMask(pop,ilev)
       pop => pop%next
    end do
    return
  end subroutine printMask
  !
  subroutine addParameter(file,par,irc)
    implicit none
    type(filetype) :: file
    type(parameter), pointer :: par
    integer :: irc
    par%prev => file%lastparameter%prev
    par%next => file%lastparameter
    file%lastparameter%prev%next => par
    file%lastparameter%prev => par
    file%npar=file%npar+1
    return
  end subroutine addParameter
  !
  subroutine addReport(par,rep,irc)
    implicit none
    type(parameter), pointer :: par
    type(report), pointer :: rep
    integer :: irc
    !if (rep%ntim.ne.0.or.rep%nxml.ne.0.or. rep%laux .or.rep%nkey.ne.0) then
       rep%prev => par%lastreport%prev
       rep%next => par%lastreport
       par%lastreport%prev%next => rep
       par%lastreport%prev => rep
       par%nrep=par%nrep+1
    !else
    !   write(*,*)myname,'Ignoring empty report...'
    !   call clearReport(rep,irc)
    !   if (irc.ne.0) then
    !      write(*,*) myname,'Error return from clearReport.',irc
    !      return
    !   end if
    !end if
    return
  end subroutine addReport
  !
  subroutine clearFile(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(parameter), pointer :: cpar=>null(),npar=>null()
    CHARACTER*14 MYNAME
    DATA MYNAME /'clearFile'/
    if (allocated(file%par)) deallocate(file%par)
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       npar=>cpar%next
       call clearParameter(cpar,irc)
       cpar=>npar
    end do
    call clearParameter(file%firstparameter,irc)
    call clearParameter(file%lastparameter,irc)
    file%npar=0
    if (allocated(file%elem80)) deallocate(file%elem80)
    if (allocated(file%lenv)) deallocate(file%lenv)
    if (allocated(file%val)) deallocate(file%val)
    if (allocated(file%var)) deallocate(file%var)
    return
  end subroutine clearFile
  !
  subroutine clearParameter(cpar,irc)
    implicit none
    type(parameter), pointer :: cpar
    integer :: irc
    type(report), pointer :: crep=>null()
    CHARACTER*14 MYNAME
    DATA MYNAME /'clearParameter'/
    if (.not.associated(cpar)) return
    if (associated(cpar%firstreport).and.associated(cpar%lastreport)) then
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          call clearReport(crep,irc)
          crep=>crep%next
       end do
       call clearReport(cpar%firstreport,irc)
       call clearReport(cpar%lastreport,irc)
    end if
    cpar%nrep=0
    call clearExpression(cpar,irc)
    call tree_destroy(cpar%tree)
    deallocate(cpar)
    nullify(cpar)
    return
  end subroutine clearParameter
  !
  subroutine clearExpression(cpar,irc)
    !use parse
    !use shape
    implicit none
    type(parameter), pointer :: cpar
    integer :: irc
    character*250 :: crc250
    integer :: lenc
    CHARACTER*16 :: MYNAME="clearExpression"
    call parse_close(cpar%exp,crc250,irc)
    if (irc.ne.0) then
       lenc=length(crc250,250,10)
       write(*,*) crc250(1:lenc)
       write(*,*)myname,'Error return from parse_close:',irc
       RETURN
    end if
    return
  end subroutine clearExpression
  !
  subroutine clearReport(crep,irc)
    implicit none
    type(report), pointer :: crep
    integer :: irc
    type(filter), pointer :: cflt=>null(),nflt=>null()
    integer :: ii
    character*250 :: crc250
    integer :: lenc
    CHARACTER*14 MYNAME
    DATA MYNAME /'clearReport'/
    if (allocated(crep%nkey1))  deallocate(crep%nkey1)
    if (allocated(crep%nkey0))  deallocate(crep%nkey0)
    if (allocated(crep%key1))   deallocate(crep%key1)
    if (allocated(crep%keyind)) deallocate(crep%keyind)
    if (allocated(crep%groups)) deallocate(crep%groups)
    do ii=1,crep%ntrg
       if (crep%trgtyp(ii).eq.trg_aux) then
          call parse_close(crep%trgexp(ii)%ptr,crc250,irc)
          if (irc.ne.0) then
             lenc=length(crc250,250,10)
             write(*,*) crc250(1:lenc)
             write(*,*)myname,'Error return from parse_close:',irc
             RETURN
          end if
       end if
    end do
    call clearOperation(crep%roperation,irc)
    if (associated(crep%firstfilter).and.associated(crep%lastfilter)) then
       cflt=>crep%firstfilter%next
       do while (.not.associated(cflt,target=crep%lastfilter))
          nflt=>cflt%next
          call clearFilter(cflt,irc)
          cflt=>nflt
       end do
       deallocate(crep%firstfilter,crep%lastfilter,stat=irc)
    end if
    return
  end subroutine clearReport
  !
  subroutine clearOperation(copr,irc)
    implicit none
    type(operation), pointer :: copr
    integer :: irc
    CHARACTER*14 MYNAME
    DATA MYNAME /'clearOperation'/
    ! 
    return
  end subroutine clearOperation
  !
  subroutine clearFilter(cflt,irc)
    implicit none
    type(filter), pointer :: cflt
    integer :: irc
    character*250 :: crc250
    integer :: lenc
    CHARACTER*14 MYNAME
    DATA MYNAME /'clearFilter'/
    if (allocated(cflt%lpar)) deallocate(cflt%lpar)
    if (allocated(cflt%npar)) deallocate(cflt%npar)
    if (allocated(cflt%rpar)) deallocate(cflt%rpar)
    call parse_close(cflt%exp,crc250,irc)
    if (irc.ne.0) then
       lenc=length(crc250,250,10)
       write(*,*) crc250(1:lenc)
       write(*,*)myname,'Error return from parse_close:',irc
    end if
    if (associated(cflt)) deallocate(cflt)
    nullify(cflt)
    return
  end subroutine clearFilter
  !
  function newFilter(irc) result(ret)
    integer :: irc
    type(filter), pointer :: ret
    character*250 :: crc250
    integer :: lenc
    CHARACTER*12 MYNAME
    DATA MYNAME /'newfilter'/
    allocate(ret,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate filter.',irc
       return
    end if
    call parse_open(ret%exp,crc250,irc)
    if (irc.ne.0) then
       lenc=length(crc250,250,10)
       write(*,*) crc250(1:lenc)
       write(*,*)myname,'Unable to open expression.',irc
       return
    end if
    return
  end function newFilter
  !
  function copyFilter(flt,irc) result(ret)
    implicit none
    type(filter), pointer :: ret
    type(filter), pointer :: flt
    integer :: irc
    integer :: ii
    character*250 :: crc250
    integer :: lenc
    CHARACTER*12 MYNAME
    DATA MYNAME /'copyFilter'/
    ! 
    allocate(ret,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate filter.',irc
       return
    end if
    ret%type=flt%type
    ret%name250=flt%name250
    ret%file250=flt%file250
    ret%eps=flt%eps
    ret%delta=flt%delta
    ret%inside=flt%inside
    ret%simplify=flt%simplify
    ret%ledge=flt%ledge
    ret%mpar=flt%mpar
    if (allocated(flt%lpar)) then
       allocate(ret%lpar(0:size(flt%lpar)-1),stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate filter.',irc
          return
       end if
       do ii=0,size(flt%lpar)-1
          ret%lpar(ii)=flt%lpar(ii)
       end do
    end if
    if (allocated(flt%npar)) then
       allocate(ret%npar(size(flt%npar)),stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate filter.',irc
          return
       end if
       do ii=1,size(flt%npar)
          ret%npar(ii)=flt%npar(ii)
       end do
    end if
    if (allocated(flt%rpar)) then
       allocate(ret%rpar(size(flt%rpar)),stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate filter.',irc
          return
       end if
       do ii=1,size(flt%rpar)
          ret%rpar(ii)=flt%rpar(ii)
       end do
    end if
    ret%spar250=flt%spar250
    ret%lens=flt%lens
    ret%sdim80=flt%sdim80
    ret%ipar=flt%ipar
    ret%active=flt%active
    ret%var250=flt%var250
    ret%minval=flt%minval
    ret%maxval=flt%maxval
    call parse_open(ret%exp,crc250,irc)
    if (irc.ne.0) then
       lenc=length(crc250,250,10)
       write(*,*) crc250(1:lenc)
       write(*,*)myname,'Unable to open expression.',irc
       return
    end if
    return
  end function copyFilter
  !
  subroutine readXmlFile(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(xmltype)  :: xml
    type(parameter), pointer :: cpar=>null()
    type(parameter), pointer :: bpar=>null()
    integer leni,lenf
    integer :: ii, jj
    logical :: bok
    CHARACTER*12 MYNAME
    DATA MYNAME /'readXmlFile'/
    !
    call replaceENV(file%inp350,350,bok,irc)
    if (.not.bok) irc=111
    if (irc.ne.0) then
       write(*,*)myname,'Error return from replaceEnv.',irc
       return
    end if
    leni=length(file%inp350,350,20)
    call xml_open( xml%info, file%inp350(1:leni), .true. ) ! "" == stdin
    call xml_options( xml%info, report_lun = 20, report_details = .true. )
    XMLL: do
       call xml_get( xml%info, xml%tag, xml%starttag, xml%endtag, &
            & xml%attribs, xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(xml%info) ) exit
       if ( xml_error(xml%info) ) then
          write(*,*)  'Error input file.'
          write(20,*) 'Error input file.'
       endif
       write(20,* ) xml%tag, xml%endtag
       do ii = 1,xml%no_attribs
          write(20,'(i3,1x,3a)') ii, trim(xml%attribs(1,ii)), '=', trim(xml%attribs(2,ii))
       end do
       write(20,'(i3,1x,3a)') (ii, '>',trim(xml%data(ii)), '<', ii=1,xml%no_data)
       ! process XMLL
       if (bdeb) write(*,*)myname,'Found TOP tag:',trim(xml%tag),xml%starttag,xml%endtag
       if (trim(xml%tag).eq."program") then
          call attributeXMLProgram(file,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from processXMLProgram.',irc
             return
          end if
       else if (trim(xml%tag).eq."file") then
          call attributeXMLFile(file,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from processXMLfile.',irc
             return
          end if
          ! read until xml%endtag is reached
          INPUT: do while(.not. (trim(xml%tag).eq."input".and.xml%endtag))
             call xml_get( xml%info, xml%tag, xml%starttag, xml%endtag, &
                  & xml%attribs, xml%no_attribs, xml%data, xml%no_data )
             if ( .not. xml_ok(xml%info) ) exit INPUT
             if (bdeb) write(*,*)myname,'Found INPUT tag:',trim(xml%tag),&
                  & xml%starttag,xml%endtag
             if (trim(xml%tag).eq."parameter") then
                call processXmlParameter(file,xml,irc)
                if (irc.ne.0) then
                   write(*,*)myname,'Error return from processXMLfile.',irc
                   return
                end if
             else if (trim(xml%tag).ne."file".and.trim(xml%tag).ne."--") then
                write(*,*)myname,'Unknown xml%tag A:',trim(xml%tag)
                irc=930
                return
             end if
          end do INPUT
       else if (trim(xml%tag).ne."--") then
          write(*,*)myname,'Unknown xml%tag F:',trim(xml%tag)
          irc=938
          return
       end if
    enddo XMLL
    call xml_close( xml%info )
    !
    call chop0(file%nco350,350)
    call chop0(file%iter80,80)
    file%leni=length(file%iter80,80,0)
    !
    return
  end subroutine readXmlFile

  subroutine attributeXMLProgram(file,xml,irc)
    implicit none
    type(filetype) :: file
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*20 MYNAME
    DATA MYNAME /'attributeXMLProgram'/
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=112
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."name") then
       else if (trim(xml%attribs(1,ii)).eq."scan") then
          file%lcrequest=(buff700(1:len2).eq."fast")
       else 
          write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
               & trim(xml%attribs(1,ii))
          irc=798
          return
       end if
    end do
  end subroutine attributeXMLProgram

  subroutine attributeXMLFile(file,xml,irc)
    implicit none
    type(filetype) :: file
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeXMLFile'/
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=113
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."input") then
          if (bdeb) write(*,*)myname,'Input file:',buff700(1:len2)
          file%ref350=buff700
          call chop0(file%ref350,350)
       elseif (trim(xml%attribs(1,ii)).eq."mask") then
          if (bdeb) write(*,*)myname,'Output netcdf file:',buff700(1:len2)
          file%nco350=buff700
          !lenn=len_trim(file%nco350)
          ! if (.not.crep%lpoly .and. file%nco350(lenn-2:lenn).eq.".nc") then
          !    crep%lpoly=.true.
          !    crep%poly350=trim(file%nco350(1:lenn-3)//".json")
          !    write(*,*)myname,"Polygon from netcdf:",trim(crep%poly350)
          ! end if
       elseif (trim(xml%attribs(1,ii)).eq."iter") then
          file%iter80=buff700
       elseif (trim(xml%attribs(1,ii)).eq."skip") then
          if (buff700(1:len2).eq."true") then
             file%skip=.true.
          else
             file%skip=.false.
          end if
       elseif (trim(xml%attribs(1,ii)).eq."ignore") then
          if (buff700(1:len2).eq."altitude") file%ignorealt=.true.
       elseif (trim(xml%attribs(1,ii)).eq."keep") then
          if (bdeb) write(*,*)myname,'Input file:',buff700(1:len2)
          read(buff700(1:len2),*,iostat=irc) file%keep
          if (irc.ne.0) then
             write(*,*)myname,'Unable to read keep "'//&
                  &buff700(1:len2)//'"'
             return
          end if
       elseif (trim(xml%attribs(1,ii)).eq."cutoff") then
          if (bdeb) write(*,*)myname,'Input file cutoff:',buff700(1:len2)
          read(buff700(1:len2),*,iostat=irc) file%cutoff
          if (irc.ne.0) then
             write(*,*)myname,'Unable to read cutoff "'//&
                  &buff700(1:len2)//'"'
             return
          end if
       elseif (trim(xml%attribs(1,ii)).eq."maxdays") then
          if (bdeb) write(*,*)myname,'Input file maxdays:',buff700(1:len2)
          read(buff700(1:len2),*,iostat=irc) file%maxdays
          if (irc.ne.0) then
             write(*,*)myname,'Unable to read maxdays "'//&
                  &buff700(1:len2)//'"'
             return
          end if
       else 
          write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
               & trim(xml%attribs(1,ii))
          irc=798
          return
       end if
    end do
  end subroutine attributeXMLFile

  subroutine attributeXmlParameter(file,cpar,xml,irc)
    implicit none
    type(filetype) :: file
    type(parameter), pointer :: cpar
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeXMLFile'/
    cpar%scl=1.0D0
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=114
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."field") then
          cpar%par350=buff700
          cpar%hasvar=.true.
       elseif (trim(xml%attribs(1,ii)).eq."name") then
          cpar%elem80=buff700(1:len2)
       elseif (trim(xml%attribs(1,ii)).eq."exp") then
          cpar%exp700=buff700
          call chop0(cpar%exp700,700)
          cpar%lene=length(cpar%exp700,700,10)
       elseif (trim(xml%attribs(1,ii)).eq."fail") then
          if (buff700(1:len2).eq."nodata") then
             cpar%find= .true.
          else if (buff700(1:len2).eq."anydata") then
             cpar%fiad= .true.
          else if (buff700(1:len2).eq."none") then
             cpar%fino= .true.
          else
             write(*,*)myname,'Unknown setting:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
          end if
       else if (trim(xml%attribs(1,ii)).eq."scale") then
          read(buff700(1:len2),*,iostat=irc) &
               & cpar%scl
          if (irc.ne.0) then
             write(*,*)myname,'Error reading attribute:',&
                  & buff700
             return
          end if
       else 
          write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
               & trim(xml%attribs(1,ii))
          irc=799
          return
       end if
    end do
    call chop0(cpar%par350,350)
  end subroutine attributeXmlParameter
  !
  subroutine processXmlParameter(file,xml,irc)
    implicit none
    type(filetype) :: file
    type(parameter), pointer :: cpar=>null()
    type(report), pointer :: crep=>null()
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2,cnt
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeXMLFile'/
    fail(fail_nodata)=.false.
    fail(fail_anydata)=.false.
    fail(fail_None)=.false.
    fail(fail_Size)=.false.
    if (xml%starttag) then
       if (associated(cpar)) then
          call clearParameter(cpar,irc)
          irc=0
          if (associated(cpar)) deallocate(cpar)
       end if
       allocate(cpar,stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate PARAMETER.',irc
          return
       end if
       call initParameter(cpar,irc)
       call attributeXmlParameter(file,cpar,xml,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from attributeXmlParameter.',irc
          return
       end if
    end if
    cnt=0
    PARR: do while(.not. (trim(xml%tag).eq."parameter".and.xml%endtag))
       call xml_get( xml%info, xml%tag, xml%starttag, xml%endtag, &
            & xml%attribs, xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(xml%info) ) exit PARR
       cnt=cnt+1
       if (bdeb.and.cnt.lt.3) write(*,*)myname,'Found PAR tag:',&
            & trim(xml%tag),xml%starttag,xml%endtag
       if (bdeb.and.cnt.eq.3) write(*,*)myname,'Found PAR tag...'
       if (trim(xml%tag).eq."report") then
          ! process the report...
          crep => processXmlReport(file,cpar,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from processXmlReport.',irc
             return
          end if
          !write(*,*)myname,'Adding report:',associated(cpar),associated(crep)
          call addReport(cpar,crep,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from addReport.',irc
             return
          end if
       end if
    end do PARR
    if (cpar%nrep .ne. 0 .and. .not.cpar%hasVar) then
       write(*,*)myname,"Parameter without 'field' can not make reports."
       irc=945
       return
    end if
    call addParameter(file,cpar,irc)
    if (irc.ne.0) then
       write(*,*)myname,'Error return from addParameter.',irc
       return
    end if
    nullify(cpar)
  end subroutine processXmlParameter
  !
  subroutine attributeXmlReport(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeXMLReport'/
    crep%skip=file%skip
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=115
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."size") then
          read(buff700(1:len2),*,iostat=irc) crep%mrest
          if (irc.ne.0) then
             write(*,*)myname,'Unable to read size "'//&
                  &buff700(1:len2)//'"'
             return
          end if
       elseif (trim(xml%attribs(1,ii)).eq."skip") then
          if (buff700(1:len2).eq."true") then
             crep%skip= .true.
          else
             crep%skip= .false.
          end if
       elseif (trim(xml%attribs(1,ii)).eq."fail") then
          if (buff700(1:len2).eq."write") then
             crep%lwrite= .true.
          else if (buff700(1:len2).eq."ignore") then
             crep%lwrite= .false.
          else if (buff700(1:len2).eq."none") then
             crep%lwrite= .false.
          else
             write(*,*)myname,'Unknown setting:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
          end if
       end if
    end do
  end subroutine attributeXmlReport
  !
  function processXmlReport(file,cpar,xml,irc)
    implicit none
    type(report), pointer :: processXmlReport
    type(filetype) :: file
    type(parameter), pointer :: cpar
    type(xmltype)  :: xml
    integer :: irc
    type(report), pointer :: crep=>null()
    integer :: lend, lenb, ii,len2
    CHARACTER*18 MYNAME
    DATA MYNAME /'processXmlReport'/
    ! prepare a new report...
    allocate(crep,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Error allocating report.',irc
       return
    end if
    call initReport(crep,irc)
    if (irc.ne.0) then
       write(*,*)myname,'Error return from initReport.',irc
       return
    end if
    call attributeXmlReport(file,crep,xml,irc)
    REP: do while(.not. (trim(xml%tag).eq."report".and.xml%endtag))
       call xml_get( xml%info, xml%tag, xml%starttag, xml%endtag, &
            & xml%attribs, xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(xml%info) ) exit REP

       if (trim(xml%tag).eq."xml") then
          call attributeXml(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from attributeXml.',irc
             return
          end if
       else if (trim(xml%tag).eq."aux") then
          call attributeAux(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from attributeAux.',irc
             return
          end if
       else if (trim(xml%tag).eq."key") then
          call attributeKey(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from attributeKey.',irc
             return
          end if
       else if (trim(xml%tag).eq."opt") then
          call optionalKey(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from optionalKey.',irc
             return
          end if
       else if (trim(xml%tag).eq."target".or.trim(xml%tag).eq."required") then
          call attributeTarget(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from attributeTarget.',irc
             return
          end if
       else if (trim(xml%tag).eq."average") then
          call attributeAverage(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from attributeAverage.',irc
             return
          end if
       else if (trim(xml%tag).eq."output") then
          call attributeOutput(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from attributeOutput.',irc
             return
          end if
       else if (trim(xml%tag).eq."extreme") then
          call attributeExtreme(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from attributeExtreme.',irc
             return
          end if
       else if (trim(xml%tag).eq."filter") then
          call processDefine(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from processDefine.',irc
             return
          end if
       else if (trim(xml%tag).eq."mask") then
          call processXmlMasks(file,crep,xml,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from processXmlMasks.',irc
             return
          end if
       else if (trim(xml%tag).ne."report".and.trim(xml%tag).ne."--") then
          write(*,*)myname,'Unknown xml%tag B:',trim(xml%tag)
          irc=931
          return
       else if (trim(xml%tag).eq."report".and.xml%endtag) then ! last REP
          ! XML must contain: start, stop, issued and expires...
          call addXML(crep,"start","@start@")
          call addXML(crep,"stop","@stop@")
          call addXML(crep,"issued","@issued@")
          call addXML(crep,"expires","@expires@")
          ! complete report...
       end if
    end do REP
    processXmlReport => crep
  end function processXmlReport
  !
  subroutine attributeXml(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'processXMLMasks'/
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=116
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       call addXML(crep,trim(xml%attribs(1,ii)),&
            & buff700(1:len2));
    end do
    return
  end subroutine attributeXml
  !
  subroutine attributeAux(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    character*250 :: crc250
    integer :: lenc
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeAux'/
    crep%ltrg=.true.
    crep%ntrg=min(mtrg,crep%ntrg+1)
    crep%trgreq(crep%ntrg)=.false.
    crep%trg10(crep%ntrg)=""
    crep%trgtyp(crep%ntrg)=trg_aux
    nullify(crep%trgexp(crep%ntrg)%ptr)
    call parse_open(crep%trgexp(crep%ntrg)%ptr,crc250,irc)
    if (irc.ne.0) then
       lenc=length(crc250,250,10)
       write(*,*) crc250(1:lenc)
       write(*,*)myname,'Unable to open expression.',irc
       return
    end if
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=117
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."name") then
          crep%trg10(crep%ntrg)=buff700(1:len2)
       elseif (trim(xml%attribs(1,ii)).eq."exp") then
          crep%laux=.true.
          crep%trgvar700(crep%ntrg)=buff700(1:len2)
          crep%trglenv(crep%ntrg)=len2
          ! add xml-parameter
          ! call addAux(crep,trim(xml%attribs(1,ii)),buff700(1:len2));
       elseif (trim(xml%attribs(1,ii)).eq."location") then
          if (buff700(1:len2).eq."max")then
             crep%trgext(crep%ntrg)=1
             crep%wmax=.true.
          elseif (buff700(1:len2).eq."min")then
             crep%trgext(crep%ntrg)=-1
             crep%wmin=.true.
          else
             crep%trgext(crep%ntrg)=0
          end if
       end if
    end do
    return
  end subroutine attributeAux
  !
  subroutine attributeKey(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'processKey'/
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=118
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       call addKey(crep,trim(xml%attribs(1,ii)),&
            & buff700(1:len2));
    end do
  end subroutine attributeKey
  !
  subroutine optionalKey(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len1,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*12 MYNAME
    DATA MYNAME /'optionalKey'/
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       ! ignore key if value is empty (bok=.true.)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (len2.ne.0) then
          call addKey(crep,trim(xml%attribs(1,ii)),&
               & buff700(1:len2));
       else
          write(*,*)myname,'*** Ignoring key: "'//trim(xml%attribs(1,ii))//'"'
       end if
    end do
  end subroutine optionalKey
  !
  subroutine attributeTarget(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2,lenn
    character*250 :: dat250,buff250
    character*250, external :: nukehead
    integer, parameter :: maxval=100
    real :: bval(maxval)
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeTarget'/
    crep%ltrg=.true.
    crep%ntrg=min(mtrg,crep%ntrg+1)
    crep%trgreq(crep%ntrg)=(trim(xml%tag).eq."required")
    crep%trg10(crep%ntrg)=""
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=119
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."name") then
          crep%trg10(crep%ntrg)=buff700(1:len2)
       else if (trim(xml%attribs(1,ii)).eq."fraction") then
          crep%trgtyp(crep%ntrg)=trg_fraction
          read(buff700(1:len2),*,iostat=irc) &
               & crep%trgval(crep%ntrg)
          if (irc.ne.0) then
             write(*,*)myname,'Error reading attribute:',&
                  & buff700
             return
          end if
          if (crep%trgval(crep%ntrg).gt.0.5) then
             crep%wmax=.true.
          else
             crep%wmin=.true.
          end if
       else if (trim(xml%attribs(1,ii)).eq."area") then
          crep%wmax=.true.
          crep%trgtyp(crep%ntrg)=trg_area
          read(buff700(1:len2),*,iostat=irc) &
               & crep%trgval(crep%ntrg)
          if (irc.ne.0) then
             write(*,*)myname,'Error reading attribute:',&
                  & buff700
             return
          end if
          crep%larea=.true.
       else if (trim(xml%attribs(1,ii)).eq."count") then
          crep%wmax=.true.
          crep%trgtyp(crep%ntrg)=trg_count
          read(buff700(1:len2),*,iostat=irc) &
               & crep%trgval(crep%ntrg)
          if (irc.ne.0) then
             write(*,*)myname,'Error reading attribute:',&
                  & buff700
             return
          end if
       else if (trim(xml%attribs(1,ii)).eq."average") then
          crep%wmax=.true.
          crep%wmin=.true.
          crep%trgtyp(crep%ntrg)=trg_average
          read(buff700(1:len2),*,iostat=irc) &
               & crep%trgval(crep%ntrg)
          if (irc.ne.0) then
             write(*,*)myname,'Error reading attribute:',&
                  & buff700
             return
          end if
       else if (trim(xml%attribs(1,ii)).eq."group") then
          crep%grouptrg=crep%ntrg
          dat250=buff700(1:len2)
          CALL NUKECP(dat250,',',' ',250)
          CALL NUKECP(dat250,':',' ',250)
          CALL NUKECP(dat250,'|',' ',250)
          CALL NUKECP(dat250,'/',' ',250)
          call chop0(dat250,250)
          lend=length(dat250,250,0)
          crep%ngroups=0
          do while (lend.ne.0.and.crep%ngroups.ne.maxval)
             buff250=nukehead(dat250,250)
             lend=length(dat250,250,0)
             lenb=length(buff250,250,5)
             crep%ngroups=min(maxval,crep%ngroups+1)
             read(buff250(1:lenb),*,iostat=irc) bval(crep%ngroups)
          end do
          if (allocated(crep%groups)) deallocate(crep%groups)
          allocate(crep%groups(crep%ngroups),stat=irc)
          if (irc.ne.0) then
             write(*,*)myname,'Unable to allocate groups:',&
                  & crep%ngroups,irc
             return
          end if
          do jj=1,crep%ngroups
             crep%groups(jj)=bval(jj)
          end do
       else if (trim(xml%attribs(1,ii)).eq."extreme") then
          if (bdeb) then
             lenn=length(crep%trg10(crep%ntrg),10,10)
             write(*,*)myname,'Extreme ',&
                  & crep%trg10(crep%ntrg)(1:lenn),&
                  & buff700(1:len2)
          end if
          if (buff700(1:len2).eq."none") then
             crep%trgext(crep%ntrg)=0
          else if (buff700(1:len2).eq."min") then
             crep%trgext(crep%ntrg)=-1
          else if (buff700(1:len2).eq."max") then
             crep%trgext(crep%ntrg)=1
          else if (buff700(1:len2).eq."all") then
             crep%trgext(crep%ntrg)=2
          else
             write(*,*)myname,'Ignoring extreme (min/max/all):',&
                  & buff700(1:len2)
          end if
       end if
    end do
    ! can only group on fraction/area/cnt
    if (crep%grouptrg.eq.crep%ntrg.and.&
         & crep%trgtyp(crep%ntrg).ne.trg_fraction.and. &
         & crep%trgtyp(crep%ntrg).ne.trg_area.and. &
         & crep%trgtyp(crep%ntrg).ne.trg_count) then
       write(*,*)myname,'Can only group on fraction/area/cnt.'
       irc=384
       return
    end if
  end subroutine attributeTarget
  !
  subroutine attributeAverage(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeAverage'/
    crep%lavg=.true.
    do ii=1,xml%no_attribs
       if (trim(xml%attribs(1,ii)).eq."name") then
          crep%avg10=xml%attribs(2,ii)(1:len2)
       end if
    end do
    crep%wmax=.true.
    crep%wmin=.true.
  end subroutine attributeAverage
  !
  subroutine attributeOutput(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    logical :: file_exists
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeOutput'/
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=120
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."xml") then
          if (bdeb) write(*,*)myname,'Output xml file:',&
               & buff700(1:len2)
          crep%xmlo350=buff700
       else if (trim(xml%attribs(1,ii)).eq."polygon") then
          if (bdeb) write(*,*)myname,'Output polygon file:',&
               & buff700(1:len2)
          crep%poly350=buff700
          ! check if file exists (do not overwrite)
          INQUIRE(FILE=buff700(1:len2),&
               & EXIST=file_exists)  
          if (file_exists) then
             write(*,*)myname,'Not overwriting: ',&
                  & buff700(1:len2)
             crep%lpoly=.false.
          else
             crep%lpoly=.true.
          end if
       else 
          write(*,*)myname,'Unknown attribute:',&
               & trim(xml%tag)//':'//trim(xml%attribs(1,ii))
          irc=801
          return
       end if
    end do
  end subroutine attributeOutput
  !
  subroutine attributeExtreme(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    logical :: file_exists
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'attributeExtreme'/
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=120
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."xml") then
          if (bdeb) write(*,*)myname,'Extreme xml file:',&
               & buff700(1:len2)
          crep%ext350=buff700
          crep%lext=.true.
       else 
          write(*,*)myname,'Unknown attribute:',&
               & trim(xml%tag)//':'//trim(xml%attribs(1,ii))
          irc=801
          return
       end if
    end do
  end subroutine attributeExtreme
  !
  subroutine processDefine(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    type(XML_PARSE) :: inc
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'processDefine'/
    if (xml%starttag.and.xml%endtag) then
       ! define new filter
       nfilter => newFilter(irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from newFilter.',irc
          return
       end if
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=121
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."name") then
             if (bdeb) write(*,*)myname,'Filter name:',&
                  & buff700(1:len2)
             nfilter%name250=buff700(1:len2)
          else if (trim(xml%attribs(1,ii)).eq."file") then
             inquire( file = buff700(1:len2), exist = exists )
             if (.not.exists) then
                write(*,*)myname,"Invalid filter file:",&
                     & buff700(1:len2)
                irc=346
                return
             else
                write(*,*)myname,'Filter file:',buff700(1:len2)
             end if
             nfilter%file250=buff700(1:len2)
          else if (trim(xml%attribs(1,ii)).eq."tolerance") then ! in km
             read(buff700(1:len2),*,iostat=irc) nfilter%eps
             if (irc.ne.0) then
                write(*,*)myname,'Error reading attribute:',buff700
                return
             end if
             if (nfilter%eps <=0.0D0) then
                nfilter%simplify=.false.
             end if
          else if (trim(xml%attribs(1,ii)).eq."delta") then ! in km
             read(buff700(1:len2),*,iostat=irc) nfilter%delta
             if (irc.ne.0) then
                write(*,*)myname,'Error reading attribute:',buff700
                return
             end if
             nfilter%ledge=(nfilter%delta>0.0D0)
          else
             write(*,*)myname,'Unknown attribute:',&
                  & trim(xml%tag)//':'//trim(xml%attribs(1,ii))
             irc=802
             return
          end if
       end do
       if (len_trim(nfilter%file250)>0) then
          call xml_open( inc, trim(nfilter%file250), .true. ) ! read file name
          call xml_options(inc, report_lun = 21, report_details = .false. )
          xml%endtag=.false. ! "open" the xml%tag and read the include-file
          call readFilter(crep,inc,xml,nfilter,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from readFilter.',irc
             return
          end if
          call xml_close( inc )
       end if
       nfilter%prev => crep%lastfilter%prev
       nfilter%next => crep%lastfilter
       crep%lastfilter%prev%next => nfilter
       crep%lastfilter%prev => nfilter
       nullify(nfilter)
    end if
    if (xml%starttag.and..not.xml%endtag) then
       ! define new filter
       nfilter => newFilter(irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from newFilter.',irc
          return
       end if
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=122
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."name") then
             if (bdeb) write(*,*)myname,'Filter name:',&
                  & buff700(1:len2)
             nfilter%name250=buff700(1:len2)
          else
             write(*,*)myname,'Unknown attribute:',&
                  & trim(xml%tag)//':'//trim(xml%attribs(1,ii))
             irc=802
             return
          end if
       end do
       call readFilter(crep,xml%info,xml,nfilter,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from readFilter.',irc
          return
       end if
       ! insert into chain
       nfilter%prev => crep%lastfilter%prev
       nfilter%next => crep%lastfilter
       crep%lastfilter%prev%next => nfilter
       crep%lastfilter%prev => nfilter
       nullify(nfilter)
    else if (xml%endtag) then
       call xml_get( xml%info, xml%tag, xml%starttag, &
            & xml%endtag, xml%attribs, xml%no_attribs, &
            & xml%data, xml%no_data )
    end if
  end subroutine processDefine
  !
  subroutine processXmlMasks(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    CHARACTER*18 MYNAME
    DATA MYNAME /'processXMLMasks'/
    if (xml%starttag) then
       if (associated(crep%roperation)) then
          write(*,*) myname,'Can only have one operation.'
          irc=348
          return
       end if
       allocate(crep%roperation,stat=irc) ! allocate root
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate mask.',irc
          return
       end if
       allocate(crep%roperation%first, &
            & crep%roperation%last,stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate mask-chain.',irc
          return
       end if
       crep%roperation%first%next => crep%roperation%last
       crep%roperation%last%prev => crep%roperation%first
       crep%coperation => crep%roperation
       do ii=1,xml%no_attribs
          if (trim(xml%attribs(1,ii)).eq."filter") then
          else
             write(*,*)myname,'Unknown attribute:',&
                  & trim(xml%tag)//':'//trim(xml%attribs(1,ii))
             irc=814
             return
          end if
       end do
    end if
    if (xml%starttag.and.xml%endtag) then ! this is a filter
       crep%roperation%type=opr_filter ! filter
       call processXmlFilter(file,crep,xml,irc)
    else
       crep%roperation%type=opr_union ! empty container is treated as union
       ! read until xml%endtag is reached
       OP: do while (.not. (trim(xml%tag).eq."mask".and.xml%endtag.and. &
            & associated(crep%coperation,target=crep%roperation)))
          call xml_get( xml%info, xml%tag, xml%starttag, xml%endtag, &
               & xml%attribs, xml%no_attribs, xml%data, xml%no_data )
          if ( .not. xml_ok(xml%info) ) exit OP
          if (bdeb) write(*,*)myname,'Found MASK xml%tag:',&
               & trim(xml%tag),xml%starttag,xml%endtag,crep%coperation%type
          if (trim(xml%tag).eq."intersection") then
             call processXmlIntersection(file,crep,xml,irc)
             if (irc.ne.0) then
                write(*,*)myname,'Error return from processXmlIntersection.',irc
                return
             end if
          else if (trim(xml%tag).eq."union") then
             call processXmlUnion(file,crep,xml,irc)
             if (irc.ne.0) then
                write(*,*)myname,'Error return from processXmlUnion.',irc
                return
             end if
          else if (trim(xml%tag).eq."mask") then
             call processXmlMask(file,crep,xml,irc)
             if (irc.ne.0) then
                write(*,*)myname,'Error return from processXmlMask.',irc
                return
             end if
          end if
       end do OP
    end if
  end subroutine processXmlMasks
  !
  subroutine processXmlFilter(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'processXMLFilter'/
    do ii=1,xml%no_attribs
       buff700=xml%attribs(2,ii)
       call replaceENV(buff700,700,bok,irc)
       if (.not.bok) irc=123
       if (irc.ne.0) then
          write(*,*)myname,'Error return from replaceEnv.',irc
          return
       end if
       len2=length(buff700,700,10)    
       if (trim(xml%attribs(1,ii)).eq."filter") then
          if (bdeb) write(*,*)myname,'Filter name:',buff700(1:len2)
          ! find filter
          nullify(nfilter)
          cfilter => crep%firstfilter%next
          do while (.not.associated(cfilter, &
               & target=crep%lastfilter))
             if (trim(cfilter%name250).eq.&
                  & buff700(1:len2)) then
                nfilter => cfilter
                cfilter => crep%lastfilter
             else
                cfilter=>cfilter%next
             end if
          end do
          !if (associated(nfilter)) then
          !   write(*,*) myname,'Using filter:',&
          !        & trim(nfilter%name250)
          !end if
          crep%roperation%name250=buff700(1:len2)
          crep%roperation%f => nfilter
          nullify(nfilter)
       else if (trim(xml%attribs(1,ii)).eq."override") then
       end if
    end do
  end subroutine processXmlFilter
  !
  subroutine processXmlIntersection(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    CHARACTER*22 MYNAME
    DATA MYNAME /'processXMLIntersection'/
    if (xml%starttag .and. .not.xml%endtag) then ! ignore empty intersections
       ! create new intersection-operation
       allocate(noperation,stat=irc) ! allocate new intersection
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate operation.',irc
          return
       end if
       allocate(noperation%first,noperation%last,stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate operation chain.',irc
          return
       end if
       noperation%first%next => noperation%last
       noperation%last%prev => noperation%first
       noperation%first%type=-2
       noperation%last%type=-1
       noperation%type=opr_inter ! intersection
       if (.not. associated(crep%coperation)) then
          write(*,*)myname,'Current operation is not associated...'
          irc=935
          return
       end if
       noperation%parent => crep%coperation
       noperation%prev => crep%coperation%last%prev
       noperation%next => crep%coperation%last
       crep%coperation%last%prev%next => noperation
       crep%coperation%last%prev => noperation
       crep%coperation => noperation
       nullify(noperation)
    end if
    if (xml%endtag .and. .not.xml%starttag) then ! ignore empty intersections
       ! intersection completed
       if (crep%coperation%type.ne.opr_inter) then
          write(*,*)myname,'Closing union which is of invalid type:',&
               & crep%coperation%type
          irc=998
          return
       end if
       crep%coperation => crep%coperation%parent
    end if
  end subroutine processXmlIntersection
  !
  subroutine processXmlUnion(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    CHARACTER*18 MYNAME
    DATA MYNAME /'processXMLUnion'/
    if (xml%starttag .and. .not.xml%endtag) then  ! ignore empty unions
       ! create new union-operation
       allocate(noperation,stat=irc) ! allocate new union
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate operation.',irc
          return
       end if
       allocate(noperation%first,noperation%last,&
            & noperation%prev,noperation%next,stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate operation chain.',irc
          return
       end if
       noperation%first%next => noperation%last
       noperation%last%prev => noperation%first
       noperation%type=opr_union ! union
       if (.not. associated(crep%coperation)) then
          write(*,*)myname,'Current operation is not associated...'
          irc=936
          return
       end if
       noperation%parent => crep%coperation
       noperation%prev => crep%coperation%last%prev
       noperation%next => crep%coperation%last
       crep%coperation%last%prev%next => noperation
       crep%coperation%last%prev => noperation
       crep%coperation => noperation
       nullify(noperation)
    end if
    if (.not.xml%starttag .and. xml%endtag) then  ! ignore empty unions
       ! union completed
       if (crep%coperation%type.ne.opr_union) then
          write(*,*)myname,'Closing union which is of invalid type:',&
               & crep%coperation%type
          irc=998
          return
       end if
       crep%coperation => crep%coperation%parent
    end if
  end subroutine processXmlUnion
  !
  subroutine processXmlMask(file,crep,xml,irc)
    implicit none
    type(filetype) :: file
    type(report), pointer :: crep
    type(xmltype)  :: xml
    integer :: irc
    integer :: ii,len2
    character*700 :: buff700
    logical :: bok
    CHARACTER*18 MYNAME
    DATA MYNAME /'processXMLMask'/
    if (xml%starttag) then
       ! new mask-operation
       allocate(noperation,stat=irc) ! allocate new use
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate operation.',irc
          return
       end if
       allocate(noperation%first,noperation%last,stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate operation chain.',irc
          return
       end if
       noperation%first%next => noperation%last
       noperation%last%prev => noperation%first
       noperation%type=opr_filter ! mask
       if (.not. associated(crep%coperation)) then
          write(*,*)myname,'Current operation is not associated...'
          irc=937
          return
       end if
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=124
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."filter") then
             if (bdeb) write(*,*)myname,'Filter name:',&
                  & buff700(1:len2)
             ! find filter
             nullify(nfilter)
             cfilter => crep%firstfilter%next
             do while (.not.associated(cfilter, target=crep%lastfilter))
                if (trim(cfilter%name250).eq.&
                     & buff700(1:len2)) then
                   nfilter => cfilter
                   cfilter => crep%lastfilter
                else
                   cfilter=>cfilter%next
                end if
             end do
             noperation%f => nfilter
             noperation%name250=buff700(1:len2)
             nullify(nfilter)
          else
             write(*,*)myname,'Unknown attribute:',&
                  & trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=815
             return
          end if
       end do
       noperation%parent => crep%coperation
       noperation%prev => crep%coperation%last%prev
       noperation%next => crep%coperation%last
       crep%coperation%last%prev%next => noperation
       crep%coperation%last%prev => noperation
       nullify(noperation)
    end if
  end subroutine processXmlMask
  !
  subroutine  setKeyVar(file,mkey,par350,key350)
    implicit none
    type(filetype) :: file
    integer yy,mm,dd,hh,mi
    real sec
    character*24 a24
    character*4 ayy
    character*2 amm,add,ahh
    integer :: mkey
    character*350 par350,key350(mkey)
    real xissued
    CHARACTER*12 MYNAME
    DATA MYNAME /'setKeyVar'/
    call dj2000(file%a2000,yy,mm,dd,hh,mi,sec)
    xissued=f1970(yy,mm,dd,hh,mi,sec)
    write (a24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.000Z")') yy,mm,dd,hh,mi
    write (ayy,'(I4.4)') yy
    write (amm,'(I2.2)') mm
    write (add,'(I2.2)') dd
    write (ahh,'(I2.2)') hh
    key350(key_issued)=a24                           ! analysis time
    call choptrim(key350(key_issued),350)
    key350(key_issued_yy)=ayy
    call choptrim(key350(key_issued_yy),350)
    key350(key_issued_mm)=amm
    call choptrim(key350(key_issued_mm),350)
    key350(key_issued_dd)=add
    call choptrim(key350(key_issued_dd),350)
    key350(key_issued_hh)=ahh
    call choptrim(key350(key_issued_hh),350)
    write(key350(key_issued_epoch),'(I0,"000")')nint(xissued)! analysis epoch
    call choptrim(key350(key_issued_epoch),350)
    key350(key_par_name)=par350
    call choptrim(key350(key_par_name),350)
    return
  end subroutine setKeyVar
  !
  subroutine setXmlVar(cpar,crep,file,mxml,mtim,xml350)
    implicit none
    type(parameter), pointer :: cpar
    type(report), pointer :: crep
    type(filetype) :: file
    integer :: mxml,mtim
    character*350 xml350(mxml,mtim)
    integer :: tmin,tmax
    real :: cauxmin,cauxmax
    integer yy,mm,dd,hh,mi
    real sec,cstart2000,cstop2000
    integer :: kk
    character*24 a24,x24,y24
    character*4 ayy
    character*2 amm,add,ahh
    character*700 :: b700
    real dta,dtad,dtah
    real :: cvalmin,cvalmax,cllmin(2),cllmax(2),ctmin2000,ctmax2000
    real xissued,xstart,xstop
    CHARACTER*12 MYNAME
    DATA MYNAME /'setXmlVar'/
    call dj2000(file%a2000,yy,mm,dd,hh,mi,sec)
    xissued=f1970(yy,mm,dd,hh,mi,sec)
    write (a24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.000Z")') yy,mm,dd,hh,mi
    write (ayy,'(I4.4)') yy
    write (amm,'(I2.2)') mm
    write (add,'(I2.2)') dd
    write (ahh,'(I2.2)') hh
    if (.not.cpar%hasvar) return
    b700=cpar%var%var250
    call chop0(b700,700)
    lenb=length(b700,700,10)
    tmin=-1
    tmax=-1
    do tt=1,mtim
       if (.not.crep%lfirst(tt)) then ! no field available
          tmin=tt
          cvalmin=crep%valmin(tt)
          cllmin(1)=crep%llmin(1,tt)
          cllmin(2)=crep%llmin(2,tt)
          ctmin2000=max(file%mint2000,crep%tj2000(tt)-cpar%hrs/(24.0D0*2.0D0))
          cvalmax=crep%valmax(tt)
          cllmax(1)=crep%llmax(1,tt)
          cllmax(2)=crep%llmax(2,tt)
          ctmax2000=max(file%mint2000,crep%tj2000(tt)-cpar%hrs/(24.0D0*2.0D0))
          cstart2000=max(file%mint2000,crep%tj2000(tt)-file%dt-cpar%hrs/(24.0D0))
          cstop2000=crep%tj2000(tt)+file%dt
          if (crep%laux) then
             do kk=1,crep%ntrg
                if (crep%trglenv(kk).ne.0) then
                   cauxmin=crep%auxmin(tt,kk)
                   cauxmax=crep%auxmax(tt,kk)
                end if
             end do
          end if
          call dj2000(cstart2000,yy,mm,dd,hh,mi,sec)
          write (x24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.0000")') &
               & yy,mm,dd,hh,mi
          xstart=f1970(yy,mm,dd,hh,mi,sec)
          call dj2000(cstop2000,yy,mm,dd,hh,mi,sec)
          write (y24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.0000")') &
               & yy,mm,dd,hh,mi
          xstop=f1970(yy,mm,dd,hh,mi,sec)
          xml350(xml_issued,tt)=a24                           ! analysis time
          call choptrim(xml350(xml_issued,tt),350)
          xml350(xml_issued_yy,tt)=ayy
          call choptrim(xml350(xml_issued_yy,tt),350)
          xml350(xml_issued_mm,tt)=amm
          call choptrim(xml350(xml_issued_mm,tt),350)
          xml350(xml_issued_dd,tt)=add
          call choptrim(xml350(xml_issued_dd,tt),350)
          xml350(xml_issued_hh,tt)=ahh
          call choptrim(xml350(xml_issued_hh,tt),350)
          write(xml350(xml_issued_epoch,tt),'(I0,"000")')nint(xissued)! analysis epoch
          call choptrim(xml350(xml_issued_epoch,tt),350)
          write(xml350(xml_start_epoch,tt),'(I0,"000")')nint(xstart)! start epoch
          call choptrim(xml350(xml_start_epoch,tt),350)
          write(xml350(xml_stop_epoch,tt),'(I0,"000")')nint(xstop)  ! stop epoch
          call choptrim(xml350(xml_stop_epoch,tt),350)
          xml350(xml_start,tt) =x24                           ! start date
          call choptrim(xml350(xml_start,tt),350)
          xml350(xml_stop,tt)  =y24                           ! stop date
          call choptrim(xml350(xml_stop,tt),350)
          xml350(xml_expires,tt)=file%t24                          ! expire time
          call choptrim(xml350(xml_expires,tt),350)
          !write(xml350(xml_start,tt),'(F12.2)') (cstart2000-t2000)*24.0 ! forecast start in hours
          !write(xml350(xml_stop,tt),'(F12.2)')  (cstop2000-t2000)*24.0 ! forecast stop in hours
          call dj2000(ctmin2000,yy,mm,dd,hh,mi,sec)
          write (x24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.0000")') &
               & yy,mm,dd,hh,mi
          xml350(xml_minval_time,tt) =x24                                          ! forecast start
          call choptrim(xml350(xml_minval_time,tt),350)
          dta=ctmin2000-file%a2000
          dtad=int(dta)
          dtah=(dta-dtad)*24.0D0
          write(xml350(xml_minval_dt,tt),"(I0)") nint(dta*24.0D0)
          call choptrim(xml350(xml_minval_dt,tt),350)
          if (dtad.gt.0.0D0.and.dtah.gt.0.0D0) then
             write(xml350(xml_minval_dc,tt),&
                  & '("+",I0,"d",I0,"h")') nint(dtad),nint(dtah)
          else if (dtad.gt.0.0D0) then
             write(xml350(xml_minval_dc,tt),'("+",I0,"d")') nint(dtad)
          else
             write(xml350(xml_minval_dc,tt),'("+",I0,"h")') nint(dtah)
          end if
          call choptrim(xml350(xml_minval_dc,tt),350)
          write(xml350(xml_minval_yy,tt),"(I4.4)") yy
          call choptrim(xml350(xml_minval_yy,tt),350)
          write(xml350(xml_minval_mm,tt),"(I2.2)") mm
          call choptrim(xml350(xml_minval_mm,tt),350)
          write(xml350(xml_minval_dd,tt),"(I2.2)") dd
          call choptrim(xml350(xml_minval_dd,tt),350)
          write(xml350(xml_minval_hh,tt),"(I2.2)") hh
          call choptrim(xml350(xml_minval_hh,tt),350)
          call dj2000(ctmax2000,yy,mm,dd,hh,mi,sec)
          write (y24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.0000")') &
               & yy,mm,dd,hh,mi
          xml350(xml_maxval_time,tt)  =y24                                          ! forecast stop
          call choptrim(xml350(xml_maxval_time,tt),350)
          dta=ctmax2000-file%a2000
          dtad=int(dta)
          dtah=(dta-dtad)*24.0D0
          write(xml350(xml_maxval_dt,tt),"(I0)") nint(dta*24.0D0)
          call choptrim(xml350(xml_maxval_dt,tt),350)
          if (dtad.gt.0.0D0.and.dtah.gt.0.0D0) then
             write(xml350(xml_maxval_dc,tt),&
                  & '("+",I0,"d",I0,"h")') nint(dtad),nint(dtah)
          else if (dtad.gt.0.0D0) then
             write(xml350(xml_maxval_dc,tt),'("+",I0,"d")') nint(dtad)
          else
             write(xml350(xml_maxval_dc,tt),'("+",I0,"h")') nint(dtah)
          end if
          call choptrim(xml350(xml_maxval_dc,tt),350)
          write(xml350(xml_maxval_yy,tt),"(I4.4)") yy
          call choptrim(xml350(xml_maxval_yy,tt),350)
          write(xml350(xml_maxval_mm,tt),"(I2.2)") mm
          call choptrim(xml350(xml_maxval_mm,tt),350)
          write(xml350(xml_maxval_dd,tt),"(I2.2)") dd
          call choptrim(xml350(xml_maxval_dd,tt),350)
          write(xml350(xml_maxval_hh,tt),"(I2.2)") hh
          call choptrim(xml350(xml_maxval_hh,tt),350)
          write(xml350(xml_acc_hh,tt),"(F10.2)") cpar%hrs
          call choptrim(xml350(xml_acc_hh,tt),350)
          write(xml350(xml_minval,tt),'(F12.2)') cvalmin       ! minimum value
          call choptrim(xml350(xml_minval,tt),350)
          write(xml350(xml_minval_lat,tt),'(F12.2)') cllmin(1) ! minimum value lat
          call choptrim(xml350(xml_minval_lat,tt),350)
          write(xml350(xml_minval_lon,tt),'(F12.2)') cllmin(2) ! minimum value lon
          call choptrim(xml350(xml_minval_lon,tt),350)
          write(xml350(xml_maxval,tt),'(F12.2)') cvalmax       ! maximum value
          call choptrim(xml350(xml_maxval,tt),350)
          write(xml350(xml_maxval_lat,tt),'(F12.2)') cllmax(1) ! minimum value lat
          call choptrim(xml350(xml_maxval_lat,tt),350)
          write(xml350(xml_maxval_lon,tt),'(F12.2)') cllmax(2) ! minimum value lon
          call choptrim(xml350(xml_maxval_lon,tt),350)
          tmin=-1
          tmax=-1
       end if
    end do
    return
  end subroutine setXmlVar
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
  subroutine replaceXML(i700,mvar,var350)
    implicit none
    character*700 :: i700
    integer :: mvar
    character*350 :: var350(mvar)
    integer opos,cpos,leni,lenx,leny
    character*700 :: b700
    integer, external :: length, lengthtrim
    CHARACTER*12 MYNAME
    DATA MYNAME /'replaceXML'/
    lenx=0
    opos=1
    cpos=1
    call chop0(i700,700)
    leni=length(i700,700,20)
    ! write(*,*) myname,'BUFF:',b700(1:leni)
    ! do ll=1,mvar
    !    call chop0(var350(ll),350)
    !    leny=lengthtrim(var350(ll),350,10)
    !    write(*,*)myname,'VAR:',ll,var350(ll)(1:leny)
    ! end do
    do while (cpos.le.leni)
       if (i700(cpos:cpos).eq."@") then ! first "$"
          b700=b700(1:lenx)//i700(opos:cpos-1)
          lenx=lenx+max(0,cpos-opos)
          ! find next "$" if any
          cpos=cpos+1
          opos=cpos
          do while (cpos.lt.leni.and.i700(cpos:cpos).ne."@") ! second "$"
             cpos=cpos+1
          end do
          if (i700(opos:cpos-1).eq."issued") then
             leny=lengthtrim(var350(xml_issued),350,10)
             b700=b700(1:lenx)//var350(xml_issued)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_yy") then
             leny=lengthtrim(var350(xml_issued_yy),350,10)
             b700=b700(1:lenx)//var350(xml_issued_yy)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_mm") then
             leny=lengthtrim(var350(xml_issued_mm),350,10)
             b700=b700(1:lenx)//var350(xml_issued_mm)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_dd") then
             leny=lengthtrim(var350(xml_issued_dd),350,10)
             b700=b700(1:lenx)//var350(xml_issued_dd)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_hh") then
             leny=lengthtrim(var350(xml_issued_hh),350,10)
             b700=b700(1:lenx)//var350(xml_issued_hh)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_epoch") then
             leny=lengthtrim(var350(xml_issued_epoch),350,10)
             b700=b700(1:lenx)//var350(xml_issued_epoch)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."start_epoch") then
             leny=lengthtrim(var350(xml_start_epoch),350,10)
             b700=b700(1:lenx)//var350(xml_start_epoch)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."stop_epoch") then
             leny=lengthtrim(var350(xml_stop_epoch),350,10)
             b700=b700(1:lenx)//var350(xml_stop_epoch)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."start") then
             leny=lengthtrim(var350(xml_start),350,10)
             b700=b700(1:lenx)//var350(xml_start)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."stop") then
             leny=lengthtrim(var350(xml_stop),350,10)
             b700=b700(1:lenx)//var350(xml_stop)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."expires") then
             leny=lengthtrim(var350(xml_expires),350,10)
             b700=b700(1:lenx)//var350(xml_expires)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval") then
             leny=lengthtrim(var350(xml_minval),350,10)
             b700=b700(1:lenx)//var350(xml_minval)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_lat") then
             leny=lengthtrim(var350(xml_minval_lat),350,10)
             b700=b700(1:lenx)//var350(xml_minval_lat)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_lon") then
             leny=lengthtrim(var350(xml_minval_lon),350,10)
             b700=b700(1:lenx)//var350(xml_minval_lon)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_time") then
             leny=lengthtrim(var350(xml_minval_time),350,10)
             b700=b700(1:lenx)//var350(xml_minval_time)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_dt") then
             leny=lengthtrim(var350(xml_minval_dt),350,10)
             b700=b700(1:lenx)//var350(xml_minval_dt)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_dc") then
             leny=lengthtrim(var350(xml_minval_dc),350,10)
             b700=b700(1:lenx)//var350(xml_minval_dc)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_yy") then
             leny=lengthtrim(var350(xml_minval_yy),350,10)
             b700=b700(1:lenx)//var350(xml_minval_yy)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_mm") then
             leny=lengthtrim(var350(xml_minval_mm),350,10)
             b700=b700(1:lenx)//var350(xml_minval_mm)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_dd") then
             leny=lengthtrim(var350(xml_minval_dd),350,10)
             b700=b700(1:lenx)//var350(xml_minval_dd)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."minval_hh") then
             leny=lengthtrim(var350(xml_minval_hh),350,10)
             b700=b700(1:lenx)//var350(xml_minval_hh)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval") then
             leny=lengthtrim(var350(xml_maxval),350,10)
             b700=b700(1:lenx)//var350(xml_maxval)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_lat") then
             leny=lengthtrim(var350(xml_maxval_lat),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_lat)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_lon") then
             leny=lengthtrim(var350(xml_maxval_lon),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_lon)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_time") then
             leny=lengthtrim(var350(xml_maxval_time),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_time)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_dt") then
             leny=lengthtrim(var350(xml_maxval_dt),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_dt)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_dc") then
             leny=lengthtrim(var350(xml_maxval_dc),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_dc)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_yy") then
             leny=lengthtrim(var350(xml_maxval_yy),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_yy)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_mm") then
             leny=lengthtrim(var350(xml_maxval_mm),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_mm)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_dd") then
             leny=lengthtrim(var350(xml_maxval_dd),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_dd)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."maxval_hh") then
             leny=lengthtrim(var350(xml_maxval_hh),350,10)
             b700=b700(1:lenx)//var350(xml_maxval_hh)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."accumulation_hours") then
             leny=lengthtrim(var350(xml_acc_hh),350,10)
             b700=b700(1:lenx)//var350(xml_acc_hh)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else  
             opos=opos-1 ! reset opos to first "@"
          end if
       end if
       !write(*,*) myname,' BUFF:::',cpos,leni,i700(cpos:cpos)
       cpos=cpos+1
    end do
    b700=b700(1:lenx)//i700(opos:cpos-1) ! copy the rest
    lenx=lenx+max(0,cpos-opos)
    i700=b700(1:lenx)
    call chop0(i700,700)
    return
  end subroutine replaceXML
  !
  !  
  subroutine replaceKEY(i700,mvar,var350)
    implicit none
    character*700 :: i700
    integer :: mvar
    character*350 :: var350(mvar)
    integer opos,cpos,lenb,lenx,leny
    character*700 :: b700
    integer, external :: length, lengthtrim
    CHARACTER*12 MYNAME
    DATA MYNAME /'replaceKey'/
    lenx=0
    opos=1
    cpos=1
    call chop0(i700,700)
    lenb=length(i700,700,20)
    ! write(*,*) myname,'BUFF:',i700(1:lenb)
    ! do ll=1,mvar
    !    call chop0(var350(ll),350)
    !    leny=lengthtrim(var350(ll),350,10)
    !    write(*,*)myname,'VAR:',ll,var350(ll)(1:leny)
    ! end do
    do while (cpos.le.lenb)
       if (i700(cpos:cpos).eq."@") then ! first "$"
          b700=b700(1:lenx)//i700(opos:cpos-1)
          lenx=lenx+max(0,cpos-opos)
          ! find next "$" if any
          cpos=cpos+1
          opos=cpos
          do while (cpos.lt.lenb.and.i700(cpos:cpos).ne."@") ! second "$"
             cpos=cpos+1
          end do
          if (i700(opos:cpos-1).eq."issued") then
             leny=lengthtrim(var350(key_issued),350,10)
             b700=b700(1:lenx)//var350(key_issued)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_yy") then
             leny=lengthtrim(var350(key_issued_yy),350,10)
             b700=b700(1:lenx)//var350(key_issued_yy)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_mm") then
             leny=lengthtrim(var350(key_issued_mm),350,10)
             b700=b700(1:lenx)//var350(key_issued_mm)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_dd") then
             leny=lengthtrim(var350(key_issued_dd),350,10)
             b700=b700(1:lenx)//var350(key_issued_dd)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_hh") then
             leny=lengthtrim(var350(key_issued_hh),350,10)
             b700=b700(1:lenx)//var350(key_issued_hh)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."issued_epoch") then
             leny=lengthtrim(var350(key_issued_epoch),350,10)
             b700=b700(1:lenx)//var350(key_issued_epoch)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."par_name") then
             leny=lengthtrim(var350(key_par_name),350,10)
             b700=b700(1:lenx)//var350(key_par_name)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else if (i700(opos:cpos-1).eq."iter") then
             leny=lengthtrim(var350(key_iter),350,10)
             b700=b700(1:lenx)//var350(key_iter)(1:leny)
             lenx=lenx+max(0,leny)
             opos=cpos+1 ! point to character after "@"
          else  
             opos=opos-1 ! reset opos to first "@"
          end if
       end if
       !write(*,*) myname,' BUFF:::',cpos,lenb,i700(cpos:cpos)
       cpos=cpos+1
    end do
    b700=b700(1:lenx)//i700(opos:cpos-1) ! copy the rest
    lenx=lenx+max(0,cpos-opos)
    i700=b700(1:lenx)
    call chop0(i700,700)
    return
  end subroutine replaceKEY
  ! YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
  recursive function getPolygonFilter(op,irc) result(flt)
    implicit none
    type(operation), pointer :: op, cop=>null()
    integer irc
    type(filter), pointer :: flt,cflt,f
    logical first
    CHARACTER*16 MYNAME
    DATA MYNAME /'getPolygonFilter'/
    ! write(*,*)myname,'Entering...'
    first=.true.
    nullify(flt)
    select case (op%type)
    case (opr_inter) ! intersection
       ! write(*,*)myname,'Looking for intersection...', op%type, opr_inter
       cop => op%first%next
       do while (.not.associated(cop,target=op%last))
          cflt=>getPolygonFilter(cop,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from getPolygonFilter.',irc
             return
          end if
          if (first) then
             flt => copyFilter(cflt,irc)
             first=.not.associated(flt)
             gfilter => flt
             gfilter%simplify=.true.
             gfilter%eps=0.1D0
             ! write(*,*)myname,'Got first filter...', associated(flt),first
          else if (associated(cflt)) then
             ! write(*,*)myname,'Intersecting next filter...',associated(cflt)
             call combinePolygons(flt,cflt,"intersection",irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from combinePolygons.',irc
                return
             end if
          end if
          if (flt%mpar .eq. 0) then ! polygon is empty
             cop=>op%last
          else
             cop=>cop%next
          end if
       end do
    case (opr_union) ! union
       ! write(*,*)myname,'Looking for union...', op%type, opr_union
       cop => op%first%next
       do while (.not.associated(cop,target=op%last))
          cflt=>getPolygonFilter(cop,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from getPolygonFilter.',irc
             return
          end if
          if (first) then
             flt => copyFilter(cflt,irc)
             first=.not.associated(flt)
             gfilter => flt
             gfilter%simplify=.true.
             gfilter%eps=0.1D0
          else if (associated(cflt)) then
             call combinePolygons(flt,cflt,"union",irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from unionPolygon.',irc
                return
             end if
          end if
          if (flt%mpar .eq. 0) then ! polygon is empty
             cop=>op%last
          else
             cop=>cop%next
          end if
       end do
    case (opr_filter) ! filter
       ! write(*,*)myname,'Looking for filter...', op%type, opr_filter
       f => op%f
       if (.not.associated(f)) then
          write(*,*) 'Missing filter:',trim(op%name250)
          irc=942
          return
       end if
       ! write(*,*)myname,'Checking filter...',f%type,flt_polygon
       select case(f%type)
       case (flt_polygon)  ! polygon
          flt => copyFilter(f,irc)
          ! write(*,*)myname,'Found polygon...',associated(flt)
       case (flt_polyline)  ! polyline
          flt => copyFilter(f,irc)
          ! write(*,*)myname,'Found polyline...',associated(flt)
       case DEFAULT
          ! do nothing
       end select
    case DEFAULT
       write(*,*) 'Unknown type:',op%type
       irc=932
       return
    end select
    ! write(*,*)myname,'Done...',associated(flt)
    return
  end function getPolygonFilter
  !
  recursive function checkMasked(op,lat,lon,ialt,irc) result(res)
    implicit none
    logical :: res
    type(operation), pointer :: op
    real lat
    real lon
    real ialt
    integer irc
    character*250 :: crc250
    integer :: lenc
    !
    real val,minlat,maxlat,minlon,maxlon
    type(operation), pointer :: cop
    type(filter), pointer :: f
    logical inside, iflag
    integer inout, idim, ii,ss,istart,istop,nsplit
    real dx,dy,dz,ds,ws,hs,dd
    real alt, dist,maxalt,minalt
    CHARACTER*18 MYNAME
    DATA MYNAME /'checkMasked'/
    !
    if (.not.associated(op)) then ! no mask, all data ok...
       res=.false.
       return
    end if
    alt=ialt
    select case (op%type)
    case (opr_inter) ! intersection
       res=.false.
       cop => op%first%next
       do while (.not.associated(cop,target=op%last))
          iflag=checkMasked(cop,lat,lon,alt,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from checkMasked.',irc
             return
          end if
          if (iflag) then
             res=.true.
             cop=>op%last
          else
             cop=>cop%next
          end if
       end do
    case (opr_union) ! union
       res=.true.
       cop => op%first%next
       do while (.not.associated(cop,target=op%last))
          iflag=checkMasked(cop,lat,lon,alt,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from checkMasked.',irc
             return
          end if
          if (.not.iflag) then
             res=.false.
          end if
          cop=>cop%next
       end do
    case (opr_filter) ! filter
       f => op%f
       if (.not.associated(f)) then
          res=.true.
          write(*,*) 'Missing filter:',trim(op%name250)
          irc=942
          return
       end if
       select case(f%type)
       case(flt_slice) ! slice
          res=.false.
          if (f%lpar(0)) then ! must initialise
             if (.not. f%lpar(3)) then ! we have 3D fields (with altitude)
                f%rpar(3)=0.0D0 ! no altitude
                f%rpar(7)=0.0D0 ! no altitude
                f%rpar(13)=0.0D0 ! no altitude
                f%rpar(17)=0.0D0 ! no altitude
             end if
             if (.not.res.and.f%lpar(1).and.f%lpar(2)) then ! we have origo
                f%rpar(20)= (re+f%rpar(3))*cosdeg(f%rpar(1))*cosdeg(f%rpar(2)) ! origo x
                f%rpar(21)= (re+f%rpar(3))*cosdeg(f%rpar(1))*sindeg(f%rpar(2)) ! origo y
                f%rpar(22)= (re+f%rpar(3))*sindeg(f%rpar(1))! origo z
                ! make normal...
                if (.not.res.and.f%lpar(15).and.f%lpar(16)) then ! we have a normal
                   f%rpar(23)= (re+f%rpar(17))*cosdeg(f%rpar(15))*cosdeg(f%rpar(16)) -f%rpar(20) ! x
                   f%rpar(24)= (re+f%rpar(17))*cosdeg(f%rpar(15))*sindeg(f%rpar(16)) -f%rpar(21) ! y
                   f%rpar(25)= (re+f%rpar(17))*sindeg(f%rpar(15)) -f%rpar(22) ! z
                   f%lpar(0)=.false. ! initialised
                else if (.not.res.and.f%lpar(5).and.f%lpar(6)) then ! we have abscissa
                   f%rpar(26)= (re+f%rpar(7))*cosdeg(f%rpar(5))*cosdeg(f%rpar(6)) -f%rpar(20) ! x
                   f%rpar(27)= (re+f%rpar(7))*cosdeg(f%rpar(5))*sindeg(f%rpar(6)) -f%rpar(21) ! y
                   f%rpar(28)= (re+f%rpar(7))*sindeg(f%rpar(5)) -f%rpar(22) ! z
                   if (.not.res.and.f%lpar(5).and.f%lpar(6)) then ! we have ordinate
                      f%rpar(29)= (re+f%rpar(13))*cosdeg(f%rpar(11))*cosdeg(f%rpar(12)) -f%rpar(20) ! x
                      f%rpar(30)= (re+f%rpar(13))*cosdeg(f%rpar(11))*sindeg(f%rpar(12)) -f%rpar(21) ! y
                      f%rpar(31)= (re+f%rpar(13))*sindeg(f%rpar(11)) -f%rpar(22) ! z
                   else ! origo and abscissa, but no ordinate
                      f%rpar(29)= 0.0D0
                      f%rpar(30)= 0.0D0
                      f%rpar(31)= -1.0D0
                   end if
                   ! calculate cross product
                   f%rpar(23)=f%rpar(27)*f%rpar(31)-f%rpar(28)*f%rpar(30)
                   f%rpar(24)=f%rpar(28)*f%rpar(29)-f%rpar(26)*f%rpar(31)
                   f%rpar(25)=f%rpar(26)*f%rpar(30)-f%rpar(27)*f%rpar(29)
                   f%lpar(0)=.false. ! initialised
                else ! origo, but no normal nor abscissa 
                   write(*,*)myname,'Missing normal/abscissa-parameter in SLICE-filter.'
                   irc=845
                   return
                end if
             else  ! no origo
                write(*,*)myname,'Missing origo-parameter in SLICE-filter.'
                irc=841
                return
             end if
          end if
          if (f%lpar(0)) then ! failed to initialise
             res=.true.
             write(*,*)myname,'Failed to initialise SLICE-filter.'
             irc=840
             return
          else
             if (.not.f%lpar(3)) then ! we have 3D fields (with altitude)
                alt=0.0D0
             end if
             dx= (re+alt)*cosdeg(lat)*cosdeg(lon) - f%rpar(20) ! pos x
             dy= (re+alt)*cosdeg(lat)*sindeg(lon) - f%rpar(21) ! pos y
             dz= (re+alt)*sindeg(lat) - f%rpar(22) ! origo z
             dd=dx*f%rpar(23)+dy*f%rpar(24)+dz*f%rpar(25)
             inside= (dd.ge.0.0D0)
             if ((f%inside.and..not.inside).or.(.not.f%inside.and.inside)) res=.true.
          end if
       case (flt_cylinder) ! cylinder
          res=.false.
          if (f%lpar(0)) then ! must initialise
             f%lpar(0)=.false. ! initialised
          end if
          if (.not.res.and.f%lpar(1).and.f%lpar(2).and.f%lpar(4)) then ! lat/lon/width
             dist=getdist(f%rpar(1),f%rpar(2),lat,lon)*40.0D3/360.0D0
             inside=(dist.lt.f%rpar(4)*0.5D0)
             if ((f%inside.and..not.inside).or.(.not.f%inside.and.inside)) res=.true.
          end if
          if (.not.res.and.f%lpar(3).and.f%lpar(5)) then ! alt/height
             maxalt=f%rpar(3)+f%rpar(5)*0.5D0
             minalt=f%rpar(3)-f%rpar(5)*0.5D0
             inside=(alt.le.maxalt .or. alt.ge.minalt)
             if ((f%inside.and..not.inside).or.(.not.f%inside.and.inside)) res=.true.
          end if
          !          if (.not.res) then
          !             write(*,*)myname,'Cylinder:',dist,f%rpar(4)*0.5D0,alt,maxalt,minalt,res
          !          end if
       case (flt_polygon)  ! polygon
          res=.false.
          if (f%lpar(0)) then ! must initialise
             f%lpar(0)=.false. ! initialised
          end if
          inout=-1 ! border=0, -1 outside, +1 inside
          minlat=f%rpar(f%mpar*2+1)
          maxlat=f%rpar(f%mpar*2+2)
          minlon=f%rpar(f%mpar*2+3)
          maxlon=f%rpar(f%mpar*2+4)
          if (isClose(lat,lon,minlat,maxlat,minlon,maxlon,f%delta)) then
             CALL PNPOLY (lat, lon, f%mpar, f%rpar(1), f%rpar(1+f%mpar), INOUT )
             !write(*,*)myname,'Polygon:',f%mpar,size(f%rpar),f%mpar*2+4,lat,lon,inout
             !inout=1
             if (f%ledge .and. inout.eq.-1) then ! check if inside edge...
                call checkEdge(lat, lon, f%mpar, f%rpar(1), f%rpar(1+f%mpar), f%delta, INOUT )
             end if
          end if
          inside=(inout.ne.-1)
          if ((f%inside.and..not.inside).or.(.not.f%inside.and.inside)) res=.true.
          !if (.not.res) then
          !   write(*,*)myname,'Polygon:',lat,lon,res
          !end if
       case (flt_polyline)  ! polyline
          res=.false.
          if (f%lpar(0)) then ! must initialise
             f%lpar(0)=.false. ! initialised
          end if
          inout=-1 ! border=0, -1 outside, +1 inside
          minlat=f%rpar(f%mpar*2+1)
          maxlat=f%rpar(f%mpar*2+2)
          minlon=f%rpar(f%mpar*2+3)
          maxlon=f%rpar(f%mpar*2+4)
          if (isClose(lat,lon,minlat,maxlat,minlon,maxlon,f%delta)) then
             if (f%ledge) then ! check if inside edge...
                do ss=1,f%npar(1)
                   if (inout.eq.-1) then
                      istart=f%npar(ss*2)
                      istop=f%npar(ss*2+1)
                      nsplit=istop-istart+1
                      call checkEdge(lat,lon,nsplit,f%rpar(istart), &
                           & f%rpar(istart+f%mpar),f%delta,INOUT )
                   end if
                end do
             end if
          end if
          inside=(inout.ne.-1)
          if ((f%inside.and..not.inside).or.(.not.f%inside.and.inside)) res=.true.
          !if (.not.res) then
          !   write(*,*)myname,'Polygon:',lat,lon,res
          !end if
       case (flt_duct) ! duct
          res=.false.
          if (f%lpar(0)) then ! must initialise
             if (.not. f%lpar(3)) then ! we have 3D fields (with altitude)
                f%rpar(3)=0.0D0 ! no altitude
                f%rpar(13)=0.0D0 ! no altitude
             end if
             if (.not.res.and.f%lpar(1).and.f%lpar(2).and.f%lpar(11).and.f%lpar(12)) then ! we have start/stop
                ! origo
                f%rpar(20)= (re+f%rpar(3))*cosdeg(f%rpar(1))*cosdeg(f%rpar(2)) ! x
                f%rpar(21)= (re+f%rpar(3))*cosdeg(f%rpar(1))*sindeg(f%rpar(2)) ! y
                f%rpar(22)= (re+f%rpar(3))*sindeg(f%rpar(1))! z
                ! make alongtrack vector (l=R1-R2)
                f%rpar(23)= (re+f%rpar(13))*cosdeg(f%rpar(11))*cosdeg(f%rpar(12)) -f%rpar(20) ! x
                f%rpar(24)= (re+f%rpar(13))*cosdeg(f%rpar(11))*sindeg(f%rpar(12)) -f%rpar(21) ! y
                f%rpar(25)= (re+f%rpar(13))*sindeg(f%rpar(11)) -f%rpar(22) ! z
                f%rpar(26)=sqrt(max(1.0D-5, f%rpar(23)*f%rpar(23)&
                     & + f%rpar(24)*f%rpar(24) + f%rpar(25)*f%rpar(25)))
                f%rpar(23)=f%rpar(23)/f%rpar(26)
                f%rpar(24)=f%rpar(24)/f%rpar(26)
                f%rpar(25)=f%rpar(25)/f%rpar(26)
                ! make radial vector (r=R/|R|)
                f%rpar(27)=f%rpar(20) ! x
                f%rpar(28)=f%rpar(21) ! y
                f%rpar(29)=f%rpar(22) ! z
                f%rpar(30)=sqrt(max(1.0D-5, f%rpar(27)*f%rpar(27)&
                     & + f%rpar(28)*f%rpar(28)+f%rpar(29)*f%rpar(29)))
                f%rpar(27)=f%rpar(27)/f%rpar(30) ! x
                f%rpar(28)=f%rpar(28)/f%rpar(30) ! y
                f%rpar(29)=f%rpar(29)/f%rpar(30) ! z
                ! make cross track vector (c=lxr)
                f%rpar(31)=f%rpar(24)*f%rpar(29)-f%rpar(25)*f%rpar(28)
                f%rpar(32)=f%rpar(25)*f%rpar(27)-f%rpar(23)*f%rpar(29)
                f%rpar(33)=f%rpar(23)*f%rpar(28)-f%rpar(24)*f%rpar(27)
                f%rpar(34)=sqrt(max(1.0D-5, f%rpar(31)*f%rpar(31)&
                     & + f%rpar(32)*f%rpar(32)+f%rpar(33)*f%rpar(33)))
                f%rpar(31)=f%rpar(31)/f%rpar(34) ! x
                f%rpar(32)=f%rpar(32)/f%rpar(34) ! y
                f%rpar(33)=f%rpar(33)/f%rpar(34) ! z
                ! make vertical vector (v=lxc)
                f%rpar(35)=f%rpar(24)*f%rpar(33)-f%rpar(25)*f%rpar(32)
                f%rpar(36)=f%rpar(25)*f%rpar(31)-f%rpar(23)*f%rpar(33)
                f%rpar(37)=f%rpar(23)*f%rpar(32)-f%rpar(24)*f%rpar(31)
                f%rpar(38)=sqrt(max(1.0D-5, f%rpar(35)*f%rpar(35)&
                     & + f%rpar(36)*f%rpar(36)+f%rpar(37)*f%rpar(37)))
                f%rpar(35)=f%rpar(35)/f%rpar(38) ! x
                f%rpar(36)=f%rpar(36)/f%rpar(38) ! y
                f%rpar(37)=f%rpar(37)/f%rpar(38) ! z
                f%lpar(0)=.false. ! initialised
             else ! missing parameters
                write(*,*)myname,'Missing start/stop-parameter in DUCT-filter.'
                irc=845
                return
             end if
          end if
          if (f%lpar(0)) then ! failed to initialise
             res=.true.
             write(*,*)myname,'Failed to initialise DUCT-filter.'
             irc=840
             return
          else
             if (.not.f%lpar(3)) then ! we have 3D fields (with altitude)
                alt=0.0D0
             end if
             dx= (re+alt)*cosdeg(lat)*cosdeg(lon) - f%rpar(20) ! pos x
             dy= (re+alt)*cosdeg(lat)*sindeg(lon) - f%rpar(21) ! pos y
             dz= (re+alt)*sindeg(lat) - f%rpar(22) ! origo z
             ! along track
             ds=(dx*f%rpar(23)+dy*f%rpar(24)+dz*f%rpar(25))/f%rpar(26)
             inside= (ds.ge.0.0D0.and.ds.le.1.0D0)
             ! cross track
             if (inside) then
                ws=abs(dx*f%rpar(31)+dy*f%rpar(32)+dz*f%rpar(33))/ &
                     & ( (1.0D0-ds)*f%rpar(4)*0.5D0 + ds*f%rpar(14)*0.5D0 )
                if (ws.gt.1.0D0) inside=.false.
             end if
             ! vertical
             if (inside.and. f%lpar(3)) then
                hs=abs(dx*f%rpar(35)+dy*f%rpar(36)+dz*f%rpar(37))/ &
                     & ( (1.0D0-ds)*f%rpar(5)*0.5D0 + ds*f%rpar(15)*0.5D0 )
                if (hs.gt.1.0D0) inside=.false.
             end if
             if ((f%inside.and..not.inside).or.(.not.f%inside.and.inside)) res=.true.
          end if
       case (flt_value) ! parameter
          if (.not.associated(f%v)) then
             write(*,*) myname,'No variable associated with filter:',trim(f%name250)
             irc=458
             return
          else if (f%lene.ne.0) then ! we have an expression
             flt_val=ncf_valuePosition(f%v,irc)
             val=parse_evalf(f%exp,flt_val,crc250,irc)
             if (irc.ne.0) then
                lenc=length(crc250,250,10)
                write(*,*) crc250(1:lenc)
                write(*,*) myname,'Error return from parse_evalf.',irc
                return
             end if
          else 
             ! set filter-value position...
             val=ncf_valuePosition(f%v,irc)
          end if
          if (irc.ne.0) then
             write(*,*) myname,'Error return from ncf_valuePosition.',irc
             return
          end if
          res=.false.
          if (f%lpar(0)) then ! must initialise
             f%minval=val
             f%maxval=val
             f%lpar(0)=.false. ! initialised
          else
             f%minval=min(val,f%minval)
             f%maxval=max(val,f%maxval)
          end if
          if (.not.res.and.f%lpar(1)) then ! min threshold
             if (val.lt.f%rpar(1)) res=.true.
          end if
          if (.not.res.and.f%lpar(2)) then ! max threshold
             if (val.gt.f%rpar(2)) res=.true.
          end if
          if (.not.res.and.f%lpar(3)) then ! value
             if ((f%lpar(4) .AND. abs(val-f%rpar(3)).gt.f%rpar(4)) &
                  & .OR. (.NOT.f%lpar(4) .AND. val.ne.f%rpar(3))) res=.true.
             !write(*,*)myname,'Value:',val
             !call ncf_printPos(f%v%f%pos)
          end if
          !write(*,*)myname,'Value:',dist,f%rpar(4)*0.5D0,alt,maxalt,minalt,res
       case (flt_string) ! string
          if (.not.associated(f%v)) then
             write(*,*) myname,'No variable associated with filter:',trim(f%name250)
             irc=458
             return
          end if
          if (.not.associated(f%sdo)) then
             write(*,*) myname,'No DimOrder associated with filter:',trim(f%name250)
             irc=459
             return
          end if
          res=.false.
          ! write(*,*)myname,'SDO:', associated(f), associated(f%v), associated(f%sdo)
          ! call ncf_printVariable(f%v)
          ! call ncf_printDimOrder(f%sdo)
          sval250=ncf_stringPosition250(f%v,f%sdo,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from ncf_stringPosition250.',irc
             return
          end if
          call chop0(sval250,250)
          lens=length(sval250,250,3)
          !write(*,*)myname,"Comparing: '"//sval250(1:lens)//"' '"//f%spar250(1:f%lens)//"'"
          if (.not.res.and.f%lpar(1)) then ! string
             if (sval250(1:lens).ne.f%spar250(1:f%lens)) res=.true.
          end if
          !write(*,*)myname,'Value:',dist,f%rpar(4)*0.5D0,alt,maxalt,minalt,res
       case (flt_dimension) ! dimension
          res=.false.
          if (f%lpar(0)) then ! initialise
             !write(*,*) 'Test:',ii,f%npar(0),trim(f%sdim80)
             f%npar(0)=ncf_getDimEntry(f%i,f%sdim80)
             if (f%npar(0).eq.0) then
                write(*,*) 'Undefined dimension:',trim(f%sdim80)
             end if
             f%lpar(0)=.false. ! initialised
          end if
          idim=f%i%pos%pos(f%npar(0))
          ! get dimension value
          if (idim.ne.-1) then ! defined
             if (f%lpar(1)) then ! min
                if (.not.res.and.idim.lt.f%npar(1)) res=.true.
             end if
             if (f%lpar(2)) then ! max
                if (.not.res.and.idim.gt.f%npar(2)) res=.true.
             end if
             !write(*,*)myname,'Checking:',idim,res,f%npar(1),f%npar(2),f%npar(0)
          end if
          !write(*,*) 'Test:',idim,res,f%npar(1),f%npar(2),f%npar(0),f%lpar(1),f%lpar(2)
       case DEFAULT
          write(*,*) 'Invalid filter type:',f%type,trim(op%name250)
          irc=941
          return
       end select
    case DEFAULT
       res=.true.
       write(*,*) 'Unknown type:',op%type
       irc=933
       return
    end select
    return
  end function checkMasked

  recursive subroutine setActive(op,irc)
    implicit none
    type(operation), pointer :: op
    integer irc
    !
    type(operation), pointer :: cop
    type(filter), pointer :: f
    CHARACTER*12 MYNAME
    DATA MYNAME /'setActive'/
    !
    if (associated(op)) then
       if (op%type .eq. opr_inter .or. op%type .eq. opr_union) then ! intersection/union
          cop => op%first%next
          do while (.not.associated(cop,target=op%last))
             call setActive(cop,irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from setActive.',irc
                return
             end if
             cop=>cop%next
          end do
       else if (op%type .eq. opr_filter) then ! mask operation
          f => op%f
          if (.not.associated(f)) then
             write(*,*) 'Missing mask filter:',trim(op%name250)
             irc=942
             return
          end if
          f%active=.true.
       else
          write(*,*) 'Unknown type:',op%type
          irc=934
          return
       end if
    end if
    return
  end subroutine setActive
  !     
  ! find closest i and j
  !     
  real function xycross(x1,y1,x2,y2,x3,y3)
    real :: x1,y1,x2,y2,x3,y3
    real :: dx1,dy1,dx2,dy2
    dx1=x3-x1
    dy1=y3-y1
    dx2=x2-x1
    dy2=y2-y1
    xycross = dx1 * dy2 - dx2 * dy1
  end function xycross

  real function degtor(x)
    implicit none
    real x
    real pi
    parameter (pi=3.14159265359)
    degtor=x*pi/180.
  end function degtor

  real function rtodeg(x)
    implicit none
    real x
    real pi
    parameter (pi=3.14159265359)
    rtodeg=x*180./pi
  end function rtodeg

  real function sindeg(x)
    implicit none
    real x,degtor
    sindeg=sin(degtor(x))
  end function sindeg

  real function cosdeg(x)
    implicit none
    real x,degtor
    cosdeg=cos(degtor(x))
  end function cosdeg

  real function tandeg(x)
    implicit none
    real x,degtor
    tandeg=tan(degtor(x))
  end function tandeg

  real function asindeg(x)
    implicit none
    real x,rtodeg
    asindeg=rtodeg(asin(x))
  end function asindeg

  real function acosdeg(x)
    implicit none
    real x,rtodeg
    acosdeg=rtodeg(acos(x))
  end function acosdeg

  real function atandeg(x)
    implicit none
    real x,rtodeg
    atandeg=rtodeg(atan(x))
  end function atandeg

  real function atan2deg(y,x)
    implicit none
    real y,x,rtodeg
    atan2deg=rtodeg(atan2(y,x))
  end function atan2deg

  function getDim(i,num)
    type(dimension), pointer :: getDim
    type(dimensionOrder) :: i
    integer num
    integer kk
    type(dimension), pointer :: d=>null()
    nullify(getDim)
    kk=0
    d=>i%firstDimension%next
    do while (.not.associated(d,i%lastDimension))
       kk=kk+1
       if (kk.eq.num) then
          getDim=>d
          d=>i%lastDimension
       else
          d=>d%next
       end if
    end do
  end function getDim

  subroutine interpolate2D(r,e,ix,iy,wgt,biok,irc)
    implicit none
    type(inventory) r
    type(inventory) e
    type(dimension) ix
    type(dimension) iy
    type(weight) wgt
    integer irc
    real :: xf,yf
    integer :: nxs,nys,nzs
    real :: xs(2,2), ys(2,2)
    logical :: biok, xok,yok
    logical :: bdone,bdeb, changed, inside
    real :: bc,rc,tc,lc,fc,tbc,lrc
    real :: brc,rtc,tlc,lbc
    integer :: jbc,jrc,jtc,jlc,jbrc,jrtc,jtlc,jlbc,ii,jj
    integer :: iterations = 0
    CHARACTER*14 MYNAME
    DATA MYNAME /'interpolate2D'/
    !e%pos%pos(ix%ind)=max(ix%sta,min(ix%lim-1,e%pos%pos(ix%ind)))
    !e%pos%pos(iy%ind)=max(iy%sta,min(iy%lim-1,e%pos%pos(iy%ind)))
    ! search for match where position is within gridcell
    xf=ncf_valuePosition(r%lonid,irc)
    if (irc.ne.0) then
       write(*,*)myname,'error return from ncf_valuePosition.',irc
       return
    end if
    yf=ncf_valuePosition(r%latid,irc)
    if (irc.ne.0) then
       write(*,*)myname,'error return from ncf_valuePosition.',irc
       return
    end if
    bdone=.false.
    bdeb=.false.
    ! iterations=0
    do while (.not. bdone)
       iterations=iterations+1
       xok=.true.
       yok=.true.
       xok=ncf_incrementDimension(e,ix,xok,1,0,irc) ! -> bottom right
       if (irc.ne.0) then
          write(*,*) myname,'Error return from incrementPosition.',irc
          return
       end if
       xs(2,1)=ncf_valuePosition(e%lonid,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition (lonid).',irc
          return
       end if
       ys(2,1)=ncf_valuePosition(e%latid,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition (latid).',irc
          return
       end if

       yok=ncf_incrementDimension(e,iy,yok,1,0,irc) ! -> top right
       if (irc.ne.0) then
          write(*,*) myname,'Error return from incrementPosition.',irc
          return
       end if
       xs(2,2)=ncf_valuePosition(e%lonid,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition (lonid).',irc
          return
       end if
       ys(2,2)=ncf_valuePosition(e%latid,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition (latid).',irc
          return
       end if

       xok=ncf_decrementDimension(e,ix,xok,1,0,irc) ! -> top left
       if (irc.ne.0) then
          write(*,*) myname,'Error return from decrementPosition.',irc
          return
       end if
       xs(1,2)=ncf_valuePosition(e%lonid,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition (lonid).',irc
          return
       end if
       ys(1,2)=ncf_valuePosition(e%latid,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition (latid).',irc
          return
       end if

       yok=ncf_decrementDimension(e,iy,yok,1,0,irc) ! -> bottom left
       if (irc.ne.0) then
          write(*,*) myname,'Error return from decrementPosition.',irc
          return
       end if
       xs(1,1)=ncf_valuePosition(e%lonid,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition (lonid).',irc
          return
       end if
       ys(1,1)=ncf_valuePosition(e%latid,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition (latid).',irc
          return
       end if

       if (.not. xok.or. .not.yok) then
          biok=.false.
          xok=ncf_decrementDimension(e,ix,xok,1,0,irc) ! -> top left
          if (irc.ne.0) then
             write(*,*) myname,'Error return from decrementPosition.',irc
             return
          end if
          yok=ncf_decrementDimension(e,iy,yok,1,0,irc) ! -> bottom left
          if (irc.ne.0) then
             write(*,*) myname,'Error return from decrementPosition.',irc
             return
          end if
          call printPos(e%pos)
          write(*,*)myname, 'Invalid starting position.'
          irc=956
          return
       else
          !write(*,*)'XS:',xf,xs
          !write(*,*)'YS:',yf,ys
          !call exit(0)

          ! walk around grid cell border and calculate cross product
          bc=xycross(xs(1,1),ys(1,1), xf,yf, xs(2,1),ys(2,1)) ! bottom
          rc=xycross(xs(2,1),ys(2,1), xf,yf, xs(2,2),ys(2,2)) ! right
          tc=xycross(xs(2,2),ys(2,2), xf,yf, xs(1,2),ys(1,2)) ! top
          lc=xycross(xs(1,2),ys(1,2), xf,yf, xs(1,1),ys(1,1)) ! left

          if (abs(bc).lt.1.0D-10) then
             jbc=0
          else
             jbc=nint(sign(1.0D0,bc))
          end if
          if (abs(rc).lt.1.0D-10) then
             jrc=0
          else
             jrc=nint(sign(1.0D0,rc))
          end if
          if (abs(tc).lt.1.0D-10) then
             jtc=0
          else
             jtc=nint(sign(1.0D0,tc))
          end if
          if (abs(lc).lt.1.0D-10) then
             jlc=0
          else
             jlc=nint(sign(1.0D0,lc))
          end if

          brc=xycross(xs(1,1),ys(1,1), xs(2,2),ys(2,2), xs(2,1),ys(2,1)) ! bottom-right
          rtc=xycross(xs(2,1),ys(2,1), xs(1,2),ys(1,2), xs(2,2),ys(2,2)) ! right-top
          tlc=xycross(xs(2,2),ys(2,2), xs(1,1),ys(1,1), xs(1,2),ys(1,2)) ! top-left
          lbc=xycross(xs(1,2),ys(1,2), xs(2,1),ys(2,1), xs(1,1),ys(1,1)) ! left-bottom

          jbrc=nint(sign(1.0D0,brc))
          jrtc=nint(sign(1.0D0,rtc))
          jtlc=nint(sign(1.0D0,tlc))
          jlbc=nint(sign(1.0D0,lbc))

          if (bdeb) then
             write(*,'("XYSEARCH ",2(3X,"(",4(X,F13.5),")"),2(3X,"(",4(X,I2),")"))') &
                  &bc,rc,tc,lc, brc,rtc,tlc,lbc,jbc,jrc,jtc,jlc, jbrc,jrtc,jtlc,jlbc
             write(*,'("XYSEARCH ",5(" (",F10.5,",",F10.5,")"))') &
                  & xf,yf, &
                  & xs(1,1),ys(1,1), &
                  & xs(2,1),ys(2,1), &
                  & xs(2,2),ys(2,2), &
                  & xs(1,2),ys(1,2)
          end if
          !     
          !     if cross product has same sign as axis-sign => inside, else outside
          !     (make sure we do not cross border while searching...)
          !     
          inside=.true.
          changed=.false.
          if (jbc.eq.-jbrc) then  ! decrease y
             if (ncf_decrementDimension(e,iy,yok,1,0, irc)) then
                changed=.true.

                !write(*,*)myname,'Y-',e%pos%pos(iy%ind),iterations,bc,brc

             end if
             if (irc.ne.0) then
                write(*,*) myname,'Error return from decrementPosition.',irc
                return
             end if
             inside=.false.
          else if (jtc.eq.-jtlc) then ! increase y
             if (ncf_incrementDimension(e,iy,yok,1,1, irc)) then
                changed=.true.


                !write(*,*)myname,'Y+',e%pos%pos(iy%ind),iterations,tc,tlc

             end if
             if (irc.ne.0) then
                write(*,*) myname,'Error return from incrementPosition.',irc
                return
             end if
             inside=.false.
          end if
          if (jlc.eq.-jlbc) then  ! decrease x
             if (ncf_decrementDimension(e,ix,xok,1,0, irc)) then
                changed=.true.

                !write(*,*)myname,'X-',e%pos%pos(ix%ind),iterations,lc,lbc

             end if
             if (irc.ne.0) then
                write(*,*) myname,'Error return from decrementPosition.',irc
                return
             end if
             inside=.false.
          else if (jrc.eq.-jrtc) then ! increase x
             if (ncf_incrementDimension(e,ix,xok,1,1, irc)) then
                changed=.true.

                !write(*,*)myname,'X+',e%pos%pos(ix%ind),iterations,rc,rtc

             end if
             if (irc.ne.0) then
                write(*,*) myname,'Error return from incrementPosition.',irc
                return
             end if
             inside=.false.
          end if
          if (inside) then       ! we are inside the cell
             biok=.true.
             bdone=.true.
             tbc=tc+bc
             lrc=lc+rc
             fc=(tbc*lrc)
             fc=max(fc,1.0D-10)
             if (abs(fc).lt.1.0D-10) then
                call printPos(e%pos)
                write(*,*) myname,'Invalid grid:',rc,tc,lc,bc,fc
                irc=957
                return
             end if
             wgt%w(1)=lc*tc/fc ! bottom right
             wgt%w(2)=lc*bc/fc ! top right
             wgt%w(3)=rc*bc/fc ! top left
             wgt%w(4)=rc*tc/fc ! bottom left
             ! store position vector
             do ii=1,wgt%nweight
                if (wgt%pos(ii)%maxdim .lt. e%pos%nrdim) then ! sufficient dimensions?
                   call allocatePos(e%pos%nrdim,wgt%pos(ii),irc)
                   if (irc.ne.0) then
                      write(*,*) myname,'Error return from allocatePos.',irc
                      return
                   end if
                else
                   wgt%pos(ii)%nrdim=e%pos%nrdim
                end if
                do jj=1,wgt%pos(ii)%nrdim
                   wgt%pos(ii)%pos(jj)=0
                end do
             end do
             wgt%pos(1)%pos(ix%ind)=+1 ! bottom right
             wgt%pos(2)%pos(iy%ind)=+1 ! top right
             wgt%pos(3)%pos(ix%ind)=-1 ! top left
             wgt%pos(4)%pos(iy%ind)=-1 ! bottom left (origo)
             ! write(*,'(X,A,6(F10.3,X))') 'Search done:',xs(1,1),xs(2,2),xf,ys(1,1),ys(2,2),yf
             if (bdeb) write(*,*)"XYSEARCH Done:",wgt%w
          else if (.not.changed) then ! not inside + no valid step
             if (bdeb) then
                write(*,'(X,A,6(F10.3,X))') 'Search fail:',xs(1,1),xs(2,2),xf,ys(1,1),ys(2,2),yf
             end if
             bdone=.true.
             biok=.false.
             return
          end if
       end if
    end do
    return
  end subroutine interpolate2D

  subroutine printWeight(wgt)
    implicit none
    type(weight) :: wgt
    integer ii
    CHARACTER*12 MYNAME
    DATA MYNAME /'printWeight'/
    do jj=1,wgt%nweight
       write(*,*) 'printWeight Weight:',jj,wgt%w(jj)
       do ii=1,wgt%pos(jj)%nrdim
          write(*,*) 'printWeight      Pos:',ii,wgt%pos(jj)%pos(ii)
       end do
    end do
  end subroutine printWeight

  subroutine printPos(pos)
    implicit none
    type(position) :: pos
    integer ii
    CHARACTER*12 MYNAME
    DATA MYNAME /'printPos'/
    if (pos%nrdim .gt.0) then
       do ii=1,pos%nrdim
          write(*,'(X,A," (",I3,")=",I8,"  (",I4,I4," )")') 'printPos',ii,pos%pos(ii),pos%sta(ii),pos%lim(ii)
       end do
    else
       write(*,*)'printPos: Position has no dimensions!'
    end if
    return
  end subroutine printPos


  character*6 function gettype(type)
    integer type
    if (type.eq.nf_char) then
       gettype="char  "
    elseif (type.eq.nf_int1) then
       gettype="int1  "
    elseif (type.eq.nf_int2) then
       gettype="int2  "
    elseif (type.eq.nf_int) then
       gettype="int   "
    elseif (type.eq.nf_real) then
       gettype="real  "
    elseif (type.eq.nf_double) then
       gettype="double"
    else
       gettype="any   "
    end if
    return
  end function gettype

  real function getDist(LATA,LONA,LATB,LONB)
    IMPLICIT NONE
    SAVE
    REAL   LONA,LATA,LONB,LATB
    REAL   CDIFF
    real sindeg, cosdeg,acosdeg
    external sindeg,cosdeg,acosdeg
    CHARACTER*8 MYNAME 
    DATA MYNAME /'getDist'/
    CDIFF=SINDEG(LATA)*SINDEG(LATB)+COSDEG(LATA)*COSDEG(LATB)*COSDEG(LONA-LONB)
    CDIFF=MAX(-1.0D0,MIN(1.0D0,CDIFF)) ! handle truncation errors
    getDist=ABS(ACOSDEG(CDIFF))
    RETURN
  END function getDist


  logical function getFootIndex(f,i,s,latc,lonc,diam,irc)
    implicit none
    type(inventory), pointer    :: f
    type(dimensionOrder), pointer        :: i
    type(dimensionOrder), pointer        :: s
    real latc
    real lonc
    real diam ! diameter in degrees
    integer irc
    integer dn(2),ii
    logical biok
    real latx,lonx,dist
    type(dimension), pointer :: d=>null(),e=>null()
    ! find increment distance in each dimension direction
    biok=.true.
    latc=ncf_valuePosition(f%latid,irc)
    if (irc.ne.0) then
       write(*,*) myname,'Error return from valuePos.',irc
       return
    end if
    lonc=ncf_valuePosition(f%lonid,irc)
    if (irc.ne.0) then
       write(*,*) myname,'Error return from valuePos.',irc
       return
    end if
    do ii=1,i%nrdim ! must be 2 (checked earlier)
       d=>getdim(i,ii)
       e=>getdim(s,ii)
       if (d%ind .ne. e%ind) then
          write(*,*)'System error:',d%ind,e%ind
          irc=845
          return
       end if
       e%sta=f%pos%pos(e%ind)
       if (biok) then
          biok=ncf_incrementDimension(f,d,biok,1,0,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from incrementDimension.',irc
             return
          end if
       end if
       if (biok) then
          latx=ncf_valuePosition(f%latid,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from valuePos.',irc
             return
          end if
          lonx=ncf_valuePosition(f%lonid,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from valuePos.',irc
             return
          end if
          biok=ncf_decrementDimension(f,d,biok,1,0,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from decrementPos.',irc
             return
          end if
          dist=2.0D0*getDist(latc,lonc,latx,lonx)
          dn(ii)=max(1,1+int(diam/max(dist,1.0D-10)))
       end if
    end do
    ! determine lower left corner
    if (biok) then
       do ii=1,i%nrdim ! must be 2 (checked earlier)
          d=>getdim(i,ii)
          biok=ncf_decrementDimension(f,d,biok,dn(ii),0,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from decrementPos.',irc
             return
          end if
          e=>getdim(s,ii)
          e%sta=e%sta-dn(ii)
          e%lim=e%sta+2*dn(ii)
          if (e%sta.lt.1.or.e%lim.gt.f%pos%lim(e%ind)) biok=.false.
       end do
    end if
    !    call printIndex(s)
    getFootIndex=biok
  end function getFootIndex

  logical function incrementFoot(f,i,latc,lonc,diam,irc)
    implicit none
    type(inventory), pointer    :: f
    type(dimensionOrder), pointer        :: i
    real latc
    real lonc
    real diam
    logical reset
    integer irc
    real latx,lonx,dist
    logical bdone
    CHARACTER*14 MYNAME
    DATA MYNAME /'incrementFoot'/
    bdone=.false.
    do while (.not. bdone)
       if (ncf_increment(f,i,irc)) then
          ! call printIndex(i)
          ! call printPos(f%pos)
          latx=ncf_valuePosition(f%latid,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from valuePos (latid).',irc
             return
          end if
          lonx=ncf_valuePosition(f%lonid,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from valuePos (lonid).',irc
             return
          end if
          dist=2.0D0*getDist(latc,lonc,latx,lonx)
          if (dist.lt.diam) then ! grid wihin footprint...
             bdone=.true.
          end if
          incrementFoot=.true.
       else !     no more grid points to check
          incrementFoot=.false.
          bdone=.true.
       end if
       if (irc.ne.0) then
          write(*,*) myname,'Error return from incrementPos.',irc
          return
       end if
    end do
  end function incrementFoot
  real function s1970(yy,mm,dd,hh,mi,sec)
    implicit none
    integer YY
    integer mm
    integer dd
    integer hh
    integer mi
    real sec
    !     returns "seconds since 1970-01-01 00:00:00 +00:00"
    real days
    !     yy=1970
    !     mm=01
    !     dd=01
    !     hh=00
    !     mi=00
    !     sec=0.0D0
    !     call jd2000(days,yy,mm,dd,hh,mi,sec)
    !     write(*,*) 'S1970 1970-reference in J2000:',days!  -10957.000000000000 
    call jd2000(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    days = days + 10957.0D0   ! convert to days since reference
    S1970=days*86400.0D0      ! convert to seconds
    return
  end function s1970
  real function j2000(sec)
    implicit none
    real sec
    j2000=sec/86400.0D0 - 10957.0D0  ! convert from seconds to days
    return
  end function j2000
  real function j1970(days)
    implicit none
    real days
    j1970=(10957.0D0+days)*86400.0D0 ! convert from days to seconds
    return
  end function j1970

  subroutine readfilter (crep,info,xml,nfilter,irc)
    implicit none
    type(report), pointer      :: crep
    type(XML_PARSE)            :: info
    type(xmltype)              :: xml
    type(filter), pointer      :: nfilter
    integer :: irc
    CHARACTER*12 MYNAME
    DATA MYNAME /'readFilter'/
    ! read until xml%endtag is reached
    DEFINE: do while(.not. (trim(xml%tag).eq."filter".and.xml%endtag))
       call xml_get( info, xml%tag, xml%starttag, xml%endtag, xml%attribs, &
            & xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(info) ) exit DEFINE
       if (bdeb) write(*,*)myname,'Found DEFINE xml%tag:',&
            & trim(xml%tag),xml%starttag,xml%endtag
       if (trim(xml%tag).eq."slice") then
          call defineSlice(xml,info,crep,nfilter,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from defineSlice.',irc
             return
          end if
       else if (trim(xml%tag).eq."cylinder") then
          call defineCylinder(xml,info,crep,nfilter,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from defineCylinder.',irc
             return
          end if
       else if (trim(xml%tag).eq."polygon") then
          call definePolygon(xml,info,crep,nfilter,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from definePolygon.',irc
             return
          end if
       else if (trim(xml%tag).eq."polyline") then
          call definePolyline(xml,info,crep,nfilter,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from definePolyline.',irc
             return
          end if
       else if (trim(xml%tag).eq."duct") then
          call defineDuct(xml,info,crep,nfilter,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from defineDuct.',irc
             return
          end if
       else if (trim(xml%tag).eq."value") then
          call defineValue(xml,info,crep,nfilter,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from defineValue.',irc
             return
          end if
       else if (trim(xml%tag).eq."string") then
          call defineString(xml,info,crep,nfilter,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from defineString.',irc
             return
          end if
       else if (trim(xml%tag).eq."dimension") then
          call defineDimension(xml,info,crep,nfilter,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from defineDimension.',irc
             return
          end if
       else if (trim(xml%tag).ne."filter") then
          write(*,*)myname,'Unknown xml%tag H:',trim(xml%tag)
          irc=934
          return
       end if
    end do DEFINE
    return
  end subroutine READFILTER

  subroutine defineSlice(xml,info,crep,nfilter,irc)
    implicit none
    type(xmltype)              :: xml
    type(XML_PARSE)            :: info
    type(report), pointer      :: crep
    type(filter), pointer      :: nfilter
    integer :: irc
    integer, external :: length
    integer :: len2,ii
    character*700 :: buff700
    logical :: bok
    CHARACTER*16 MYNAME
    DATA MYNAME /'defineSlice'/
    nfilter%mpar=100
    if (allocated(nfilter%lpar)) deallocate(nfilter%lpar)
    if (allocated(nfilter%npar)) deallocate(nfilter%npar)
    if (allocated(nfilter%rpar)) deallocate(nfilter%rpar)
    allocate(nfilter%lpar(0:nfilter%mpar),nfilter%npar(nfilter%mpar),&
         & nfilter%rpar(nfilter%mpar),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    nfilter%lpar(:)=.false.
    nfilter%npar(:)=0
    nfilter%rpar(:)=0.0D0
    if (xml%starttag) then
       nfilter%type=flt_slice ! slice
       nfilter%lpar(0)=.true. ! need initialisation
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=125
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."volume") then
             if (buff700(1:len2).eq."frontside") then
                nfilter%inside = .true. ! front
             else if (buff700(1:len2).eq."backside") then
                nfilter%inside = .false. ! back
             else
                write(*,*)myname,'Unknown setting:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
             end if
             !                       else if (trim(xml%attribs(1,ii)).eq."surface") then
             !                          nfilter%var250=buff700(1:len2)
             !                          nfilter%lpar(100)=.true.
          else
             write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=803
             return
          end if
       end do
    end if
    ! read until xml%endtag is reached
    SLICE: do while(.not. (trim(xml%tag).eq."slice".and.xml%endtag))
       call xml_get( info, xml%tag, xml%starttag, xml%endtag, xml%attribs, &
            & xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(info) ) exit SLICE
       if (bdeb) write(*,*)myname,'Found DEFINE/SLICE xml%tag:',&
            & trim(xml%tag),xml%starttag,xml%endtag
       if (trim(xml%tag).eq."origo") then
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=126
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(1)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lat:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(1)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(2)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lon:',buff700(1:len2)
                   return
                end if
                nfilter%lpar(2)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."altitude") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(3)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',buff700(1:len2)
                   return
                end if
                nfilter%lpar(3)=.true.
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//trim(xml%attribs(1,ii))
                irc=804
                return
             end if
          end do
       else if (trim(xml%tag).eq."abscissa") then
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=127
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(5)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-abs-lat:',buff700(1:len2)
                   return
                end if
                nfilter%lpar(5)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(6)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-abs-lon:',buff700(1:len2)
                   return
                end if
                nfilter%lpar(6)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."altitude") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(7)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-abs-alt:',buff700(1:len2)
                   return
                end if
                nfilter%lpar(7)=.true.
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//trim(xml%attribs(1,ii))
                irc=805
                return
             end if
          end do
       else if (trim(xml%tag).eq."ordinate") then
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=128
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(11)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ord-lat:',buff700(1:len2)
                   return
                end if
                nfilter%lpar(10)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(12)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ord-lon:',buff700(1:len2)
                   return
                end if
                nfilter%lpar(11)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."altitude") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(13)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ord-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(12)=.true.
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
                irc=806
                return
             end if
          end do
       else if (trim(xml%tag).eq."normal") then
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=129
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(15)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-nor-lat:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(15)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(16)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-nor-lon:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(16)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."altitude") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(17)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-nor-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(17)=.true.
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
                irc=807
                return
             end if
          end do
       else if (trim(xml%tag).ne."slice") then
          write(*,*)myname,'Unknown xml%tag C:',trim(xml%tag)
          irc=931
          return
       end if
    end do SLICE

  end subroutine defineSlice

  subroutine defineCylinder(xml,info,crep,nfilter,irc)
    implicit none
    type(xmltype)              :: xml
    type(XML_PARSE)            :: info
    type(report), pointer      :: crep
    type(filter), pointer      :: nfilter
    integer :: irc
    integer, external :: length
    integer :: len2,ii
    character*700 :: buff700
    logical :: bok
    CHARACTER*16 MYNAME
    DATA MYNAME /'defineCylinder'/
    nfilter%mpar=100
    if (allocated(nfilter%lpar)) deallocate(nfilter%lpar)
    if (allocated(nfilter%npar)) deallocate(nfilter%npar)
    if (allocated(nfilter%rpar)) deallocate(nfilter%rpar)
    allocate(nfilter%lpar(0:nfilter%mpar),nfilter%npar(nfilter%mpar),nfilter%rpar(nfilter%mpar),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    nfilter%lpar(:)=.false.
    nfilter%npar(:)=0
    nfilter%rpar(:)=0.0D0
    if (xml%starttag) then
       nfilter%type=flt_cylinder ! cylinder
       nfilter%lpar(0)=.true. ! need initialisation
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=130
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
                write(*,*)myname,'Unknown setting:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
             end if
             !                       else if (trim(xml%attribs(1,ii)).eq."surface") then
             !                          nfilter%var250=buff700(1:len2)
             !                          nfilter%lpar(100)=.true.
          else
             write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=808
             return
          end if
       end do
    end if
    ! read until xml%endtag is reached
    CYLINDER: do while(.not. (trim(xml%tag).eq."cylinder".and.xml%endtag))
       call xml_get( info, xml%tag, xml%starttag, xml%endtag, xml%attribs, xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(info) ) exit CYLINDER
       if (bdeb) write(*,*)myname,'Found DEFINE/CYLINDER xml%tag:',&
            & trim(xml%tag),xml%starttag,xml%endtag
       if (trim(xml%tag).eq."center") then
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=131
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(1)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lat:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(1)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(2)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lon:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(2)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."altitude") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(3)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(3)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."width") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(4)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(4)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."height") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(5)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(5)=.true.
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
                irc=809
                return
             end if
          end do
       else if (trim(xml%tag).ne."cylinder") then
          write(*,*)myname,'Unknown xml%tag D:',trim(xml%tag)
          irc=932
          return
       end if
    end do CYLINDER
    if (crep%lcen) then
       crep%sx=crep%sx + cosdeg(nfilter%rpar(1))*cosdeg(nfilter%rpar(2))
       crep%sy=crep%sy + cosdeg(nfilter%rpar(1))*sindeg(nfilter%rpar(2))
       crep%sz=crep%sz + sindeg(nfilter%rpar(1))
       crep%sd=max(crep%sd,(nfilter%rpar(4)*0.5D0*360.0D0/40.0D3)) ! in degrees
       crep%sn=crep%sn + 1.0D0
    else
       crep%lcen=.true.
       crep%sx=cosdeg(nfilter%rpar(1))*cosdeg(nfilter%rpar(2))
       crep%sy=cosdeg(nfilter%rpar(1))*sindeg(nfilter%rpar(2))
       crep%sz=sindeg(nfilter%rpar(1))
       crep%sd=nfilter%rpar(4)*0.5D0*360.0D0/40.0D3 ! in degrees
       crep%sn=1.0D0
    end if
  end subroutine defineCylinder

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
    real minlat,maxlat,minlon,maxlon
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
             if (crep%lcen) then
                crep%sx=crep%sx + cosdeg(lat)*cosdeg(lon)
                crep%sy=crep%sy + cosdeg(lat)*sindeg(lon)
                crep%sz=crep%sz + sindeg(lat)
                crep%sn=crep%sn + 1.0D0
             else
                crep%lcen=.true.
                crep%sx=cosdeg(lat)*cosdeg(lon)
                crep%sy=cosdeg(lat)*sindeg(lon)
                crep%sz=sindeg(lat)
                crep%sn=1.0D0
             end if
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
    real minlat,maxlat,minlon,maxlon
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
             if (crep%lcen) then
                crep%sx=crep%sx + cosdeg(lat)*cosdeg(lon)
                crep%sy=crep%sy + cosdeg(lat)*sindeg(lon)
                crep%sz=crep%sz + sindeg(lat)
                crep%sn=crep%sn + 1.0D0
             else
                crep%lcen=.true.
                crep%sx=cosdeg(lat)*cosdeg(lon)
                crep%sy=cosdeg(lat)*sindeg(lon)
                crep%sz=sindeg(lat)
                crep%sn=1.0D0
             end if
          else
             write(*,*)'Ignoring mask-node missing LAT/LON.'
          end if
       else if (trim(xml%tag).ne."polyline") then
          write(*,*)myname,'Unknown xml%tag E:',trim(xml%tag)
          irc=933
          return
       end if
    end do POLYLINE
    if (nfilter%mpar .lt. 3) then
       write(*,*) myname,'Too few nodes in polyline.',nfilter%mpar
       irc=458
       return
    end if
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

  subroutine defineDuct(xml,info,crep,nfilter,irc)
    implicit none
    type(xmltype)              :: xml
    type(XML_PARSE)            :: info
    type(report), pointer      :: crep
    type(filter), pointer      :: nfilter
    integer :: irc
    integer, external :: length
    integer :: len2,ii
    character*700 :: buff700
    logical :: bok
    CHARACTER*16 MYNAME
    DATA MYNAME /'defineDuct'/
    nfilter%mpar=100
    if (allocated(nfilter%lpar)) deallocate(nfilter%lpar)
    if (allocated(nfilter%npar)) deallocate(nfilter%npar)
    if (allocated(nfilter%rpar)) deallocate(nfilter%rpar)
    allocate(nfilter%lpar(0:nfilter%mpar),nfilter%npar(nfilter%mpar),&
         & nfilter%rpar(nfilter%mpar),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    nfilter%lpar(:)=.false.
    nfilter%npar(:)=0
    nfilter%rpar(:)=0.0D0
    if (xml%starttag) then
       nfilter%type=flt_duct ! duct
       nfilter%lpar(0)=.true. ! need initialisation
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=134
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
                write(*,*)myname,'Unknown setting:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
             end if
             !                       else if (trim(xml%attribs(1,ii)).eq."surface") then
             !                          nfilter%var250=buff700(1:len2)
             !                          nfilter%lpar(100)=.true.
          else
             write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=810
             return
          end if
       end do
    end if
    ! read until xml%endtag is reached
    DUCT: do while(.not. (trim(xml%tag).eq."duct".and.xml%endtag))
       call xml_get( info, xml%tag, xml%starttag, xml%endtag, xml%attribs, &
            & xml%no_attribs, xml%data, xml%no_data )
       if ( .not. xml_ok(info) ) exit DUCT
       if (bdeb) write(*,*)myname,'Found DEFINE/DUCT xml%tag:',trim(xml%tag),&
            & xml%starttag,xml%endtag
       if (trim(xml%tag).eq."start") then
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=135
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(1)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lat:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(1)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(2)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lon:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(2)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."altitude") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(3)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(3)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."width") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(4)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(4)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."height") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(5)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(5)=.true.
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
                irc=811
                return
             end if
          end do
       else if (trim(xml%tag).eq."stop") then
          nfilter%lpar(2)=.true.
          do ii=1,xml%no_attribs
             buff700=xml%attribs(2,ii)
             call replaceENV(buff700,700,bok,irc)
             if (.not.bok) irc=136
             if (irc.ne.0) then
                write(*,*)myname,'Error return from replaceEnv.',irc
                return
             end if
             len2=length(buff700,700,10)    
             if (trim(xml%attribs(1,ii)).eq."lat") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(11)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lat:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(11)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."lon") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(12)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-lon:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(12)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."altitude") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(13)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(13)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."width") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(14)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(14)=.true.
             elseif (trim(xml%attribs(1,ii)).eq."height") then
                read(buff700(1:len2),*,iostat=irc) nfilter%rpar(15)
                if (irc.ne.0) then
                   write(*,*)myname,'Unable to read slice-ori-alt:',&
                        & buff700(1:len2)
                   return
                end if
                nfilter%lpar(15)=.true.
             else
                write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                     & trim(xml%attribs(1,ii))
                irc=812
                return
             end if
          end do
       else if (trim(xml%tag).ne."duct") then
          write(*,*)myname,'Unknown xml%tag G:',trim(xml%tag)
          irc=933
          return
       end if
    end do DUCT
  end subroutine defineDuct

  subroutine defineValue(xml,info,crep,nfilter,irc)
    implicit none
    type(xmltype)              :: xml
    type(XML_PARSE)            :: info
    type(report), pointer      :: crep
    type(filter), pointer      :: nfilter
    integer :: irc
    integer, external :: length
    integer :: len2,ii
    character*700 :: buff700
    logical :: bok
    CHARACTER*16 MYNAME
    DATA MYNAME /'defineValue'/
    nfilter%mpar=100
    if (allocated(nfilter%lpar)) deallocate(nfilter%lpar)
    if (allocated(nfilter%npar)) deallocate(nfilter%npar)
    if (allocated(nfilter%rpar)) deallocate(nfilter%rpar)
    allocate(nfilter%lpar(0:nfilter%mpar),nfilter%npar(nfilter%mpar),&
         & nfilter%rpar(nfilter%mpar),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    nfilter%lpar(:)=.false.
    nfilter%npar(:)=0
    nfilter%rpar(:)=0.0D0
    if (xml%starttag) then
       nfilter%type=flt_value ! value
       nfilter%lpar(0)=.true. ! need initialisation
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=137
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."field") then
             nfilter%var250=buff700(1:len2)
             nfilter%ipar=.true.
          else if (trim(xml%attribs(1,ii)).eq."exp") then
             nfilter%exp700=buff700
             nfilter%lene=len2
          else if (trim(xml%attribs(1,ii)).eq."min") then
             read(buff700(1:len2),*,iostat=irc) nfilter%rpar(1)
             if (irc.ne.0) then
                write(*,*)myname,'Unable to read slice-ori-alt:',&
                     & buff700(1:len2)
                return
             end if
             nfilter%lpar(1)=.true.
          else if (trim(xml%attribs(1,ii)).eq."max") then
             read(buff700(1:len2),*,iostat=irc) nfilter%rpar(2)
             if (irc.ne.0) then
                write(*,*)myname,'Unable to read slice-ori-alt:',&
                     & buff700(1:len2)
                return
             end if
             nfilter%lpar(2)=.true.
          else if (trim(xml%attribs(1,ii)).eq."value") then
             read(buff700(1:len2),*,iostat=irc) nfilter%rpar(3)
             if (irc.ne.0) then
                write(*,*)myname,'Unable to read slice-ori-alt:',&
                     & buff700(1:len2)
                return
             end if
             nfilter%lpar(3)=.true.
          else if (trim(xml%attribs(1,ii)).eq."tolerance") then
             read(buff700(1:len2),*,iostat=irc) nfilter%rpar(4)
             if (irc.ne.0) then
                write(*,*)myname,'Unable to read slice-ori-alt:',&
                     & buff700(1:len2)
                return
             end if
             nfilter%lpar(4)=.true.
          else
             write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=813
             return
          end if
       end do
    end if
  end subroutine defineValue

  subroutine defineString(xml,info,crep,nfilter,irc)
    implicit none
    type(xmltype)              :: xml
    type(XML_PARSE)            :: info
    type(report), pointer      :: crep
    type(filter), pointer      :: nfilter
    integer :: irc
    integer, external :: length
    integer :: len2,ii
    character*700 :: buff700
    logical :: bok
    CHARACTER*16 MYNAME
    DATA MYNAME /'defineString'/
    nfilter%mpar=100
    if (allocated(nfilter%lpar)) deallocate(nfilter%lpar)
    if (allocated(nfilter%npar)) deallocate(nfilter%npar)
    if (allocated(nfilter%rpar)) deallocate(nfilter%rpar)
    allocate(nfilter%lpar(0:nfilter%mpar),nfilter%npar(nfilter%mpar),&
         & nfilter%rpar(nfilter%mpar),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc
       return
    end if
    nfilter%lpar(:)=.false.
    nfilter%npar(:)=0
    nfilter%rpar(:)=0.0D0
    if (xml%starttag) then
       nfilter%type=flt_string ! string
       nfilter%lpar(0)=.true. ! need initialisation
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=138
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."field") then
             nfilter%var250=buff700(1:len2)
             nfilter%ipar=.true.
          else if (trim(xml%attribs(1,ii)).eq."target") then
             nfilter%spar250=buff700
             call chop0(nfilter%spar250,250)
             nfilter%lens=length(nfilter%spar250,250,3)
             nfilter%lpar(1)=.true.
          else if (trim(xml%attribs(1,ii)).eq."dimension") then
             nfilter%sdim80=buff700(1:len2)
             nfilter%lpar(2)=.true.
          else
             write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=813
             return
          end if
       end do
    end if
  end subroutine defineString

  subroutine defineDimension(xml,info,crep,nfilter,irc)
    implicit none
    type(xmltype)              :: xml
    type(XML_PARSE)            :: info
    type(report), pointer      :: crep
    type(filter), pointer      :: nfilter
    integer :: irc
    integer, external :: length
    integer :: len2,ii
    character*700 :: buff700
    logical :: bok
    CHARACTER*16 MYNAME
    DATA MYNAME /'defineDimension'/
    nfilter%mpar=100
    if (allocated(nfilter%lpar)) deallocate(nfilter%lpar)
    if (allocated(nfilter%npar)) deallocate(nfilter%npar)
    if (allocated(nfilter%rpar)) deallocate(nfilter%rpar)
    allocate(nfilter%lpar(0:nfilter%mpar),nfilter%npar(0:nfilter%mpar),&
         & nfilter%rpar(nfilter%mpar),stat=irc)
    if (irc.ne.0) then
       write(*,*) myname,'Unable to allocate filter-parameters.',irc,&
            & nfilter%mpar
       return
    end if
    nfilter%lpar(:)=.false.
    nfilter%npar(:)=0
    nfilter%rpar(:)=0.0D0
    if (xml%starttag) then
       nfilter%type=flt_dimension ! dimension
       nfilter%lpar(0)=.true. ! need initialisation
       do ii=1,xml%no_attribs
          buff700=xml%attribs(2,ii)
          call replaceENV(buff700,700,bok,irc)
          if (.not.bok) irc=139
          if (irc.ne.0) then
             write(*,*)myname,'Error return from replaceEnv.',irc
             return
          end if
          len2=length(buff700,700,10)    
          if (trim(xml%attribs(1,ii)).eq."field") then
             nfilter%var250=buff700(1:len2)
             nfilter%ipar=.true.
          else if (trim(xml%attribs(1,ii)).eq."name") then
             nfilter%sdim80=buff700(1:len2)
             nfilter%lpar(4)=.true.
          else if (trim(xml%attribs(1,ii)).eq."min") then
             read(buff700(1:len2),*,iostat=irc) nfilter%npar(1)
             if (irc.ne.0) then
                write(*,*)myname,'Unable to read dim-min:',buff700(1:len2)
                return
             end if
             nfilter%lpar(1)=.true.
          else if (trim(xml%attribs(1,ii)).eq."max") then
             read(buff700(1:len2),*,iostat=irc) nfilter%npar(2)
             if (irc.ne.0) then
                write(*,*)myname,'Unable to read dim-max:',buff700(1:len2)
                return
             end if
             nfilter%lpar(2)=.true.
          else
             write(*,*)myname,'Unknown attribute:',trim(xml%tag)//':'//&
                  & trim(xml%attribs(1,ii))
             irc=813
             return
          end if
       end do
    end if
  end subroutine defineDimension

  subroutine readDataFile(file,bok,irc)
    !use parse
    !use shape
    implicit none
    type(filetype) :: file
    logical :: bok
    integer :: irc
    !
    type(Dimension), pointer :: sd => null()
    type(variable), pointer :: v=>null()
    type(report), pointer :: crep=>null()
    type(parameter), pointer :: cpar=>null(),bpar=>null()
    integer :: lenc,lenl,lenv,ii,jj,kk
    logical :: first
    character*250 :: crc250
    CHARACTER*14 MYNAME 
    DATA MYNAME /'readdatafile'/
    !
    if (bok) then
       file%rok(1)=file%rok(1)+1
    else
       file%rrm(1)=file%rrm(1)+1 ! unable to open file
    end if
    if (bok) then
       call ncf_readInventory(file%ref,bok,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from readInventory.',irc
          return
       end if
       if (bok) then
          file%rok(2)=file%rok(2)+1
       else
          file%rrm(2)=file%rrm(2)+1 ! unable to read inventory
       end if
    end if
    !
    ! check that latitude, longitude and altitude are present...
    !
    if (bok) then
       write(*,*)myname,'Checking inventory.'
       call ncf_checkCoordinates(file%ref,bok,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from checkCoordinates.',irc
          return
       end if
       if (bok) then
          file%rok(3)=file%rok(3)+1
       else
          file%rrm(3)=file%rrm(3)+1 ! contents error
       end if
       if (file%ignorealt) then
          write(*,*)myname,'*** WARNING Ignoring Altitude.'
          nullify(file%ref%altid)
       end if
    end if
    !
    ! make indexes
    !
    if (bok) then
       call ncf_clearDimOrder(file%refLatDO)
       call ncf_clearDimOrder(file%refLonDO)
       call ncf_clearDimOrder(file%refAltDO)
       call ncf_clearDimOrder(file%refTimDO)
       call ncf_clearDimOrder(file%refAllDO)
       call ncf_clearDimOrder(file%refStrDO)
       call ncf_clearDimOrder(file%refDimDO)
       file%refLatDO=>ncf_makeDimOrder(file%ref%latid,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from makeDimOrder (lat).',irc
          return
       end if
       file%refLonDO=>ncf_makeDimOrder(file%ref%lonid,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from makeDimOrder (lon).',irc
          return
       end if
       file%refAltDO=>ncf_makeDimOrder(file%ref%altid,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Missing: altitude (ignoring).',file%ignorealt
          irc=0
       end if
       file%refTimDO=>ncf_makeDimOrder(file%ref%timid,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from makeDimOrder (time).',irc
          return
       end if
       file%refStrDo=>ncf_newDimOrder(file%ref,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from newDimOrder (str).',irc
          return
       end if
       file%refDimDo=>ncf_newDimOrder(file%ref,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from newDimOrder (dim).',irc
          return
       end if
       !     file%refAllDO=>getAllIndex(file%ref,irc)
       ! remove any lat/lon dimensions from altitude
       call ncf_removeDimOrder(file%refAltDO,file%refLatDO)
       call ncf_removeDimOrder(file%refAltDO,file%refLonDO)
       !
       ! make index of inner dimensions (lat/lon dimensions)
       !
       file%refLatLonAltDO => ncf_copyDimOrder(file%refLatDO,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from copyDimOrder.',irc
          return
       end if
       call ncf_addDimOrder(file%refLatLonAltDO,file%refLonDO,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from addDimOrder.',irc
          return
       end if
       call ncf_addDimOrder(file%refLatLonAltDO,file%refAltDO,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from addDimOrder.',irc
          return
       end if
    end if
    !
    ! read non-parameter fields into memory
    !
    if (bok) then
       ! read reference latitude field into memory
       v=>file%ref%latid
       bbok=.true.
       if (v%lend.le.0) then ! not in memory
          write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
          call ncf_readRealData(v,bbok,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from latid-readData.',irc
             return
          end if
       end if

       ! read reference longitude field into memory
       v=>file%ref%lonid
       if (v%lend.le.0) then ! not in memory
          write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
          call ncf_readRealData(v,bbok,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from lonid-readData.',irc
             return
          end if
       end if
       if (associated(file%ref%altid)) then
          ! read reference altitude field into memory
          v=>file%ref%altid
          if (v%lend.le.0) then ! not in memory
             write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
             call ncf_readRealData(v,bbok,irc)
             if (irc.ne.0) then
                write(*,*)myname,'Error return from altid-readData.',irc
                return
             end if
          end if
       end if
       ! read reference time field into memory
       v=>file%ref%timid
       if (v%lend.le.0) then ! not in memory
          write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
          call ncf_readData(v,bbok,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from timid-readData.',irc
             return
          end if
          a => ncf_getAttribute(v,"units",irc)
          if (associated(a)) then
             buff250=ncf_getAttributeText(a)
             call chop0(buff250,250)
             lenb=length(buff250,250,10)
             !write(*,*)myname,"Analysis is in "//buff250(1:lenb), t2000
             if (buff250(1:4).eq."days") then ! convert days to seconds
                timfact=86400.0D0
             end if
          end if
       end if
       ! read analysis time field into memory
       v=>file%ref%tanid
       if (associated(v)) then
          if (v%lend.le.0) then ! not in memory
             write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
             call ncf_readData(v,bbok,irc)
             if (irc.ne.0) then
                write(*,*)myname,'Error return from tanid-readData.',irc
                return
             end if
          end if
          a => ncf_getAttribute(v,"units",irc)
          if (associated(a)) then
             buff250=ncf_getAttributeText(a)
             call chop0(buff250,250)
             lenb=length(buff250,250,10)
             !write(*,*)myname,"Analysis is in "//buff250(1:lenb), t2000
             if (buff250(1:4).eq."days") then ! convert days to seconds
                tanfact=86400.0D0
             end if
          end if
       end if
    end if
    if (bok .and. associated(file%ref%xcoid)) then
       ! read x into memory
       v=>file%ref%xcoid
       bok=associated(v)
       write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
       call ncf_readData(v,bok,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from latid-readData.',irc
          return
       end if
    end if
    if (bok .and. associated(file%ref%ycoid)) then
       ! read y into memory
       v=>file%ref%ycoid
       bok=associated(v)
       write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
       call ncf_readData(v,bok,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from latid-readData.',irc
          return
       end if
    end if
    if (bok) then
       ! read gridid into memory
       v=>file%ref%gridid
       if (associated(v)) then
          !bok=associated(v)
          write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
          call ncf_readData(v,bok,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from gridid-readData.',irc
             irc=0
             bok=.false.
             !return
          end if
          !if (bok) then
          !   file%rok(3)=file%rok(3)+1
          !else
          !   file%rrm(3)=file%rrm(3)+1 ! contents error
          !end if
       end if
    end if
    ! read any filter fields into memory
    if (bok) then
       file%refFlt => ncf_newDimOrder(file%ref,irc) ! empty dimOrder
       ! loop over reports, check for variables....
       write(*,*)myname,'Reading filter-variables.'
       cpar=>file%firstparameter%next
       do while (.not.associated(cpar,target=file%lastparameter))
          ! write(*,*)myname,'Reading parameter...'
          crep=>cpar%firstreport%next
          do while (.not.associated(crep,target=cpar%lastreport))
             ! write(*,*)myname,'Reading report...'
             call setActive(crep%roperation,irc)
             if (irc.ne.0) then
                write(*,*)myname,'No mask available.'
                irc=374
                return
             end if
             cfilter=>crep%firstfilter%next
             do while (.not. associated(cfilter,target=crep%lastfilter))
                ! write(*,*)myname,'Reading filter...'
                cfilter%i=>file%ref
                if (cfilter%ipar.and.cfilter%active) then
                   call ncf_checkParameter(file%ref,cfilter%var250,bok,irc)
                   if (irc.ne.0) then
                      write(*,*)myname,'Error return from CHECKPARAMETER.',irc
                      return
                   end if
                   if (bok) then
                      cfilter%v => file%ref%parid
                      first=ncf_variableClean(cfilter%v,irc)
                      if (first) then
                         write(*,*)myname,'Reading variable:',trim(cfilter%var250),&
                              & ' ~> ',cfilter%v%var250(1:cfilter%v%lenv)
                         if (cfilter%type==flt_string) then
                            call ncf_readData(cfilter%v,bok,irc)
                            if (irc.ne.0) then
                               write(*,*)myname,'Error return from readData.',irc
                               return
                            end if
                         else
                            !write(*,*)myname,'Reading variable:',trim(cfilter%var250)
                            call ncf_readRealData(cfilter%v,bok,irc)
                            if (irc.ne.0) then
                               write(*,*)myname,'Error return from readData.',irc
                               return
                            end if
                         end if
                      end if
                      if (associated(cfilter%v)) then
                         if (cfilter%type==flt_string) then
                            cfilter%sdo=>ncf_newDimOrder(file%ref,irc)
                            if (irc.ne.0) then
                               write(*,*)myname,'Unable to create new dimension dimorder.',irc
                               return
                            end if
                            jj=ncf_getDimEntry(file%ref,cfilter%sdim80)
                            if (jj.le.0) then
                               write(*,*)myname,'Invalid dimension.',trim(cfilter%sdim80),jj
                               irc=834
                               return
                            end if
                            sd=>ncf_getInventoryDimension(file%ref,jj,irc)
                            if (irc.ne.0) then
                               write(*,*)myname,'Unable to get dimension.',irc,jj
                               return
                            end if
                            call ncf_addDimOrderDim(cfilter%sdo,sd,irc)
                            if (first) then
                               sd=>ncf_getInventoryDimension(file%ref,jj,irc)
                               if (irc.ne.0) then
                                  write(*,*)myname,'Unable to get dimension.',irc,jj
                                  return
                               end if
                               call ncf_addDimOrderDim(file%refStrDo,sd,irc)
                               if (bdeb) then
                                  write(*,*)myname,">>>>> Filter dimension order:"
                                  call ncf_printDimOrder(cfilter%sdo)
                               end if
                            end if
                         else
                            ! add dimensions to inner dimension...
                            cfilter%sdo=>ncf_makeDimOrder(cfilter%v,irc)
                            if (first) then
                               call ncf_addDimOrder(file%refFlt,cfilter%sdo,irc)
                               if (irc.ne.0) then
                                  write(*,*)myname,'Error return from addDimOrder.',irc
                                  return
                               end if
                            end if
                         end if
                      else if (.not.associated(cfilter%v)) then
                         write(*,*)myname,'Unable to read variable:',trim(cfilter%var250)
                      end if
                   else 
                      write(*,*)myname,'Unable to find variable:',trim(cfilter%var250)
                   end if
                else if (len(trim(cfilter%var250)).gt.0) then
                   write(*,*)myname,'Filter variable ignored:',&
                        & trim(cfilter%var250),cfilter%ipar,cfilter%active
                end if
                if (cfilter%type==flt_dimension) then ! only interested in the dimensions
                   jj=ncf_getDimEntry(file%ref,cfilter%sdim80)
                   if (jj.le.0) then
                      write(*,*)myname,'Invalid dimension.',trim(cfilter%sdim80),jj
                      irc=834
                      return
                   end if
                   sd=>ncf_getInventoryDimension(file%ref,jj,irc)
                   if (irc.ne.0) then
                      write(*,*)myname,'Unable to get dimension.',irc,jj
                      return
                   end if
                   call ncf_addDimOrderDim(file%refDimDo,sd,irc)
                end if
                cfilter=>cfilter%next
             end do
             crep=>crep%next
          end do
          cpar=>cpar%next
       end do
    end if
    if (bok) then
       call ncf_clearDimOrder(file%refIterDO)
       if (file%leni.gt.0) then ! we have a variable
          file%npst=max(1,ncf_getDimEntry(file%ref,file%iter80(1:file%leni)))
          file%i => ncf_getVariable(file%ref,file%iter80(1:file%leni),bbok,irc)
          if (.not.bbok.or.irc.ne.0) then
             file%npst=1
             bbok=.false.
             write(*,*)myname,"Unable to find ITERATION variable:",file%iter80(1:file%leni)
             !irc=347
             !return
          else
             ! read values into memory
             call ncf_readRealData(file%i,bbok,irc)
             if (.not.bbok.or.irc.ne.0) then
                write(*,*)myname,"Unable to read ITERATION variable:",file%iter80(1:file%leni)
                irc=348
                return
             end if
          end if
       else
          file%npst=1
          bbok=.false.
       end if
       if (bbok) then
          file%refIterDO => ncf_makeDimOrder(file%i,irc)
       else
          file%refIterDO => ncf_newDimOrder(file%ref,irc) ! empty dimOrder
       end if
    end if
    !
    ! loop over requested variables, make parid-array and parAll index
    if (bok) then
       write(*,*)myname,'Reading request-variables.'
       call ncf_clearDimOrder(file%refAllDO)
       bok=.false.
       if (file%npar.eq.0) then
          write(*,*)myname,'No parameters requested.'
       else 
          if (allocated(file%elem80))  deallocate(file%elem80)
          if (allocated(file%lenv))  deallocate(file%lenv)
          if (allocated(file%val))  deallocate(file%val)
          if (allocated(file%var))  deallocate(file%var)
          if (allocated(file%par))  deallocate(file%par)
          allocate(file%elem80(file%npar),&
               & file%lenv(file%npar),file%val(file%npar),&
               & file%var(file%npar),file%par(file%npar),stat=irc)
          if (irc.ne.0) then
             write(*,*)myname,'Unable to allocate expr.',irc
             return
          end if
          file%val=0.0D0
       end if
       ii=0
       cpar=>file%firstparameter%next
       do while (.not.associated(cpar,target=file%lastparameter))
          ii=ii+1
          file%par(ii)%ptr=>cpar
          nullify(file%ref%parid)
          bbok=(cpar%hasvar)
          if (bbok) then
             call ncf_checkParameter(file%ref,cpar%par350,bbok,irc)
             if (irc.ne.0) then
                write(*,*)myname,'Error return from checkParameter.',irc
                return
             end if
             if (bbok) then
                file%rok(3)=file%rok(3)+1
             else
                file%rrm(3)=file%rrm(3)+1 ! contents error
             end if
          end if
          lenl=len_trim(cpar%elem80)
          if (lenl.ne.0) then
             file%elem80(ii)=cpar%elem80(1:lenl)
             file%lenv(ii)=lenl
          end if
          ! read field into memory
          if (bbok) then
             v => file%ref%parid
             file%var(ii)%ptr=>v
             if (lenl.eq.0) then
                file%elem80(ii)=v%var250(1:min(80,v%lenv))
                file%lenv(ii)=min(80,v%lenv)
             end if
             a => ncf_getAttribute(v,"accumulation_hours",irc)
             if (associated(a)) then
                buff250=ncf_getAttributeText(a)
                call chop0(buff250,250)
                lenb=length(buff250,250,10)
                read (buff250(1:lenb),*,iostat=irc)hrs ! accumulation hours
                if (irc.ne.0) then
                   write(*,*)myname,"Unable to read attribute 'accumulation_hours':'"&
                        & //buff250(1:lenb)//"'"
                   return
                end if
             else
                hrs=0.0D0
             end if
          end if
          cpar=>cpar%next
       end do
       ! compile expressions and report errors (before reading fields)...
       ii=0
       cpar=>file%firstparameter%next
       do while (.not.associated(cpar,target=file%lastparameter))
          ii=ii+1
          if (cpar%lene.gt.0) then
             file%hasexp=.true.
             write(*,*)myname,"Compiling "//file%elem80(ii)(1:file%lenv(ii))//":'"//cpar%exp700(1:cpar%lene)//"'"
             call parse_parsef(cpar%exp,cpar%exp700(1:cpar%lene),file%elem80,crc250,irc)
             if (irc.ne.0) then
                lenc=length(crc250,250,10)
                write(*,*) crc250(1:lenc)
                write(*,*)myname,'Error return from parse_parsef "',cpar%exp700(1:cpar%lene),'"->',irc
                if (len(file%elem80).le.0) then
                      write(*,*)myname,"No elements defined."
                else
                   write(*,*)myname,"List of elements:"
                   do jj=1,file%npar
                      write(*,*)myname,jj,trim(file%elem80(jj))
                   end do
                end if
                return
             end if
          end if
          ! loop over reports and aux-expressions
          crep=>cpar%firstreport%next
          do while (.not.associated(crep,target=cpar%lastreport))
             do kk=1,crep%ntrg
                if (crep%trgtyp(kk).eq.trg_aux) then
                   if (crep%trglenv(kk).ne.0) then
                      file%hasexp=.true.
                      if (bdeb) write(*,*)myname,"Compiling "//file%elem80(ii)(1:file%lenv(ii))//":'"//&
                           & crep%trgvar700(kk)(1:crep%trglenv(kk))//"'"
                      call parse_parsef(crep%trgexp(kk)%ptr,&
                           & crep%trgvar700(kk)(1:crep%trglenv(kk)),file%elem80,crc250,irc)
                      if (irc.ne.0) then
                         lenc=length(crc250,250,10)
                         write(*,*) crc250(1:lenc)
                         write(*,*)myname,'Error return from parse_parsef "',&
                              & crep%trgvar700(kk)(1:crep%trglenv(kk)),'"->',irc
                         if (len(file%elem80).le.0) then
                            write(*,*)myname,"No elements defined."
                         else
                            write(*,*)myname,"List of elements:"
                            do jj=1,file%npar
                               write(*,*)myname,jj,trim(file%elem80(jj))
                            end do
                         end if
                         return
                      end if
                   end if
                end if
             end do
             crep=>crep%next
          end do
          cpar=>cpar%next
       end do
       ! read fields into memory
       ii=0
       cpar=>file%firstparameter%next
       do while (.not.associated(cpar,target=file%lastparameter))
          ii=ii+1
          v=> file%var(ii)%ptr
          if (associated(v)) then
             bbok=.true.
             ! read parameter field into memory
             if (v%lend.le.0) then ! not in memory
                write(*,*)myname,'Reading variable:',v%var250(1:v%lenv)
                call ncf_readRealData(v,bbok,irc)
                if (irc.ne.0) then
                   write(*,*)myname,'Error return from readData.',irc
                   return
                end if
             end if
          else
             bbok=.false.
          end if
          if (bbok) then
             file%refParDO=>ncf_makeDimOrder(v,irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from makeDimOrder (parid).',irc
                return
             end if
             if (.not.associated(file%refAllDO)) then
                file%refAllDO => ncf_copyDimOrder(file%refParDO,irc)
                if (irc.ne.0) then
                   write(*,*) myname,'Error return from copyDimOrder.',irc
                   return
                end if
             else
                call ncf_addDimOrder(file%refAllDO,file%refParDO,irc)
                if (irc.ne.0) then
                   write(*,*) myname,'Error return from addDimOrder.',irc
                   return
                end if
             end if
             cpar%var=>v
             cpar%hrs=hrs
             bok=.true.
          end if
          if (.not.bbok.and.cpar%hasvar) then
             lenc=length(cpar%par350,350,10)
             write(*,*)myname,'Error preparing:',cpar%par350(1:lenc)
          end if
          cpar=>cpar%next
       end do
    end if
    ! compile filter variables...
    if (bok) then
       cpar=>file%firstparameter%next
       do while (.not.associated(cpar,target=file%lastparameter))
          ! write(*,*)myname,'Reading parameter...'
          crep=>cpar%firstreport%next
          do while (.not.associated(crep,target=cpar%lastreport))
             ! write(*,*)myname,'Reading report...'
             cfilter=>crep%firstfilter%next
             do while (.not. associated(cfilter,target=crep%lastfilter))
                if (cfilter%ipar.and.cfilter%active.and.cfilter%type==flt_value) then
                   ! look for expression -> compile
                   if (cfilter%lene.ne.0) then
                      file%hasfltexp=.true.
                      if (bdeb) write(*,*)myname,"Compiling "//trim(cfilter%name250)//":'"//cfilter%exp700(1:cfilter%lene)//"'"
                      lenv=length(cfilter%var250,250,1)
                      flt_elem80(1)=cfilter%var250(1:lenv)
                      call parse_parsef(cfilter%exp,cfilter%exp700(1:cfilter%lene),&
                           & flt_elem80,crc250,irc)
                      if (irc.ne.0) then
                         lenc=length(crc250,250,10)
                         write(*,*) crc250(1:lenc)
                         write(*,*)myname,'Error return from parse_parsef "',cfilter%exp700(1:cfilter%lene),'"->',irc
                         write(*,*)myname,"List of elements:"
                         do jj=1,1
                            write(*,*)myname,jj,trim(flt_elem80(jj))
                         end do
                         return
                      end if
                   end if
                end if
                cfilter=>cfilter%next
             end do
             crep=>crep%next
          end do
          cpar=>cpar%next
       end do
    end if
    !
    return
  end subroutine readDataFile
  !
  subroutine extractTimes(file,bok,irc)
    implicit none
    type(filetype) :: file
    logical bok
    integer irc
    integer :: timEntry
    type(dimension), pointer :: timdim=>null()
    type(variable), pointer :: v=>null()
    type(report), pointer :: crep=>null()
    type(parameter), pointer :: cpar=>null(),bpar=>null()
    real :: dt;
    integer :: tt,ii,lenr
    integer, external :: length
    !
    CHARACTER*14 MYNAME 
    DATA MYNAME /'getTimeInfo'/
    file%firstt=.true.
    if (bok) then
       file%mint2000=j2000(timfact*file%ref%timid%fd(1))
       file%maxt2000=j2000(timfact*file%ref%timid%fd(file%ref%timid%lend))
       dt=file%maxt2000 -file%mint2000
       if (file%maxdays > 0.0D0 .and. abs(dt) > file%maxdays) then ! sanity check
          ! this happens when input file is not ready...
          lenr=length(file%ref350,350,10)
          write(*,'(X,A,A,A,A,F0.3,A,F0.3)')myname,"Corrupted input file:",&
               & file%ref350(1:lenr),"; Dt=",dt,">",file%maxdays
          do ii=1,file%ref%timid%lend
             write(*,'(X,A,A,I0,A,F0.1,A,F0.3)')myname,"time(",II,") = ",file%ref%timid%fd(II),&
                  & " ; JD2000= ",j2000(timfact*file%ref%timid%fd(II))
          end do
          irc=333
          return
       end if
       if (file%keep > 0 .or. file%ref%timid%lend.le.1) then
          dt=file%keep/24.0D0
       else
          dt=j2000(timfact*file%ref%timid%fd(file%ref%timid%lend))-&
               & j2000(timfact*file%ref%timid%fd(max(file%ref%timid%lend-1,1)))
       end if
       dt=max(0.0D0,min(1.0D0,dt*0.5D0));
       file%tt2000=file%maxt2000 + dt
       file%firstt=.false.
    end if
    if (associated(file%ref%timid)) then
       file%mtime=file%ref%timid%lend
    else
       file%mtime=1
    end if
    if (.not.associated(file%refTimDO)) then
       write(*,*)myname,"Missing refTimDO..."
       bok=.false.
       irc=273
       return
    end if
    file%mrest=0
    timEntry=min(mtim,ncf_getDimEntry(file%ref,"time"))
    timdim => ncf_getInventoryDimension(file%ref,timEntry,irc) ! get dimension
    !write(*,*)myname,'Time:',timEntry,associated(timdim)
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       v => cpar%var ! variable
       if (associated(v)) then
          if (ncf_hasDimension(v,timdim,irc)) then
             file%mrest=max(file%mrest,v%lend/file%mtime)
          else
             file%mrest=max(file%mrest,v%lend)
          end if
       end if
       ! loop over times
       call ncf_resetPos(file%refTimDO,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from resetPos (file%refLatLon).',irc
          return
       end if
       do while (ncf_increment(file%ref,file%refTimDO,irc))
          tt=max(1,min(mtim,file%ref%pos%pos(timEntry)))
          file%t2000=j2000(timfact*ncf_valuePosition(file%ref%timid,irc))
          if (irc.ne.0) then
             write(*,*) myname,'Error return from ncf_valuePosition.',irc
             return
          end if
          ! loop over reports
          crep => cpar%firstreport%next
          do while (.not.associated(crep,target=cpar%lastreport))
             crep%tj2000(tt)=file%t2000
             crep => crep%next
          end do
       end do
       cpar=>cpar%next
    end do
    return
  end subroutine extractTimes
  !
  subroutine writeTimes(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    integer :: yy,mm,dd,hh,mi
    real :: sec
    character*4 ayy
    character*2 amm,add,ahh
    !real xissued
    CHARACTER*14 MYNAME 
    DATA MYNAME /'writeTimes'/
    file%a2000=file%t2000
    call dj2000(file%a2000,yy,mm,dd,hh,mi,sec)
    !write(*,*)myname,'T2000:',file%t2000,yy,mm,dd,hh
    write (file%a24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.000Z")') &
         &yy,mm,dd,hh,mi
    write (ayy,'(I4.4)') yy
    write (amm,'(I2.2)') mm
    write (add,'(I2.2)') dd
    write (ahh,'(I2.2)') hh
    write(*,*)myname,'Analysis time:   '//file%a24
    ! xissued=f1970(yy,mm,dd,hh,mi,sec)
    if (file%firstt) then
       file%t24="NotAvailable";
       file%s24="NotAvailable";
       file%e24="NotAvailable";
    else
       call dj2000(file%mint2000,yy,mm,dd,hh,mi,sec)
       write (file%s24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.0000")') &
            &yy,mm,dd,hh,mi
       call dj2000(file%maxt2000,yy,mm,dd,hh,mi,sec)
       write (file%e24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.0000")') &
            &yy,mm,dd,hh,mi
       call dj2000(file%tt2000,yy,mm,dd,hh,mi,sec)
       write (file%t24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.000Z")') &
            &yy,mm,dd,hh,mi

    end if
    !
    write(*,*)myname,'Data start time: '//file%s24
    write(*,*)myname,'Data end time:   '//file%e24
    write(*,*)myname,'Expiration time: '//file%t24
    return
  end subroutine writeTimes
  !
  subroutine processKeys(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(parameter), pointer :: cpar=>null(),bpar=>null()
    type(report), pointer :: crep=>null()
    integer :: lenx
    CHARACTER*14 MYNAME 
    DATA MYNAME /'processKeys'/
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       ! loop over reports
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          ! make report independent keys
          call setKeyVar(file,mkeyvar,cpar%par350,crep%keyvar350)
          ! make static macros
          call setXmlVar(cpar,crep,file,mxmlvar,mtim,crep%xmlvar350)
          crep=>crep%next
       end do
       cpar=>cpar%next
    end do
    return
  end subroutine processKeys
  !
  subroutine processDataFile(file,fail,bok,irc)
    implicit none
    type(filetype) :: file
    logical :: fail(4)
    logical :: bok
    integer :: irc
    integer :: timEntry
    logical :: used
    type(report), pointer :: crep=>null()
    type(parameter), pointer :: cpar=>null(),bpar=>null()
    type(node2d), pointer :: cnode=>null()
    type(variable), pointer :: v=>null()
    integer :: cnt
    real :: dist
    CHARACTER*16 MYNAME 
    DATA MYNAME /'processDataFile'/
    file%dt=0.0D0
    if (bok) then ! use filter
       ! information about iteration
       if (associated(file%i)) then
          ii=max(1,ncf_getLocation(file%i))
          jj=nint(ncf_valuePosition(file%i,irc))
       else
          ii=1
          jj=0
       end if
       !
       write(*,*) myname,'Using input mask.'
       timEntry=min(mtim,ncf_getDimEntry(file%ref,"time"))
       !
       ! make index of outer dimensions in reference
       !
       file%refOut => ncf_copyDimOrder(file%refAllDO,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from copyDimOrder.',irc
          return
       end if
       if (bdeb) then
          write(*,*) myname,'>>> Initial dimensions:'
          call ncf_printDimOrder(file%refAllDO)
          !
          write(*,*) myname,'>>> Iteration dimensions:'
          call ncf_printDimOrder(file%refIterDO)
          !
          write(*,*) myname,'>>> LatLon dimensions:'
          call ncf_printDimOrder(file%refLatLonAltDO)
       end if
       !
       call ncf_removeDimOrder(file%refOut,file%refLatLonAltDO)
       call ncf_removeDimOrder(file%refOut,file%refIterDO)
       !
       call ncf_removeDimOrder(file%refFlt,file%refLatLonAltDO)
       call ncf_removeDimOrder(file%refFlt,file%refIterDO)
       !
       file%refInn => ncf_copyDimOrder(file%refOut,irc)
       call ncf_removeDimOrder(file%refInn,file%refDimDO)
       call ncf_removeDimOrder(file%refInn,file%refFlt)
       !
       call ncf_removeDimOrder(file%refOut,file%refInn)
       !
       ! loop over output grid and match grid points
       !
       if (bdeb) then
          write(*,*) myname,'>>> Outer loop dimensions:'
          call ncf_printDimOrder(file%refOut)
          write(*,*) myname,'>>> Inner loop dimensions:'
          call ncf_printDimOrder(file%refInn)
          write(*,*) myname,'RefDim:'
          call ncf_printDimOrder(file%refDimDo)
          write(*,*) myname,'RefStr:'
          call ncf_printDimOrder(file%refStrDo)
          write(*,*) myname,'ReflatLon:'
          call ncf_printDimOrder(file%refLatLonAltDO)
       end if
       write(*,'(X,A,A,I0,A,I0)') myname,&
            & '>>> Max size=',file%mrest," times=",file%mtime
       !
       ! allocate trg-arrays (time*parameters*grid-size)... XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
       call initFileKeys(file,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from initKeys.',irc
          return
       end if
       if (timfact*file%ref%timid%lend.ge.2) then
          file%dt=(j2000(timfact*file%ref%timid%fd(2))-&
               & j2000(timfact*file%ref%timid%fd(1)))*0.5D0
       else
          file%dt=1.0D0
       end if
       ! write(*,*)myname,'Dt=',file%dt
       write(*,*)myname,'Applying filter.'
       call ncf_resetPos(file%refLatLonAltDO,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from resetPos (file%refLatLon).',irc
          return
       end if
       do while (ncf_increment(file%ref,file%refLatLonAltDO,irc))
          ! write(*,*)myname,'inside regLatLonAlt'
          lat=ncf_valuePosition(file%ref%latid,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from valuePosition (latid).',irc
             return
          end if
          lon=ncf_valuePosition(file%ref%lonid,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from valuePosition (lonid).',irc
             return
          end if
          if (associated(file%ref%altid)) then
             alt=ncf_valuePosition(file%ref%altid,irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from valuePosition (altid).',irc
                return
             end if
          else
             alt=0.0D0
          end if
          !
          ! loop over parameters...
          cpar=>file%firstparameter%next
          PARR:do while (.not.associated(cpar,target=file%lastparameter))
             ! write(*,*)myname,'inside cpar-loop',cpar%hasvar
             if (.not.cpar%hasVar) then
                cpar=>cpar%next
                cycle PARR
             end if
             v => cpar%var ! variable
             if (.not.associated(v)) then
                cpar=>cpar%next
                cycle PARR
             end if
             used=.false.
             ! loop over reports...
             if (file%lcrequest) then ! check tree
                call initSearchNode()
                if (rangesearch(cpar%tree%root, lat, lon, 0)) then
                   cnt=0
                   cnode=>searchfirst
                   do while (associated(cnode))
                      crep=>cnode%rep
                      if (.not.crep%skip) then
                         dist=max(dist,crep%cdist)
                         cnt=cnt+1
                         if (processReport(file,cpar,v,crep,lat,lon,alt,timentry,irc)) then
                            used=.true.
                         end if
                         if (irc.ne.0) then
                            write(*,*) myname,'Error return from processreport.',irc
                            return
                         end if
                      end if
                      cnode=>cnode%next
                   end do
                   !write(*,*)myname,'Found reports...',cnt, &
                   !     & lat,lon,associated(searchfirst),associated(searchlast),&
                   !     & dist,cpar%tree%root%dist
                end if
             else                    ! check all, all the time
                crep=>cpar%firstreport%next
                REPP:do while (.not.associated(crep,target=cpar%lastreport))
                   if (.not.crep%skip) then
                      if (processReport(file,cpar,v,crep,lat,lon,alt,timentry,irc)) then
                         used=.true.
                      end if
                      if (irc.ne.0) then
                         write(*,*) myname,'Error return from processreport.',irc
                         return
                      end if
                   end if
                   crep=>crep%next
                end do REPP
             end if
             if (.not.used.and.cpar%nrep.ne.0) then ! mark value not unused in mask-file
                ! loop over dimension that the filter depends on...
                call ncf_resetPos(file%refOut,irc)
                if (irc.ne.0) then
                   write(*,*) myname,'Error return from resetPos (file%refOut).',irc
                   return
                end if
                do while (ncf_increment(file%ref,file%refOut,irc))
                   call ncf_resetPos(file%refInn,irc)
                   if (irc.ne.0) then
                      write(*,*) myname,'Error return from resetPos (refOut).',irc
                      return
                   end if
                   do while (ncf_increment(file%ref,file%refInn,irc))
                      call ncf_setValue(v,-999.99D0,irc) 
                      !call ncf_setValue(v,nf_fill_double,irc) 
                      if (irc.ne.0) then
                         write(*,*) myname,'Error return from setValue.',irc
                         return
                      end if
                   end do
                end do
             else
                !write(*,*)myname,'Keeping:',lat,lon
             end if
             cpar=>cpar%next
          end do PARR
       end do ! latlon
       if (irc.ne.0) then
          write(*,*) myname,'Error return from increment (refLatLonOut).',irc
          return
       end if
       if (associated(file%ref%tanid)) then
          file%t2000=j2000(tanfact*ncf_valuePosition(file%ref%tanid,irc))
          !write(*,*)myname,'TANID:',ncf_valuePosition(file%ref%tanid,irc),&
          !     & (file%t2000+10957.0D0)*86400.0D0
          if (irc.ne.0) then
             write(*,*) myname,'Error return from ncf_valuePosition.',irc
             return
          end if
       else
          file%t2000=j2000(timfact*file%ref%timid%fd(1)) - file%cutoff/24.0D0
       end if
    end if     ! bok
    return
  end subroutine processDataFile

  logical function processReport(file,cpar,v,crep,lat,lon,alt,timEntry,irc)
    type(filetype) :: file
    type(parameter), pointer :: cpar
    type(variable), pointer :: v
    type(report), pointer :: crep
    real :: lat,lon,alt
    integer :: timEntry
    integer :: irc
    real val,aux
    logical :: used
    logical masked
    integer loc,ii
    logical :: first
    CHARACTER*16 MYNAME 
    DATA MYNAME /'processDataFile'/
    used=.false.
    crep%used=.true.
    write(crep%keyvar350(key_iter),"(I0)") jj
    call choptrim(crep%keyvar350(key_iter),350)
    ! loop over dimension that the filter depends on...
    call ncf_resetPos(file%refOut,irc)
    if (irc.ne.0) then
       write(*,*) myname,'Error return from resetPos (file%refOut).',irc
       return
    end if
    do while (ncf_increment(file%ref,file%refOut,irc))
       ! write(*,*)myname,'Position:',lat,lon,alt
       ! call printMask(crep%roperation,0)
       ! calculate the ignore-data-flag based on the report mask...
       masked=checkMasked(crep%roperation,lat,lon,alt,irc)
       !if (bdeb) write(*,*)myname,'inside refOut',masked
       if (irc.ne.0) then
          write(*,*) myname,'Error return from checkMasked.',irc
          return
       end if
       if (.not.masked) then ! use this value
          used=.true.
          if (file%firsttrg) then
             file%firsttrg=.false.
             file%refLatlonDo => ncf_copyDimOrder(file%refLatDo,irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from copyDimOrder.',irc
                return
             end if
             call ncf_addDimOrder(file%refLatlonDo,file%refLonDo,irc)
             if (irc.ne.0) then
                write(*,*) myname,'Error return from addDimOrder.',irc
                return
             end if
             file%ix=>ncf_getdimensionOrderDimension(file%refLatlonDo,1)
             file%iy=>ncf_getdimensionOrderDimension(file%refLatlonDo,2)
             file%darea=ncf_getArea(file%ref,file%ix,file%iy,irc)
             if (irc.ne.0.and..not.crep%larea) irc=0 ! not needed anyways...
             if (irc.ne.0) then
                write(*,*) myname,'Error return from ncf_getArea.',irc
                return
             end if
             call ncf_clearDimOrder(file%refLatLonDO)
             write(*,'(X,A,A,F0.2,A)')myname,&
                  & 'Grid point area = ',file%darea,' km**2'
          end if
          call ncf_resetPos(file%refInn,irc)
          if (irc.ne.0) then
             write(*,*) myname,'Error return from resetPos (refOut).',irc
             return
          end if
          !call ncf_printDimOrder(file%refInn)
          do while (ncf_increment(file%ref,file%refInn,irc))
             if (file%hasexp) call setExpVar(file,irc)
             ! val=ncf_valuePosition(v,irc)
             val=getExpVal(file,cpar,v,irc)
             call ncf_setValue(v,val,irc) ! use function value....
             ! call ncf_printpos(v%f%pos)
             ! write(*,*) "                    Value=",val
             tt=max(1,min(mtim,file%ref%pos%pos(timEntry)))
             if (val.ne.nf_fill_double) then
                !write(*,*) "                    Ok value=",val
#ifdef JNK                             
                loc=ncf_getLocation(v)
                write(50,'(I10,3(X,F10.5),X,I5,X,A)')loc,val, &
                     & lat,lon,tt,v%var250(1:v%lenv)
#endif
                if (crep%ltrg) then
                   crep%nkey1(tt)=crep%nkey1(tt)+1
                   crep%nrest=max(crep%nrest,crep%nkey1(tt))
                   crep%arest=max(crep%arest,crep%nkey1(tt)+crep%nkey0(tt))
                   if (crep%nkey1(tt).gt.crep%mrest) then
                      if (ecnt.lt.10) then
                         ecnt=ecnt+1
                         write(*,'(X,A,A,I0,A,I0,A)')myname,&
                              & '*** Too small report size: ',&
                              & crep%mrest," (time step=",tt,")"
                      else if (ecnt.eq.10) then
                         ecnt=ecnt+1
                         write(*,'(X,A,A)')myname,'*** ...'
                      end if
                      fail(fail_size)=.true.
                      bok=.false.
                   else
                      crep%key1(crep%nkey1(tt),tt)=val
                      crep%keyind(crep%nkey1(tt),tt)=crep%nkey1(tt)
                   end if
                else
                   crep%nkey0(tt)=crep%nkey0(tt)+1
                end if
                !
                !write(*,*)myname,'Value:',val,v%var250(1:v%lenv),&
                !     & tt,crep%ltrg
                !call ncf_printVarPos(v)
                crep%ntim=max(crep%ntim,tt)
                if (crep%ntim .gt. mtim) then
                   write(*,*)myname,'Too many times.',mtim
                   irc=457
                   return
                end if
                if (crep%lfirst(tt)) then
                   crep%lfirst(tt)=.false.
                   crep%valavg(tt)=val
                   crep%valcnt(tt)=1.0
                   crep%valmin(tt)=val
                   crep%valmax(tt)=val
                   crep%llmin(1,tt)=lat
                   crep%llmax(1,tt)=lat
                   crep%llmin(2,tt)=lon
                   crep%llmax(2,tt)=lon
                   crep%ttmin(tt)=tt
                   crep%ttmax(tt)=tt
                   ! process auxiliaries
                   if (crep%laux) then
                      do ii=1,crep%ntrg
                         if (crep%trglenv(ii).ne.0) then
                            aux=getAuxVal(file,crep,ii,irc)
                            crep%auxmax(tt,ii)=aux
                            crep%auxmin(tt,ii)=aux
                         end if
                      end do
                   end if
                else
                   crep%valavg(tt)=crep%valavg(tt)+val
                   crep%valcnt(tt)=crep%valcnt(tt)+1.0
                   if (val.gt.crep%valmax(tt)) then
                      ! write(*,'(X,A,X,I4,X,F10.3,X,I8)')myname//&
                      !      & 'Found new max:',tt,val,loc
                      crep%valmax(tt)=val
                      crep%llmax(1,tt)=lat
                      crep%llmax(2,tt)=lon
                      crep%ttmax(tt)=tt
                      if (crep%laux) then
                         do ii=1,crep%ntrg
                            if (crep%trglenv(ii).ne.0) then
                               aux=getAuxVal(file,crep,ii,irc)
                               crep%auxmax(tt,ii)=aux
                            end if
                         end do
                      end if
                   end if
                   if (val.lt.crep%valmin(tt)) then
                      crep%valmin(tt)=val
                      crep%llmin(1,tt)=lat
                      crep%llmin(2,tt)=lon
                      crep%ttmin(tt)=tt
                      if (crep%laux) then
                         do ii=1,crep%ntrg
                            if (crep%trglenv(ii).ne.0) then
                               aux=getAuxVal(file,crep,ii,irc)
                               crep%auxmin(tt,ii)=aux
                            end if
                         end do
                      end if
                   end if
                end if
             else
                crep%nkey0(tt)=crep%nkey0(tt)+1
             end if
          end do ! inner
          if (irc.ne.0) then
             write(*,*) myname,'Error return from increment (refInn).',irc
             return
          end if
       end if ! masked-block
    end do ! outer
    if (irc.ne.0) then
       write(*,*) myname,'Error return from increment (refOut).',irc
       return
    end if
    processReport=used
    return
  end function processReport
  !
  real function getAuxVal(file,crep,ii,irc)
    implicit none
    type(filetype) :: file
    type(report),pointer :: crep
    integer :: ii
    integer :: irc
    character*250 :: crc250
    integer :: lenc
    getAuxVal=parse_evalf(crep%trgexp(ii)%ptr,file%val,crc250,irc)
    if (irc.ne.0) then
       lenc=length(crc250,250,10)
       write(*,*) crc250(1:lenc)
       write(*,*) myname,'Error return from parse_evalf.',irc
       return
    end if
    return
  end function getAuxVal
  !
  real function getExpVal(file,cpar,v,irc)
    !use parse
    !use shape
    implicit none
    type(filetype) :: file
    type(parameter),pointer :: cpar
    type(variable),pointer :: v
    integer :: irc
    character*250 :: crc250
    integer :: lenc
    CHARACTER*14 :: MYNAME ="getExpVal"
    if (cpar%lene.ne.0) then
       getExpVal=parse_evalf(cpar%exp,file%val,crc250,irc)
       if (irc.ne.0) then
          lenc=length(crc250,250,10)
          write(*,*) crc250(1:lenc)
          write(*,*) myname,'Error return from parse_evalf.',irc
          return
       end if
       !write(*,*)myname,"val:",getExpVal,file%val,file%elem80
       !stop("Debug")
    else if (associated(v)) then
       getExpVal=ncf_valuePosition(v,irc)
       if (irc.ne.0) then
          write(*,*) myname,'Error return from valuePosition.',irc
          return
       end if
    else
       getExpVal=0.0D0
    end if
    return
  end function getExpVal
  !
  subroutine setExpVar(file,irc)
    implicit none
    type(filetype) :: file
    type(variable),pointer :: v => null()
    integer :: irc
    character*250 :: crc250
    integer :: lenc
    CHARACTER*14 :: MYNAME ="setExpVar"
    integer:: ii
    ! loop over variables
    do ii=1,file%npar
       if (associated(file%var(ii)%ptr)) then
          file%val(ii)=ncf_valuePosition(file%var(ii)%ptr,irc)
          !write(*,*)myname,ii,"var:",file%val(ii)
       else
          file%val(ii)=parse_evalf(file%par(ii)%ptr%exp,file%val,crc250,irc)
          if (irc.ne.0) then
             lenc=length(crc250,250,10)
             write(*,*) crc250(1:lenc)
             write(*,*) myname,'Error return from parse_evalf.',irc
             return
          end if
       end if
    end do
    return
  end subroutine setExpVar
  !
  subroutine sortTargetData(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(report), pointer    :: crep=>null()
    type(parameter), pointer :: cpar=>null()
    integer :: newnn,minn,tt
    real :: eps=1.0D-10
    logical :: uniq=.false.
    CHARACTER*14 MYNAME 
    DATA MYNAME /'sortTargetData'/
    minn=-1
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          if (crep%ltrg .and. .not.crep%skip) then
             do tt=1,file%mtime
                if (crep%nkey1(tt).le.crep%mrest) then
                   call heapsort1(crep%mrest,crep%key1(1,tt),newnn,crep%nkey1(tt),&
                        & crep%keyind(1,tt),eps,uniq)
                   if (newnn.gt.0.and. (minn.lt.0 .or. minn.gt.newnn)) then
                      minn=newnn
                   end if
                end if
             end do
          end if
          crep=>crep%next
       end do
       cpar=>cpar%next
    end do
    if (minn.ne.-1) then
       write(*,'(X,A,A,I0,A,F0.2,A)')myname,&
            & 'Grid points in mask = ',minn,'  ( ',&
            & real(minn)*file%darea,' km**2 )'
    end if
    return
  end subroutine sortTargetData
  !
  subroutine checkExitCondition(file,fail,irc)
    implicit none
    type(filetype) :: file
    logical :: fail(3)
    integer :: irc
    type(parameter), pointer :: cpar=>null()
    type(variable), pointer :: v=>null()
    integer :: lenp
    character*700 :: b700
    integer cnt,cntud
    real :: maxval,minval
    CHARACTER*20 MYNAME 
    DATA MYNAME /'checkExitCondition'/
    ! global fail conditions...
    fail(fail_NoData)=.false.
    fail(fail_AnyData)=.false.
    fail(fail_None)=.false.
    !
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       v => cpar%var ! variable
       if (associated(v).and. (cpar%find.or.cpar%fiad.or.cpar%fino)) then
          !call ncf_printVariable(v)
          call ncf_countField(v,cnt,cntud,minval,maxval,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from COUNTFIELD.',irc
             return
          end if
          if (cnt.ne.0) file%nodata=.false.
          lenp=length(cpar%par350,350,10)
          if (cpar%find.and.cnt.eq.0) then
             fail(fail_NoData)=.true.
             write(b700,'("PAR Check=",A,";no data;FAIL;UNDEF=",I10,'//&
                  & '";MIN=",F17.10,";MAX=",F17.10)') &
                  &     cpar%par350(1:lenp),cntud,minval,maxval
          else if (cpar%fiad.and.cnt.gt.0) then
             fail(fail_AnyData)=.true.
             write(b700,'("PAR Check=",A,";cnt=",I0,";FAIL;UNDEF=",I10,'//&
                  & '";MIN=",F17.10,";MAX=",F17.10)') &
                  &     cpar%par350(1:lenp),cnt,cntud,minval,maxval
          else
             write(b700,'("PAR Check=",A,";cnt=",I0,";OK;UNDEF=",I10,'//&
                  & '";MIN=",F17.10,";MAX=",F17.10)') &
                  &     cpar%par350(1:lenp),cnt,cntud,minval,maxval
          end if
          call chop0(b700,700)
          lenb=length(b700,700,10)
          write(*,*)b700(1:lenb)
       end if
       cpar=>cpar%next
    end do
    return
  end subroutine checkExitCondition
  !
  subroutine hintReports(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(parameter), pointer :: cpar=>null()
    type(report), pointer :: crep=>null()
    CHARACTER*14 MYNAME 
    DATA MYNAME /'hintReports'/
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          if (.not.crep%skip) then
             call hintReport(file,cpar,crep,irc)
          end if
          crep=>crep%next
       end do
       cpar=>cpar%next
    end do
    return
  end subroutine hintReports
  !
  subroutine hintReport(file,cpar,crep,irc)
    implicit none
    type(filetype) :: file
    type(parameter), pointer :: cpar
    type(report), pointer :: crep
    integer :: irc
    integer :: lenx,leni
    character*700 :: i700
    character*60 :: b60
    CHARACTER*14 MYNAME 
    DATA MYNAME /'hintReport'/
    if (.not.associated(crep)) then
       write(*,*)myname,'Missing report...'
       return
    end if
    call chop0(crep%xmlo350,350)
    lenx=length(crep%xmlo350,350,10)
    if (lenx.eq.0) then
       return
    end if
    i700=crep%xmlo350(1:lenx)
    call chop0(i700,700)
    call replaceKEY(i700,mkeyvar,crep%keyvar350)
    leni=length(i700,700,20)
    ! write(*,*) myname,'Buffer:',i700(1:leni)
    write(b60,'(2(A,I0))',iostat=irc) "Suggested size=",crep%mrest," Actual=",crep%arest
    if (irc.ne.0) then
       write(*,*)myname,'Unable to write size,',crep%nrest
       irc=993
       return
    end if
    if (crep%nrest.eq.0) then
       write(*,*) myname,'    '//i700(1:leni)
    else if (crep%nrest.gt.crep%mrest) then
       write(*,*) myname,'***'//" ("//trim(b60)//") "//i700(1:leni)
    else if (crep%arest.gt.crep%mrest) then
       write(*,*) myname,'+++'//" ("//trim(b60)//") "//i700(1:leni)
    else
       write(*,*) myname,'   '//" ("//trim(b60)//") "//i700(1:leni)
    end if
    return
  end subroutine hintReport
  !
  subroutine writeReports(file,irc)
    implicit none
    type(filetype) :: file
    integer :: irc
    type(parameter), pointer :: cpar=>null()
    type(report), pointer :: crep=>null()
    type(filter), pointer :: flt=>null()
    integer ii,kk
    CHARACTER*14 MYNAME 
    DATA MYNAME /'writeReports'/
    cpar=>file%firstparameter%next
    do while (.not.associated(cpar,target=file%lastparameter))
       crep=>cpar%firstreport%next
       do while (.not.associated(crep,target=cpar%lastreport))
          if (.not.crep%skip) then
             if (crep%lext) then
                call writeReport(file,cpar,crep,.true.,irc)
             end if
             call writeReport(file,cpar,crep,.false.,irc)
             if (crep%lpoly) then          ! write masked polygon to file
                flt => getPolygonFilter(crep%roperation,irc)
                if (irc.ne.0) then
                   write(*,*)myname,'Error return from getPolygonFilter.',irc
                   return
                end if
                if (associated(flt)) then
                   write(*,*)myname,'Polygon nodes:',flt%mpar
                   !do ii=1,min(100,flt%mpar)
                   !   write(*,*) 'DEBUG POS:',ii,flt%rpar(ii),flt%rpar(ii+flt%mpar)
                   !end do
                   
                   call chop0(crep%poly350,350)
                   call writePolygon(crep%poly350,flt%mpar,flt%rpar,flt%eps,irc)
                   if (irc.ne.0) then
                      write(*,*) myname,'Error return from writePolygon.',irc
                      return
                   end if
                   call clearFilter(flt,irc)
                   if (irc.ne.0) then
                      write(*,*) myname,'Error return from clearFilter.',irc
                      return
                   end if
                end if
             end if
          end if
          crep=>crep%next
       end do
       cpar=>cpar%next
    end do
    if (file%ilen1.ne.-1) then
       write(*,*)myname,"Ignored key '"//&
            & file%ign700(1)(1:file%ilen1)//"'->'"//&
            & file%ign700(2)(1:file%ilen2)//"'"
    end if
    return
  end subroutine writeReports
  !
  subroutine writeReport(file,cpar,crep,lext,irc)
    implicit none
    type(filetype) :: file
    type(parameter), pointer :: cpar
    type(report), pointer :: crep
    logical :: lext
    integer :: irc
    character*25 x25,y25,z25,l25,d25
    integer lenx25,leny25,lenz25,lenl25,lend25
    integer :: yy,mm,dd,hh,mi
    real :: sec
    real :: val
    character*4 ayy
    character*2 amm,add,ahh
    character*700 :: keb700,i700
    character*60 :: b60
    integer :: cpos,opos,lenx,leni,lenk,lenb,lenk1,lenk2
    integer :: iepos,ifpos,idd,lenv,ilenv,ii
    logical :: first
    character*350 :: name350
    character*30 :: val30
    integer, external :: length
    real :: epos,fpos,trg
    real :: rpos,dpos
    integer :: gg, leng
    character*10 :: gg10
    logical :: bgg
    integer,dimension(8) :: values
    CHARACTER*14 MYNAME 
    DATA MYNAME /'writeReport'/
    if (.not.associated(crep)) then
       write(*,*)myname,'Missing report...'
       return
    end if
    if (lext) then             ! make extreme index
       name350=crep%ext350
    else                            ! write all values
       name350=crep%xmlo350
    end if
    call chop0(name350,350)
    lenx=length(name350,350,10)
    if (lenx.eq.0) then
       return
    end if
    if (file%nodata) then
       write(*,*)myname,'File contains no data:'//name350(1:lenx)
    end if
    ! loop over groups... (over-write file every time)
    bgg=.false.
    gg=0
    do while (gg.le.max(0,crep%ngroups))
       write(gg10,'(I10)') max(0,gg)
       call chop0(gg10,10)
       leng=length(gg10,10,1)
       i700=name350(1:lenx)
       call chop0(i700,700)
       leni=length(i700,700,20)
       !write(*,*) myname,'Buffer:',i700(1:leni)
       lenb=0
       opos=1
       cpos=1
       write(b60,'(2(A,I0))',iostat=irc) "Suggested size=",crep%mrest," Actual=",crep%arest
       if (irc.ne.0) then
          write(*,*)myname,'Unable to write size,',crep%nrest
          irc=993
          return
       end if
       b700="";
       do while (cpos.le.leni)
          if (i700(cpos:cpos).eq."@") then ! first "@"
             b700=b700(1:lenb)//i700(opos:cpos-1)
             lenb=lenb+max(0,cpos-opos)
             ! find next "$" if any
             cpos=cpos+1
             opos=cpos
             do while (cpos.lt.leni.and.i700(cpos:cpos).ne."@") ! second "$"
                cpos=cpos+1
             end do
             if (i700(opos:cpos-1).eq."issued") then
                b700=b700(1:lenb)//file%a24
                lenb=lenb+24
                opos=cpos+1 ! point to character after "@"
             elseif (i700(opos:cpos-1).eq."group") then
                b700=b700(1:lenb)//gg10(1:leng)
                lenb=lenb+leng
                opos=cpos+1 ! point to character after "@"
                bgg=.true.
             else  
                opos=opos-1 ! reset opos to first "@"
             end if
          end if
          !write(*,*) myname,' BUFF:::',cpos,leni,i700(cpos:cpos)
          cpos=cpos+1
       end do
       b700=b700(1:lenb)//i700(opos:cpos-1) ! copy the rest
       lenb=lenb+max(0,cpos-opos)
       i700=b700(1:lenb)
       call chop0(i700,700)
       leni=length(i700,700,20)
       call replaceKEY(i700,mkeyvar,crep%keyvar350)
       leni=length(i700,700,10)
       !
       if (.not.bgg) then ! only do loop once if name does not contain @group@
          crep%ngroups=0
       end if
       !
       ! check if file contains data (if no data => iteration variable not set)
       !
       if (.not.crep%used .and. .not.crep%lwrite) then
          write(*,*) myname," Omitting:"//i700(1:leni)
       else
          write(*,*) myname,"("//trim(b60)//") "//i700(1:leni)
          !
          ! write to xml-file
          !
          unitx=ftunit(irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from ftunit.',irc
             return
          end if
          open(unit=unitx,file=i700(1:leni),STATUS="UNKNOWN", FORM="FORMATTED",iostat=irc)
          if (irc.ne.0) then
             write(*,*)myname,'Unable to open: ',i700(1:leni)
             return
          end if
          !
          write(unitx,"(A)",iostat=irc) "<xweather>"
          if (irc.ne.0) then
             write(*,*)myname,'Error writing to:',unitx
             return
          end if
          ! write date and time of execution
          call date_and_time(VALUES=values)
          yy=values(1) ! year
          mm=values(2) ! month
          dd=values(3) ! day
          hh=values(5) ! hour
          mi=values(6) ! minutes
          sec=real(values(7))+real(values(8))/1000.0D0 ! seconds
          call jd2000(file%i2000,yy,mm,dd,hh,mi,sec)
          file%i2000=file%i2000-real(values(4))/1440.0d0 ! time zone
          call dj2000(file%i2000,yy,mm,dd,hh,mi,sec)
          write (file%i24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.000Z")') yy,mm,dd,hh,mi
          write(unitx,'(A)',advance='no',iostat=irc) ' <time run="'//file%i24
          if (bok) then
             write(unitx,'(A)',advance='no',iostat=irc) '" issued="'//file%a24
          end if
          if (.not.lext .and. file%keep > 0.0D0) then
             write(unitx,'(A)',advance='no',iostat=irc) '" expires="'//file%t24
          end if
          write(unitx,'(A)',iostat=irc) '"/>'
          !
          if (file%nodata) then
             write(unitx,'(A)',iostat=irc) ' <parameter>'
             write(unitx,'(A)',iostat=irc) '   <warning '
             write(unitx,'(A)',iostat=irc) '      type="No valid grid inside mask"'
             write(unitx,'(A)',iostat=irc) '      level="0"'
             if (file%firstt) then
                file%a2000=file%I2000
                write (file%a24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.000Z")') &
                     & yy,mm,dd,hh,mi
                write (ayy,'(I4.4)') yy
                write (amm,'(I2.2)') mm
                write (add,'(I2.2)') dd
                write (ahh,'(I2.2)') hh
                file%s24=file%a24
                file%t2000=file%t2000+1.0D0
                call dj2000(file%t2000,yy,mm,dd,hh,mi,sec)
                write (file%e24,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,"-",I2.2,"-00.000Z")') &
                     & yy,mm,dd,hh,mi
             end if
             write(unitx,'(A)',iostat=irc) '      time_issued="'//file%a24//'"'
             write(unitx,'(A)',iostat=irc) '      time_expires="'//file%t24//'"'
             write(unitx,'(A)',iostat=irc) '      time_start="'//file%s24//'"'
             write(unitx,'(A)',iostat=irc) '      time_stop="'//file%e24//'"'
             write(unitx,'(A)',iostat=irc) '   />'
             write(unitx,'(A)',iostat=irc) ' </parameter>'
          else if (crep%nkey.ne.0.or.cpar%hasVar) then
             ! write data to xml-file here...
             write(unitx,'(A)',iostat=irc) ' <data>'
             lenp=length(cpar%par350,250,10)
             if (crep%nkey.eq.0) then
                write(unitx,'(A)',iostat=irc) '  <par name="'//cpar%par350(1:lenp)//'">'
             else 
                write(unitx,'(A)',iostat=irc) '  <parameter name="'//cpar%par350(1:lenp)//'">'
             end if
             ! write keys
             lenk=0
             keb700=''
             do kk=1,crep%nkey
                lenk1=length(crep%key700(1,kk),700,10)
                lenk2=length(crep%key700(2,kk),700,10)
                if (lenk1.gt.0.and.lenk2.gt.0) then
                   if (lenk+4+lenk1+lenk2.gt.700) cycle ! well... dont use too many keys...
                   keb700(lenk+1:lenk+1)=" "
                   lenk=lenk+1
                   keb700(lenk+1:lenk+lenk1)=crep%key700(1,kk)(1:lenk1)
                   lenk=lenk+lenk1
                   keb700(lenk+1:lenk+2)="='"
                   lenk=lenk+2
                   keb700(lenk+1:lenk+lenk2)=crep%key700(2,kk)(1:lenk2)
                   lenk=lenk+lenk2
                   keb700(lenk+1:lenk+1)="'"
                   lenk=lenk+1
                   ! write(*,*) myname,"KEB700:",kk,keb700(1:lenk)
                else
                   file%ilen1=lenk1
                   file%ign700(1)=crep%key700(1,kk)
                   file%ilen2=lenk2
                   file%ign700(2)=crep%key700(2,kk)
                end if
             end do
             call replaceKEY(keb700,mkeyvar,crep%keyvar350)
             lenk=length(keb700,700,10)
             ! write(*,*) '   '//keb700(1:lenk)
             ! write time data
             if (crep%nkey.ne.0) then
                write(unitx,'(A)',iostat=irc) &
                     & "   <key "//keb700(1:lenk)//" />"
             end if
             call writeRecords(unitx,file,cpar,crep,gg,lext,irc)
             if (crep%nkey.eq.0) then
                write(unitx,'(A)',iostat=irc) '  </par>'
             else
                write(unitx,'(A)',iostat=irc) '  </parameter>'
             end if
             write(unitx,'(A)',iostat=irc) ' </data>'
          end if
          write(unitx,"(A)",iostat=irc) "</xweather>"
          if (irc.ne.0) then
             write(*,*)myname,'Error writing to:',unitx
             return
          end if
          close(unitx,iostat=irc)
          if (irc.ne.0) then
             write(*,*)myname,'Unable to close: ',name350(1:lenx)
             return
          end if
       end if
       gg=gg+1
    end do
    return
  end subroutine writeReport
  !
  subroutine writeRecords(unitx,file,cpar,crep,group,lext,irc)
    implicit none
    integer :: unitx
    type(filetype) :: file
    type(parameter), pointer :: cpar
    type(report), pointer :: crep
    integer :: group
    logical :: lext
    integer :: irc
    character*25 x25,y25,z25,l25,d25
    integer lenx25,leny25,lenz25,lenl25,lend25
    integer :: yy,mm,dd,hh,mi
    real :: sec
    real :: val
    character*4 ayy
    character*2 amm,add,ahh
    character*700 :: keb700,i700
    character*60 :: b60
    integer :: cpos,opos,lenx,leni,lenk,lenb,lenk1,lenk2
    integer :: iepos,ifpos,idd,ilenv,ii,kk,ll,tt
    character*30 :: val30(crep%ntim,crep%ntrg)
    integer :: lenv
    integer, external :: length
    real :: epos,fpos,trg
    real :: rpos,dpos
    real :: trgpos(mtrg)
    logical :: bok(max(crep%ntim,1)), lok(max(crep%ntim,1)), brm, first
    real,allocatable :: avg(:)
    integer :: ij2000, imax, imin, cnt
    real :: savg, scnt, vmin, vmax
    integer,dimension(8) :: values
    integer :: nerr,grp
    CHARACTER*14 MYNAME 
    DATA MYNAME /'writeRecords'/
    nerr=0
    if (bdeb) write(*,*)myname,'Entering...',crep%ntim,crep%ntrg,crep%ltrg
    if (bdeb.and.crep%nxtr.eq.0) write(*,*)myname,'No times found.',crep%ltrg
    TIM1: do tt=1,crep%ntim
       if (bdeb) write(*,*)myname,'Time:',tt,crep%ntim
       bok(tt)=.false.
       lok(tt)=.true.
       if (.not.crep%lfirst(tt)) then
          ! check if record should be written...
          if (bdeb) write(*,*)myname,'Checking targets:',crep%ltrg
          brm=.false.
          if (crep%ltrg) then
             TRG1: do kk=1,crep%ntrg
                if (bdeb) write(*,*)myname,'Target:',kk,crep%trgtyp(kk)
                if (crep%trgtyp(kk).eq.trg_aux) then
                   trgpos(kk)=0.0D0
                   bok(tt)=.true.
                else
                   ! remember ascending order
                   if (crep%trgtyp(kk).eq.trg_fraction) then
                      rpos=max(0.0D0,min(1.0D0,crep%trgval(kk)))*&
                           & real(crep%nkey1(tt)-1)&
                           & +1.0D0
                      !write(*,*)myname,'Fraction:',crep%trgval(kk),rpos
                   else if (crep%trgtyp(kk).eq.trg_area) then
                      trg=(crep%trgval(kk)/max(1.0D-5,file%darea)) ! #grid-cells
                      if (trg.lt.0.0D0) then
                         rpos=-trg ! from lowest end
                      else 
                         rpos=real(crep%nkey1(tt))-trg ! from highest end
                      end if
                      !write(*,*)myname,'Area:',crep%trgval(kk),trg,rpos
                   else if (crep%trgtyp(kk).eq.trg_count) then
                      trg=crep%trgval(kk) ! #grid-cells
                      if (trg.ge.0.5D0) then
                         rpos=trg
                      else 
                         rpos=real(crep%nkey1(tt))+trg
                      end if
                   else if (crep%trgtyp(kk).eq.trg_average) then
                      trg=crep%trgval(kk)
                      if (trg.lt.0.0D0) then ! average of values above
                         rpos=max(0.0D0,min(1.0D0,1.0D0-trg))*&
                              & real(crep%nkey1(tt)-1)&
                              & +1.0D0
                      else                   ! average of values below
                         rpos=max(0.0D0,min(1.0D0,trg))*&
                              & real(crep%nkey1(tt)-1)&
                              & +1.0D0
                      end if
                      ! write(*,*)myname,'Average:',crep%trgval(kk),rpos,crep%nkey1(tt)
                   end if
                   trgpos(kk)=rpos
                   fpos=floor(rpos)
                   epos=ceiling(rpos)
                   dpos=max(1.0D0,epos-fpos)
                   ifpos=nint(fpos)
                   iepos=nint(epos)
                   !write(*,*)myname,'Nkey:',tt,crep%nkey1(tt),crep%trgtyp(kk),crep%trgval(kk),rpos,ifpos,iepos
                   if (ifpos.ge.1 .and. iepos.le.crep%nkey1(tt)) then ! ignore?
                      bok(tt)=.true.
                   else if (crep%trgreq(kk)) then
                      brm=.true.
                      if (bdeb) write(*,*)myname,'>>> Missing required target...'
                      
                   end if
                end if
             end do TRG1
          else
             if (bdeb) write(*,*)myname,'No targets...'
             bok(tt)=.true.
          end if
          if (bdeb) write(*,*)myname,'Req met:',brm,bok(tt)
          if (brm) bok(tt)=.false. ! did not meet requirements
          if (bdeb.and..not.bok(tt)) write(*,*)myname,'>>> Removed report...',brm,bok(tt)
          if (bok(tt).and.crep%grouptrg.ne.0.and.crep%ntrg.ge.crep%grouptrg) then
             ! check if record is in a valid group
             kk=crep%grouptrg
             if (bdeb) write(*,*)myname,'Checking group:',kk
             if (crep%trgtyp(kk).eq.trg_aux) then
                bok(tt)=(group.eq.1)
                ! do nothing
             else
                rpos=trgpos(kk)
                fpos=floor(rpos)
                epos=ceiling(rpos)
                dpos=max(1.0D0,epos-fpos)
                ifpos=nint(fpos)
                iepos=nint(epos)
                if (ifpos.ge.1 .and. iepos.le.crep%nkey1(tt)) then ! ignore invalid
                   if (crep%trgtyp(kk).eq.trg_average) then
                      ! do nothing
                      bok(tt)=(group.eq.1)
                   else if (crep%ngroups.eq.0) then ! file name did not contain group
                      bok(tt)=.true. ! use all data
                   else
                      val=crep%key1(crep%keyind(ifpos,tt),tt)+(rpos-fpos)*&
                           & (crep%key1(crep%keyind(iepos,tt),tt)-&
                           & crep%key1(crep%keyind(ifpos,tt),tt))/dpos
                      grp=0
                      do ii=1,crep%ngroups
                         if (val.ge.crep%groups(ii)) then
                            grp=ii
                         end if
                      end do
                      bok(tt)=(grp.eq.group)
                   end if
                end if
             end if
             if (bdeb) write(*,*)myname,'Group target... ',group,grp,crep%ngroups,crep%trgtyp(kk).eq.trg_aux,bok(tt)
             if (bdeb.and..not.bok(tt)) then
                write(*,*)myname,'*** Value:',val,' in G',grp,' but considering G',group
                do ii=1,crep%ngroups
                   write(*,*)myname,'     Groups... ',ii,crep%groups(ii)
                end do
             end if
          end if
          if (bdeb) write(*,*)myname,'Making values:',tt,bok(tt),crep%ntrg
          if (bok(tt)) then
             first=.true.
             TRG2:do kk=1,crep%ntrg
                if (bdeb) write(*,*)myname,'Processing target:',kk,crep%ntrg
                if (crep%trgtyp(kk).eq.trg_aux) then
                   ! if (bdeb) write(*,*)myname,'Is macro:',kk
                   if (crep%trgext(kk).eq.1) then
                      val=crep%auxmax(tt,kk)
                   elseif (crep%trgext(ii).eq.-1) then
                      val=crep%auxmin(tt,kk)
                   else
                      val=0
                   end if
                   val30(tt,kk)=pretty30(val,lenx)
                else
                   if (bdeb) write(*,*)myname,'Normal target:',kk
                   rpos=trgpos(kk)
                   fpos=floor(rpos)
                   epos=ceiling(rpos)
                   dpos=max(1.0D0,epos-fpos)
                   ifpos=nint(fpos)
                   iepos=nint(epos)
                   !write(*,*)myname,"There:",ifpos,iepos,rpos,crep%nkey1(tt)
                   if (ifpos.ge.1 .and. iepos.le.crep%nkey1(tt)) then ! ignore invalid requests... (->undefined level)
                      if (bdeb) write(*,*)myname,'Target is valid:',ifpos,iepos,crep%trgtyp(kk).eq.trg_average
                      if (crep%trgtyp(kk).eq.trg_average) then
                         ! average of the values below
                         if (first) then
                            first=.false.
                            allocate(avg(crep%nkey1(tt)),stat=irc)
                            if (irc.ne.0) then
                               write(*,*)myname,'Error return from initfile.',irc
                               return
                            end if
                            ! make avg
                            savg=0.0D0
                            scnt=0.0D0
                            KEY:do ii=1,crep%nkey1(tt)
                               savg=savg+crep%key1(crep%keyind(ii,tt),tt)
                               scnt=scnt+1.0D0
                               avg(ii)=savg/scnt ! average of the values below
                               !write(*,*)myname,'Avg:',tt,ii,crep%key1(crep%keyind(ii,tt),tt),&
                               !     & avg(ii),savg,scnt

                            end do KEY
                         end if
                         !write(*,*)myname,"Ind:",fpos,epos,(crep%keyind(ii,tt),ii=1,crep%nkey1(tt))
                         val=avg(ifpos)+(rpos-fpos)*&
                              & (avg(iepos)-&
                              & avg(ifpos))/dpos
                         if (crep%trgval(kk) .lt. 0) then ! average of the values above
                            val= (savg - val*rpos)/scnt
                         end if
                      else
                         !write(*,*)myname,'Nkey:',tt,crep%nkey1(tt),crep%trgtyp(kk),crep%trgval(kk),rpos,ifpos,iepos
                         val=crep%key1(crep%keyind(ifpos,tt),tt)+(rpos-fpos)*&
                              & (crep%key1(crep%keyind(iepos,tt),tt)-&
                              & crep%key1(crep%keyind(ifpos,tt),tt))/dpos
                      end if
                      val30(tt,kk)=pretty30(val,lenx)
                   else ! no data available, ignore request...
                      val30(tt,kk)=""
                   end if
                end if
             end do TRG2
             if (allocated(avg)) deallocate(avg,stat=irc)
          end if
       else if (bdeb.and.nerr.lt.5) then
          nerr=nerr+1
          write(*,*)myname,'Missing data for time:',tt
       end if
    end do TIM1
    if (bdeb) write(*,*)myname,'Checking extremes:',lext
    if (lext) then ! mark records that have extreme values...
       lok(:)=.false.
       cnt=0
       do kk=1,crep%ntrg
          if (crep%trgext(kk).ne.0)cnt=cnt+1
       end do
       if (cnt.eq.0) then
          do kk=1,crep%ntrg
             crep%trgext(kk)=2 ! report all extremes
          end do
       end if
       TRG3:do kk=1,crep%ntrg
          if (crep%trgext(kk).eq.0) cycle TRG3
          cnt=0
          imax=0
          imin=0
          vmin=0.0D0
          vmax=0.0D0
          first=.true.
          ij2000=int(crep%tj2000(1)) ! old time
          TIM3: do tt=1,crep%ntim
             if (.not.crep%lfirst(tt).and.bok(tt)) then ! valid time-record...
                call chop0(val30(tt,kk),30)
                lenv=length(val30(tt,kk),30,10)
                if (lenv.ne.0) then
                   read(val30(tt,kk)(1:lenv),*,iostat=irc) val
                   if (irc.ne.0) then
                      write(*,*)myname,"Unable to number from:",val30(tt,kk)(1:lenv)
                      return
                   end if
                   if (first)  then 
                      first=.false.
                      imax=tt
                      imin=tt
                      vmin=val
                      vmax=val
                      ij2000=int(crep%tj2000(tt))
                   else if (ij2000.ne.int(crep%tj2000(tt))) then
                      if (crep%trgext(kk).eq.-1.or.crep%trgext(kk).eq.+2) lok(imin)=.true.
                      if (crep%trgext(kk).eq.+1.or.crep%trgext(kk).eq.+2) lok(imax)=.true.
                      imax=tt
                      imin=tt
                      vmin=val
                      vmax=val
                      ij2000=int(crep%tj2000(tt))
                   else if (val.gt.vmax) then
                      imax=tt
                      vmax=val
                   else if (val.lt. vmin) then
                      imin=tt
                      vmin=val
                   end if
                   cnt=cnt+1
                end if
                ! write(*,*)myname,'Vals:',tt,imin,imax,val30(tt,kk)(1:lenv),val
             end if
          end do TIM3
          if (cnt.ne.0) then
             if (crep%trgext(kk).eq.-1.or.crep%trgext(kk).eq.+2) lok(imin)=.true.
             if (crep%trgext(kk).eq.+1.or.crep%trgext(kk).eq.+2) lok(imax)=.true.
          end if
       end do TRG3
    end if
    if (bdeb) write(*,*)myname,'Writing to file:',crep%ntim
    ! finally write records to file...
    TIM4: do tt=1,crep%ntim
       if (bok(tt).and.lok(tt)) then ! write record
          if (bdeb) write(*,*)myname,'>>> Writing record...'
          first=.true.
          ! write record
          call writeStart(unitx,tt,file,cpar,crep,irc)
          if (crep%ltrg) then
             TRG4:do kk=1,crep%ntrg
                call chop0(val30(tt,kk),30);
                lenx=length(val30(tt,kk),30,10)
                IF (lenx.ne.0) then
                   write(unitx,'(A,A)',advance='no',iostat=irc) &
                        & "' "//trim(crep%trg10(kk))//"='",val30(tt,kk)(1:lenx)
                end if
             end do TRG4
          else
             write(unitx,'(2(A,F0.8))',advance='no',iostat=irc) &
                  & "' max='",crep%valmax(tt),&
                  & "' min='",crep%valmin(tt)
          end if
          write(unitx,'(A)',iostat=irc) &
               & "'/>"
          ! & "' time='"//z25(1:lenz25)//&
          ! & "' epoch='",crep%tj2000(tt),&
       else if (bdeb) then
          write(*,*)myname,'>>> ignoring report...'
       end if
    end do TIM4
  end subroutine WRITERECORDS
  !
  subroutine writeStart(unitx,tt,file,cpar,crep,irc)
    implicit none
    integer :: unitx
    integer :: tt
    type(filetype) :: file
    type(parameter), pointer :: cpar
    type(report), pointer :: crep
    integer :: irc
    character*25 x25,y25,z25,l25,d25
    integer lenx25,leny25,lenz25,lenl25,lend25
    integer :: yy,mm,dd,hh,mi
    real :: sec
    real :: val
    character*4 ayy
    character*2 amm,add,ahh
    character*700 :: keb700,i700
    character*60 :: b60
    integer :: cpos,opos,lenx,leni,lenk,lenb,lenk1,lenk2
    integer :: iepos,ifpos,idd,lenv,ilenv,ii
    character*30 :: val30
    integer, external :: length
    real :: epos,fpos,trg
    real :: rpos,dpos
    logical :: first
    integer,dimension(8) :: values
    CHARACTER*14 MYNAME 
    DATA MYNAME /'writeStart'/
    write(l25,'("+",I0)') nint((crep%tj2000(tt)-file%a2000)*24.0D0);
    call chop0(l25,25)
    lenl25=length(l25,25,3);
    ! write(d25,'("+",I0)') int((crep%tj2000(tt)-int(file%a2000)));
    idd=int((crep%tj2000(tt)-int(file%i2000)));
    if (idd.lt.0) then
       write(d25,'(I0)') idd
    else
       write(d25,'("+",I0)') idd
    end if
    call chop0(d25,25)
    lend25=length(d25,25,3);
    call dj2000(crep%tj2000(tt),yy,mm,dd,hh,mi,sec)
    x25=short25(+0,yy,mm,dd,hh,mi,lenx25) ! dtg
    y25=short25(-1,yy,mm,dd,00,00,leny25) ! date
    z25=short25(+1,00,00,00,hh,mi,lenz25) ! time
    ! write(*,*)myname,"DAY:",tt,crep%tj2000(tt),file%i2000,idd,d25(1:lend25)
    write(unitx,'(A)',advance='no',iostat=irc) &
         & "   <dtg dtg='"//x25(1:lenx25)//&
         & "' date='"//y25(1:leny25)//&
         & "' lead='"//l25(1:lenl25)
    !         & "' lead='"//l25(1:lenl25)//"' day='"//d25(1:lend25)
    if (crep%wmax) then
       write(unitx,'(2(A,F0.8))',advance='no',iostat=irc) &
            & "' maxlat='",crep%llmax(1,tt),&
            & "' maxlon='",crep%llmax(2,tt)
    end if
    if (crep%wmin) then
       write(unitx,'(2(A,F0.8))',advance='no',iostat=irc) &
            & "' minlat='",crep%llmin(1,tt),&
            & "' minlon='",crep%llmin(2,tt)
    end if
    if (crep%lavg.and.crep%valcnt(tt).gt.0.0D0) then
       val=crep%valavg(tt)/crep%valcnt(tt)
       write(unitx,'(A,F0.8)',advance='no',iostat=irc) &
            & "' "//trim(crep%avg10)//"='",val
    end if
    return
  end subroutine writeStart
  !
  subroutine addPar(mxml,nxml,xml700,par,val)
    implicit none
    integer :: mxml
    integer :: nxml
    character*700 :: xml700(2,mxml)
    character*(*) par
    character*(*) val
    integer,external :: length
    integer :: II,lenp,lenv
    logical :: found
    CHARACTER*14 MYNAME 
    DATA MYNAME /'addPar'/
    lenp=len(trim(par))
    found=.false.
    do ii=1,nxml
       lenx=length(xml700(1,ii),700,lenp)
       if (xml700(1,ii)(max(1,lenx-lenp+1):lenx).eq.&
            & trim(par)) then
          found=.true.
          exit
       end if
    end do
    if (.not.found) then
       nxml=min(mxml,nxml+1)
       xml700(1,nxml)=par
       xml700(2,nxml)=val
       call chop0(xml700(1,nxml),700)
       call chop0(xml700(2,nxml),700)
    end if
    return
  end subroutine addPar
  !
  subroutine addXML(crep,par,val)
    implicit none
    type(report), pointer :: crep
    character*(*) par
    character*(*) val
    integer :: II,lenp,lenv
    logical :: found
    CHARACTER*14 MYNAME 
    DATA MYNAME /'addXML'/
    call addPar(mxml,crep%nxml,crep%xml700,par,val)
    return
  end subroutine addXML
  !
  ! subroutine addAux(crep,par,val)
  !   implicit none
  !   type(report), pointer :: crep
  !   character*(*) par
  !   character*(*) val
  !   integer :: II,lenp,lenv
  !   logical :: found
  !   CHARACTER*14 MYNAME 
  !   DATA MYNAME /'addXML'/
  !   integer maux,naux
  !   maux=1
  !   naux=0
  !   call addPar(maux,naux,crep%aux700,par,val)
  !   crep%laux=.true.
  !   return
  ! end subroutine addAux
  !
  subroutine addKEY(crep,par,val)
    implicit none
    type(report), pointer :: crep
    character*(*) par
    character*(*) val
    integer :: II,lenp,lenv
    logical :: found
    CHARACTER*14 MYNAME 
    DATA MYNAME /'addKEY'/
    call addPar(mkey,crep%nkey,crep%key700,par,val)
    return
  end subroutine addKEY
  !
  real function f1970(yy,mm,dd,hh,mi,sec)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    real :: sec
    !     returns "seconds since 1970-01-01 00:00:00 +00:00"
    real days
    !     yy=1970
    !     mm=01
    !     dd=01
    !     mi=00
    !     sec=0.0D0
    !     call m_date2jd2000(days,yy,mm,dd,hh,mi,sec)
    !     write(*,*) 'S1970 1970-reference in J2000:',days!  2440587.5
    call m_date2jd(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    days = days - 2440587.5  ! convert to days since reference
    f1970=days*86400.0D0      ! convert to seconds
    return
  end function f1970
  subroutine m_date2jd (jd, year,month,day,hour,minutes,seconds)
    !     (corresponds to jd2000)
    !     computes julian day from gregorian (civil) calendar
    !     o  (real*8) jd = julian. day
    !     i  (int*4) yy = year
    !     i  (int*4) mm = month
    !     i  (int*4) dd = day
    !     i  (int*4) hh = hour
    !     i  (int*4) mi = minute
    !     i  (real*8) sec = second.
    implicit none
    real jd
    integer year,month,day,hour,minutes,i,j,k
    real seconds
    i= year
    j= month
    k= day
    jd= k-32075+1461*(i+4800+(j-14)/12)/4+367*(j-2-(j-14)/12*12) &
         /12-3*((i+4900+(j-14)/12)/100)/4
    jd=jd+((hour-12.0d0)/24.0d0)+(minutes/1440.0d0)+(seconds/86400.0d0)
    return
  end subroutine m_date2jd
  function short25(type,yy,mm,dd,hh,mi,len)
    implicit none
    character*25 :: short25
    integer :: type ! -1:date, 0:dtg, 1:time
    integer :: yy
    integer :: mm
    integer :: dd
    integer :: hh
    integer :: mi
    integer :: len
    len=0
    if (type.le.0) then ! add date
       if (len.ne.0) then
          short25(len+1:len+1)="_"
          len=len+1
       end if
       write(short25(len+1:len+10),'(I4.4,"-",I2.2,"-",I2.2)') yy,mm,dd
       len=len+10
    end if
    if (type.ge.0) then ! add time
       if (len.ne.0) then
          short25(len+1:len+1)="_"
          len=len+1
       end if
       write(short25(len+1:len+2),'(I2.2)') hh
       len=len+2
       if (mi.ne.0) then
          short25(len+1:len+1)="-"
          len=len+1
          write(short25(len+1:len+2),'(I2.2)') mi
          len=len+2
       end if
    end if
    if (len.eq.0) then
       short25(1:2)="00"
       len=2
    end if
    return
  end function short25

  character*30 function pretty30(val,ilenv)
    real :: val
    integer :: ilenv
    character*30 :: val30
    integer :: lenv,ii
    integer, external :: length
    character*1 :: c1
    C1=CHAR(0)
    write(val30,'(F0.8)',iostat=irc) val
    call chop0(val30,30)
    lenv=length(val30,30,10)
    ilenv=lenv
    do ii=lenv,1,-1
       if (ilenv.eq.ii .and. val30(ii:ii).eq."0") then
          ilenv=ilenv-1
       end if
    end do
    if (ilenv.lt.lenv.and.val30(ilenv:ilenv).eq.".") then
       ilenv=ilenv+1
    end if
    do ii=ilenv+1,min(lenv+1,30)
       val30(ii:ii)=c1
    end do
    pretty30=val30
    !if (ilenv.ne.lenv) then
    !   write(*,*)'Pretty:',val,' -> ',val30(1:ilenv),ilenv
    !end if
    return
  end function pretty30

  logical function isClose(lat,lon,minlat,maxlat,minlon,maxlon,delta)
    real :: lat,lon,minlat,maxlat,minlon,maxlon,delta
    real, parameter :: fact=360.0D0/40075.0D0 ! km -> deg
    real dlon,dlat
    dlat=delta*fact
    dlon=dlat/max(0.1,cosdeg(lat))
    isClose=.false.
    if (lat.ge.minlat-dlat .and. lat.le.maxlat+dlat) then
       if (lon.ge.minlon-dlon .and. lon.le.maxlon+dlon) then
          isClose=.true.
       end if
    end if
    return   
  end function isClose
  ! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
  subroutine combinePolygons(flt,fltB,type,irc)
    implicit none
    type(filter), pointer :: flt, fltB
    character*(*) :: type
    integer :: irc
    CHARACTER*16 MYNAME
    DATA MYNAME /'INTERSECTPOLYGON'/
    integer :: npos, npob
    real, allocatable :: lons(:), lats(:), lonb(:), latb(:)
    integer, parameter :: nwrk=100000
    logical :: llat, llon
    real :: minlon,maxlon,minlat,maxlat
    type(node), pointer :: cnode=>null()
    real     :: rwrk(nwrk)
    integer  :: iwrk(nwrk)
    integer :: offA,offB,offC,lenA,lenB,lenC
    if (.not.associated(flt)) then
       write(*,*)myname,'Invalid primary filter.'
       irc=384
       return
    end if
    if (.not.associated(fltB)) then
       write(*,*)myname,'Invalid secondary filter.'
       irc=384
       return
    end if

    if (flt%mpar.gt.0.and.fltB%mpar.gt.0) then
       ! initiate global polygon
       call initPolygon(irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from initPolygon.',irc
          return
       end if
       if (allocated(lons))deallocate(lons)
       if (allocated(lats))deallocate(lats)
       if (allocated(lonb))deallocate(lonb)
       if (allocated(latb))deallocate(latb)
       ! only use the largest polygon in the set...
       offA=0
       npos=0
       offC=1 ! current offrt node
       do ii=1,flt%mpar
          if (flt%rpar(ii).eq.flt%rpar(1).and.&
               & flt%rpar(ii+flt%mpar).eq.flt%rpar(1+flt%mpar)) then
             lenC=ii-offC ! length of polygon
             if (lenC.gt.npos) then
                offA=offC
                npos=lenC
             end if
             offC=ii-1
          end if
       end do
       offB=0
       npob=0
       offC=1 ! current offrt node
       do ii=1,fltB%mpar
          if (fltB%rpar(ii).eq.fltB%rpar(1).and.&
               & fltB%rpar(ii+fltB%mpar).eq.fltB%rpar(1+fltB%mpar)) then
             lenC=ii-offC ! length of polygon
             if (lenC.gt.npob) then
                offB=offC
                npob=lenC
             end if
             offC=ii-1
          end if
       end do
       write(*,'(X,A,A,6(I0,A))') myname,'Initial reduction:',&
            & flt%mpar,'->',npos,' (',offA,') ',fltB%mpar,'->',&
            & npob,' (',offB,')'
       allocate(lons(npos),lats(npos),lonb(npob),latb(npob),stat=irc)
       if (irc.ne.0) then
          write(*,*)myname,'Unable to allocate lons lats.',irc,npos
          return
       end if
       do ii=1,npos
          lats(ii)=flt%rpar(offA+ii)
          lons(ii)=flt%rpar(offA+ii+flt%mpar)
          !if (ii.lt.10) write(*,*)myname," Flt:",lats(ii),lons(ii)
       end do
       do ii=1,npob
          latb(ii)=fltB%rpar(offB+ii)
          lonb(ii)=fltB%rpar(offB+ii+fltB%mpar)
          !if (ii.lt.10) write(*,*)myname," FlB:",latb(ii),lonb(ii)
       end do
       ! pinpo/addPolygonSegment writes to "global polygon"
       write(*,*)myname,'Calling PP...',npos,npob,type
       if (bdeb) then
          do ii=1,min(3,npos)
             write(*,*) myname,'Node A:',ii,lats(ii),lons(ii)
          end do
          do ii=max(1,npos-3),npos
             write(*,*) myname,'Node A:',ii,lats(ii),lons(ii)
          end do
          do ii=1,min(3,npob)
             write(*,*) myname,'Node B:',ii,latb(ii),lonb(ii)
          end do
          do ii=max(1,npob-3),npob
             write(*,*) myname,'Node B:',ii,latb(ii),lonb(ii)
          end do
       end if
       if (type.eq."intersection") then
          call PPINPO(lons,lats,npos, &
               & lonb,latb,npob, &
               & rwrk,iwrk,nwrk,addPolygonSegment,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from PPINPO.',irc
             return
          end if
       else if (type.eq."union") then
          call PPUNPO(lons,lats,npos, &
               & lonb,latb,npob, &
               & rwrk,iwrk,nwrk,addPolygonSegment,irc)
          if (irc.ne.0) then
             write(*,*)myname,'Error return from PPINPO.',irc
             return
          end if
       else
          write(*,*)myname,'Unknown type:',type
          irc=944
          return
       end if
       if (allocated(lons))deallocate(lons)
       if (allocated(lats))deallocate(lats)
       if (allocated(lonb))deallocate(lonb)
       if (allocated(latb))deallocate(latb)
       ! export global polygon back to flt...
       llat=.false.
       llon=.false.
       if (allocated(flt%rpar)) deallocate(flt%rpar)
       flt%mpar=nodecnt ! nodes in global polygon....
       if (flt%mpar .lt. 3) then
          write(*,*) myname,'Too few nodes in polygon.',flt%mpar
          flt%mpar=0
          return
       end if
       allocate(flt%rpar(flt%mpar*2+4),stat=irc)
       if (irc.ne.0) then
          write(*,*) myname,'Unable to allocate filter-parameters.',&
               & irc,flt%mpar
          return
       end if
       flt%rpar(:)=0.0D0
       flt%lpar(:)=.false.
       flt%npar(:)=0
       ii=0
       cnode=>firstnode%next
       do while (.not.associated(cnode,target=lastnode))
          lat=cnode%lat
          lon=cnode%lon
          !if (abs(lat).lt.0.01D0 .and. abs(lon).lt.0.01D0) write(*,*)myname,'Found zero-node.'
          if (.not.llat) then
             llat=.true.
             minlat=lat
             maxlat=lat
          else
             minlat=min(minlat,lat)
             maxlat=max(maxlat,lat)
          end if
          if (.not.llon) then
             llon=.true.
             minlon=lon
             maxlon=lon
          else
             minlon=min(minlon,lon)
             maxlon=max(maxlon,lon)
          end if
          ii=ii+1
          flt%rpar(ii)=lat
          flt%rpar(ii+flt%mpar)=lon
          cnode => cnode%next
       end do
       write(*,*)myname,' LATLON:',minlat,maxlat,minlon,maxlon
       flt%rpar(flt%mpar*2+1)=minlat
       flt%rpar(flt%mpar*2+2)=maxlat
       flt%rpar(flt%mpar*2+3)=minlon
       flt%rpar(flt%mpar*2+4)=maxlon
       ! simplify filter
       ii=flt%mpar
       if (flt%simplify) then
          call simplify(flt%mpar,flt%rpar,flt%eps)
          if (flt%mpar.gt.ii) then
             write(*,*)myname,'Corrupt simplification.',flt%mpar,ii
             irc=999
             return
          end if
       end if
       ! clear global polygon
       call clearPolygon(irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from clearPolygon.',irc
          return
       end if
    else
       write(*,*)myname,"Strange polygons... Aborting."
       irc=945
       return
    end if
    return
  end subroutine combinePolygons
  !
  subroutine unionPolygon(flt,fltB,irc)
    implicit none
    type(filter), pointer :: flt, fltB
    integer :: irc
    CHARACTER*12 MYNAME
    DATA MYNAME /'UNIONPOLYGON'/
    ! 
    irc=999
    write(*,*)myname,'Routine not implemented...',irc
    return
  end subroutine unionPolygon
  !
  subroutine addPolygonNode(lat,lon,irc)
    implicit none
    real :: lat,lon
    integer :: irc
    type(node), pointer :: cnode=>null()
    CHARACTER*14 MYNAME
    DATA MYNAME /'addPolygonNode'/
    allocate(cnode,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate cnode.',irc
       return
    end if
    cnode%lat=lat
    cnode%lon=lon
    cnode%next => lastnode
    cnode%prev => lastnode%prev
    lastnode%prev%next => cnode
    lastnode%prev => cnode
    nodecnt=nodecnt+1
    nullify(cnode)
    return
  end subroutine addPolygonNode
  !
  subroutine initPolygon(irc)
    implicit none
    integer :: irc
    type(node), pointer :: cnode=>null(),nnode=>null()
    CHARACTER*14 MYNAME 
    DATA MYNAME /'initPolygon'/
    ifirst=.true.
    call clearPolygon(irc)
    if (irc.ne.0) then
       write(*,*)myname,'Error return from clearPolygon.',irc
       return
    end if
    allocate(firstnode,lastnode,stat=irc)
    if (irc.ne.0) then
       write(*,*)myname,'Unable to allocate nodechain.',irc
       return
    end if
    firstnode%next => lastnode
    lastnode%prev=>firstnode
    nodecnt=0
    return
  end subroutine initPolygon
  !
  subroutine clearPolygon(irc)
    implicit none
    integer :: irc
    type(node), pointer :: cnode=>null(),nnode=>null()
    CHARACTER*14 MYNAME 
    DATA MYNAME /'clearPolygon'/
    if (associated(firstnode).and.associated(lastnode)) then
       cnode=>firstnode%next
       do while (.not.associated(cnode,lastnode))
          nnode=>cnode%next
          deallocate(cnode)
          nullify(cnode)
          cnode=>nnode
       end do
    end if
    if (associated(firstnode)) deallocate(firstnode)
    if (associated(lastnode)) deallocate(lastnode)
    nodecnt=0
    return
  end subroutine clearPolygon
  !
  subroutine addPolygonSegment(lons,lats,npos)
    integer :: npos
    real :: lons(npos)
    real :: lats(npos)
    integer :: ii,irc
    real,allocatable :: latlon(:)
    CHARACTER*18 MYNAME
    DATA MYNAME /'ADDPOLYGONSEGMENT'/
    if (allocated(latlon)) deallocate(latlon)
    allocate(latlon(npos*2),stat=irc)
    do ii=1,npos
       latlon(ii)=lats(ii)
       latlon(ii+npos)=lons(ii)
       !if (ii.lt.10) write(*,*)myname," Pos:",lats(ii),lons(ii)
    end do
    if (associated(gfilter)) then
       if (gfilter%simplify) then
          call simplify(npos,latlon,gfilter%eps)
       else
          write(*,*) myname," Segments:",npos
       end if
    else
       write(*,*) myname," Segments:",npos
    end if
    ! add segment
    do ii=1,npos
       ! lat=latlon(ii), lon=latlon(ii+npos)
       call addPolygonNode(latlon(ii),latlon(ii+npos),irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from addPolygonNode.',irc
          return
       end if
       !if (ii.lt.10) write(*,*)myname," Exx:",lats(ii),lons(ii)
    end do
    ! add node to initial position... (multiple polygons)
    if (ifirst) then
       ifirst=.false.
       ilon=lons(1)
       ilat=lats(1)
    else
       ! add initial node
       ! ilat, ilon
       call addPolygonNode(ilat,ilon,irc)
       if (irc.ne.0) then
          write(*,*)myname,'Error return from addPolygonNode.',irc
          return
       end if
    end if
    if (allocated(latlon)) deallocate(latlon)
    return
  end subroutine addPolygonSegment

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name:  select_nth
!!!
!!! Description: Select the Kth smallest element (out of n) in
!!!              the key array.  Leaves ind() semi-sorted,
!!!              with the Kth smallest element in key(ind(k)).
!!!              Leaves ind() in the following state:
!!!              key(ind(0:k-1)) < key(ind(k)) < key(ind(k+1:n))
!!!              For more information, see Sedgewick: _Algorithms_
!!!
!!! Input:       ind - array of indexes into to the key array
!!!              key - what to sort by
!!!              k   - which element to find
!!!
!!! Output:      ind - partially sorted array of indexes
!!!

  subroutine select_nth(ind, key, k)

    implicit none

    ! Parameters
    integer*4 ind(:)            ! array of indexes
    real*8 key(:)               ! sort key
    integer*4 k

    ! Local variables
    integer*4 n, t, i, j, l, r,nv
    real*8 v

    n = size(ind)
    l = 1
    r = n
    do while (r > l)
       v = key(ind(r))
       i = l-1
       j = r
       partition: do
          do 
             i = i+1
             if (key(ind(i)) >= v) exit
          end do
          do
             j = j-1
             if (j <= i) exit
             if (key(ind(j)) <= v) exit
          end do
          t = ind(i)
          ind(i) = ind(j)
          ind(j) = t
          if (j <= i) exit partition
       end do partition
       ind(j) = ind(i)
       ind(i) = ind(r)
       ind(r) = t
       if (i >= k) r = i-1
       if (i <= k) l = i+1
    end do

  end subroutine select_nth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name:  Mergesort
!!!
!!! Description: Sort an array.  Instead of sorting the key array
!!!              itself, the ind array (an array with indexes into
!!!              the key array) is sorted.  Use the mergesort
!!!              algorithm, which is a stable method, preserving
!!!              the initial order of equal elements.
!!!              For details, see Sedgewick: _Algorithms_
!!!
!!! Input:       ind - array of indexes into to the key array
!!!              key - what to sort by
!!!
!!! Output:      ind - sorted array of indexes
!!!

  subroutine mergesort(ind, key)

    implicit none
    integer, target :: ind(:)
    real, target :: key(:)

    integer, target, dimension(size(ind)) :: tmp
    integer*4 n

    n = size(ind)
    call mergesort1(ind, key, tmp, n, 1, n)

  end subroutine mergesort

  recursive subroutine mergesort1(ind, key, tmp, n, l, r)

    implicit none

    integer*4 n
    integer*4 ind(n)
    real*8 key(:)
    integer*4 tmp(size(ind))
    integer*4 l, r

    integer*4 i, j, k, m

    if (r > l) then
       m = (r+l)/2
       call mergesort1(ind, key, tmp, n, l, m)
       call mergesort1(ind, key, tmp, n, m+1, r)
       do i = l, m            !m, l, -1
          tmp(i) = ind(i)
       end do
       do j = m+1, r
          tmp(r+m+1-j) = ind(j)
       end do
       i = l
       j = r
       do k = l, r
          if (key(tmp(i)) < key(tmp(j))) then
             ind(k) = tmp(i)
             i = i+1
          else
             ind(k) = tmp(j)
             j = j-1
          end if
       end do
    end if

  end subroutine mergesort1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name: node_insert
!!!
!!! Description: Insert a number pair into tree.
!!!
!!! Input:       tree - tree to insert into
!!!              lon, lat - number pair to insert
!!!              dir - 0 for lon-branching, 1 for lat-branching
!!!
!!! Output:      root - modified tree
!!!              irc - error return code
!!!
!!! Returns:     new - pointer to new node
!!!

  recursive function node_insert(root, lon, lat, rep, parent, dir, irc) &
       &     result (new)

    implicit none

    type(node2d), pointer :: root
    real*8 lon
    real*8 lat
    type(report), pointer :: rep
    type(node2d), pointer :: parent
    integer*4 dir
    integer*4 irc
    type(node2d), pointer :: new
    type(node2d), pointer :: cparent

    integer*4 newdir
    character (len=15), parameter :: myname = 'NODE_INSERT'

    if (.not.associated(root)) then
       ! New leaf node
       allocate(root, stat=irc)
       if (irc /= 0) then
          write(*,*) myname, "Couldn't allocate memory: ", irc
          irc = 900
          return
       end if
       root%lon = lon
       root%lat = lat
       root%dist = rep%cdist
       root%rep => rep
       root%enabled = .true.
       if (associated(parent)) then
          root%parent=>parent
          root%next=>null()
          cparent=>parent
          do while (associated(cparent))
             if (cparent%dist < root%dist) then
                cparent%dist = root%dist
                cparent => cparent%parent
             else
                cparent => null()
             end if
          end do
       end if
       root%parent => parent
       nullify(root%l, root%r)
       new => root
       return
    else
       newdir = mod(dir+1, 2)
       ! Recursively insert point in either left or right subtree
       if ((dir == 0 .and. lon < root%lon)  &
            &        .or.(dir == 1 .and. lat < root%lat)) then
          new => node_insert(root%l, lon, lat, rep, root, newdir, irc)
       else
          new => node_insert(root%r, lon, lat, rep, root, newdir, irc)
       end if
    end if

  end function node_insert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name: tree_build
!!!
!!! Description:
!!! 
!!!   Build a 2D tree.  To optimize speed of both insertion and
!!!   searching, wee need the tree to be as balanced as possible,
!!!   with minimum depth.  We attempt to reach this goal by
!!!   finding the point with median longitude, using this as our
!!!   root element.  This ensures that left and right sides of
!!!   the tree contains the same number of elements, plus or minus
!!!   one or two.  Further, we insert the rest of the points
!!!   randomly, attempting to balance left and right subtrees
!!!   properly.
!!!
!!! Input:   tree - tree information
!!!          ind - points to process; array with indexes into lon/lat
!!!          lon, lat - longitude and latitude information
!!!
!!! Output:  tree - tree information, now initialised and
!!!                 x and y points inserted
!!!          irc - error return code
!!!

  subroutine tree_build(tree, ind, lon, lat, rep, irc)

    implicit none

    type(tree2d), intent(inout) :: tree ! tree trunk information
    integer, intent(in) :: ind(:)
    real, intent(in) :: lon(:)
    real, intent(in) :: lat(size(lon))
    type(reportptr), intent(in) :: rep(size(lon))
    integer, intent(inout) :: irc

    integer*4 iind(size(ind))
    integer*4 tmpind(size(ind))
    integer*4 i, j, median
    type(node2d), pointer :: median_node=>null()
    character (len=15), parameter :: myname = 'TREE_BUILD'

    integer*4 n                 ! number of entries to sort

    n = size(ind)

    ! Prepare the tree structure
    tree%N = 0
    nullify(tree%root)
    if (n < 1) return
    allocate(tree%list(n), stat=irc)
    if (irc /= 0) then
       write(*,*) myname, "Couldn't allocate memory for list: ", irc
       irc = 901
       return
    end if

    ! First insert element with median longitude.  This is our root node.
    tmpind = ind
    call select_nth(tmpind, lon, (n+1)/2)
    median = tmpind((n+1)/2)
    tree%N = tree%N + 1
    median_node => node_insert(tree%root, &
         &     lon(median), lat(median), rep(median)%ptr,null(), 0, irc)
    if (irc /= 0) then
       write(*,*) myname, 'Insertion of first element failed'
       return
    end if

    ! In order to get a properly balanced tree, we want to insert the rest
    ! of the elements in random order, using iind() to index ind()
    iind(1:n) = (/(i, i = 1, n)/)
    call randomize_array(iind)

    do i = 1, n
       j = ind(iind(i))
       ! Special case for median element
       if (j == median) then
          tree%list(iind(i))%p => median_node
          cycle
       end if
       tree%N = tree%N + 1
       tree%list(iind(i))%p => node_insert(tree%root, &
            &        lon(j), lat(j), rep(j)%ptr, null(), 0, irc)
       if (irc /= 0) then
          write(*,*) myname, 'Insertion of element ', i, ' failed'
          return
       end if
    end do

  end subroutine tree_build

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name: randomize_array
!!!
!!! Description: relocate elements in the array at random
!!!
!!! Input: ind - array to randomize
!!! Output: ind - randomized array
!!!

  subroutine randomize_array(ind)

    implicit none

    integer*4 ind(:)

    real*8 rnd
    integer*4 n
    integer*4 i, j, tmp

    n = size(ind)
    do i = 1, n
       call random_number(rnd)
       j = int(1 + real(n)*rnd)
       j = min(max(j, 1), n)  ! Ensure 1 <= j <= n
       tmp = ind(i)
       ind(i) = ind(j)
       ind(j) = tmp
    end do

  end subroutine randomize_array


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name: branch_chop
!!!
!!! Description: Recursively traverse and deallocate
!!!              all branches and nodes in a (sub)tree
!!!
!!! Input:       tree - tree to chop down
!!!
!!! Output:      tree - chopped down tree
!!!

  recursive subroutine branch_chop(tree)

    implicit none
    type(node2d), pointer :: tree

    if (associated(tree)) then
       ! Remove left branch
       if (associated(tree%l)) call branch_chop(tree%l)
       ! Remove right branch
       if (associated(tree%r)) call branch_chop(tree%r)
       ! Remove root node and disassociate it
       deallocate(tree)
       nullify(tree)
    end if

  end subroutine branch_chop


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name: tree_destroy
!!!
!!! Description: Destroy a 2d tree
!!!
!!! Input:       tree - tree to chop down
!!!
!!! Output:      tree - chopped down tree
!!!

  subroutine tree_destroy(tree)

    implicit none
    type(tree2d) :: tree

    tree%N = 0
    if (associated(tree%list)) then
       deallocate(tree%list)
       nullify(tree%list)
    end if
    call branch_chop(tree%root)

  end subroutine tree_destroy


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name: distance_deg
!!!
!!! Description: Distance between two points on a spherical shell
!!!
!!! Input:       alon, alat, blon, blat - lon/lat for points
!!!
!!! Returns:     distance between point a and b (in degrees)
!!!

  real*8 function distance_deg(alon, alat, blon, blat)

    implicit none

    real*8 alon
    real*8 alat
    real*8 blon
    real*8 blat
    real*8 d

    real, external :: cosdeg, sindeg, acosdeg

    d = sindeg(alat)*sindeg(blat) + &
         &     cosdeg(alat)*cosdeg(blat)*cosdeg(alon-blon)
    d = max(-1.0, min(1.0, d))
    distance_deg = abs(acosdeg(d))

  end function distance_deg


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name: rangesearch
!!!
!!! Description: Check if a 2D tree contains a node within
!!!              maxdist distance away from a given point
!!!
!!! Input: root - root node of subtree to search
!!!        lat,lon - position to check
!!!        dist - max. radial distance (in degrees)
!!!               away from node pointed to by self
!!!        dir - 0->search in lon direction, 1->lat direction
!!!
!!! Returns: .true. if a node was found, .false. otherwise
!!!       nodes are identified using searchfirst, searchlast 
!!!

  recursive logical function rangesearch(root, lat, lon, dir)&
       &     result (success)

    implicit none

    type(node2d), pointer :: root
    real*8 lon
    real*8 lat
    integer*4 dir

    integer*4 newdir
    logical*4 d, goleft, goright

    success=.false.
    if (associated(root)) then

!!! Check current node for match
       if (root%enabled .and.  &
            & distance_deg(lon, lat, root%lon, root%lat) <= root%dist) then
          call addSearchNode(root);
          !write(*,*) 'Found node:', root%lon, root%lat, root%dist
          success = .true.
       end if

!!! Determine which subtree(s) we must search in
       if (dir == 0) then
          d = distance_deg(lon, lat, root%lon, lat) <=root%dist
          goleft = d .or. root%lon >= lon
          goright = d .or. root%lon <= lon
       else
          d = distance_deg(lon, lat, lon, root%lat) <=root%dist
          goleft = d .or. root%lat >= lat
          goright = d .or. root%lat <= lat
       end if

!!! Change sorting direction for next level
       newdir = mod(dir+1, 2)
       if (goleft) then
          success = rangesearch(root%l, lat, lon, newdir).or.success
       end if
       if (goright) then
          success = rangesearch(root%r, lat, lon, newdir).or.success
       end if
    else
       success = .false.
    end if
  end function rangesearch
  !
  subroutine initSearchNode()
    implicit none
    searchfirst=>null()
    searchlast=>null()
    return
  end subroutine initSearchNode
  !
  subroutine addSearchNode(node)
    implicit none
    type(node2d), pointer :: node
    if (.not.associated(searchfirst)) then
       searchfirst=>node
    else
       searchlast%next=>node
    end if
    searchlast=>node
    searchlast%next=>null()
    return
  end subroutine addSearchNode
  !
end subroutine MNCMASK

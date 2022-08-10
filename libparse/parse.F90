module parse
  IMPLICIT NONE
  !
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  ! Global constants
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  !
  INTEGER, PARAMETER :: rn = KIND(0.0d0)          ! Precision of real numbers
  INTEGER, PARAMETER :: is = 4                    ! Data type of bytecode
  logical     :: parse_bdeb=.false.
  logical     :: parse_init=.false.
  real(rn)    :: secperday = 86400.0D0
  integer,dimension(8) :: val8    
  real :: days
  real    :: eps  = 0.0D0
  logical :: leps = .false.
  real    :: t0 = 273.15
  real    :: t1 = 273.16
  !
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  ! Fortran 90 function parser v1.1
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  !
  ! This function parser module is intended for applications where a set of mathematical
  ! fortran-style expressions is specified at runtime and is then evaluated for a large 
  ! number of variable values. This is done by compiling the set of function strings 
  ! into byte code, which is interpreted efficiently for the various variable values. 
  !
  ! The source code is available from http://fparser.sourceforge.net
  !
  ! Please send comments, corrections or questions to the author:
  ! Roland Schmehl <roland.schmehl@alumni.uni-karlsruhe.de>
  !
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  ! The function parser concept is based on a C++ class library written by  Juha 
  ! Nieminen <warp@iki.fi> available from http://warp.povusers.org/FunctionParser/
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  PUBLIC :: parse_open,   & ! Open parse session
       parse_close,       & ! Close parse session
       parse_parsef,      & ! Parse single function string
       parse_evalf,       & ! Evaluate single function
       parse_evals,       & ! Evaluate single function
       parse_used,        & ! which targets are used?
       parse_EvalErrMsg,  &! Error message (Use only when EvalErrType>0)
       parse_date_and_time ! date and time which uses loop offset
  INTEGER, PUBLIC            :: EvalErrType ! =0: no error occured, >0: evaluation error
  !  !------- -------- --------- --------- --------- --------- --------- --------- -------
  !  PRIVATE
  !  SAVE
  integer,                                  PARAMETER :: parse_laa=ichar('a')
  integer,                                  PARAMETER :: parse_lzz=ichar('z')
  integer,                                  PARAMETER :: parse_uaa=ichar('A')
  integer,                                  PARAMETER :: parse_uzz=ichar('Z')
  integer,                                  PARAMETER :: parse_und=ichar('_')
  INTEGER(is),                              PARAMETER :: parse_delay       = 0,&
       parse_empty       = 1,&
       parse_constant    = 2,&
       parse_internal    = 3,&
       parse_variable    = 4,&
       parse_expression  = 5
  INTEGER(is),                              PARAMETER :: cImmed     = 1,          &
       cNeg       = 2,          &
       cAdd       = 3,          & 
       cSub       = 4,          & 
       cMul       = 5,          & 
       cDiv       = 6,          & 
       cPow       = 7,          & 
       cAbs       = 8,          &
       cExp       = 9,          &
       cLog10     = 10,         &
       cLog       = 11,         &
       cSqrt      = 12,         &
       cmsgmax    = 13,         &
       cmsgmin    = 14,         &
       cmsgclosest= 15,         &
       cismember  = 16,         &
       cisbelow   = 17,         &
       cisabove   = 18,         &
       cisbetween = 19,         &
       cthinned   = 20,         &
       cand       = 21,         &
       cor        = 22,         &
       cnot       = 23,         &
       c1970yy    = 24,         &
       c1970mm    = 25,         &
       c1970dd    = 26,         &
       c1970hh    = 27,         &
       c1970mi    = 28,         &
       c1970      = 29,         &
       cjulianyy  = 30,         &
       cjulianmm  = 31,         &
       cjuliandd  = 32,         &
       cjulianhh  = 33,         &
       cjulianmi  = 34,         &
       cjulian    = 35,         &
       cMidnight  = 36,         &
       cNow       = 37,         &
       cRound     = 38,         &
       cVarName   = 39,         &
       cValidRange= 40,         &
       cShpPre    = 41,         &
       cShpVic    = 42,         &
       ctd2q      = 43,         &
       crh2td     = 44,         &
       ctd2rh     = 45,         &
       cq2rh      = 46,         &
       ck2c       = 47,         &
       cc2k       = 48,         &
       cSinh      = 49,         &
       cCosh      = 50,         &
       cTanh      = 51,         &
       cSin       = 52,         &
       cCos       = 53,         &
       cTan       = 54,         &
       cAsin      = 55,         &
       cAcos      = 56,         &
       cAtan2     = 57,         &
       cAtan      = 58,         &
       VarBegin   = 59
  CHARACTER (LEN=1), DIMENSION(cAdd:cPow),  PARAMETER :: Ops        = (/ '+',     &
       '-',     &
       '*',     &
       '/',     &
       '^' /)
  CHARACTER (LEN=10), DIMENSION(cAbs:cAtan), PARAMETER :: Funcs      = (/ 'abs       ', &
       'exp       ', &
       'log10     ', &
       'log       ', &
       'sqrt      ', &
       'msgmax    ', &
       'msgmin    ', &
       'msgclosest', &
       'ismember  ', &
       'isbelow   ', &
       'isabove   ', &
       'isbetween ', &
       'thinned   ', &
       'and       ', &
       'or        ', &
       'not       ', &
       'sec1970yy ', &
       'sec1970mm ', &
       'sec1970dd ', &
       'sec1970hh ', &
       'sec1970mi ', &
       'sec1970   ', &
       'julianyy  ', &
       'julliamm  ', &
       'juliandd  ', &
       'julianhh  ', &
       'julianmi  ', &
       'julian    ', &
       'midnight  ', &
       'now       ', &
       'round     ', &
       'name      ', &
       'range     ', &
       'precinct  ', &
       'vicinity  ', &
       'td2q      ', &
       'rh2td     ', &
       'td2rh     ', &
       'q2rh      ', &
       'k2c       ', &
       'c2k       ', &
       'sinh      ', &
       'cosh      ', &
       'tanh      ', &
       'sin       ', &
       'cos       ', &
       'tan       ', &
       'asin      ', &
       'acos      ', &
       'atan2     ', &
       'atan      ' /)
  integer :: nconst =0 ! number of constant variables (including rerun)
  integer :: srerun =0 ! start index for rerun variables
  integer :: nrerun =0 ! number of rerun variables
  CHARACTER (LEN=25), allocatable :: Const(:)
  real(rn), allocatable  :: constval(:)
  type :: parse_shape
     integer :: index =0                    ! shape index 
     !CHARACTER(len=:), allocatable :: name  ! name of shape...
     CHARACTER(len=80), allocatable :: name  ! name of shape...
     integer :: lenn =0                     ! length of name
     real :: minxx,maxxx,minyy,maxyy    ! bounding box
     real :: map(3,3), minzz, maxrad
     logical :: offset = .false.            !
     integer :: nll = 0                     ! number of nodes in shape
     real, allocatable :: yy(:),xx(:)       ! shape node in degrees
     integer :: npos = 0                    ! number of pos nodes in shape
     real, allocatable :: pos(:,:)          ! position of nodes
     real :: mimi(3),mima(3),mami(3),mama(3)! bounding box (lon,lat)
     logical, allocatable :: actual(:)      ! is segment actual
  end type parse_shape
  !
  type :: parse_shapefile
     character*250 :: shp250=""             ! default shapefile
     integer ::  lens = 0                   ! length of shapefile name
     character*11 :: cname11                ! db field name
     integer ::  lenc = 0                   ! length of db field name
     integer ::  lenn = 0                   ! allocated length of names
     integer :: nshp =0                     ! number of shapes
     type(parse_shape),allocatable :: shp(:)
     real ::    shapeidlat = -999.0D0
     real ::    shapeidlon = -999.0D0
     integer :: shapeid = 0
     real ::    vicinitylat = -999.0D0
     real ::    vicinitylon = -999.0D0
     integer :: vicinity = 0
  end type parse_shapefile
  type(parse_shapefile) :: sf

  !
  TYPE,PUBLIC ::  parse_session
     INTEGER(is), DIMENSION(:), POINTER   :: ByteCode => null()
     INTEGER                              :: ByteCodeSize
     REAL(rn),    DIMENSION(:), POINTER   :: Immed => null()
     INTEGER                              :: ImmedSize
     REAL(rn),    DIMENSION(:), POINTER   :: Stack => null()
     integer                              :: nPos=0
     REAL(rn),    DIMENSION(:,:), POINTER :: Stacka => null()
     INTEGER                              :: StackSize,StackPtr
     real(rn),    DIMENSION(:), POINTER   :: Wrka => null()
     INTEGER                              :: ArgsSize
     INTEGER,     DIMENSION(:), POINTER   :: ArgsByte => null()
     INTEGER                              :: ArgsPtr
     !character(len=700), allocatable        :: cbuff   ! some functions write to the string buffer
     character*700        :: cbuff   ! some functions write to the string buffer
     integer                              :: clen=0  ! length of cbuff
     integer :: VarEnd                         ! VarBegin+nVar
     character*100 :: funcStr100=""
     integer :: lenf =0
  END TYPE parse_session
  type parse_pointer
     type(parse_session), pointer  :: ptr => null()
  end type parse_pointer
  !
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  ! Rerun variables
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  ! 
  integer :: rerun_start = 0
  integer :: rerun_nvar = 0
  character*80,allocatable :: rerun_var80(:)
  integer,allocatable :: rerun_lenv(:)
  real,allocatable :: rerun_value(:)
  character*250 :: rerun_off250 =""
  integer :: rerun_leno=0
  type(parse_session), pointer :: rerun_offset => null()  ! offset parse pointer
  !
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  ! Source code
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  !
CONTAINS
  !	
  subroutine parse_initialise()
    real :: ss
    if (parse_init) return
    call date_and_time(VALUES=val8)
    ss=real(val8(7))
    call date2jd( days,val8(1),val8(2),val8(3),&
         & val8(5),val8(6),ss)
    parse_init=.true.
    return
  end subroutine parse_initialise

  subroutine parse_date_and_time(values)
    integer,dimension(8) :: values
    character*250 :: crc250
    integer :: irc
    real :: offset
    real :: ss
    call parse_initialise()
    offset=parse_getTimeOffset(crc250,irc)
    ! retrieve date from julian days...
    call jd2date( days+offset,values(1),values(2),values(3),&
         & values(5),values(6),ss)
    values(7)=int(ss)
    return
  end subroutine parse_date_and_time

  SUBROUTINE parse_open (css,crc250,irc)
    IMPLICIT NONE
    type(parse_session), pointer :: css
    character*250 :: crc250
    integer :: irc
    integer :: ii,nshp
    character*22 :: myname ="parse_open"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (.not.allocated(const).or..not.allocated(constval)) then
       if (allocated(const))deallocate(const)
       if (allocated(constval))deallocate(constval)
       ! create a constant variable with name shp%name... and value shp%index
       nshp=0
       do ii=1,sf%nshp
          if (sf%shp(ii)%index.eq.ii) then ! only count unique indexes
             nshp=nshp+1
          end if
       end do
       nconst=3+nshp+rerun_nvar
       rerun_start=3+nshp
       allocate(const(0:nconst),constval(0:nconst))
       const(0)='unknown'
       constval(0)=0
       nshp=0
       do ii=1,sf%nshp
          if (parse_bdeb) write(*,*) ">>",ii,sf%shp(ii)%index,trim(sf%shp(ii)%name)
          if (sf%shp(ii)%index.eq.ii) then ! only count unique indexes
             nshp=nshp+1
             const(nshp)=camelCase(sf%shp(ii)%name,25)
             constval(nshp)=sf%shp(ii)%index
             if (parse_bdeb) write(*,*) "  ",nshp,nint(constval(nshp)),trim(const(nshp))
          end if
       end do
       const(1+nshp)='pi'
       const(2+nshp)='e'
       const(3+nshp)='na'
       if(parse_bdeb)write(*,*)myname,"Nvar:",rerun_nvar,rerun_var80
       do ii=1,rerun_nvar
          const(3+nshp+ii)=rerun_var80(ii)(1:rerun_lenv(ii))
       end do
       constval(1+nshp)=3.14159265359
       constval(2+nshp)=2.71828182846
       constval(3+nshp)=1.7D38
       do ii=1,rerun_nvar
          constval(3+nshp+ii)=rerun_value(ii)
       end do
       if(parse_bdeb)write(*,*)myname,"Added constants:",nconst
    end if
    ! css must be nullified if not declared...
    if (.not.associated(css)) then
       ALLOCATE (css,stat=irc)
       if (irc.ne.0) then
          call parse_errorappend(crc250,myname//" Unable to allocate session.")
          call parse_errorappendi(crc250,irc)
          call parse_errorappend(crc250,"\n")
          return
       end if
    end if
    if (associated(css%ByteCode)) deallocate(css%ByteCode)
    if (associated(css%Immed)) deallocate(css%Immed)
    if (associated(css%Stack)) deallocate(css%Stack)
    if (associated(css%Stacka)) deallocate(css%Stacka)
    if (associated(css%Wrka)) deallocate(css%Wrka)
    if (associated(css%ArgsByte)) deallocate(css%ArgsByte)
    !if (allocated(css%cbuff)) deallocate(css%cbuff)
    NULLIFY (css%ByteCode,css%Immed,css%Stack,css%Stacka,css%Wrka,css%ArgsByte)
    !css%parse_laa=ichar('a')
    !css%parse_lzz=ichar('z')
    !css%parse_uaa=ichar('A')
    !css%parse_uzz=ichar('Z')
    !css%parse_und=ichar('_')
    css%npos=0
    call parse_initialise()
    return
  END SUBROUTINE parse_open
  !
  SUBROUTINE parse_close (css,crc250,irc)
    IMPLICIT NONE
    type(parse_session), pointer :: css
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="parse_close"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (associated(css)) then
       IF (ASSOCIATED(css%ByteCode)) DEALLOCATE ( css%ByteCode,stat=irc)
       IF (ASSOCIATED(css%Immed))    DEALLOCATE ( css%Immed,   stat=irc)
       IF (ASSOCIATED(css%Stack))    DEALLOCATE ( css%Stack,   stat=irc)
       IF (ASSOCIATED(css%Stacka))   DEALLOCATE ( css%Stacka,  stat=irc)
       IF (ASSOCIATED(css%Wrka))     DEALLOCATE ( css%Wrka,    stat=irc)
       IF (ASSOCIATED(css%ArgsByte)) DEALLOCATE ( css%ArgsByte,stat=irc)
       !if (allocated(css%cbuff)) deallocate(css%cbuff)
       deallocate(css,stat=irc)
       nullify(css)
    end if
    return
  END SUBROUTINE parse_close
  !
  subroutine parse_setshapefile(path250,cname11,crc250,irc)
    USE,INTRINSIC :: ISO_C_BINDING
    USE shape
    implicit none
    character*250 :: path250
    character*11 :: cname11
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    INTEGER                         :: nvar = 0
    TYPE(shpfileobject) :: shphandle
    TYPE(shpobject) :: shpobj
    INTEGER :: ii, jj, kk, iname, lendec
    INTEGER :: nshpr, tshpr, nfield, nrec, nd, ftype
    REAL(kind=c_double) :: minbound(4), maxbound(4)
    real :: rr
    character(len=11) :: cname
    character*22 :: myname="parse_setshapefile"
    real :: north(3) =(/0.0D0,0.0D0,1.0D0/)
    logical :: first
    real :: spos(3),epos(3)
    !
    if(parse_bdeb)write(*,*)myname,' Entering.',irc
    sf%shp250=path250
    call chop0(sf%shp250,250)
    sf%lens=length(sf%shp250,250,10)
    sf%cname11 = cname11 !  "SOVEREIGNT"
    sf%lenc=length(sf%cname11,11,10)
    if (sf%lenc.eq.0) then
       sf%cname11 =  "SOVEREIGNT"
       sf%lenc=length(sf%cname11,11,10)
    end if
    if(parse_bdeb)write(*,*)myname," Path: '"//sf%shp250(1:sf%lens)//"'"
    ! remove old shapefile data
    call parse_clearshapefile(crc250,irc)
    if (irc.ne.0) then
       call parse_errorappend(crc250,myname//" Error return from clearshapeFile")
       call parse_errorappendi(crc250,irc)
       call parse_errorappend(crc250,"\n")
       return
    end if
    ! read new shape file...
    shphandle = shpopen(sf%shp250(1:sf%lens), 'rb')
    ! error check
    IF (shpfileisnull(shphandle) .OR. dbffileisnull(shphandle)) THEN
       call parse_errorappend(crc250,myname//" Error opening '"//sf%shp250(1:sf%lens)//"' for reading\n")
       irc=458
       return
    ENDIF
    ! get general information about the shapefile object
    CALL shpgetinfo(shphandle, nshpr, tshpr, minbound, maxbound, nfield, nrec)
    if (parse_bdeb) then
       write(*,*)myname,'Shpgetinfo, shapes:',nshpr
       write(*,*)myname,'shpgetinfo, type of shapes:',tshpr,'(Polygon=',shpt_polygon,')'
       write(*,*)myname,'shpgetinfo, number of db-fields',nfield
       write(*,*)myname,'shpgetinfo, number of db-records',nrec
    end if
    ! get field "name" index
    iname=dbfgetfieldindex (shphandle,sf%cname11)
    if (iname.eq.-1) then
       call parse_errorappend(crc250,myname//" Error in dbfgetfieldindex ")
       call parse_errorappendi(crc250,ii)
       call parse_errorappend(crc250,"\n")
       irc=458
    end if
    cname="" ! use iname to identify field...
    ftype=dbfgetfieldinfo (shphandle, iname, cname, sf%lenn, lendec)
    sf%nshp=nshpr
    allocate(sf%shp(sf%nshp),stat=irc)
    if (irc.ne.0) then
       call parse_errorappend(crc250,myname//" Unable to allocate shape:")
       call parse_errorappendi(crc250,sf%nshp)
       call parse_errorappendi(crc250,irc)
       call parse_errorappend(crc250,"\n")
       return
    end if
    ! read the nshp shapes
    DO ii = 1, nshpr
       jj=ii-1
       sf%shp(ii)%index=ii
       !allocate(character(len=sf%lenn) :: sf%shp(ii)%name)
       allocate(sf%shp(ii)%name,stat=irc)
       if (irc.ne.0) then
          call parse_errorappend(crc250,myname//" Unable to allocate shape-name:")
          call parse_errorappendi(crc250,sf%lenn)
          call parse_errorappendi(crc250,irc)
          call parse_errorappend(crc250,"\n")
          return
       end if
       ! read databse attribute
       CALL dbfreadattribute(shphandle, jj, iname, sf%shp(ii)%name)
       sf%shp(ii)%lenn=len_trim(sf%shp(ii)%name)
       nameloop: do kk=1,ii
          if (sf%shp(ii)%name.eq.sf%shp(kk)%name) then
             sf%shp(ii)%index=kk
             exit nameloop
          end if
       end do nameloop
       !if (trim(sf%shp(ii)%name).eq."Norway")then
       !   do kk=1,shpobj%nvertices
       !      write(*,*) kk, shpobj%padfx(kk),shpobj%padfy(kk)
       !   end do
       !end if
       ! read the i-th shape from the shapefile object and obtain a shape object
       shpobj = shpreadobject(shphandle, jj)
       ! error check
       IF (shpisnull(shpobj)) THEN
          irc=348
          call parse_errorappend(crc250,myname//" Error in shpreadobject.")
          call parse_errorappendi(crc250,jj)
          call parse_errorappendi(crc250,irc)
          call parse_errorappend(crc250,"\n")
          return
       ENDIF
       ! rewind shape-rings..
       call shpretraceobject(shphandle,shpobj,leps,eps)
       !
       sf%shp(ii)%nll=shpobj%nseg
       do jj=1,shpobj%nvertices
          if (shpobj%valid(jj)) then
             sf%shp(ii)%nll=sf%shp(ii)%nll+1
          end if
       end do
       if(parse_bdeb)write(*,*)'Number of segments:',shpobj%nvertices,&
            & '+',shpobj%nseg,'->',sf%shp(ii)%nll
       if (sf%shp(ii)%nll.lt.3) then ! sanity check
          irc=945
          call parse_errorappend(crc250,myname//" Invalid shape-ll:")
          call parse_errorappendi(crc250,sf%shp(ii)%nll)
          call parse_errorappend(crc250,":")
          call parse_errorappendi(crc250,irc)
          call parse_errorappend(crc250,"\n")
          return
       end if
       sf%shp(ii)%npos=sf%shp(ii)%nll
       allocate(sf%shp(ii)%xx(sf%shp(ii)%nll),sf%shp(ii)%yy(sf%shp(ii)%nll),&
            & sf%shp(ii)%pos(3,sf%shp(ii)%npos),sf%shp(ii)%actual(sf%shp(ii)%npos),stat=irc)
       if (irc.ne.0) then
          call parse_errorappend(crc250,myname//" Unable to allocate shape-ll:")
          call parse_errorappendi(crc250,sf%shp(ii)%nll)
          call parse_errorappendi(crc250,irc)
          call parse_errorappend(crc250,"\n")
          return
       end if
       kk=0
       do jj=1,shpobj%nvertices
          if (shpobj%valid(jj)) then
             kk=kk+1
             call shape_lonlat2pos(shpobj%padfx(jj),shpobj%padfy(jj),&
                  & sf%shp(ii)%pos(1,kk))
             sf%shp(ii)%actual(kk)=shpobj%actual(jj)
          end if
       end do
       do while (shpLoopSegment(shpobj,spos,epos))
          kk=kk+1
          call shape_lonlat2pos(epos(1),epos(2),&
               & sf%shp(ii)%pos(1,kk))
          sf%shp(ii)%actual(kk)=.false.
       end do
       ! get map center
       sf%shp(ii)%map(1,3)=0.0D0
       sf%shp(ii)%map(2,3)=0.0D0
       sf%shp(ii)%map(3,3)=0.0D0
       do jj=1,sf%shp(ii)%npos
          sf%shp(ii)%map(1,3)=sf%shp(ii)%map(1,3)+sf%shp(ii)%pos(1,jj)
          sf%shp(ii)%map(2,3)=sf%shp(ii)%map(2,3)+sf%shp(ii)%pos(2,jj)
          sf%shp(ii)%map(3,3)=sf%shp(ii)%map(3,3)+sf%shp(ii)%pos(3,jj)
       end do
       rr=dsqrt(shape_dot(sf%shp(ii)%map(1,3),sf%shp(ii)%map(1,3)))
       if (rr.lt.1.0D-10) then ! shape covers half the world...
          irc=843
          call parse_errorappend(crc250,myname//" Great circle shape: "//trim(sf%shp(ii)%name))
          call parse_errorappendi(crc250,ii)
          call parse_errorappend(crc250,"\n")
          return
       end if
       sf%shp(ii)%map(1,3)=sf%shp(ii)%map(1,3)/rr
       sf%shp(ii)%map(2,3)=sf%shp(ii)%map(2,3)/rr
       sf%shp(ii)%map(3,3)=sf%shp(ii)%map(3,3)/rr
       call shape_cross(north,sf%shp(ii)%map(1,3),sf%shp(ii)%map(1,1))
       rr=dsqrt(shape_dot(sf%shp(ii)%map(1,1),sf%shp(ii)%map(1,1)))
       ! get vector along X-axis (west): map(:,1)
       if (rr.lt.1.0D-10) then ! shape center is towards north pole
          sf%shp(ii)%map(1,1)=1.0D0
          sf%shp(ii)%map(2,1)=0.0D0
          sf%shp(ii)%map(3,1)=0.0D0
       else
          sf%shp(ii)%map(1,1)=sf%shp(ii)%map(1,1)/rr
          sf%shp(ii)%map(2,1)=sf%shp(ii)%map(2,1)/rr
          sf%shp(ii)%map(3,1)=sf%shp(ii)%map(3,1)/rr
       end if
       ! get vector along Y-axis (north): map(:,2)
       call shape_cross(sf%shp(ii)%map(1,3),sf%shp(ii)%map(1,1),sf%shp(ii)%map(1,2))
       rr=dsqrt(shape_dot(sf%shp(ii)%map(1,2),sf%shp(ii)%map(1,2)))
       if (rr.lt.1.0D-10) then ! error
          irc=845
          call parse_errorappend(crc250,myname//" Failed sanity check: "//trim(sf%shp(ii)%name))
          call parse_errorappendi(crc250,ii)
          call parse_errorappend(crc250,"\n")
          return
       else ! not necessary... in theory
          sf%shp(ii)%map(1,2)=sf%shp(ii)%map(1,2)/rr
          sf%shp(ii)%map(2,2)=sf%shp(ii)%map(2,2)/rr
          sf%shp(ii)%map(3,2)=sf%shp(ii)%map(3,2)/rr
       end if
       first=.true.
       do jj=1,sf%shp(ii)%npos
          sf%shp(ii)%xx(jj)=shape_dot(sf%shp(ii)%map(1,1),sf%shp(ii)%pos(1,jj))
          sf%shp(ii)%yy(jj)=shape_dot(sf%shp(ii)%map(1,2),sf%shp(ii)%pos(1,jj))
          if (first) then
             first=.false.
             sf%shp(ii)%minzz=shape_dot(sf%shp(ii)%map(1,3),sf%shp(ii)%pos(1,jj))
             sf%shp(ii)%minxx=sf%shp(ii)%xx(jj)
             sf%shp(ii)%minyy=sf%shp(ii)%yy(jj)
             sf%shp(ii)%maxxx=sf%shp(ii)%xx(jj)
             sf%shp(ii)%maxyy=sf%shp(ii)%yy(jj)
          else
             sf%shp(ii)%minzz=min(sf%shp(ii)%minzz,&
                  & shape_dot(sf%shp(ii)%map(1,3),sf%shp(ii)%pos(1,jj)))
             sf%shp(ii)%minxx=min(sf%shp(ii)%minxx,sf%shp(ii)%xx(jj))
             sf%shp(ii)%minyy=min(sf%shp(ii)%minyy,sf%shp(ii)%yy(jj))
             sf%shp(ii)%maxxx=max(sf%shp(ii)%maxxx,sf%shp(ii)%xx(jj))
             sf%shp(ii)%maxyy=max(sf%shp(ii)%maxyy,sf%shp(ii)%yy(jj))
          end if
       end do
       sf%shp(ii)%maxrad=acos(sf%shp(ii)%minzz)
       if (parse_bdeb)write(*,'(4(X,F10.3),X,A)') &
            & shape_rtodeg(sf%shp(ii)%maxrad),(sf%shp(ii)%map(1,jj),jj=1,3), &
            & trim(sf%shp(ii)%name)
       if (sf%shp(ii)%minzz.lt.0.0D0) then ! shape covers more than half the world...
          if (parse_bdeb) then
             do jj=1,sf%shp(ii)%npos
                write(*,*)myname,'Pos:',jj,sf%shp(ii)%pos(1,jj),sf%shp(ii)%pos(2,jj),sf%shp(ii)%pos(3,jj)
             end do
             write(*,*)myname,'Center:',sf%shp(ii)%map(1,3),sf%shp(ii)%map(2,3),sf%shp(ii)%map(3,3)
             write(*,*)myname,'Minzz:',sf%shp(ii)%minzz,trim(sf%shp(ii)%name)
          end if
          irc=844
          call parse_errorappend(crc250,myname//" Too large shape: "//trim(sf%shp(ii)%name))
          call parse_errorappendi(crc250,ii)
          call parse_errorappend(crc250,"\n")
          return
       end if
       if (first) then ! shape has no nodes
          irc=899
          call parse_errorappend(crc250,myname//" Too small shape: "//trim(sf%shp(ii)%name))
          call parse_errorappendi(crc250,ii)
          call parse_errorappend(crc250,"\n")
          return
       end if
       !write(*,'(X,A,X,I0,6(X,F6.1),2X,I5,X,A,X,F6.1)') &
       !     & 'dbfreadattribute ', ii, &
       !     & shpobj%padfx(1),sf%shp(ii)%minxx,sf%shp(ii)%minxx, &
       !     & shpobj%padfy(1),sf%shp(ii)%minyy,sf%shp(ii)%maxyy, &
       !     & sf%shp(ii)%nll,trim(sf%shp(ii)%name)
       !
       ! now access all the components of the shape object
       ! number of vertices
       ! write(*,*)myname,'shpreadobject, number of vertices',ii,shpobj%nvertices
       ! write(*,*)myname,'shpreadobject, x:',ii
       ! write(*,*)myname,shpobj%padfx(:)
       ! write(*,*)myname,'shpreadobject, y:',ii
       ! write(*,*)myname,shpobj%padfy(:)
       ! write(*,*)myname,'shpreadobject, z:',ii
       ! write(*,*)myname,shpobj%padfz(:)
       !
       ! destroy the shape object to avoid memory leaks
       ! notice that for accessing dbf attributes the shape object is not required
       CALL shpdestroyobject(shpobj)
    ENDDO
    
    ! close the shapefile object
    CALL shpclose(shphandle)
    !
  end subroutine parse_setshapefile
  !
  subroutine parse_simplifyShapes(tol20,crc250,irc)
    use shape
    implicit none
    character*20 :: tol20
    character*250 :: crc250
    integer :: irc
    integer :: ii,lent
    integer, external :: length
    character*22 :: myname="parse_simplifyShapes"
    call chop0(tol20,20)
    lent=length(tol20,20,10)
    read (tol20(1:lent),*,iostat=irc) eps
    if (irc.ne.0) then
       call parse_errorappend(crc250,myname//" Invalid tolerance:")
       call parse_errorappendr(crc250,eps)
       call parse_errorappend(crc250,"\n")
       return
    end if
    leps=.true.
    if (parse_bdeb) write(*,*)myname,'Tolerance:',eps
    return
  end subroutine parse_simplifyShapes
  !
  subroutine parse_clearshapefile(crc250,irc)
    implicit none
    character*250 :: crc250
    integer :: irc
    integer :: ii
    character*22 :: myname="parse_clearshapefile"
    if (parse_bdeb)write(*,*)myname,'Entering.',sf%nshp
    if (sf%nshp.gt.0) then
       do ii=1,sf%nshp
          if (allocated(sf%shp(ii)%xx)) deallocate(sf%shp(ii)%xx)
          if (allocated(sf%shp(ii)%yy)) deallocate(sf%shp(ii)%yy)
          if (allocated(sf%shp(ii)%pos)) deallocate(sf%shp(ii)%pos)
          if (allocated(sf%shp(ii)%name)) deallocate(sf%shp(ii)%name)
       end do
       deallocate(sf%shp)
    end if
    sf%nshp=0
    return
  end subroutine parse_clearshapefile
  !
  integer function parse_type(funcstr,var,crc250,irc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Identify type of function (empty, constant, variable or expression)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),               INTENT(in) :: FuncStr   ! Function string
    CHARACTER (LEN=*), allocatable, INTENT(in) :: Var(:)     ! internal variables
    character*250 :: crc250
    integer :: irc
    CHARACTER (LEN=LEN(FuncStr))                :: Func      ! Function string, local use
    integer :: irc2
    integer :: ii,lfunc
    integer :: c
    integer :: ival
    real(rn) :: rval
    character*25 :: myname = "parse_type"
    Func = trim(FuncStr)                                           ! Local copy of function string
    lFunc = LEN_TRIM(Func)
    if (lfunc.eq.0) then
       parse_type=parse_empty
       return
    end if
    read (func(1:lfunc),*,iostat=irc2) ival
    if (irc2.ne.0) then
       read (func(1:lfunc),*,iostat=irc2) rval
    end if
    if (irc2.eq.0) then ! constant
       parse_type=parse_constant
       return
    end if
    DO ii=lbound(var,1),ubound(var,1)
       IF (trim(func) .eq. trim(Var(ii))) THEN                     
          parse_type=parse_internal
          return
       end if
    end Do
    DO ii=1,lFunc
       c=ichar(func(ii:ii))
       if (.not.((c.ge.parse_laa.and. c.le.parse_lzz).or.&
            & (c.ge.parse_uaa.and. c.le.parse_uzz).or.&
            & (c.eq.parse_und)))then
          parse_type=parse_expression
          return
       end if
    end do
    parse_type=parse_variable
    return
  end function parse_type
  !
  ! real(rn) function parse_val(FuncStr,crc250,irc)
  !   character (Len=*), INTENT(in)  :: FuncStr
  !   character*250 :: crc250
  !   integer :: irc
  !   CHARACTER (LEN=*), allocatable, INTENT(in) :: Var(:)       ! dummy
  !   REAL(rn), allocatable, INTENT(in)          :: Val(:)  ! dummy
  !   type(parse_session), pointer :: css => null()
  !   call parse_open(css,crc250,irc)
  !   if (irc.ne.0) return
  !   call parse_parsef (css, FuncStr, Var,crc250,irc)
  !   if (irc.ne.0) return
  !   parse_val=parse_evalf(css,val)
  !   call parse_close(css,crc250,irc)
  !   if (irc.ne.0) return
  !   return
  ! end function parse_val

  SUBROUTINE parse_parsef (css, FuncStr, Var,crc250,irc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Parse function string FuncStr and compile it into bytecode
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer :: css
    CHARACTER (LEN=*),               INTENT(in) :: FuncStr   ! Function string
    CHARACTER (LEN=*), allocatable, INTENT(in) :: Var(:)       ! Array with variable names
    character*250 :: crc250
    integer :: irc
    CHARACTER (LEN=LEN(FuncStr))                :: Func      ! Function string, local use
    integer, external :: length
    character*25 :: myname = "parse_parsef"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (.not.associated(css)) then
       if(parse_bdeb)write(*,*)myname,"Input ",&
            & allocated(var)
       irc=910
       EvalErrType=10
       call parse_errorappend(crc250,myname//" Invalid session.")
       return
    end if
    css%varEnd=varBegin+ubound(var,1)-lbound(var,1)+1
    Func = FuncStr                                           ! Local copy of function string
    CALL Replace ('**','^ ',Func)                            ! Exponent into 1-Char. format
    CALL RemoveSpaces (Func)                                 ! Condense function string
    if(parse_bdeb)write(*,*) myname,"Parsing '"//func(1:len_trim(func))//"'"
    CALL parse_CheckSyntax (Func,FuncStr,Var,crc250,irc)
    if (irc.ne.0) return
    if(parse_bdeb) write(*,*)myname,"Compiling '"//funcstr(1:LEN_TRIM(FuncStr))//"'"
    CALL Parse_compile (css,Func,Var,crc250,irc)                                ! Compile into bytecode
    css%funcStr100=FuncStr
    call chop0(css%funcStr100,100)
    css%lenf=length(css%funcStr100,100,10)
    if(parse_bdeb)write(*,'(X,A,X,A,1000(X,I0))') myname,"Done '"//&
         & funcstr(1:LEN_TRIM(FuncStr))//"'",css%bytecode
    return
  END SUBROUTINE parse_parsef
  !
  logical function parse_string(css, cbuff) result (ret)
    IMPLICIT NONE
    type(parse_session), pointer,INTENT(IN) :: css
    ! character(len=700), allocatable,INTENT(OUT):: cbuff   ! some functions write to the string buffer
    character*700,INTENT(OUT):: cbuff   ! some functions write to the string buffer
    ret=(css%clen.ne.0)
    !if (allocated(cbuff)) deallocate(cbuff)
    if (ret) then
       !allocate(character(len=len_trim(css%cbuff)) :: cbuff)
       !allocate(cbuff)
       cbuff=trim(css%cbuff)
    end if
    if (parse_bdeb .and. ret) then
       write(*,*)'Parse_string Found: "'//cbuff//'"',css%clen
    end if
    return
  end function parse_string
  !
  FUNCTION parse_evalf (css, Val, crc250,irc) RESULT (res)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Evaluate bytecode of ith function for the values passed in array Val(:)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer,INTENT(IN) :: css
    REAL(rn), allocatable, INTENT(in)       :: Val(:)  ! Variable values
    character*250 :: crc250
    integer :: irc
    REAL(rn)                                :: res     ! Result
    !
    INTEGER                                 :: IP,   & ! Instruction pointer
         DP,   & ! Data pointer
         SP,   & ! Stack pointer
         AP,   & ! arguments pointer
         AI      ! arguments index
    REAL(rn),                PARAMETER :: zero = 0._rn
    integer :: ii, imax,nargs
    character*22 :: myname ="parse_evalf"
    character*250 :: str250
    real(rn)  :: eps
    integer :: lens
    integer, external :: length
    logical :: above,below,found
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (.not.associated(css)) then
       if(parse_bdeb)write(*,*)myname,"Input ",&
            & imax,allocated(val)
       irc=910
       call parse_errorappend(crc250,myname//" Invalid session.")
       return
    end if
    if (.not.associated(css%stack)) then
       if(parse_bdeb)write(*,*)myname,"Input '"//css%funcStr100(1:css%lenf)//"'",&
            & imax,allocated(val)
       irc=911
       call parse_errorappend(crc250,myname//" Invalid BYTECODE for function '"//css%funcStr100(1:css%lenf)//"'")
       return
    end if
    imax=size(val)
    css%clen=0
    if(parse_bdeb)write(*,*)myname,"Entering '"//css%funcStr100(1:css%lenf)//"'",imax,allocated(val)
    DP = 1
    SP = 0
    AP = 0 ! argument position
    AI = 0 ! argument function index
    DO IP=1,css%ByteCodeSize
       NARGS=css%ArgsByte(AI+1)
       if(parse_bdeb)then
          write(*,'(X,A,X,A,X,I3,X,I3,3X,A,3X,I0,X,I0)')myname,"Looping",&
               & IP,css%ByteCode(IP),parse_code20(css%bytecode(ip),nargs)
       end if
       SELECT CASE (css%ByteCode(IP))
       CASE (cImmed)
          SP=SP+1
          css%Stack(SP)=css%Immed(DP)
          DP=DP+1
       CASE   (cNeg)
          css%Stack(SP)=-css%Stack(SP)
       CASE   (cAdd)
          css%Stack(SP-1)=css%Stack(SP-1)+css%Stack(SP)
          SP=SP-1
       CASE   (cSub)
          css%Stack(SP-1)=css%Stack(SP-1)-css%Stack(SP)
          SP=SP-1
       CASE   (cMul)
          css%Stack(SP-1)=css%Stack(SP-1)*css%Stack(SP)
          SP=SP-1
       CASE   (cDiv)
          IF (css%Stack(SP)==0._rn) THEN
             if (parse_bdeb) write(*,*)"*** Division by zero."
             irc=313
             call parse_errorappend(crc250,myname//" Division by zero.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=1
             res=zero
             RETURN
          ENDIF
          css%Stack(SP-1)=css%Stack(SP-1)/css%Stack(SP)
          SP=SP-1
       CASE   (cPow)
          css%Stack(SP-1)=css%Stack(SP-1)**css%Stack(SP)
          SP=SP-1
       CASE   (cAbs)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=ABS(css%Stack(SP))
       CASE   (cExp)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=EXP(css%Stack(SP))
       CASE (cLog10)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (css%Stack(SP)<=0._rn) THEN
             if (parse_bdeb) write(*,*)"*** Negative argument to LOG10.",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Negative argument to LOG10.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=3
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=LOG10(css%Stack(SP))
       CASE   (cLog)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (css%Stack(SP)<=0._rn) THEN
             if (parse_bdeb) write(*,*)"*** Negative argument to LOG.",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Negative argument to LOG.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=3
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=LOG(css%Stack(SP))
       CASE  (cSqrt)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (css%Stack(SP)<0._rn) THEN
             if (parse_bdeb) write(*,*)"*** Negative argument to SQRT.",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Negative argument to SQRT.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=2
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=SQRT(css%Stack(SP))
       CASE  (cMsgMax)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=1.0D0
       CASE  (cMsgMin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=1.0D0
       CASE  (cMsgClosest)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=1.0D0
       CASE  (cisMember)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=.false.
          ISMEMBER: DO II=1,NARGS-1
             if (css%stack(SP).eq.css%Stack(SP+II)) then
                found=.true.
                exit ISMEMBER
             end if
          end do ISMEMBER
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cisBelow)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=.true.
          ISBELOW: DO II=1,NARGS-1
             if (css%stack(SP).ge.css%Stack(SP+II)) then
                found=.false.
                exit ISBELOW
             end if
          end do ISBELOW
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cisAbove)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=.true.
          ISABOVE: DO II=1,NARGS-1
             if (css%stack(SP).le.css%Stack(SP+II)) then
                found=.false.
                exit ISABOVE
             end if
          end do ISABOVE
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cisBetween)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          above=.false.
          below=.false.
          ISBETWEEN: DO II=1,NARGS-1
             if (css%stack(SP).le.css%Stack(SP+II)) then
                BELOW=.TRUE.
             end if
             if (css%stack(SP).ge.css%Stack(SP+II)) then
                ABOVE=.TRUE.
             end if
             if (above.and.below) exit ISBETWEEN
          end do ISBETWEEN
          if (above.and.below) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cThinned)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.ge.1) THEN ! 
             if (rand()*100 .gt. css%Stack(SP)) then
                css%Stack(SP)=1.0D0
             else
                css%Stack(SP)=0.0D0
             end if
          end if
       CASE  (cand)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=(abs(css%stack(SP)).gt.0.5D0)
          if (found) then
             AND: DO II=1,NARGS-1
                if (abs(css%Stack(SP+II)).lt.0.5D0) then
                   found=.false.
                   exit AND
                end if
             end do AND
          end if
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cor)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=(abs(css%stack(SP)).gt.0.5D0)
          if (.not.found) then
             OR: DO II=1,NARGS-1
                if (abs(css%Stack(SP+II)).gt.0.5D0) then
                   found=.true.
                   exit OR
                end if
             end do OR
          end if
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cnot)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          ! .not.a .and. .not.b ....
          found=(abs(css%stack(SP)).lt.0.5D0)
          if (found) then
             NOT: DO II=1,NARGS-1
                if (abs(css%Stack(SP+II)).gt.0.5D0) then
                   found=.false.
                   exit NOT
                end if
             end do NOT
          end if
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (c1970yy)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_yy(css%Stack(SP))
       CASE  (c1970mm)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_mm(css%Stack(SP))
       CASE  (c1970dd)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_dd(css%Stack(SP))
       CASE  (c1970hh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_hh(css%Stack(SP))
       CASE  (c1970mi)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_mi(css%Stack(SP))
       CASE  (c1970)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.1) THEN ! dtg()
             css%Stack(SP)=css%Stack(SP)+parse_f1970(real(val8(1)),real(val8(2)),&
                  &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
          ELSE IF (NARGS.EQ.6) THEN ! dtg(year,month,day,hour,min,sec)
             css%Stack(SP)=parse_f1970(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  css%Stack(SP+4), &
                  css%Stack(SP+5))
          ELSE IF (NARGS.EQ.5) THEN ! dtg(year,month,day,hour,min)
             css%Stack(SP)=parse_f1970(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  css%Stack(SP+4), &
                  0.0D0)
          ELSE IF (NARGS.EQ.4) THEN ! dtg(year,month,day,hour)
             css%Stack(SP)=parse_f1970(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  0.0D0, &
                  0.0D0)
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to s1970:",nargs
             irc=308
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to s1970.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          END IF
       CASE  (cjulianyy)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_yy(css%Stack(SP))
       CASE  (cjulianmm)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_mm(css%Stack(SP))
       CASE  (cjuliandd)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_dd(css%Stack(SP))
       CASE  (cjulianhh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_hh(css%Stack(SP))
       CASE  (cjulianmi)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_mi(css%Stack(SP))
       CASE  (cjulian)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.1) THEN ! dtg()
             css%Stack(SP)=css%Stack(SP)+parse_fjulian(real(val8(1)),real(val8(2)),&
                  &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
          ELSE IF (NARGS.EQ.6) THEN ! dtg(year,month,day,hour,min,sec)
             css%Stack(SP)=parse_fjulian(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  css%Stack(SP+4), &
                  css%Stack(SP+5))
          ELSE IF (NARGS.EQ.5) THEN ! dtg(year,month,day,hour,min)
             css%Stack(SP)=parse_fjulian(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  css%Stack(SP+4), &
                  0.0D0)
          ELSE IF (NARGS.EQ.4) THEN ! dtg(year,month,day,hour)
             css%Stack(SP)=parse_fjulian(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  0.0D0, &
                  0.0D0)
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to d2000:",nargs
             irc=309
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to d2000.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          END IF
       CASE  (cmidnight)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=css%Stack(SP)*secperday+parse_f1970(real(val8(1)),real(val8(2)),&
               &real(val8(3)),0.0D0,0.0D0,0.0D0)
          do ii=1,nargs-1
             css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday             
          end do
       CASE  (cnow)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=css%Stack(SP)*secperday+parse_f1970(real(val8(1)),real(val8(2)),&
               &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
          do ii=1,nargs-1
             css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday
          end do
       CASE  (cround)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.gt.1) then
             eps=max(1.0D-5,abs(css%Stack(SP+1)))
             css%Stack(SP)=dnint(css%Stack(SP)/eps)*eps
          else
             css%Stack(SP)=dnint(css%Stack(SP))
          end if
       CASE  (cVarName)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.1) then
             if (parse_bdeb) write(*,*)"*** Found shape:",nint(css%stack(sp))
             !if (allocated(css%cbuff)) deallocate(css%cbuff)
             css%clen=25
             !allocate(character(len=css%clen) :: css%cbuff)
             !allocate(css%cbuff)
             css%cbuff=trim(getname25(css%Stack(SP)))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shapes:",nargs
             irc=312
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shapes.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cValidRange)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.3) then
             if (parse_bdeb) write(*,*)"*** Found range-check"
             if (css%Stack(SP).lt.css%Stack(SP+1).or.&
                  & css%Stack(SP).gt.css%Stack(SP+2)) then
                !if (allocated(css%cbuff)) deallocate(css%cbuff)
                css%clen=2
                !allocate(character(len=css%clen) :: css%cbuff)
                !allocate(css%cbuff)
                css%cbuff="NA"
             end if
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to validRange:",nargs
             irc=312
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to validRange.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cShpPre)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=shapeid(css%Stack(SP),css%Stack(SP+1))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shape:",nargs
             irc=310
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shape.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cShpVic)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.3) then
             css%Stack(SP)=vicinity(css%Stack(SP),css%Stack(SP+1),css%Stack(SP+2))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shape:",nargs
             irc=310
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shape.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ctd2q) ! td,p
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=td2q(css%Stack(SP),css%Stack(SP+1))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to td2q:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to td2q.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (crh2td) ! rh, t, [ice=1,0]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=rh2td(css%Stack(SP),css%Stack(SP+1))
          else if (nargs.eq.3) then
             css%Stack(SP)=rh2td(css%Stack(SP),css%Stack(SP+1),css%Stack(SP+2))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to rh2td:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to rh2td.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ctd2rh) ! td, t, [ice=1,0]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=td2rh(css%Stack(SP),css%Stack(SP+1))
          else if (nargs.eq.3) then
             css%Stack(SP)=td2rh(css%Stack(SP),css%Stack(SP+1),css%Stack(SP+2))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to td2rh:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to td2rh.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cq2rh) ! q, t, [p|1013.25]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=q2rh(css%Stack(SP),css%Stack(SP+1))
          else if (nargs.eq.3) then
             css%Stack(SP)=q2rh(css%Stack(SP),css%Stack(SP+1),css%Stack(SP+2))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to q2rh:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to q2rh.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ck2c)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=css%Stack(SP)-t0
       CASE  (cc2k)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=css%Stack(SP)+t0
       CASE  (cSinh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=SINH(css%Stack(SP))
       CASE  (cCosh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=COSH(css%Stack(SP))
       CASE  (cTanh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=TANH(css%Stack(SP))
       CASE   (cSin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=SIN(css%Stack(SP))
       CASE   (cCos)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=COS(css%Stack(SP))
       CASE   (cTan)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=TAN(css%Stack(SP))
       CASE  (cAsin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF ((css%Stack(SP)<-1._rn).OR.(css%Stack(SP)>1._rn)) THEN
             if (parse_bdeb) write(*,*)"*** Invalid argument to ASIN:",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Invalid argument to ASIN.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=4
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=ASIN(css%Stack(SP))
       CASE  (cAcos)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF ((css%Stack(SP)<-1._rn).OR.(css%Stack(SP)>1._rn)) THEN
             if (parse_bdeb) write(*,*)"*** Invalid argument to ACOS:",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Invalid argument to ACOS.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=4
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=ACOS(css%Stack(SP))
       CASE  (cAtan2)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.2) THEN
             css%Stack(SP)=ATAN2(css%Stack(SP),css%Stack(SP+1))
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to atan2:",nargs
             irc=310
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to atan2.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          END IF
       CASE  (cAtan)
          css%Stack(SP)=ATAN(css%Stack(SP))
       CASE  DEFAULT
          SP=SP+1
          if (css%ByteCode(IP) .le. css%VarEnd) then
             ii=css%ByteCode(IP)-VarBegin+1
             if (parse_bdeb.and.ii.gt.imax)write(*,*) myname,'Invalid VAL-index:',ii,"(max=",size(val),")"
             css%Stack(SP)=Val(ii)
          else
             css%Stack(SP)=ConstVal(css%ByteCode(IP)-css%VarEnd)
          end if
       END SELECT
       if(parse_bdeb)then
          if (sp.ne.0) then
             write(str250,'(100(X,F0.2))') &
                  & (css%Stack(II),II=1,min(SP,100))
          else
             STR250=""
          end if
          call chop0(str250,250)
          lens=length(str250,250,10)
          write(*,'(X,A,5X,A)') myname,&
               & "stack=["//str250(1:LENS)//"]"
       end if
    END DO
    EvalErrType = 0
    res = css%Stack(1)
    if(parse_bdeb)write(*,*) myname,"Result=",res
    !if(parse_bdeb)write(*,*)myname,"Done."
  END FUNCTION parse_evalf
  !
  subroutine parse_evals (css, Val, set, res, ret, crc250,irc) 
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Evaluate bytecode of ith function for the values passed in array Val(:)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer :: css
    REAL(rn), allocatable, INTENT(in)  :: Val(:)             ! Variable values
    logical, allocatable, INTENT(in)   :: set(:)            ! is variable set?
    REAL(rn)                           :: res                ! Result
    logical                            :: ret                ! is result set?
    character*250 :: crc250
    integer :: irc
    INTEGER                            :: IP,              & ! Instruction pointer
         DP,              & ! Data pointer
         SP,              & ! Stack pointer
         AP,              & ! arguments pointer
         AI                 ! arguments index
    REAL(rn),                PARAMETER :: zero = 0._rn
    integer :: ii, nargs
    character*22 :: myname ="parse_evals"
    character*250 :: str250
    real(rn) :: eps
    integer :: lens,imax
    integer, external :: length
    logical :: above,below,found
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (.not.associated(css)) then
       if(parse_bdeb)write(*,*)myname,"Input ",&
            & imax,size(set),&
            & allocated(val),allocated(set)
       irc=912
       call parse_errorappend(crc250,myname//" Invalid session.")
       return
    end if
    imax=size(val)
    css%clen=0
    if(parse_bdeb)write(*,*)myname,"Entering '"//css%funcStr100(1:css%lenf)//"'",&
         & imax,size(set),&
         & allocated(val),allocated(set)
    if(parse_bdeb)write(*,*)myname,"Entering"
    ret=.true.
    DP = 1
    SP = 0
    AP = 0 ! argument position
    AI = 0 ! argument function index
    DO IP=1,css%ByteCodeSize
       SELECT CASE (css%ByteCode(IP))
       CASE (cImmed)
          SP=SP+1
          css%Stack(SP)=css%Immed(DP)
          DP=DP+1
       CASE   (cNeg)
          css%Stack(SP)=-css%Stack(SP)
       CASE   (cAdd)
          css%Stack(SP-1)=css%Stack(SP-1)+css%Stack(SP)
          SP=SP-1
       CASE   (cSub)
          css%Stack(SP-1)=css%Stack(SP-1)-css%Stack(SP)
          SP=SP-1
       CASE   (cMul)
          css%Stack(SP-1)=css%Stack(SP-1)*css%Stack(SP)
          SP=SP-1
       CASE   (cDiv)
          IF (css%Stack(SP)==0._rn) THEN
             if (parse_bdeb) write(*,*)"*** Division by zero."
             irc=313
             call parse_errorappend(crc250,myname//" Division by zero.\n")
             EvalErrType=1
             res=zero
             RETURN
          ENDIF
          css%Stack(SP-1)=css%Stack(SP-1)/css%Stack(SP)
          SP=SP-1
       CASE   (cPow)
          css%Stack(SP-1)=css%Stack(SP-1)**css%Stack(SP)
          SP=SP-1
       CASE   (cAbs)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=ABS(css%Stack(SP))
       CASE   (cExp)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=EXP(css%Stack(SP))
       CASE (cLog10)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (css%Stack(SP)<=0._rn) THEN
             if (parse_bdeb) write(*,*)"*** Negative argument to LOG10:",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Negative argument to LOG10.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=3
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=LOG10(css%Stack(SP))
       CASE   (cLog)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (css%Stack(SP)<=0._rn) THEN
             if (parse_bdeb) write(*,*)"*** Negative argument to LOG:",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Negative argument to LOG.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=3
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=LOG(css%Stack(SP))
       CASE  (cSqrt)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (css%Stack(SP)<0._rn) THEN
             if (parse_bdeb) write(*,*)"*** Negative argument to SQRT:",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Negative argument to SQRT.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=3
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=SQRT(css%Stack(SP))
       CASE  (cMsgMax)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=1.0D0
       CASE  (cMsgMin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=1.0D0
       CASE  (cMsgClosest)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=1.0D0
       CASE  (cisMember)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=.false.
          ISMEMBER: DO II=1,NARGS-1
             if (css%stack(SP).eq.css%Stack(SP+II)) then
                found=.true.
                exit ISMEMBER
             end if
          end do ISMEMBER
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cisBelow)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=.true.
          ISBELOW: DO II=1,NARGS-1
             if (css%stack(SP).ge.css%Stack(SP+II)) then
                found=.false.
                exit ISBELOW
             end if
          end do ISBELOW
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cisAbove)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=.true.
          ISABOVE: DO II=1,NARGS-1
             if (css%stack(SP).le.css%Stack(SP+II)) then
                found=.false.
                exit ISABOVE
             end if
          end do ISABOVE
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cisBetween)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          above=.false.
          below=.false.
          ISBETWEEN: DO II=1,NARGS-1
             if (css%stack(SP).le.css%Stack(SP+II)) then
                BELOW=.TRUE.
             end if
             if (css%stack(SP).ge.css%Stack(SP+II)) then
                ABOVE=.TRUE.
             end if
             if (above.and.below) exit ISBETWEEN
          end do ISBETWEEN
          if (above.and.below) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cThinned)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.ge.1) THEN ! 
             if (rand()*100 .gt. css%Stack(SP)) then
                css%Stack(SP)=1.0D0
             else
                css%Stack(SP)=0.0D0
             end if
          end if
       CASE  (cand)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=(abs(css%stack(SP)).gt.0.5D0)
          if (found) then
             AND: DO II=1,NARGS-1
                if (abs(css%Stack(SP+II)).lt.0.5D0) then
                   found=.false.
                   exit AND
                end if
             end do AND
          end if
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cor)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          found=(abs(css%stack(SP)).gt.0.5D0)
          if (.not.found) then
             OR: DO II=1,NARGS-1
                if (abs(css%Stack(SP+II)).gt.0.5D0) then
                   found=.true.
                   exit OR
                end if
             end do OR
          end if
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (cnot)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          ! .not.a .and. .not.b ....
          found=(abs(css%stack(SP)).lt.0.5D0)
          if (found) then
             NOT: DO II=1,NARGS-1
                if (abs(css%Stack(SP+II)).gt.0.5D0) then
                   found=.false.
                   exit NOT
                end if
             end do NOT
          end if
          if (found) then
             css%Stack(SP)=1.0D0
          else
             css%Stack(SP)=0.0D0
          end if
       CASE  (c1970yy)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_yy(css%Stack(SP))
       CASE  (c1970mm)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_mm(css%Stack(SP))
       CASE  (c1970dd)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_dd(css%Stack(SP))
       CASE  (c1970hh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_hh(css%Stack(SP))
       CASE  (c1970mi)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_f1970_mi(css%Stack(SP))
       CASE  (c1970)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.1) THEN ! dtg()
             css%Stack(SP)=css%Stack(SP)+parse_f1970(real(val8(1)),real(val8(2)),&
                  &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
          ELSE IF (NARGS.EQ.6) THEN ! dtg(year,month,day,hour,min,sec)
             css%Stack(SP)=parse_f1970(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  css%Stack(SP+4), &
                  css%Stack(SP+5))
          ELSE IF (NARGS.EQ.5) THEN ! dtg(year,month,day,hour,min)
             css%Stack(SP)=parse_f1970(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  css%Stack(SP+4), &
                  0.0D0)
          ELSE IF (NARGS.EQ.4) THEN ! dtg(year,month,day,hour)
             css%Stack(SP)=parse_f1970(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  0.0D0, &
                  0.0D0)
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to s1970:",nargs
             irc=311
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to s1970.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          END IF
       CASE  (cjulianyy)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_yy(css%Stack(SP))
       CASE  (cjulianmm)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_mm(css%Stack(SP))
       CASE  (cjuliandd)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_dd(css%Stack(SP))
       CASE  (cjulianhh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_hh(css%Stack(SP))
       CASE  (cjulianmi)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=parse_fjulian_mi(css%Stack(SP))
       CASE  (cjulian)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.1) THEN ! dtg()
             css%Stack(SP)=css%Stack(SP)+parse_fjulian(real(val8(1)),real(val8(2)),&
                  &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
          ELSE IF (NARGS.EQ.6) THEN ! dtg(year,month,day,hour,min,sec)
             css%Stack(SP)=parse_fjulian(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  css%Stack(SP+4), &
                  css%Stack(SP+5))
          ELSE IF (NARGS.EQ.5) THEN ! dtg(year,month,day,hour,min)
             css%Stack(SP)=parse_fjulian(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  css%Stack(SP+4), &
                  0.0D0)
          ELSE IF (NARGS.EQ.4) THEN ! dtg(year,month,day,hour)
             css%Stack(SP)=parse_fjulian(css%Stack(SP), &
                  css%Stack(SP+1), &
                  css%Stack(SP+2), &
                  css%Stack(SP+3), &
                  0.0D0, &
                  0.0D0)
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to d2000:",nargs
             irc=312
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to d2000.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          END IF
       CASE  (cmidnight)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=css%Stack(SP)*secperday+parse_f1970(real(val8(1)),real(val8(2)),&
               &real(val8(3)),0.0D0,0.0D0,0.0D0)
          do ii=1,nargs-1
             css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday
          end do
       CASE  (cnow)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=css%Stack(SP)*secperday+parse_f1970(real(val8(1)),real(val8(2)),&
               &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
          do ii=1,nargs-1
             css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday
          end do
       CASE  (cround)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.gt.1) then
             eps=max(1.0D-5,abs(css%Stack(SP+1)))
             css%Stack(SP)=dnint(css%Stack(SP)/eps)*eps
          else
             css%Stack(SP)=dnint(css%Stack(SP))
          end if
       CASE  (cVarName)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.1) then
             if (parse_bdeb) write(*,*)"*** Found shape:",nint(css%stack(sp))
             !if (allocated(css%cbuff)) deallocate(css%cbuff)
             css%clen=25
             !allocate(character(len=css%clen) :: css%cbuff)
             !allocate(css%cbuff)
             css%cbuff=trim(getname25(css%Stack(SP)))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shapes:",nargs
             irc=312
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shapes.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cValidRange)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.3) then
             if (parse_bdeb) write(*,*)"*** Found range-check"
             if (css%Stack(SP).lt.css%Stack(SP+1).or.&
                  & css%Stack(SP).gt.css%Stack(SP+2)) then
                !if (allocated(css%cbuff)) deallocate(css%cbuff)
                css%clen=2
                !allocate(character(len=css%clen) :: css%cbuff)
                !allocate(css%cbuff)
                css%cbuff="NA"
             end if
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to rangeCheck:",nargs
             irc=312
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to rangeCheck.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cShpPre)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=shapeid(css%Stack(SP),css%Stack(SP+1))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shape:",nargs
             irc=310
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shape.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cShpVic)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.3) then
             css%Stack(SP)=vicinity(css%Stack(SP),css%Stack(SP+1),css%Stack(SP+2))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shape:",nargs
             irc=310
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shape.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ctd2q) ! td,p
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=td2q(css%Stack(SP),css%Stack(SP+1))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to td2q:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to td2q.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (crh2td) ! rh, t, [ice=1,0]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=rh2td(css%Stack(SP),css%Stack(SP+1))
          else if (nargs.eq.3) then
             css%Stack(SP)=rh2td(css%Stack(SP),css%Stack(SP+1),css%Stack(SP+2))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to rh2td:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to rh2td.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ctd2rh) ! td, t, [ice=1,0]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=td2rh(css%Stack(SP),css%Stack(SP+1))
          else if (nargs.eq.3) then
             css%Stack(SP)=td2rh(css%Stack(SP),css%Stack(SP+1),css%Stack(SP+2))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to td2rh:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to td2rh.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cq2rh) ! q, t, [p|1013.25]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             css%Stack(SP)=q2rh(css%Stack(SP),css%Stack(SP+1))
          else if (nargs.eq.3) then
             css%Stack(SP)=q2rh(css%Stack(SP),css%Stack(SP+1),css%Stack(SP+2))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to q2rh:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to q2rh.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ck2c)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=css%Stack(SP)-t0
       CASE  (cc2k)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=css%Stack(SP)+t0
       CASE  (cSinh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=SINH(css%Stack(SP))
       CASE  (cCosh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=COSH(css%Stack(SP))
       CASE  (cTanh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=TANH(css%Stack(SP))
       CASE   (cSin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=SIN(css%Stack(SP))
       CASE   (cCos)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=COS(css%Stack(SP))
       CASE   (cTan)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          css%Stack(SP)=TAN(css%Stack(SP))
       CASE  (cAsin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF ((css%Stack(SP)<-1._rn).OR.(css%Stack(SP)>1._rn)) THEN
             if (parse_bdeb) write(*,*)"*** Invalid argument to ASIN:",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Invalid argument to ASIN.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=4
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=ASIN(css%Stack(SP))
       CASE  (cAcos)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF ((css%Stack(SP)<-1._rn).OR.(css%Stack(SP)>1._rn)) THEN
             if (parse_bdeb) write(*,*)"*** Invalid argument to ACOS:",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname//" Invalid argument to ACOS.")
             call parse_errorappendr(crc250,css%Stack(SP))
             call parse_errorappend(crc250,"\n")
             EvalErrType=4
             res=zero
             RETURN
          ENDIF
          css%Stack(SP)=ACOS(css%Stack(SP))
       CASE  (cAtan2)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.2) THEN
             css%Stack(SP)=ATAN2(css%Stack(SP),css%Stack(SP+1))
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to atan2:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to atan2.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          END IF
       CASE  (cAtan)
          css%Stack(SP)=ATAN(css%Stack(SP))
       CASE  DEFAULT
          SP=SP+1
          if (css%ByteCode(IP) .le. css%VarEnd) then
             css%Stack(SP)=Val(css%ByteCode(IP)-VarBegin+1)
             if (.not.set(css%ByteCode(IP)-VarBegin+1)) ret=.false.
          else
             css%Stack(SP)=ConstVal(css%ByteCode(IP)-css%VarEnd)
          end if
       END SELECT
       if(parse_bdeb)then
          if (sp.ne.0) then
             write(str250,'(100(X,F0.2))') &
                  & (css%Stack(II),II=1,min(SP,100))
          else
             STR250=""
          end if
          call chop0(str250,250)
          lens=length(str250,250,10)
          write(*,'(X,A,5X,A)') myname,&
               & "Stack=["//str250(1:LENS)//"]"
       end if
    END DO
    EvalErrType = 0
    res = css%Stack(1)
    if(parse_bdeb)write(*,'(X,A,X,A,X,F0.2,X,L1)') myname," result=",res,ret
    if(parse_bdeb)write(*,*)myname,"Done."
  END subroutine parse_evals
  !
  subroutine parse_evala (css, ctrg, cpos, npos, Val, Set, Res, crc250,irc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Evaluate bytecode of ith function for the values passed in array Val(:)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer,INTENT(IN):: css
    integer(is),INTENT(in)                 :: ctrg,cpos  ! allocated targets, positions
    integer(is),INTENT(in)                 :: npos       ! number of positions
    REAL(rn), allocatable, INTENT(in)      :: Val(:,:)   ! Variable values
    logical, allocatable, INTENT(in)       :: Set(:)     ! is position defined?
    REAL(rn),allocatable,INTENT(INOUT)       :: res(:)     ! Result
    character*250 :: crc250
    integer :: irc
    INTEGER                            :: IP,              & ! Instruction pointer
         DP,              & ! Data pointer
         SP,              & ! Stack pointer
         AP,              & ! arguments pointer
         AI                 ! arguments index
    REAL(rn),                PARAMETER :: zero = 0._rn
    integer :: ii, jj, nargs, imax,imin,iclo
    real(rn) :: vmax,vmin,vclo,v,buff
    logical :: above, below, found
    character*250 :: str250
    real(rn) :: eps
    integer :: lens
    integer, external :: length
    character*12 :: myname ="parse_evala"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (.not.associated(css)) then
       if(parse_bdeb)write(*,*)myname,"Input ",&
            & ctrg, cpos, npos, size(val),size(set),size(res),&
            & allocated(val),allocated(set),allocated(res)
       irc=911
       call parse_errorappend(crc250,myname//" Invalid session.")
       return
    end if
    css%clen=0
    if(parse_bdeb)write(*,*)myname,"Entering '"//css%funcStr100(1:css%lenf)//"'",&
         & ctrg, cpos, npos, size(val),size(set),size(res),&
         & allocated(val),allocated(set),allocated(res)
    if (css%npos.lt.npos) then
       css%npos=npos
       IF (ASSOCIATED(css%Stacka)) DEALLOCATE ( css%Stacka, stat=irc)
       IF (ASSOCIATED(css%Wrka))   DEALLOCATE ( css%Wrka,   stat=irc)
       ALLOCATE (css%Stacka(max(1,css%StackSize),css%npos),css%Wrka(css%npos),STAT = irc)
       IF (irc.ne. 0) THEN
          call parse_errorappend(crc250,myname//" Unable to allocate stacka, stacka, wrka.\n")
          return
       end if
    end if
    !
    DP = 1
    SP = 0
    AP = 0
    AI = 0
    DO IP=1,css%ByteCodeSize
       if(parse_bdeb)then
          NARGS=css%ArgsByte(AI+1)
          write(*,*)myname,'Index:',ip,css%ByteCodeSize
          write(*,'(X,A,X,A,X,I3,X,I3,3X,A,3X,I0,X,I0)')myname,"Looping",&
               & IP,css%ByteCode(IP),parse_code20(css%bytecode(ip),nargs)
       end if
       SELECT CASE (css%ByteCode(IP))
       CASE (cImmed)
          if(parse_bdeb)write(*,*)myname,'Immed enter:',SP,DP,NPOS,size(set)
          SP=SP+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                if(parse_bdeb)write(*,*)myname,'Immed eval:',SP,jj,&
                     & associated(css%stacka),associated(css%immed)
                css%Stacka(SP,JJ)=css%Immed(DP)
             END IF
          END DO
          DP=DP+1
          if(parse_bdeb)write(*,*)myname,'Immed done:',SP,DP,NPOS,size(set)
       CASE   (cNeg)
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                if (parse_bdeb)write(*,*)myname,'Neg.'
                css%Stacka(SP,JJ)=-css%Stacka(SP,JJ)
             END IF
          END DO
       CASE   (cAdd)
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP-1,JJ)=css%Stacka(SP-1,JJ)+css%Stacka(SP,JJ)
             END IF
          END DO
          SP=SP-1
       CASE   (cSub)
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP-1,JJ)=css%Stacka(SP-1,JJ)-css%Stacka(SP,JJ)
             END IF
          END DO
          SP=SP-1
       CASE   (cMul)
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP-1,JJ)=css%Stacka(SP-1,JJ)*css%Stacka(SP,JJ)
             END IF
          END DO
          SP=SP-1
       CASE   (cDiv)
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                IF (css%Stacka(SP,JJ)==0._rn) THEN
                   if (parse_bdeb) write(*,*)"*** Division by zero."
                   irc=314
                   call parse_errorappend(crc250,myname//" Division by zero.\n")
                   EvalErrType=1
                   RETURN
                ENDIF
                css%Stacka(SP-1,JJ)=css%Stacka(SP-1,JJ)/css%Stacka(SP,JJ)
             END IF
          END DO
          SP=SP-1
       CASE   (cPow)
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP-1,JJ)=css%Stacka(SP-1,JJ)**css%Stacka(SP,JJ)
             END IF
          END DO
          SP=SP-1
       CASE   (cAbs)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=ABS(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE   (cExp)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=EXP(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE (cLog10)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                IF (css%Stacka(SP,JJ)<=0._rn) THEN
                   if (parse_bdeb) write(*,*)"*** Invalid argument to ACOS:",css%Stack(SP)
                   irc=315
                   call parse_errorappend(crc250,myname//" Negative argument to LOG10.")
                   call parse_errorappendr(crc250,css%Stack(SP))
                   call parse_errorappend(crc250,"\n")
                   EvalErrType=3
                   RETURN
                ELSE
                   css%Stacka(SP,JJ)=LOG10(css%Stacka(SP,JJ))
                ENDIF
             END IF
          END DO
       CASE   (cLog)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                IF (css%Stacka(SP,JJ)<=0._rn) THEN
                   if (parse_bdeb) write(*,*)"*** Invalid argument to ACOS:",css%Stack(SP)
                   irc=316
                   call parse_errorappend(crc250,myname//" Negative argument to LOG.")
                   call parse_errorappendr(crc250,css%Stack(SP))
                   call parse_errorappend(crc250,"\n")
                   EvalErrType=3
                   RETURN
                ELSE
                   css%Stacka(SP,JJ)=LOG(css%Stacka(SP,JJ))
                ENDIF
             END IF
          END DO
       CASE  (cSqrt)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                IF (css%Stacka(SP,JJ)<0._rn) THEN
                   if (parse_bdeb) write(*,*)"*** Invalid argument to ACOS:",css%Stack(SP)
                   irc=317
                   call parse_errorappend(crc250,myname//" Negative argument to SQRT.")
                   call parse_errorappendr(crc250,css%Stack(SP))
                   call parse_errorappend(crc250,"\n")
                   EvalErrType=3
                   RETURN
                ELSE
                   css%Stacka(SP,JJ)=SQRT(css%Stacka(SP,JJ))
                ENDIF
             END IF
          END DO
       CASE  (cMsgMax)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IMAX=0
          VMAX=0.0D0
          DO JJ=1,NPOS
             if(set(jj))then
                if (imax.eq.0) then
                   imax=jj
                   vmax=css%Stacka(SP,JJ)
                else if (css%Stacka(SP,JJ).gt.vmax) then
                   imax=jj
                   vmax=css%Stacka(SP,JJ)
                end if
             end if
          END DO
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=0.0D0
             END IF
          END DO
          IF (IMAX.NE.0) css%Stacka(SP,imax)=1.0D0
       CASE  (cMsgMin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IMIN=0
          VMIN=0.0D0
          DO JJ=1,NPOS
             if(set(jj))then
                if (imin.eq.0) then
                   imin=jj
                   vmin=css%Stacka(SP,JJ)
                else if (css%Stacka(SP,JJ).lt.vmin) then
                   imin=jj
                   vmin=css%Stacka(SP,JJ)
                end if
             end if
          END DO
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=0.0D0
             END IF
          END DO
          IF (IMIN.NE.0) css%Stacka(SP,imin)=1.0D0
       CASE  (cMsgClosest)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             css%wrka(jj)=0.0D0
          END DO
          DO II=1,NARGS-1
             iclo=0
             vclo=0.0D0
             DO JJ=1,NPOS
                if(set(jj))then
                   v=abs(css%Stacka(SP,jj)-css%stacka(SP+II,JJ))
                   if (iclo.eq.0) then
                      iclo=jj
                      vclo=v
                   else if (v.lt.vclo) then
                      iclo=jj
                      vclo=v
                   end if
                end if
             END DO
             if (iclo.ne.0)css%wrka(iclo)=1.0D0
          end do
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=css%wrka(jj)
             END IF
          END DO
       CASE  (cisMember)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                found=.false.
                ISMEMBER: DO II=1,NARGS-1
                   if (css%stacka(SP,JJ).eq.css%stacka(SP+II,JJ)) then
                      found=.true.
                      exit ISMEMBER
                   end if
                end do ISMEMBER
                if (found) then
                   css%Stacka(SP,JJ)=1.0D0
                else
                   css%Stacka(SP,JJ)=0.0D0
                end if
             END IF
          END DO
       CASE  (cisBelow)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                found=.true.
                ISBELOW: DO II=1,NARGS-1
                   if (css%stacka(SP,JJ).ge.css%stacka(SP+II,JJ)) then
                      found=.false.
                      exit ISBELOW
                   end if
                end do ISBELOW
                if (found) then
                   css%Stacka(SP,JJ)=1.0D0
                else
                   css%Stacka(SP,JJ)=0.0D0
                end if
             END IF
          END DO
       CASE  (cisAbove)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                found=.true.
                ISABOVE: DO II=1,NARGS-1
                   if (parse_bdeb) write(*,*)myname,'Isabove:',jj,sp,css%stacka(SP,JJ),css%stacka(SP+II,JJ)
                   if (css%stacka(SP,JJ).le.css%stacka(SP+II,JJ)) then
                      found=.false.
                      exit ISABOVE
                   end if
                end do ISABOVE
                if (found) then
                   css%Stacka(SP,JJ)=1.0D0
                else
                   css%Stacka(SP,JJ)=0.0D0
                end if
                if (parse_bdeb) write(*,*)myname,'Isabove done:',jj,sp,found
             END IF
          END DO
       CASE  (cisBetween)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                above=.false.
                below=.false.
                ISBETWEEN: DO II=1,NARGS-1
                   if (css%stacka(SP,JJ).le.css%stacka(SP+II,JJ)) then
                      BELOW=.TRUE.
                   end if
                   if (css%stacka(SP,JJ).ge.css%stacka(SP+II,JJ)) then
                      ABOVE=.TRUE.
                   end if
                   if (above.and.below) exit ISBETWEEN
                end do ISBETWEEN
                if (above.and.below) then
                   css%Stacka(SP,JJ)=1.0D0
                else
                   css%Stacka(SP,JJ)=0.0D0
                end if
             END IF
          END DO
       CASE  (cThinned)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.ge.1) THEN ! 
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   if (rand()*100 .gt. css%Stacka(SP,JJ)) then
                      css%Stacka(SP,JJ)=1.0D0
                   else
                      css%Stacka(SP,JJ)=0.0D0
                   end if
                END IF
             END DO
          end if
       CASE  (cand)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                found=(abs(css%stacka(SP,JJ)).gt.0.5D0)
                if (found) then
                   AND: DO II=1,NARGS-1
                      if (abs(css%stacka(SP+II,JJ)).lt.0.5D0) then
                         found=.false.
                         exit AND
                      end if
                   end do AND
                end if
                if (found) then
                   css%Stacka(SP,JJ)=1.0D0
                else
                   css%Stacka(SP,JJ)=0.0D0
                end if
             END IF
          END DO
       CASE  (cor)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                found=(abs(css%stacka(SP,JJ)).gt.0.5D0)
                if (.not.found) then
                   OR: DO II=1,NARGS-1
                      if (abs(css%stacka(SP+II,JJ)).gt.0.5D0) then
                         found=.true.
                         exit OR
                      end if
                   end do OR
                end if
                if (found) then
                   css%Stacka(SP,JJ)=1.0D0
                else
                   css%Stacka(SP,JJ)=0.0D0
                end if
             END IF
          END DO
       CASE  (cnot)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                ! .not.a .and. .not.b ....
                found=(abs(css%stacka(SP,JJ)).lt.0.5D0)
                if (found) then
                   NOT: DO II=1,NARGS-1
                      if (abs(css%stacka(SP+II,JJ)).gt.0.5D0) then
                         found=.false.
                         exit NOT
                      end if
                   end do NOT
                end if
                if (found) then
                   css%Stacka(SP,JJ)=1.0D0
                else
                   css%Stacka(SP,JJ)=0.0D0
                end if
             END IF
          END DO
       CASE  (c1970yy)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_f1970_yy(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (c1970mm)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_f1970_mm(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (c1970dd)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_f1970_dd(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (c1970hh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_f1970_hh(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (c1970mi)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_f1970_mi(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (c1970)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.1) THEN ! dtg(days)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=css%Stacka(SP,JJ)+parse_f1970(real(val8(1)),real(val8(2)),&
                        &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
                END IF
             END DO
          ELSE IF (NARGS.EQ.6) THEN ! dtg(year,month,day,hour,min,sec)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=parse_f1970(css%Stacka(SP,JJ), &
                        css%stacka(SP+1,JJ), &
                        css%stacka(SP+2,JJ), &
                        css%stacka(SP+3,JJ), &
                        css%stacka(SP+4,JJ), &
                        css%stacka(SP+5,JJ))
                END IF
             END DO
          ELSE IF (NARGS.EQ.5) THEN ! dtg(year,month,day,hour,min)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=parse_f1970(css%Stacka(SP,JJ), &
                        css%stacka(SP+1,JJ), &
                        css%stacka(SP+2,JJ), &
                        css%stacka(SP+3,JJ), &
                        css%stacka(SP+4,JJ), &
                        0.0D0)
                END IF
             END DO
          ELSE IF (NARGS.EQ.4) THEN ! dtg(year,month,day,hour)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=parse_f1970(css%Stacka(SP,JJ), &
                        css%stacka(SP+1,JJ), &
                        css%stacka(SP+2,JJ), &
                        css%stacka(SP+3,JJ), &
                        0.0D0, &
                        0.0D0)
                END IF
             END DO
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to s1970:",nargs
             irc=318
             call parse_errorappend(crc250,myname//" Invalid number of arguments to s1970.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             RETURN
          END IF
       CASE  (cjulianyy)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_fjulian_yy(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (cjulianmm)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_fjulian_mm(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (cjuliandd)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_fjulian_dd(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (cjulianhh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_fjulian_hh(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (cjulianmi)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=parse_fjulian_mi(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (cjulian)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.1) THEN ! dtg()
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=css%Stacka(SP,JJ)+parse_fjulian(real(val8(1)),real(val8(2)),&
                        &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
                END IF
             END DO
          ELSE IF (NARGS.EQ.6) THEN ! dtg(year,month,day,hour,min,sec)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=parse_fjulian(css%Stacka(SP,JJ), &
                        css%stacka(SP+1,JJ), &
                        css%stacka(SP+2,JJ), &
                        css%stacka(SP+3,JJ), &
                        css%stacka(SP+4,JJ), &
                        css%stacka(SP+5,JJ))
                END IF
             END DO
          ELSE IF (NARGS.EQ.5) THEN ! dtg(year,month,day,hour,min)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=parse_fjulian(css%Stacka(SP,JJ), &
                        css%stacka(SP+1,JJ), &
                        css%stacka(SP+2,JJ), &
                        css%stacka(SP+3,JJ), &
                        css%stacka(SP+4,JJ), &
                        0.0D0)
                END IF
             END DO
          ELSE IF (NARGS.EQ.4) THEN ! dtg(year,month,day,hour)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=parse_fjulian(css%Stacka(SP,JJ), &
                        css%stacka(SP+1,JJ), &
                        css%stacka(SP+2,JJ), &
                        css%stacka(SP+3,JJ), &
                        0.0D0, &
                        0.0D0)
                END IF
             END DO
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to d2000:",nargs
             irc=319
             call parse_errorappend(crc250,myname//" Invalid number of arguments to d2000.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             RETURN
          END IF
       CASE  (cmidnight)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          buff=parse_f1970(real(val8(1)),real(val8(2)),&
               &real(val8(3)),0.0D0,0.0D0,0.0D0)
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stack(SP)=css%Stack(SP)*secperday+buff
                do ii=1,nargs-1
                   css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday
                end do
             end if
          end do
       CASE  (cnow)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          buff=parse_f1970(real(val8(1)),real(val8(2)),&
               &real(val8(3)),real(val8(5)),real(val8(6)),real(val8(7)))
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=css%Stacka(SP,JJ)*secperday+buff
                do ii=1,nargs-1
                   css%Stacka(SP,jj)=css%Stacka(SP,jj)+css%Stacka(SP+ii,jj)*secperday
                end do
             end if
          end do
       CASE  (cround)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.gt.1) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   eps=max(1.0D-5,abs(css%Stacka(SP+1,JJ)))
                   css%Stacka(SP,JJ)=dnint(css%Stacka(SP,JJ)/eps)*eps
                end if
             end do
          else
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=dnint(css%Stacka(SP,JJ))
                end if
             end do
          end if
       CASE  (cVarName)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.1) then
             if (parse_bdeb) write(*,*)"*** Found shape:",nint(css%stacka(sp,1))
             !if (allocated(css%cbuff)) deallocate(css%cbuff)
             css%clen=25
             !allocate(character(len=css%clen) :: css%cbuff)
             !allocate(css%cbuff)
             css%cbuff=trim(getname25(css%Stacka(SP,1)))
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shapes:",nargs
             irc=312
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shapes.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cValidRange)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.3) then
             if (parse_bdeb) write(*,*)"*** Found validRange"
             DO JJ=1,NPOS
                if (css%StackA(SP,JJ).lt.css%Stacka(SP+1,JJ).or.&
                  & css%Stacka(SP,JJ).gt.css%Stacka(SP+2,JJ)) then
                   !if (allocated(css%cbuff)) deallocate(css%cbuff)
                   css%clen=25
                   !allocate(character(len=css%clen) :: css%cbuff)
                   !allocate(css%cbuff)
                   css%cbuff=trim(getname25(css%Stacka(SP,1)))
                end if
             end do
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to validRange:",nargs
             irc=312
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to validRange.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cShpPre)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=shapeid(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ))
                end if
             end do
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shape:",nargs
             irc=310
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shape.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cShpVic)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.3) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=vicinity(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ),css%Stacka(SP+2,JJ))
                end if
             end do
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to shape:",nargs
             irc=310
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to shape.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ctd2q) ! td,p
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=td2q(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ))
                END IF
             END DO
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to td2q:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to td2q.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (crh2td) ! rh, t, [ice=1,0]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=rh2td(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ))
                END IF
             END DO
          else if (nargs.eq.3) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=rh2td(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ),css%Stacka(SP+2,JJ))
                END IF
             END DO
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to rh2td:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to rh2td.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ctd2rh) ! td, t, [ice=1,0]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=td2rh(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ))
                END IF
             END DO
          else if (nargs.eq.3) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=td2rh(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ),css%Stacka(SP+2,JJ))
                END IF
             END DO
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to td2rh:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to td2rh.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (cq2rh) ! q, t, [p|1013.25]
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.eq.2) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=q2rh(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ))
                END IF
             END DO
          else if (nargs.eq.3) then
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=q2rh(css%Stacka(SP,JJ),css%Stacka(SP+1,JJ),css%Stacka(SP+2,JJ))
                END IF
             END DO
          else
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to q2rh:",nargs
             irc=313
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to q2rh.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             res=zero
             RETURN
          end if
       CASE  (ck2c)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=css%Stacka(SP,JJ)-t0
             END IF
          END DO
       CASE  (cc2k)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=css%Stacka(SP,JJ)+t0
             END IF
          END DO
       CASE  (cSinh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=SINH(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (cCosh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=COSH(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (cTanh)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=TANH(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE   (cSin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=SIN(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE   (cCos)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=COS(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE   (cTan)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=TAN(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  (cAsin)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                IF ((css%Stacka(SP,JJ)<-1._rn).OR.(css%Stacka(SP,JJ)>1._rn)) THEN
                   if (parse_bdeb) write(*,*)"*** Invalid argument to ASIN:",css%Stacka(SP,JJ)
                   irc=320
                   call parse_errorappend(crc250,myname//" Invalid arguments to asin.")
                   call parse_errorappendr(crc250,css%Stacka(SP,JJ))
                   call parse_errorappend(crc250,"\n")
                   EvalErrType=4
                   RETURN
                ELSE
                   css%Stacka(SP,JJ)=ASIN(css%Stacka(SP,JJ))
                ENDIF
             END IF
          END DO
       CASE  (cAcos)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                IF ((css%Stacka(SP,JJ)<-1._rn).OR.(css%Stacka(SP,JJ)>1._rn)) THEN
                   if (parse_bdeb) write(*,*)"*** Invalid argument to ACOS:",css%Stacka(SP,JJ)
                   irc=321
                   call parse_errorappend(crc250,myname//" Invalid arguments to acos.")
                   call parse_errorappendr(crc250,css%Stacka(SP,JJ))
                   call parse_errorappend(crc250,"\n")
                   EvalErrType=4
                   RETURN
                ELSE
                   css%Stacka(SP,JJ)=ACOS(css%Stacka(SP,JJ))
                ENDIF
             END IF
          END DO
       CASE  (cAtan2)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          IF (NARGS.EQ.2) THEN
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=ATAN2(css%Stacka(SP,JJ),css%stacka(SP+1,JJ))
                END IF
             END DO
          ELSE 
             if (parse_bdeb) write(*,*)"*** Unexpected number of arguments to atan2:",nargs
             irc=322
             call parse_errorappend(crc250,myname//" Unexpected number of arguments to atan2.")
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             RETURN
          END IF
       CASE  (cAtan)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=ATAN(css%Stacka(SP,JJ))
             END IF
          END DO
       CASE  DEFAULT
          SP=SP+1
          if (css%ByteCode(IP) .le. css%VarEnd) then
             ii=css%ByteCode(IP)-VarBegin+1
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=Val(ii,JJ)
                END IF
             END DO
          else
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=ConstVal(css%ByteCode(IP)-css%VarEnd)
                END IF
             END DO
          end if
       END SELECT
       if(parse_bdeb)then
          if (sp.ne.0) then
             write(str250,'(100(X,F0.2))') &
                  & (css%Stacka(II,1),II=1,min(SP,10))
          else
             STR250=""
          end if
          call chop0(str250,250)
          lens=length(str250,250,10)
          write(*,'(X,A,5X,A,2(X,I0))') myname,&
               & "STACK=["//str250(1:LENS)//"]",AP,nargs
       end if
    END DO
    if(parse_bdeb)write(*,*)myname,'Loop done.',npos,allocated(set)
    EvalErrType = 0
    DO JJ=1,NPOS
       IF(SET(JJ))THEN
          if(parse_bdeb)write(*,'(X,A,X,A,I0,A,F0.10)') myname," result(",jj,")=",css%Stacka(1,JJ)
          res(JJ) = css%Stacka(1,JJ)
       END IF
    END DO
    if(parse_bdeb)write(*,*)myname,"Done."
    return
  END subroutine parse_evala
  !
  subroutine parse_used (css, set)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Evaluate bytecode of ith function for the values passed in array Val(:)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer :: css
    logical                      :: Set(:)  ! is variable used?
    INTEGER                      :: IP       ! Instruction pointer
    character*22 :: myname ="parse_used"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if(parse_bdeb)write(*,*)myname,"Entering"
    if (.not.associated(css)) then
       if(parse_bdeb)write(*,*)myname,"Invalid session."
       EvalErrType=10
    else
       DO IP=1,css%ByteCodeSize
          if (css%ByteCode(IP).ge.VarBegin.and.css%ByteCode(IP).le.css%VarEnd)then
             set(css%ByteCode(IP)-VarBegin+1)=.true.
          end if
       END DO
    end if
    if(parse_bdeb)write(*,*)myname,"Done."
  END subroutine parse_used
  !
  SUBROUTINE parse_CheckSyntax (Func,FuncStr,Var,crc250,irc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check syntax of function string,  returns 0 if syntax is ok
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),               INTENT(in) :: Func      ! Function string without spaces
    CHARACTER (LEN=*),               INTENT(in) :: FuncStr   ! Original function string
    CHARACTER (LEN=*), allocatable, INTENT(in) :: Var(:)       ! Array with variable names
    character*250 :: crc250
    integer :: irc
    INTEGER(is)                                 :: n
    CHARACTER (LEN=1)                           :: c
    REAL(rn)                                    :: r
    LOGICAL                                     :: err
    INTEGER                                     :: ParCnt, & ! Parenthesis counter
         j,ib,in,lFunc
    character*22 :: myname ="parse_checkSyntax"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if(parse_bdeb)write(*,*)myname,'Entering.',irc
    j = 1
    ParCnt = 0
    lFunc = LEN_TRIM(Func)
    step: DO
       IF (j > lFunc) then
          CALL parse_ParseErrMsg (j, FuncStr,"",crc250,irc)
          return
       end if
       c = Func(j:j)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Check for valid operand (must appear)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       IF (c == '-' .OR. c == '+') THEN                      ! Check for leading - or +
          j = j+1
          IF (j > lFunc) then
             CALL parse_ParseErrMsg (j, FuncStr, 'Missing operand',crc250,irc)
             return
          end IF
          c = Func(j:j)
          IF (ANY(c == Ops)) then
             CALL parse_ParseErrMsg (j, FuncStr, 'Multiple operators',crc250,irc)
             return
          end IF
       END IF
       n = parse_MathFunctionIndex (Func(j:))
       IF (n > 0) THEN                                       ! Check for math function
          j = j+LEN_TRIM(Funcs(n))
          IF (j > lFunc) then
             CALL parse_ParseErrMsg (j, FuncStr, 'Missing function argument',crc250,irc)
             return
          end IF
          c = Func(j:j)
          IF (c /= '(') then
             CALL parse_ParseErrMsg (j, FuncStr, 'Missing opening parenthesis',crc250,irc)
             return
          end IF
       END IF
       IF (c == '(') THEN                                    ! Check for opening parenthesis
          ParCnt = ParCnt+1
          j = j+1
          CYCLE step
       END IF
       IF (SCAN(c,'0123456789.') > 0) THEN                   ! Check for number
          r = parse_RealNum (Func(j:),ib,in,err)
          IF (err) then
             CALL parse_ParseErrMsg (j, FuncStr, 'Invalid number format:  '//Func(j+ib-1:j+in-2),crc250,irc)
             return
          end IF
          j = j+in-1
          IF (j > lFunc) EXIT
          c = Func(j:j)
       ELSE                                                  ! Check for variable
          if(parse_bdeb)write(*,*)myname,'Checking variables.'
          if (allocated(var)) then
             n = parse_VariableIndex (Func(j:),Var,ib,in)
          else
             n=0
          end if
          IF (n == 0) then
             if(parse_bdeb)write(*,*)myname,'Checking constants.'
             n = parse_VariableIndex (Func(j:),const,ib,in)
             if (n==0) then
                CALL parse_ParseErrMsg (j, FuncStr, 'Invalid element: '//Func(j+ib-1:j+in-2),crc250,irc)
                return
             else 
                j = j+in-1
                IF (j > lFunc) EXIT
                c = Func(j:j)
             end if
          else
             j = j+in-1
             IF (j > lFunc) EXIT
             c = Func(j:j)
          end if
          if(parse_bdeb)write(*,*)myname,'Var/Const:',n
       END IF
       DO WHILE (c == ')')                                   ! Check for closing parenthesis
          ParCnt = ParCnt-1
          IF (ParCnt < 0) then
             CALL parse_ParseErrMsg (j, FuncStr, 'Mismatched parenthesis',crc250,irc)
             return
          end IF
          IF (Func(j-1:j-1) == '(') then
             CALL parse_ParseErrMsg (j-1, FuncStr, 'Empty parentheses',crc250,irc)
             return
          end if
          j = j+1
          IF (j > lFunc) EXIT
          c = Func(j:j)
       END DO
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Now, we have a legal operand: A legal operator or end of string must follow
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       IF (j > lFunc) EXIT
       IF (ANY(c == Ops)) THEN                               ! Check for multiple operators
          IF (j+1 > lFunc) then
             CALL parse_ParseErrMsg (j, FuncStr,"",crc250,irc)
             return
          end if
          IF (ANY(Func(j+1:j+1) == Ops)) then
             CALL parse_ParseErrMsg (j+1, FuncStr, 'Multiple operators',crc250,irc)
             return
          end if
       ELSE IF (c == ",") THEN
       ELSE ! Check for next operand
          CALL parse_ParseErrMsg (j, FuncStr, 'Missing operator',crc250,irc)
          return
       END IF
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       ! Now, we have an operand and an operator: the next loop will check for another 
       ! operand (must appear)
       !-- -------- --------- --------- --------- --------- --------- --------- -------
       j = j+1
    END DO step
    IF (ParCnt > 0) then
       CALL parse_ParseErrMsg (j, FuncStr, 'Missing )',crc250,irc)
       return
    end if
    if(parse_bdeb)write(*,*)myname,'Done checking.',irc
  END SUBROUTINE parse_CheckSyntax
  !
  FUNCTION parse_EvalErrMsg () RESULT (msg)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return error message
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*), DIMENSION(5), PARAMETER :: m = (/ 'Division by zero                ', &
         'Argument of SQRT negative       ', &
         'Argument of LOG negative        ', &
         'Argument of ASIN or ACOS illegal', &
         'Unexpected number of arguments  ' /)
    CHARACTER (LEN=LEN(m))                     :: msg
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IF (EvalErrType < 1 .OR. EvalErrType > SIZE(m)) THEN
       msg = ''
    ELSE
       msg = m(EvalErrType)
    ENDIF
  END FUNCTION parse_EvalErrMsg
  !
  SUBROUTINE parse_ParseErrMsg (j, FuncStr, Msg, crc250,irc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Print error message and terminate program
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    INTEGER,                     INTENT(in) :: j
    CHARACTER (LEN=*),           INTENT(in) :: FuncStr       ! Original function string
    CHARACTER (LEN=*),           INTENT(in) :: Msg
    character*250 :: crc250
    integer :: irc
    character*25 :: myname = "fparse"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    call parse_errorappend(crc250,myname//" "//Msg//" in string '"//&
         & FuncStr//"' [pos=")
    call parse_errorappendi(crc250,j)
    call parse_errorappend(crc250,"]\n")
    irc=323
    return
  END SUBROUTINE parse_ParseErrMsg
  !
  FUNCTION parse_OperatorIndex (c) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return operator index
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=1), INTENT(in) :: c
    INTEGER(is)                   :: n,j
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0
    DO j=cAdd,cPow
       IF (c == Ops(j)) THEN
          n = j
          EXIT
       END IF
    END DO
  END FUNCTION parse_OperatorIndex
  !
  FUNCTION parse_MathFunctionIndex (str) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return index of math function beginnig at 1st position of string str
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(in) :: str
    INTEGER(is)                   :: n,j
    INTEGER                       :: k
    CHARACTER (LEN=LEN(Funcs))    :: fun
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0
    DO j=cAbs,cAtan                                          ! Check all math functions
       k = MIN(LEN_TRIM(Funcs(j)), LEN(str))   
       CALL parse_LowCase (str(1:k), fun)
       IF (fun == Funcs(j)) THEN                             ! Compare lower case letters
          n = j                                              ! Found a matching function
          EXIT
       END IF
    END DO
  END FUNCTION parse_MathFunctionIndex
  !
  FUNCTION parse_VariableIndex (str, Var, ibegin, inext) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return index of variable at begin of string str (returns 0 if no variable found)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),               INTENT(in) :: str       ! String
    CHARACTER (LEN=*), allocatable, INTENT(in) :: Var(:)       ! Array with variable names
    INTEGER(is)                                 :: n         ! Index of variable
    INTEGER, OPTIONAL,              INTENT(out) :: ibegin, & ! Start position of variable name
         inext     ! Position of character after name
    INTEGER                                     :: j,ib,in,lstr
    character*25 :: myname = "parse_VariableIndex"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0
    if(parse_bdeb)write(*,*)myname,"Entering: '"//trim(str)//"'",ibegin,lbound(var,1),ubound(var,1)
    lstr = LEN_TRIM(str)
    IF (lstr > 0) THEN
       DO ib=1,lstr                                          ! Search for first character in str
          IF (str(ib:ib) /= ' ') EXIT                        ! When lstr>0 at least 1 char in str
       END DO
       DO in=ib,lstr                                         ! Search for name terminators
          IF (SCAN(str(in:in),'+-*/^) ,') > 0) EXIT
       END DO
       if (allocated(var)) then
          DO j=lbound(var,1),ubound(Var,1)
             IF (str(ib:in-1) .eq. trim(Var(j))) THEN                     
                n = j                                           ! Variable name found
                IF (PARSE_BDEB) THEN
                   write(*,*) myname,"** MATCH: '"//str(ib:in-1)//"' == '"//trim(Var(j))//"'"
                end if
                EXIT
             !ELSE IF (PARSE_BDEB) THEN
             !   write(*,*) myname,"no match: '"//str(ib:in-1)//"' != '"//trim(Var(j))//"'",&
             !        & in-ib,len_trim(var(j))
             END IF
          END DO
       end if
    END IF
    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in
    if (n.eq.0.and.parse_bdeb)write(*,*) myname,"No match for '"//str(ib:in-1)//"'"
    if(parse_bdeb)write(*,*)myname,"Done.",ib,in
  END FUNCTION parse_VariableIndex
  !
  SUBROUTINE RemoveSpaces (str)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Remove Spaces from string, remember positions of characters in old string
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(inout) :: str
    INTEGER                          :: lstr,ii,jj
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    lstr = LEN(str)
    ii=0
    do jj=1,lstr
       if (str(jj:jj).ne." ".and.ichar(str(jj:jj)).ge.32) then ! ascii characters >= 32
          ii=ii+1
          str(ii:ii)=str(jj:jj)
       end if
    end do
    do jj=ii+1,lstr
       str(jj:jj)=' '
    end do
    !do jj=1,lstr
    !   write(*,*)'parse_removespaces ',jj,"'"//str(jj:jj)//"'",ichar(str(jj:jj))
    !end do
  END SUBROUTINE RemoveSpaces
  !
  SUBROUTINE Replace (ca,cb,str)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Replace ALL appearances of character set ca in string str by character set cb
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),       INTENT(in) :: ca
    CHARACTER (LEN=LEN(ca)), INTENT(in) :: cb                ! LEN(ca) must be LEN(cb)
    CHARACTER (LEN=*),    INTENT(inout) :: str
    INTEGER                             :: j,lca
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    lca = LEN(ca)
    DO j=1,LEN_TRIM(str)-lca+1
       IF (str(j:j+lca-1) == ca) str(j:j+lca-1) = cb
    END DO
  END SUBROUTINE Replace
  !
  SUBROUTINE Parse_compile (css, F, Var, crc250,irc)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Compile i-th function string F into bytecode
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer :: css
    CHARACTER (LEN=*),               INTENT(in) :: F         ! Function string
    CHARACTER (LEN=*), allocatable, INTENT(in) :: Var(:)       ! Array with variable names
    character*250 :: crc250
    integer :: irc
    INTEGER                                     :: istat
    character*25 :: myname = "parse_compile"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IF (ASSOCIATED(css%ByteCode)) DEALLOCATE ( css%ByteCode, &
         css%Immed,    &
         css%Stack,    &
         css%ArgsByte     )
    css%ByteCodeSize = 0
    css%ImmedSize    = 0
    css%StackSize    = 0
    css%StackPtr     = 0
    css%ArgsSize     = 0
    css%ArgsPtr     = 0
    if (parse_bdeb) write(*,*)myname,"Compiling '"//F(1:LEN_TRIM(F))//"'"
    CALL parse_CompileSubstr (css,F,1,LEN_TRIM(F),Var,.false.)               ! Compile string to determine size
    if(parse_bdeb) write(*,*)myname,'>>>>> Arg size:',css%ArgsSize,css%ArgsPtr
    ALLOCATE ( css%ByteCode(css%ByteCodeSize), & 
         css%Immed(css%ImmedSize),       &
         css%Stack(max(1,css%StackSize)),       &
         css%ArgsByte(css%ArgsSize+1),    &
         STAT = istat                            )
    IF (istat /= 0) THEN
       call parse_errorappend(crc250,myname//" Memmory allocation for byte code failed.\n")
       irc=324
       return
    ELSE
       css%ByteCodeSize = 0
       css%ImmedSize    = 0
       css%StackSize    = 0
       css%StackPtr     = 0
       css%ArgsSize     = 0
       css%ArgsPtr     = 0
       css%argsByte(css%ArgsSize+1)=0 ! dummy value...
       CALL parse_CompileSubstr (css,F,1,LEN_TRIM(F),Var,.false.)            ! Compile string into bytecode
    END IF
    !
  END SUBROUTINE Parse_compile
  !
  SUBROUTINE parse_AddCompiledByte (css, b)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Add compiled byte to bytecode
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer :: css
    INTEGER(is), INTENT(in) :: b                             ! Value of byte to be added
    character*22 :: myname ="parse_AddCompiledByte"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    css%ByteCodeSize = css%ByteCodeSize + 1
    IF (ASSOCIATED(css%ByteCode)) css%ByteCode(css%ByteCodeSize) = b
    if (parse_bdeb)write(*,'(X,A,"Cmd(",I0,") -> ",A)')myname,css%ByteCodeSize,parse_code20(b,0)
  END SUBROUTINE parse_AddCompiledByte
  !
  FUNCTION parse_MathItemIndex (css, F, Var) RESULT (n)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Return math item index, if item is real number, enter it into Comp-structure
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer :: css
    CHARACTER (LEN=*),               INTENT(in) :: F         ! Function substring
    CHARACTER (LEN=*), allocatable, INTENT(in) :: Var(:)       ! Array with variable names
    INTEGER(is)                                 :: n         ! Byte value of math item
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    n = 0
    if (len_trim(f).eq.0)return
    IF (SCAN(F(1:1),'0123456789.') > 0) THEN                 ! Check for begin of a number
       css%ImmedSize = css%ImmedSize + 1
       IF (ASSOCIATED(css%Immed)) css%Immed(css%ImmedSize) = parse_RealNum (F)
       n = cImmed
    ELSE                                                     ! Check for a variable
       n = parse_VariableIndex (F, Var)
       IF (n > 0) then
          n = VarBegin+n-1
       else
          n = parse_VariableIndex (F, Const)
          IF (n > 0) then
             n = css%VarEnd+n
          end if
       end if
    END IF
  END FUNCTION parse_MathItemIndex
  !
  FUNCTION parse_CompletelyEnclosed (F, b, e) RESULT (res)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check if function substring F(b:e) is completely enclosed by a pair of parenthesis
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(in) :: F                       ! Function substring
    INTEGER,           INTENT(in) :: b,e                     ! First and last pos. of substring
    LOGICAL                       :: res
    INTEGER                       :: j,k
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    res=.false.
    IF (F(b:b) == '(' .AND. F(e:e) == ')') THEN
       k = 0
       DO j=b+1,e-1
          IF     (F(j:j) == '(') THEN
             k = k+1
          ELSEIF (F(j:j) == ')') THEN
             k = k-1
          END IF
          IF (k < 0) EXIT
       END DO
       IF (k == 0) res=.true.                                ! All opened parenthesis closed
    END IF
  END FUNCTION parse_CompletelyEnclosed
  !
  RECURSIVE SUBROUTINE parse_CompileSubstr (css, F, b, e, Var, comma)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Compile css function string F into bytecode
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    type(parse_session), pointer :: css
    CHARACTER (LEN=*),               INTENT(in) :: F         ! Function substring
    INTEGER,                         INTENT(in) :: b,e       ! Begin and end position substring
    CHARACTER (LEN=*), allocatable, INTENT(in)  :: Var(:)       ! Array with variable names
    logical, INTENT(in)                         :: comma
    INTEGER(is)                                 :: n
    INTEGER                                     :: b2,j,k,io
    CHARACTER (LEN=*),                PARAMETER :: calpha = 'abcdefghijklmnopqrstuvwxyz'// &
         'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    integer :: iArgsPtr
    integer :: nargs
    character*22 :: myname ="parse_compileSustr"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check for special cases of substring
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    nargs=0
    if (parse_bdeb) write(*,*) myname,'** Processing:',F(b:e)
    if (b.gt.e) return ! nothing to do
    IF (parse_CompletelyEnclosed (F, b, e)) THEN               ! Case 2: F(b:e) = '(...)'
       if (parse_bdeb) WRITE(*,*)myname,'2. F(b:e) = "(...)"',F(b:e)
       CALL parse_CompileSubstr (css, F, b+1, e-1, Var,.true.)
       RETURN
    ELSEIF (SCAN(F(b:b),calpha) > 0) THEN        
       if (parse_bdeb) WRITE(*,*)myname,'3. Found Alphanumeric: ',F(b:e),allocated(var)
       n = parse_MathFunctionIndex (F(b:e))
       IF (n > 0) THEN
          b2 = b+INDEX(F(b:e),'(')-1
          IF (parse_CompletelyEnclosed(F, b2, e)) THEN             ! Case 3: F(b:e) = 'fcn(...)'
             if (parse_bdeb) WRITE(*,*)myname,'3. F(b:e) = "fcn(...)" ',F(b2+1:e-1)
             iArgsPtr=css%ArgsPtr ! current argument pointer
             CALL parse_CompileSubstr(css, F, b2+1, e-1, Var,.true.)
             css%ArgsSize=css%ArgsSize+1 ! increase function counter
             nargs=css%ArgsPtr-iArgsPtr+1 ! (number of commas) + 1
             css%ArgsPtr=iArgsPtr
             if (parse_bdeb) WRITE(*,'(X,A,X,A,I0,A,A)')myname,&
                  & '3. Number of arguments= ',nargs," '"//F(b2+1:e-1)//"'"
             IF (ASSOCIATED(css%ArgsByte))then
                css%ArgsByte(css%ArgsSize)=nargs ! number of arguments
                if (parse_bdeb)write(*,'(X,A,X,A,I0,A,I0)')myname,&
                     & 'Argsbyte(',css%ArgsSize,')=',css%ArgsByte(css%ArgsSize)
             end if
             CALL parse_AddCompiledByte (css, n)
             RETURN
          END IF
       END IF
    END IF
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check for commas in argument lists
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (comma) then
       k = 0
       DO j=e,b,-1
          IF     (F(j:j) == ')') THEN
             k = k+1
          ELSEIF (F(j:j) == '(') THEN
             k = k-1
          END IF
          IF (k == 0 .AND. F(j:j) == ",") THEN
             if (parse_bdeb) WRITE(*,*)myname,'3. Found comma: ', F(j:j)
             CALL parse_CompileSubstr (css, F, b, j-1, Var,comma)
             CALL parse_CompileSubstr (css, F, j+1, e, Var,comma)
             css%ArgsPtr=css%ArgsPtr+1 ! increase function counter
             RETURN
          END IF
       END DO
    end if
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check for signs
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IF (F(b:b) == '+') THEN                              ! Case 1: F(b:e) = '+...'
       if (parse_bdeb) WRITE(*,*)myname,'1. F(b:e) = "+..."'
       CALL parse_CompileSubstr (css, F, b+1, e, Var,comma)
       RETURN
    ELSEIF (F(b:b) == '-') THEN
       if (parse_bdeb) WRITE(*,*)myname,'3. Found Minus'
       CALL parse_CompileSubstr (css, F, b+1, e, Var,comma)
       CALL parse_AddCompiledByte (css, cNeg)
       RETURN
    END IF
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check for operator in substring: check only base level (k=0), exclude expr. in ()
    !----- -------- --------- --------- --------- --------- --------- --------- -------    
    DO io=cAdd,cPow                                          ! Increasing priority +-*/^
       k = 0
       DO j=e,b,-1
          IF     (F(j:j) == ')') THEN
             k = k+1
          ELSEIF (F(j:j) == '(') THEN
             k = k-1
          END IF
          IF (k == 0 .AND. F(j:j) == Ops(io) .AND. parse_IsBinaryOp (j, F)) THEN
             if (parse_bdeb) WRITE(*,*)myname,'3. Found Binary op at: ',j,'('//F(j:j)//')'
             IF (ANY(F(j:j) == Ops(cMul:cPow)) .AND. F(b:b) == '-') THEN ! Case 6: F(b:e) = '-...Op...' with Op > -
                if (parse_bdeb) WRITE(*,*)myname,'6. F(b:e) = "-...Op..." with Op > -'
                CALL parse_CompileSubstr (css, F, b+1, e, Var,comma)
                CALL parse_AddCompiledByte (css, cNeg)
                RETURN                 
             ELSE                                                        ! Case 7: F(b:e) = '...BinOp...'
                if (parse_bdeb) WRITE(*,*)myname,'7. Binary operator ',F(j:j),b,j,e
                CALL parse_CompileSubstr (css, F, b, j-1, Var,comma)
                CALL parse_CompileSubstr (css, F, j+1, e, Var,comma)
                CALL parse_AddCompiledByte (css, parse_OperatorIndex(Ops(io)))
                css%StackPtr = css%StackPtr - 1
                RETURN
             END IF
          END IF
       END DO
    END DO
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check for remaining items, i.e. variables or explicit numbers
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    b2 = b
    IF (F(b:b) == '-') b2 = b2+1
    n = parse_MathItemIndex(css, F(b2:e), Var)
    if (parse_bdeb) WRITE(*,*)myname,'8. parse_AddCompiledByte ',parse_code20(n,0)," = '"//F(b2:e)//"'",&
         & css%StackPtr
    CALL parse_AddCompiledByte (css, n)
    css%StackPtr = css%StackPtr + 1
    IF (css%StackPtr > css%StackSize) css%StackSize = css%StackSize + 1
    IF (b2 > b) CALL parse_AddCompiledByte (css, cNeg)
  END SUBROUTINE parse_CompileSubstr
  !
  FUNCTION parse_IsBinaryOp (j, F) RESULT (res)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Check if operator F(j:j) in string F is binary operator
    ! Special cases already covered elsewhere:              (that is corrected in v1.1)
    ! - operator character F(j:j) is first character of string (j=1)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    INTEGER,           INTENT(in) :: j                       ! Position of Operator
    CHARACTER (LEN=*), INTENT(in) :: F                       ! String
    LOGICAL                       :: res                     ! Result
    INTEGER                       :: k
    LOGICAL                       :: Dflag,Pflag
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    res=.true.
    IF (F(j:j) == '+' .OR. F(j:j) == '-') THEN               ! Plus or minus sign:
       IF (j == 1) THEN                                      ! - leading unary operator ?
          res = .false.
       ELSEIF (SCAN(F(j-1:j-1),'+-*/^(') > 0) THEN           ! - other unary operator ?
          res = .false.
       ELSEIF (SCAN(F(j+1:j+1),'0123456789') > 0 .AND. &     ! - in exponent of real number ?
            SCAN(F(j-1:j-1),'eEdD')       > 0) THEN
          Dflag=.false.
          Pflag=.false.
          k = j-1
          DO WHILE (k > 1)                                   !   step to the left in mantissa 
             k = k-1
             IF     (SCAN(F(k:k),'0123456789') > 0) THEN
                Dflag=.true.
             ELSEIF (F(k:k) == '.') THEN
                IF (Pflag) THEN
                   EXIT                                      !   * EXIT: 2nd appearance of '.'
                ELSE
                   Pflag=.true.                              !   * mark 1st appearance of '.'
                ENDIF
             ELSE
                EXIT                                         !   * all other characters
             END IF
          END DO
          IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
       END IF
    END IF
  END FUNCTION parse_IsBinaryOp
  !
  FUNCTION parse_RealNum (str, ibegin, inext, error) RESULT (res)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Get real number from string - Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),  INTENT(in) :: str                    ! String
    REAL(rn)                       :: res                    ! Real number
    INTEGER, OPTIONAL, INTENT(out) :: ibegin,              & ! Start position of real number
         inext                  ! 1st character after real number
    LOGICAL, OPTIONAL, INTENT(out) :: error                  ! Error flag
    INTEGER                        :: ib,in,istat
    LOGICAL                        :: Bflag,               & ! .T. at begin of number in str
         InMan,               & ! .T. in mantissa of number
         Pflag,               & ! .T. after 1st '.' encountered
         Eflag,               & ! .T. at exponent identifier 'eEdD'
         InExp,               & ! .T. in exponent of number
         DInMan,              & ! .T. if at least 1 digit in mant.
         DInExp,              & ! .T. if at least 1 digit in exp.
         err                    ! Local error flag
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    Bflag=.true.
    InMan=.false.
    Pflag=.false.
    Eflag=.false.
    InExp=.false.
    DInMan=.false.
    DInExp=.false.
    ib   = 1
    in   = 1
    DO WHILE (in <= LEN_TRIM(str))
       SELECT CASE (str(in:in))
       CASE (' ')                                            ! Only leading blanks permitted
          ib = ib+1
          IF (InMan .OR. Eflag .OR. InExp) EXIT
       CASE ('+','-')                                        ! Permitted only
          IF     (Bflag) THEN           
             InMan=.true.
             Bflag=.false.                     ! - at beginning of mantissa
          ELSEIF (Eflag) THEN               
             InExp=.true.
             Eflag=.false.                     ! - at beginning of exponent
          ELSE
             EXIT                                            ! - otherwise STOP
          ENDIF
       CASE ('0':'9')                                        ! Mark
          IF     (Bflag) THEN           
             InMan=.true.
             Bflag=.false.                     ! - beginning of mantissa
          ELSEIF (Eflag) THEN               
             InExp=.true.
             Eflag=.false.                     ! - beginning of exponent
          ENDIF
          IF (InMan) DInMan=.true.                           ! Mantissa contains digit
          IF (InExp) DInExp=.true.                           ! Exponent contains digit
       CASE ('.')
          IF     (Bflag) THEN
             Pflag=.true.                                    ! - mark 1st appearance of '.'
             InMan=.true.
             Bflag=.false.                     !   mark beginning of mantissa
          ELSEIF (InMan .AND..NOT.Pflag) THEN
             Pflag=.true.                                    ! - mark 1st appearance of '.'
          ELSE
             EXIT                                            ! - otherwise STOP
          END IF
       CASE ('e','E','d','D')                                ! Permitted only
          IF (InMan) THEN
             Eflag=.true.
             InMan=.false.                     ! - following mantissa
          ELSE
             EXIT                                            ! - otherwise STOP
          ENDIF
       CASE DEFAULT
          EXIT                                               ! STOP at all other characters
       END SELECT
       in = in+1
    END DO
    err = (ib > in-1) .OR. (.NOT.DInMan) .OR. ((Eflag.OR.InExp).AND..NOT.DInExp)
    IF (err) THEN
       res = 0.0_rn
    ELSE
       READ(str(ib:in-1),*,IOSTAT=istat) res
       err = istat /= 0
    END IF
    IF (PRESENT(ibegin)) ibegin = ib
    IF (PRESENT(inext))  inext  = in
    IF (PRESENT(error))  error  = err
  END FUNCTION parse_RealNum
  !  
  SUBROUTINE parse_LowCase (str1, str2)
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    ! Transform upper case letters in str1 into lower case letters, result is str2
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    IMPLICIT NONE
    CHARACTER (LEN=*),  INTENT(in) :: str1
    CHARACTER (LEN=*), INTENT(out) :: str2
    INTEGER                        :: j,k
    CHARACTER (LEN=*),   PARAMETER :: lc = 'abcdefghijklmnopqrstuvwxyz'
    CHARACTER (LEN=*),   PARAMETER :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    str2 = str1
    DO j=1,LEN_TRIM(str1)
       k = INDEX(uc,str1(j:j))
       IF (k > 0) str2(j:j) = lc(k:k)
    END DO
  END SUBROUTINE parse_LowCase
  !
  real(rn) function parse_f1970(yy,mm,dd,hh,mi,sec)
    implicit none
    real(rn)  :: yy,mm,dd,hh,mi,sec
    !     returns "seconds since 1970-01-01 00:00:00 +00:00"
    real(rn) days
    !     yy=1970
    !     mm=01
    !     dd=01
    !     mi=00
    !     sec=0.0D0
    !     call date2jd2000(days,yy,mm,dd,hh,mi,sec)
    !     write(*,*) 'S1970 1970-reference in J2000:',days!  2440587.5
    call date2jd(days,nint(yy),nint(mm),nint(dd),nint(hh),nint(mi),sec) ! get days since 2000/1/1 0:0
    days = days - 2440587.5  ! convert to days since reference
    parse_f1970=days*86400.0D0      ! convert to seconds
    return
  end function parse_f1970
  !
  real(rn) function parse_f1970_yy(secs)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "seconds since 1970-01-01 00:00:00 +00:00"
    real(rn) days,sec,secs
    !     yy=1970
    !     mm=01
    !     dd=01
    !     mi=00
    !     sec=0.0D0
    !     call date2jd2000(days,yy,mm,dd,hh,mi,sec)
    !     write(*,*) 'S1970 1970-reference in J2000:',days!  2440587.5
    days=secs/86400.0D0+2440587.5
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_f1970_yy=yy      ! convert to seconds
    return
  end function parse_f1970_yy
  !
  real(rn) function parse_f1970_mm(secs)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "seconds since 1970-01-01 00:00:00 +00:00"
    real(rn) days,sec,secs
    !     yy=1970
    !     mm=01
    !     dd=01
    !     mi=00
    !     sec=0.0D0
    !     call date2jd2000(days,yy,mm,dd,hh,mi,sec)
    !     write(*,*) 'S1970 1970-reference in J2000:',days!  2440587.5
    days=secs/86400.0D0+2440587.5
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_f1970_mm=mm      ! convert to seconds
    return
  end function parse_f1970_mm
  !
  real(rn) function parse_f1970_dd(secs)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "seconds since 1970-01-01 00:00:00 +00:00"
    real(rn) days,sec,secs
    !     yy=1970
    !     mm=01
    !     dd=01
    !     mi=00
    !     sec=0.0D0
    !     call date2jd2000(days,yy,mm,dd,hh,mi,sec)
    !     write(*,*) 'S1970 1970-reference in J2000:',days!  2440587.5
    days=secs/86400.0D0+2440587.5
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_f1970_dd=dd      ! convert to seconds
    return
  end function parse_f1970_dd
  !
  real(rn) function parse_f1970_hh(secs)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "seconds since 1970-01-01 00:00:00 +00:00"
    real(rn) days,sec,secs
    !     yy=1970
    !     mm=01
    !     dd=01
    !     mi=00
    !     sec=0.0D0
    !     call date2jd2000(days,yy,mm,dd,hh,mi,sec)
    !     write(*,*) 'S1970 1970-reference in J2000:',days!  2440587.5
    days=secs/86400.0D0+2440587.5
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_f1970_hh=hh      ! convert to seconds
    return
  end function parse_f1970_hh
  !
  real(rn) function parse_f1970_mi(secs)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "seconds since 1970-01-01 00:00:00 +00:00"
    real(rn) days,sec,secs
    !     yy=1970
    !     mm=01
    !     dd=01
    !     mi=00
    !     sec=0.0D0
    !     call date2jd2000(days,yy,mm,dd,hh,mi,sec)
    !     write(*,*) 'S1970 1970-reference in J2000:',days!  2440587.5
    days=secs/86400.0D0+2440587.5
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_f1970_mi=mi      ! convert to seconds
    return
  end function parse_f1970_mi
  !
  real(rn) function parse_fjulian(yy,mm,dd,hh,mi,sec)
    implicit none
    real(rn)  :: yy,mm,dd,hh,mi
    !     returns "julian days since 2000
    real(rn) days,sec
    call date2jd(days,nint(yy),nint(mm),nint(dd),nint(hh),nint(mi),sec) ! get days since 2000/1/1 0:0
    parse_fjulian=days
    return
  end function parse_fjulian
  !
  real(rn) function parse_fjulian_yy(days)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "julian days since 2000
    real(rn) days,sec
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_fjulian_yy=yy
    return
  end function parse_fjulian_yy
  !
  real(rn) function parse_fjulian_mm(days)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "julian days since 2000
    real(rn) days,sec
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_fjulian_mm=real(mm)
    return
  end function parse_fjulian_mm
  !
  real(rn) function parse_fjulian_dd(days)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "julian days since 2000
    real(rn) days,sec
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_fjulian_dd=real(dd)
    return
  end function parse_fjulian_dd
  !
  real(rn) function parse_fjulian_hh(days)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "julian days since 2000
    real(rn) days,sec
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_fjulian_hh=real(hh)
    return
  end function parse_fjulian_hh
  !
  real(rn) function parse_fjulian_mi(days)
    implicit none
    integer  :: yy,mm,dd,hh,mi
    !     returns "julian days since 2000
    real(rn) days,sec
    call jd2date(days,yy,mm,dd,hh,mi,sec) ! get days since 2000/1/1 0:0
    parse_fjulian_mi=real(mi)
    return
  end function parse_fjulian_mi
  !
  subroutine DATE2JD (JD, YEAR,MONTH,DAY,HOUR,MINUTES,SECONDS)
    !     (corresponds to JD2000)
    !     Computes julian day from gregorian (civil) calendar
    !     O  (REAL*8) JD = JULIAN. DAY
    !     I  (INT*4) YY = YEAR
    !     I  (INT*4) MM = MONTH
    !     I  (INT*4) DD = DAY
    !     I  (INT*4) HH = HOUR
    !     I  (INT*4) MI = MINUTE
    !     I  (REAL*8) SEC = SECOND.
    implicit none
    REAL(rn) JD
    INTEGER(is) YEAR,MONTH,DAY,HOUR,MINUTES,I,J,K
    REAL(rn) SECONDS
    I= YEAR
    J= MONTH
    K= DAY
    JD= K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12) &
         /12-3*((I+4900+(J-14)/12)/100)/4
    JD=JD+((HOUR-12.0D0)/24.0D0)+(MINUTES/1440.0D0)+(SECONDS/86400.0D0)
    RETURN
  END subroutine DATE2JD
  SUBROUTINE JD2DATE (JD, YEAR,MONTH,DAY,HOUR,MINUTES,SECONDS)
    !     (corresponds to DJ2000)
    !     Computes gregorian (civil) calendar from julian day
    !     I  (REAL*8) JD = JULIAN. DAY
    !     O  (INT*4) YY = YEAR
    !     O  (INT*4) MM = MONTH
    !     O  (INT*4) DD = DAY
    !     O  (INT*4) HH = HOUR
    !     O  (INT*4) MI = MINUTE
    !     O  (REAL*8) SEC = SECOND.
    !
    implicit none
    REAL*8 JD
    INTEGER YEAR,MONTH,DAY,HOUR,MINUTES
    REAL*8 SECONDS
    REAL*8 DJ
    INTEGER*8 I,J,K,L,N,IJ
    IJ= INT(JD+0.5D0)
    DJ=(JD+0.5D0)-real(ij)
    L= IJ+68569
    N= 4*L/146097
    L= L-(146097*N+3)/4
    I= 4000*(L+1)/1461001
    L= L-1461*I/4+31
    J= 80*L/2447
    K= L-2447*J/80
    L= J/11
    J= J+2-12*L
    I= 100*(N-49)+I+L
    YEAR= INT(I)
    MONTH= INT(J)
    DAY= INT(K)
    DJ=DJ*86400.0D0 ! seconds
    HOUR=int(DJ/3600.0D0)
    Dj=DJ-HOUR*3600.0D0
    MINUTES=INT(DJ/60.0D0)
    Dj=DJ-MINUTES*60.0D0
    SECONDS=DJ
    RETURN
  END SUBROUTINE JD2DATE
  !
  ! E R R O R    R O U T I N E S
  !
  subroutine parse_errorappend(crc250,string)
    implicit none
    character*250 :: crc250
    character*(*) :: string
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    character*22 :: myname ="parse_errorappend"
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    lenb=len(trim(string))
    buff250=string(1:lenb)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//""//buff250(1:min(250-lenc-1,lenb))
    end if
    if (parse_bdeb)write(*,*)myname,buff250(1:lenb)
    call chop0(crc250,250)
    return
  end subroutine parse_errorappend
  subroutine parse_errorappendi(crc250,inum)
    implicit none
    character*250 :: crc250
    integer :: inum
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    character*22 :: myname ="parse_errorappendi"
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    write(buff250,'(I12)')inum
    call chop0(buff250,250)
    lenb=length(buff250,250,1)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//""//buff250(1:min(250-lenc-1,lenb))
    end if
    if (parse_bdeb)write(*,*)myname,buff250(1:lenb)
    call chop0(crc250,250)
    return
  end subroutine parse_errorappendi
  subroutine parse_errorappendr(crc250,rnum)
    implicit none
    character*250 :: crc250
    real(rn) :: rnum
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    character*22 :: myname ="parse_errorappendi"
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    write(buff250,'(F0.2)')rnum
    call chop0(buff250,250)
    lenb=length(buff250,250,1)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//""//buff250(1:min(250-lenc-1,lenb))
    end if
    if (parse_bdeb)write(*,*)myname,buff250(1:lenb)
    call chop0(crc250,250)
    return
  end subroutine parse_errorappendr
  !
  character*20 function parse_code20(code,nargs)
    integer code,nargs,ii
    if (code.ge.cabs.and.code.le.catan) then
       if (nargs.eq.0) then
          parse_code20=funcs(code)
       else
          write(parse_code20,'(A,"(",I0,")")')trim(funcs(code)),nargs
       end if
    ELSE IF (code.eq.cImmed) THEN
       parse_code20="+stack=fix"
    ELSE IF (code.eq.cNeg) THEN
       parse_code20="neg"
    ELSE IF (code.eq.cAdd) THEN
       parse_code20="add"
    ELSE IF (code.eq.cSub) THEN
       parse_code20="sub"
    ELSE IF (code.eq.cMul) THEN
       parse_code20="mul"
    ELSE IF (code.eq.cDiv) THEN
       parse_code20="div"
    ELSE IF (code.eq.cPow) THEN
       parse_code20="pow"
    else
       ii=code-VarBegin+1
       write(parse_code20,'("+stack=",I0)')ii
    end if
    return
  end function parse_code20
  !
  ! CEOP Derived Parameter Equations
  ! http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html
  
  ! Compute the Specific Humidity  from dew point temperature (Bolton 1980):

  real function td2q(td,p) ! td,p
    real :: td,p
    real :: e,q
    ! Vapour pressure in mb
    !    where:
    !       e = vapor pressure in mb;
    !       Td = dew point in deg C;
    !       p = surface pressure in mb;
    !       q = specific humidity in g/kg.
    e= 6.112*exp((17.67*td)/(td + 243.5))
    q= 1000*(0.622 * e)/(p - (0.378 * e))
    td2q=q
    return ! q
  end function td2q
  
  ! Compute Dew Point Temperature (Bolton 1980):
  
  real function rh2td(rh,t,rice) ! rh, t, ice=NULL
    real :: rh,t
    real, OPTIONAL :: rice
    logical :: ice
    real :: es,e,td
    !     where:
    !      rh - relative Humidity in percent;
    !       t - temperature in deg C
    ! Saturation vapor pressure in mb:
    if (present(rice)) then
       ice=rice.ge.0.5D0
    else
       ice=(t.le.0.0D0)
    end if
    es=satVapPres(t+273.15,ice)
    ! Vapor pressure in mb:
    e =es * rh/100.0
    ! Dew point in deg C
    td=log(e/6.112)*243.5/(17.67-log(e/6.112))
    rh2td=td
    return ! td
  end function rh2td
    
  ! Compute Relative Humidity (Bolton 1980):

  real function td2rh(td,t,rice) ! td, t, ice=NULL
    real :: td,t
    real, optional :: rice
    !     where:
    !       td - dew point in deg C
    !       t - temperature in deg C
    logical :: ice
    real :: es,e,rh
    if (present(rice)) then
       ice=rice.ge.0.5D0
    else
       ice=(t.le.0.0D0)
    end if
    es=satVapPres(t+273.15, ice)
    ! Vapor pressure in mb;
    e=6.112*exp((17.67*td)/(td + 243.5))
    ! Relative Humidity in percent 
    rh=100.0 * (e/es)
    if (rh>100) rh=100.0D0
    td2rh=rh
    return ! rh
  end function td2rh
  
  !  From somewhere else
  !  GANSKE nøyaktig...
  
  real function q2rh(q,t,rp) ! q, t, p = 1013.25
    real :: q, t
    real, optional :: rp
    real :: es,e,rh,p
    !   Q --> RH
    !   q - specific humidity
    !   t - temperature in Kelvin
    !   p - pressure
    if (present(rp)) then
       p=rp
    else
       p=1013.25
    end if
    es        = 6.112 * exp((17.67 * (t-273.15))/(t - 29.65))
    e         =q * p / (0.378 * q + 0.622)
    rh        =e / es
    if (rh > 1) rh=1.0D0
    if (rh < 0) rh=0.0D0
    q2rh=rh*100.0D0
    return ! rh*100
  end function q2rh
  
  real function satVapPres(t,ice) ! t, ice
    real :: t
    logical :: ice
    real :: tc,ew,log10ei,ei,e
    !  Saturation vapour pressure above water
    !
    !    Guide to Meteorological Instruments and Methods of Observation (CIMO Guide)
    !    (WMO, 2008) with t in [°C] and ew in [hPa]
    tc =t - 273.15
    ew =6.112*exp(17.62*(tc/(243.12 + tc)))
    !  Saturation vapour pressure above ice
    ! Vapor pressure over ice
    !    Goff Gratch equation (Smithsonian Tables, 1984):
    !
    !    Log10 ei =  -9.09718 (273.16/T - 1)                                                             [12]
    !                       - 3.56654 Log10(273.16/ T)
    !                       + 0.876793 (1 - T/ 273.16)
    !                       + Log10(6.1071)
    !    with T in [K] and ei in [hPa]
    tc =t - 273.15
    ew =6.112*exp(17.62*(tc/(243.12 + tc)))
    !  Saturation vapour pressure above ice
    ! Vapor pressure over ice
    !    Goff Gratch equation (Smithsonian Tables, 1984):
    !
    !    Log10 ei =  -9.09718 (273.16/T - 1)                                                             [12]
    !                       - 3.56654 Log10(273.16/ T)
    !                       + 0.876793 (1 - T/ 273.16)
    !                       + Log10(6.1071)
    !    with T in [K] and ei in [hPa]
    log10ei =  -9.09718*(273.16/t - 1)&
         & - 3.56654*log10(273.16/t) &
         & + 0.876793*(1 - t/273.16) &
         & + log10(6.1071)
    ei      =   10**log10ei
    log10ei =  -9.09718*(273.16/t - 1) &
         & - 3.56654*log10(273.16/t) &
         & + 0.876793*(1 - t/273.16) &
         & + log10(6.1071)
    ei      =   10**log10ei
    if(ice) then
       e=ei
    else
       e=ew
    end if
    satVapPres=e
    return ! e
  end function satVapPres
  function camelCase(strIn,lens) result(strOut)
    implicit none
    integer :: lens
    character(len=*), intent(in) :: strIn
    character(len=len(strIn))    :: strBuff
    character(len=lens)          :: strOut
    integer :: i,j,k
    logical :: lup
    strBuff="";
    k=0
    lup=.true. ! start with uppercase
    do i = 1, len(strIn)
       j = iachar(strIn(i:i))
       if (j>= iachar("a") .and. j<=iachar("z") ) then ! lower case
          k=k+1
          if (lup) then ! make upper case
             strBuff(k:k) = achar(iachar(strIn(i:i))-32)
             lup=.false.
          else
             strBuff(k:k) = strIn(i:i)
          end if
       else if (j>= iachar("A") .and. j<=iachar("Z") ) then
          k=k+1
          if (lup) then
             strBuff(k:k) = strIn(i:i)
             lup=.false.
          else ! make lower case
             strBuff(k:k) = achar(iachar(strIn(i:i))+32)
          end if
       else if (j.eq.iachar(" ")) then
          lup=.true.
       else
          k=k+1
          strBuff(k:k) = strIn(i:i)
       end if
    end do
    strOut=strBuff
  end function camelCase
  function to_upper(strIn) result(strOut)
    ! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
    ! Original author: Clive Page
    
    implicit none
    
    character(len=*), intent(in) :: strIn
    character(len=len(strIn)) :: strOut
    integer :: i,j

    do i = 1, len(strIn)
       j = iachar(strIn(i:i))
       if (j>= iachar("a") .and. j<=iachar("z") ) then
          strOut(i:i) = achar(iachar(strIn(i:i))-32)
       else
          strOut(i:i) = strIn(i:i)
       end if
    end do
    
  end function to_upper
  function to_lower(strIn) result(strOut)
    ! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
    ! Original author: Clive Page
    
    implicit none
    
    character(len=*), intent(in) :: strIn
    character(len=len(strIn)) :: strOut
    integer :: i,j
    
    do i = 1, len(strIn)
       j = iachar(strIn(i:i))
       if (j>= iachar("A") .and. j<=iachar("Z") ) then
          strOut(i:i) = achar(iachar(strIn(i:i))+32)
       else
          strOut(i:i) = strIn(i:i)
       end if
    end do
    
  end function to_lower
  ! returns the shape identification
  integer function shapeid (lon,lat)
    USE shape
    implicit none
    real :: lon
    real :: lat
    integer :: ii,inout
    real :: pos(3),xx,yy,zz
    if (abs(sf%shapeidlat-lat).lt.1.0D-6.and. &
         & abs(sf%shapeidlon-lon).lt.1.0D-6) then
       shapeid=sf%shapeid
       if (parse_bdeb) write(*,*)"Shapeid Using cache:",lat,lon,shapeid
       return
    end if
    do ii=1,sf%nshp
       inout=0
       call shape_lonlat2pos(lon,lat,pos)
       zz=shape_dot(sf%shp(ii)%map(1,3),pos)
       if (zz.ge.sf%shp(ii)%minzz) then
          xx=shape_dot(sf%shp(ii)%map(1,1),pos)
          yy=shape_dot(sf%shp(ii)%map(1,2),pos)
          if (xx.ge.sf%shp(ii)%minxx.and. &
               & xx.le.sf%shp(ii)%maxxx.and. &
               & yy.ge.sf%shp(ii)%minyy.and. &
               & yy.le.sf%shp(ii)%maxyy) then
             if (parse_bdeb) write(*,*)"shapeid Checking shape:",ii,&
                  & sf%shp(ii)%index,sf%shp(ii)%npos, &
                  & trim(sf%shp(ii)%name)
             call shape_pnpoly(xx,yy,sf%shp(ii)%npos,&
                  & sf%shp(ii)%xx,sf%shp(ii)%yy,inout)
             if (parse_bdeb) write(*,*)"Shapeid checked shape:",ii,inout,&
                  & sf%shp(ii)%minxx,sf%shp(ii)%maxxx,&
                  & sf%shp(ii)%minyy,sf%shp(ii)%maxyy,&
                  & trim(sf%shp(ii)%name)
             if (inout.ge.0) then
                shapeid=sf%shp(ii)%index
                sf%shapeid=shapeid
                sf%shapeidlat=lat
                sf%shapeidlon=lon
                return
             end if
          end if
       end if
    end do
    shapeid=0 ! no matching shapes
    sf%shapeid=shapeid
    sf%shapeidlat=lat
    sf%shapeidlon=lon
    return
  end function shapeid
  ! returns the closest shape identification
  integer function vicinity (lon,lat,eps)
    USE shape
    implicit none
    real :: lon ! longitude in degrees
    real :: lat ! latitude in degrees
    real :: eps ! tolerance in km
    integer :: ii,inout
    real :: dd, dbbox, epr
    logical lbbox
    integer ibbox
    real :: pos(3),rr,xx,yy,zz
    ! first check if position is inside a polygon...
    if (abs(sf%vicinitylat-lat).lt.1.0D-6.and. &
         & abs(sf%vicinitylon-lon).lt.1.0D-6) then
       vicinity=sf%vicinity
       if (parse_bdeb) write(*,*)"Vicinity Using cache:",lat,lon,vicinity
       return
    end if
    do ii=1,sf%nshp
       inout=0
       call shape_lonlat2pos(lon,lat,pos)
       zz=shape_dot(sf%shp(ii)%map(1,3),pos)
       if (zz.ge.sf%shp(ii)%minzz) then
          xx=shape_dot(sf%shp(ii)%map(1,1),pos)
          yy=shape_dot(sf%shp(ii)%map(1,2),pos)
          if (xx.ge.sf%shp(ii)%minxx.and. &
               & xx.le.sf%shp(ii)%maxxx.and. &
               & yy.ge.sf%shp(ii)%minyy.and. &
               & yy.le.sf%shp(ii)%maxyy) then
             if (parse_bdeb) write(*,*)"vicinity Checking shape:",ii,&
                  & sf%shp(ii)%index,sf%shp(ii)%npos, &
                  & trim(sf%shp(ii)%name)
             call shape_pnpoly(xx,yy,sf%shp(ii)%npos,&
                  & sf%shp(ii)%xx,sf%shp(ii)%yy,inout)
             if (parse_bdeb) write(*,*)"vicinity Checking shape:",ii,inout,&
                  & sf%shp(ii)%minxx,sf%shp(ii)%maxxx,&
                  & sf%shp(ii)%minyy,sf%shp(ii)%maxyy,&
                  & trim(sf%shp(ii)%name)
             if (inout.ge.0) then ! position inside shape, we are done...
                vicinity=sf%shp(ii)%index
                sf%vicinity=vicinity
                sf%vicinitylat=lat
                sf%vicinitylon=lon
                return
             end if
          end if
       end if
    end do
    ! find closest polygon (in cartesian coordinates)
    ! We mix distance on cirle with distance through circle..
    call shape_lonlat2pos(lon,lat,pos) ! radius==1
    dbbox=abs(eps/6371.0D0) ! km -> rad
    if(parse_bdeb)write(*,*)'VICINITY limit:',dbbox
    lbbox=.false.
    ibbox=0
    do ii=1,sf%nshp ! loop over shapes
       ! get distance to bounding box
       dd=acos(shape_dot(pos,sf%shp(ii)%map(1,3)))
       if (dd.lt.1.57D0.and.dd-sf%shp(ii)%maxrad.lt.dbbox) then ! check bounding-box
          if(parse_bdeb)write(*,'(X,A,I3,X,A,4(X,F5.2))')'Vicinity close: ',&
               & ii,trim(sf%shp(ii)%name),dbbox,dd,shape_rtodeg(dd),shape_rtodeg(sf%shp(ii)%maxrad)
          dd = shape_ddpoly(pos,sf%shp(ii)%npos,sf%shp(ii)%pos,sf%shp(ii)%actual)
          if(parse_bdeb)write(*,'(X,A,I3,X,A,3(X,F5.2))')'                ',&
               & ii,trim(sf%shp(ii)%name),dbbox,dd,shape_rtodeg(dd)
          if (dd.ge.0.0D0.and.dd.lt.dbbox) then
             dbbox=dd
             ibbox=sf%shp(ii)%index
          end if
       end if
    end do
    if(parse_bdeb) then
       if (ibbox.ne.0) write(*,*)'VICINITY finally:',ibbox,trim(sf%shp(ibbox)%name)
    end if
    vicinity=ibbox
    sf%vicinity=vicinity
    sf%vicinitylat=lat
    sf%vicinitylon=lon
    return
  end function vicinity
  !
  character*25 function getname25(val)
    implicit none
    real :: val
    integer :: jj
    do jj=lbound(constval,1),ubound(constval,1)
       if (abs(constval(jj)-val).lt.1.0D-5) then
          getname25=const(jj)
          return
       end if
    end do
    getname25="undefined"
    return
  end function getname25
  !
  !
  !###############################################################################
  ! RERUN ROUTINES
  !###############################################################################
  !
  !
  subroutine parse_setvariable(var,crc250,irc)
    implicit none
    character*(*) :: var
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    integer :: lenv
    character*22 :: myname="parse_setvariable"
    !if(parse_bdeb)write(*,*)myname,' Entering.',irc
    if (allocated(rerun_var80)) deallocate (rerun_var80)
    if (allocated(rerun_lenv)) deallocate (rerun_lenv)
    if (allocated(rerun_value)) deallocate (rerun_value)
    lenv=len_trim(var)
    if (lenv.eq.0) then
       rerun_nvar=0
    else
       rerun_nvar=1
       allocate(rerun_var80(rerun_nvar),&
            & rerun_lenv(rerun_nvar),&
            & rerun_value(rerun_nvar),&
            & stat=irc)
       if (irc.ne.0) then
          call parse_errorappend(crc250,myname)
          call parse_errorappend(crc250,"Unable to allocate &
               & 'rerun_var80'.")
          call parse_errorappend(crc250,"\n")
          return
       end if
       rerun_var80(1)=trim(var)
       call chop0(rerun_var80(1),80)
       rerun_lenv(1)=length(rerun_var80(1),80,10)
       if(parse_bdeb)write(*,*)myname,"Variable:'"//&
            & rerun_var80(1)(1:rerun_lenv(1))//"'",irc
    end if
    return
  end subroutine parse_setvariable
  !
  subroutine parse_setvalue(val,crc250,irc)
    implicit none
    integer :: val
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    character*22 :: myname="parse_setvariable"
    !if(parse_bdeb)write(*,*)myname,' Entering.',irc
    if (rerun_nvar.eq.1) then
       rerun_value(1)=real(val)
       if(parse_bdeb)write(*,*)myname,"Value:'",rerun_value(1),"'",irc
       if (allocated(constval)) then
          constval(rerun_start+1)=rerun_value(1)
       end if
    end if
  end subroutine parse_setvalue
  !
  subroutine parse_setoffset(off,crc250,irc)
    implicit none
    character*(*) :: off
    character*250 :: crc250
    integer :: irc
    integer, external :: length
    integer :: jj
    character*22 :: myname="parse_setvariable"
    !if(parse_bdeb)write(*,*)myname,' Entering.',irc
    rerun_off250=trim(off)
    call chop0(rerun_off250,250)
    rerun_leno=length(rerun_off250,250,10)
    if(parse_bdeb)write(*,*)myname,"Offset:'"//rerun_off250(1:rerun_leno)//"'",irc
    if (associated(rerun_offset)) then
       call parse_close (rerun_offset,crc250,irc)
       if (irc.ne.0) then
          call parse_errorappend(crc250,"parse_close")
          return
       end if
    end if
    if (rerun_leno.gt.0) then
       call parse_open(rerun_offset,crc250,irc)
       if (irc.ne.0) then
          call parse_errorappend(crc250,myname)
          call parse_errorappend(crc250," Error return from parse_open.")
          call parse_errorappend(crc250,"\n")
          return
       end if
       call parse_parsef(rerun_offset,&
            & rerun_off250(1:rerun_leno),rerun_var80,crc250,irc)
       if (irc.ne.0) then
          if(parse_bdeb)then
             do jj=1,size(rerun_var80)
                write(*,'(A,A,I0,A)')myname,"     var(",jj,") = '"//&
                     & trim(rerun_var80(jj))//"'"
             end do
          end if
          call parse_errorappend(crc250,myname)
          call parse_errorappend(crc250," Error return from parsef.")
          call parse_errorappendi(crc250,irc)
          call parse_errorappend(crc250,"\n")
          return
       end if
    end if
    return
  end subroutine parse_setoffset
  !
  real function parse_gettimeoffset(crc250,irc)
    character*250 :: crc250
    integer :: irc
    character*22 :: myname="parse_gettimeoffset"
    if (associated(rerun_offset)) then
       parse_gettimeoffset=parse_evalf(rerun_offset,rerun_value,crc250,irc)
       if (irc.ne.0) then
          call parse_errorappend(crc250,myname)
          call parse_errorappend(crc250," Error return from evalf.")
          call parse_errorappendi(crc250,irc)
          call parse_errorappend(crc250,"\n")
          return
       end if
    else
       parse_gettimeoffset=0.0D0
    end if
    return
  end function parse_gettimeoffset
  !
end module parse

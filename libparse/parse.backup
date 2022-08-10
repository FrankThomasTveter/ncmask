module parse
  IMPLICIT NONE
  !
  ! Global constants
  !
  logical     :: parse_bdeb=.false.
  real        :: secperday = 86400.0D0
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
  INTEGER, PARAMETER :: rn = KIND(0.0d0)          ! Precision of real numbers
  INTEGER, PARAMETER :: is = 4                    ! Data type of bytecode
  !------- -------- --------- --------- --------- --------- --------- --------- -------
  PUBLIC                     :: parse_open,    & ! Open parse session
       parse_close,   & ! Close parse session
       parse_parsef,   & ! Parse single function string
       parse_evalf,    & ! Evaluate single function
       parse_evals,    & ! Evaluate single function
       parse_used,     & ! which targets are used?
       parse_EvalErrMsg  ! Error message (Use only when EvalErrType>0)
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
       cSinh      = 39,         &
       cCosh      = 40,         &
       cTanh      = 41,         &
       cSin       = 42,         &
       cCos       = 43,         &
       cTan       = 44,         &
       cAsin      = 45,         &
       cAcos      = 46,         &
       cAtan2     = 47,         &
       cAtan      = 48,         &
       VarBegin   = 49,         &
       VarEnd     = VarBegin+cAtan-cAbs
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
  integer, parameter :: nconst = 3
  CHARACTER (LEN=2), allocatable :: Const(:)
  real(rn), allocatable  :: constval(:)
  TYPE,PUBLIC ::  parse_session
     INTEGER(is), DIMENSION(:), POINTER  :: ByteCode => null()
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
     character*100 :: funcStr100=""
     integer :: lenf =0
  END TYPE parse_session
  type parse_pointer
     type(parse_session), pointer  :: ptr => null()
  end type parse_pointer
  !
CONTAINS
  !	
  SUBROUTINE parse_open (css,crc250,irc)
    IMPLICIT NONE
    type(parse_session), pointer :: css
    character*250 :: crc250
    integer :: irc
    character*22 :: myname ="parse_open"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if(parse_bdeb)write(*,*)myname,"Opening"
    if (.not.allocated(const)) then
       allocate(const(3))
       const(1)='pi'
       const(2)='e'
       const(3)='na'
    end if
    if (.not.allocated(constval)) then
       allocate(constval(3))
       constval(1)=3.14159265359
       constval(2)=2.71828182846
       constval(3)=1.7D38
    end if
    ! css must be nullified if not declared...
    if (.not.associated(css)) ALLOCATE (css)
    if (associated(css%ByteCode)) deallocate(css%ByteCode)
    if (associated(css%Immed)) deallocate(css%Immed)
    if (associated(css%Stack)) deallocate(css%Stack)
    if (associated(css%Stacka)) deallocate(css%Stacka)
    if (associated(css%Wrka)) deallocate(css%Wrka)
    if (associated(css%ArgsByte)) deallocate(css%ArgsByte)
    NULLIFY (css%ByteCode,css%Immed,css%Stack,css%Stacka,css%Wrka,css%ArgsByte)
    !css%parse_laa=ichar('a')
    !css%parse_lzz=ichar('z')
    !css%parse_uaa=ichar('A')
    !css%parse_uzz=ichar('Z')
    !css%parse_und=ichar('_')
    !if(parse_bdeb)write(*,*)myname,"Done"
    return
  END SUBROUTINE parse_open
  !
  SUBROUTINE parse_close (css,crc250,irc)
    IMPLICIT NONE
    type(parse_session), pointer :: css
    character*250 :: crc250
    integer :: irc
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if (associated(css)) then
       IF (ASSOCIATED(css%ByteCode)) DEALLOCATE ( css%ByteCode,stat=irc)
       IF (ASSOCIATED(css%Immed))    DEALLOCATE ( css%Immed,   stat=irc)
       IF (ASSOCIATED(css%Stack))    DEALLOCATE ( css%Stack,   stat=irc)
       IF (ASSOCIATED(css%ArgsByte)) DEALLOCATE ( css%ArgsByte,stat=irc)
       IF (ASSOCIATED(css%Stacka))   DEALLOCATE ( css%Stacka,  stat=irc)
       IF (ASSOCIATED(css%Wrka))     DEALLOCATE ( css%Wrka,    stat=irc)
       deallocate(css,stat=irc)
       nullify(css)
    end if
    return
  END SUBROUTINE parse_close
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
    real :: rval
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
    DO ii=1,SIZE(Var)
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
  ! real function parse_val(FuncStr,crc250,irc)
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
    integer,dimension(8) :: values    
    integer :: ii, imax,nargs
    character*22 :: myname ="parse_evalf"
    character*250 :: str250
    real  :: eps
    integer :: lens
    integer, external :: length
    logical :: above,below,found
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    imax=size(val)
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Division by zero.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Negative argument to LOG10.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Negative argument to LOG.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Negative argument to SQRT.')
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
             call date_and_time(VALUES=values)
             css%Stack(SP)=css%Stack(SP)+parse_f1970(real(values(1)),real(values(2)),&
                  &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Unexpected number of arguments to s1970.')
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
             call date_and_time(VALUES=values)
             css%Stack(SP)=css%Stack(SP)+parse_fjulian(real(values(1)),real(values(2)),&
                  &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Unexpected number of arguments to d2000.')
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
          call date_and_time(VALUES=values)
          css%Stack(SP)=css%Stack(SP)*secperday+parse_f1970(real(values(1)),real(values(2)),&
               &real(values(3)),0.0D0,0.0D0,0.0D0)
          do ii=1,nargs-1
             css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday             
          end do
       CASE  (cnow)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          call date_and_time(VALUES=values)
          css%Stack(SP)=css%Stack(SP)*secperday+parse_f1970(real(values(1)),real(values(2)),&
               &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
          do ii=1,nargs-1
             css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday
          end do
       CASE  (cround)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.gt.1) then
             eps=max(1.0D-5,abs(css%Stack(SP+1)))
             css%Stack(SP)=nint(css%Stack(SP)/eps)*eps
          else
             css%Stack(SP)=nint(css%Stack(SP))
          end if
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Invalid argument to ASIN.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Invalid argument to ACOS.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Unexpected number of arguments to atan2.')
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
          if (css%ByteCode(IP) .le. VarEnd) then
             ii=css%ByteCode(IP)-VarBegin+1
             if (parse_bdeb.and.ii.gt.imax)write(*,*) myname,'Invalid VAL-index:',ii,"(max=",size(val),")"
             css%Stack(SP)=Val(ii)
          else
             css%Stack(SP)=ConstVal(css%ByteCode(IP)-VarEnd)
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
    integer,dimension(8) :: values    
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
    real :: eps
    integer :: lens
    integer, external :: length
    logical :: above,below,found
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if(parse_bdeb)write(*,*)myname,"Entering '"//css%funcStr100(1:css%lenf)//"'",&
         & size(val),size(set),&
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Division by zero.')
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
             if (parse_bdeb) write(*,*)"*** Negative argument to LOG10:",css%Stack(SP)
             irc=313
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Negative argument to LOG10.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Negative argument to LOG.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Negative argument to SQRT.')
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
             call date_and_time(VALUES=values)
             css%Stack(SP)=css%Stack(SP)+parse_f1970(real(values(1)),real(values(2)),&
                  &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Unexpected number of arguments to s1970.')
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
             call date_and_time(VALUES=values)
             css%Stack(SP)=css%Stack(SP)+parse_fjulian(real(values(1)),real(values(2)),&
                  &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Unexpected number of arguments to d2000.')
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
          call date_and_time(VALUES=values)
          css%Stack(SP)=css%Stack(SP)*secperday+parse_f1970(real(values(1)),real(values(2)),&
               &real(values(3)),0.0D0,0.0D0,0.0D0)
          do ii=1,nargs-1
             css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday
          end do
       CASE  (cnow)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          call date_and_time(VALUES=values)
          css%Stack(SP)=css%Stack(SP)*secperday+parse_f1970(real(values(1)),real(values(2)),&
               &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
          do ii=1,nargs-1
             css%Stack(SP)=css%Stack(SP)+css%Stack(SP+ii)*secperday
          end do
       CASE  (cround)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          if (nargs.gt.1) then
             eps=max(1.0D-5,abs(css%Stack(SP+1)))
             css%Stack(SP)=nint(css%Stack(SP)/eps)*eps
          else
             css%Stack(SP)=nint(css%Stack(SP))
          end if
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Invalid argument to ASIN.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Invalid argument to ACOS.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Unexpected number of arguments to atan2.')
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
          if (css%ByteCode(IP) .le. VarEnd) then
             css%Stack(SP)=Val(css%ByteCode(IP)-VarBegin+1)
             if (.not.set(css%ByteCode(IP)-VarBegin+1)) ret=.false.
          else
             css%Stack(SP)=ConstVal(css%ByteCode(IP)-VarEnd)
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
    integer,dimension(8)    :: values                        ! time array
    INTEGER                            :: IP,              & ! Instruction pointer
         DP,              & ! Data pointer
         SP,              & ! Stack pointer
         AP,              & ! arguments pointer
         AI                 ! arguments index
    REAL(rn),                PARAMETER :: zero = 0._rn
    integer :: ii, jj, nargs, imax,imin,iclo
    real :: vmax,vmin,vclo,v,buff
    logical :: above, below, found
    character*250 :: str250
    real :: eps
    integer :: lens
    integer, external :: length
    character*12 :: myname ="parse_evala"
    !----- -------- --------- --------- --------- --------- --------- --------- -------
    if(parse_bdeb)write(*,*)myname,"Entering '"//css%funcStr100(1:css%lenf)//"'",&
         & ctrg, cpos, npos, size(val),size(set),size(res),&
         & allocated(val),allocated(set),allocated(res)
    if (css%npos.lt.npos) then
       css%npos=npos
       IF (ASSOCIATED(css%Stacka)) DEALLOCATE ( css%Stacka, stat=irc)
       IF (ASSOCIATED(css%Wrka))   DEALLOCATE ( css%Wrka,   stat=irc)
       ALLOCATE (css%Stacka(css%StackSize,css%npos),css%Wrka(css%npos),STAT = irc)
       IF (irc.ne. 0) THEN
          call parse_errorappend(crc250,myname)
          call parse_errorappend(crc250,'Unable to allocate stacka, stacka, wrka.')
          call parse_errorappend(crc250,"\n")
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
          write(*,'(X,A,X,A,X,I3,X,I3,3X,A,3X,I0,X,I0)')myname,"Looping",&
               & IP,css%ByteCode(IP),parse_code20(css%bytecode(ip),nargs)
       end if
       SELECT CASE (css%ByteCode(IP))
       CASE (cImmed)
          SP=SP+1
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                css%Stacka(SP,JJ)=css%Immed(DP)
             END IF
          END DO
          DP=DP+1
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
                   call parse_errorappend(crc250,myname)
                   call parse_errorappend(crc250,'Division by zero.')
                   call parse_errorappend(crc250,"\n")
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
                   call parse_errorappend(crc250,myname)
                   call parse_errorappend(crc250,'Negative argument to LOG10.')
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
                   call parse_errorappend(crc250,myname)
                   call parse_errorappend(crc250,'Negative argument to LOG.')
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
                   call parse_errorappend(crc250,myname)
                   call parse_errorappend(crc250,'Negative argument to SQRT.')
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
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                IF (NARGS.ge.1) THEN ! 
                   if (rand()*100 .gt. css%Stacka(SP,JJ)) then
                      css%Stacka(SP,JJ)=1.0D0
                   else
                      css%Stacka(SP,JJ)=0.0D0
                   end if
                end if
             END IF
          END DO
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
             call date_and_time(VALUES=values)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=css%Stacka(SP,JJ)+parse_f1970(real(values(1)),real(values(2)),&
                        &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Invalid number of arguments to s1970.')
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
             call date_and_time(VALUES=values)
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=css%Stacka(SP,JJ)+parse_fjulian(real(values(1)),real(values(2)),&
                        &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Invalid number of arguments to d2000.')
             call parse_errorappendi(crc250,nargs)
             call parse_errorappend(crc250,"\n")
             EvalErrType=5
             RETURN
          END IF
       CASE  (cmidnight)
          AI=AI+1
          NARGS=css%ArgsByte(AI)
          SP=SP-NARGS+1
          call date_and_time(VALUES=values)
          buff=parse_f1970(real(values(1)),real(values(2)),&
               &real(values(3)),0.0D0,0.0D0,0.0D0)
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
          call date_and_time(VALUES=values)
          buff=parse_f1970(real(values(1)),real(values(2)),&
               &real(values(3)),real(values(5)),real(values(6)),real(values(7)))
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
          DO JJ=1,NPOS
             IF(SET(JJ))THEN
                if (nargs.gt.1) then
                   eps=max(1.0D-5,abs(css%Stacka(SP+1,JJ)))
                   css%Stacka(SP,JJ)=nint(css%Stacka(SP,JJ)/eps)*eps
                else
                   css%Stacka(SP,JJ)=nint(css%Stacka(SP,JJ))
                end if
             end if
          end do
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
                   call parse_errorappend(crc250,myname)
                   call parse_errorappend(crc250,'Invalid arguments to asin.')
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
                   call parse_errorappend(crc250,myname)
                   call parse_errorappend(crc250,'Invalid arguments to acos.')
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
             call parse_errorappend(crc250,myname)
             call parse_errorappend(crc250,'Unexpected number of arguments to atan2.')
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
          if (css%ByteCode(IP) .le. VarEnd) then
             ii=css%ByteCode(IP)-VarBegin+1
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=Val(ii,JJ)
                END IF
             END DO
          else
             DO JJ=1,NPOS
                IF(SET(JJ))THEN
                   css%Stacka(SP,JJ)=ConstVal(css%ByteCode(IP)-VarEnd)
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
    DO IP=1,css%ByteCodeSize
       if (css%ByteCode(IP).ge.VarBegin.and.css%ByteCode(IP).le.VarEnd)then
          set(css%ByteCode(IP)-VarBegin+1)=.true.
       end if
    END DO
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
    call parse_errorappend(crc250,myname)
    call parse_errorappend(crc250,'*** Error in syntax of function string: ')
    call parse_errorappend(crc250,' '//FuncStr)
    call parse_errorappend(crc250,"\nPos=")
    call parse_errorappendi(crc250,j)
    call parse_errorappend(crc250,' '//Msg)
    call parse_errorappend(crc250,"\n")
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
    if(parse_bdeb)write(*,*)myname,"Entering: '"//trim(str)//"'",ibegin,size(var)
    lstr = LEN_TRIM(str)
    IF (lstr > 0) THEN
       DO ib=1,lstr                                          ! Search for first character in str
          IF (str(ib:ib) /= ' ') EXIT                        ! When lstr>0 at least 1 char in str
       END DO
       DO in=ib,lstr                                         ! Search for name terminators
          IF (SCAN(str(in:in),'+-*/^) ,') > 0) EXIT
       END DO
       DO j=1,SIZE(Var)
          IF (str(ib:in-1) .eq. trim(Var(j))) THEN                     
             n = j                                           ! Variable name found
             IF (PARSE_BDEB) THEN
                write(*,*) myname,"** MATCH: '"//str(ib:in-1)//"' == '"//trim(Var(j))//"'"
             end if
             EXIT
             !          ELSE IF (PARSE_BDEB) THEN
             !             write(*,*) myname,"no match: '"//str(ib:in-1)//"' != '"//trim(Var(j))//"'",&
             !                  & in-ib,len_trim(var(j))
          END IF
       END DO
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
         css%Stack(css%StackSize),       &
         css%ArgsByte(css%ArgsSize+1),    &
         STAT = istat                            )
    IF (istat /= 0) THEN
       call parse_errorappend(crc250,myname)
       call parse_errorappend(crc250,'*** Parser error: Memmory allocation for byte code failed')
       call parse_errorappend(crc250,"\n")
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
             n = VarEnd+n
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
    IF (F(b:b) == '+') THEN                              ! Case 1: F(b:e) = '+...'
       if (parse_bdeb) WRITE(*,*)myname,'1. F(b:e) = "+..."'
       CALL parse_CompileSubstr (css, F, b+1, e, Var,comma)
       RETURN
    ELSEIF (F(b:b) == '-') THEN
       if (parse_bdeb) WRITE(*,*)myname,'3. Found Minus'
       CALL parse_CompileSubstr (css, F, b+1, e, Var,comma)
       CALL parse_AddCompiledByte (css, cNeg)
       RETURN
    ELSEIF (parse_CompletelyEnclosed (F, b, e)) THEN               ! Case 2: F(b:e) = '(...)'
       if (parse_bdeb) WRITE(*,*)myname,'2. F(b:e) = "(...)"',F(b:e)
       CALL parse_CompileSubstr (css, F, b+1, e-1, Var,.true.)
       RETURN
    ELSEIF (SCAN(F(b:b),calpha) > 0) THEN        
       if (parse_bdeb) WRITE(*,*)myname,'3. Found Alphanumeric: ',F(b:b)
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
    REAL SECONDS
    REAL DJ
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
  end subroutine parse_errorappendi
  subroutine parse_errorappendr(crc250,rnum)
    implicit none
    character*250 :: crc250
    real :: rnum
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
end module parse

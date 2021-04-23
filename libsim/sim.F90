! simplify polygons
module sim
  implicit none
  public
  
contains
  REAL(kind=8) FUNCTION length2(x)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(2) :: x 
    length2 = dsqrt(DOT_PRODUCT(x,x))
  END FUNCTION length2

  ! Calcule la distance d'un point P a un segment (V1,V2)
  ! Input     vP  : coordonnees du point P
  !           v1  : coordonnees du debut du segment V1
  !           v2  : coordonnees de la fin du segment V2
  function perpendicularDistance(vP, v1, v2)
    implicit none
    REAL(kind=8)              :: perpendicularDistance
    REAL(kind=8),DIMENSION(2) :: vP,v1,v2,LI,LP
    LI = v2 - v1
    LP = vP - v1
    perpendicularDistance = abs( LI(1) * LP(2) - LP(1) * LI(2)) / length2(LI)
  end function perpendicularDistance

  ! simplification of a polygon based on Ramer–Douglas–Peucker algorithm
  ! Input    v(2,n)   : coordinates of the vertices
  !          i1       : indice of first vertex
  !          i2       : indice of last vertex
  !          eps      : distance of simplification
  ! Output   valid(n) : boolean to know if this vertex belongs or not to the simplified polygon
  recursive subroutine DouglasPeuckerRecursive(v, i1, i2, eps, valid)
    implicit none
    INTEGER,INTENT(IN)                   :: i1,i2
    REAL(kind=8),INTENT(IN)              :: eps
    REAL(kind=8),DIMENSION(:,:),POINTER  :: v
    LOGICAL,DIMENSION(:),POINTER         :: valid
    INTEGER                              :: ip,imax
    REAL(kind=8)                         :: dist,dmax
    dmax = 0.D0
    imax = 1
    ! regarde les points intermediaires
    ! et calcule la distance perpenticulaire maximale
    DO ip = i1+1,i2-1
      dist = perpendicularDistance(v(:,ip), v(:,i1), v(:,i2))
      IF ( dist > dmax ) THEN
        imax = ip
        dmax = dist
      ENDIF
    ENDDO
    ! si la distance perpenticulaire maximale est plus grande que eps, 
    ! simplifie les 2 branches [i1,imax] et [imax,i2]
    IF ( dmax > eps ) THEN
      call DouglasPeuckerRecursive(v, i1, imax, eps, valid)
      call DouglasPeuckerRecursive(v, imax, i2, eps, valid)
    ! sinon on vire tous les points entre i1 et i2   
    ELSE
      valid(i1+1:i2-1) = .FALSE.
    ENDIF
  end subroutine DouglasPeuckerRecursive

  real function log2(x)
    implicit none
    real, intent(in) :: x
    log2 = log(x) / log(2.)
  end function log2

  ! simplification of a polygon based on Ramer–Douglas–Peucker algorithm
  ! Input    v(2,n)   : coordinates of the vertices
  !          i1       : indice of first vertex
  !          i2       : indice of last vertex
  !          eps      : distance of simplification
  ! Output   valid(n) : boolean to know if this vertex belongs or not to the simplified polygon
  recursive subroutine DouglasPeuckerIteratif(v, eps, valid)
    implicit none
    REAL(kind=8),INTENT(IN)              :: eps
    REAL(kind=8),DIMENSION(:,:),POINTER  :: v
    LOGICAL,DIMENSION(:),POINTER         :: valid
    INTEGER                              :: i1,i2
    INTEGER                              :: ip,imax
    REAL(kind=8)                         :: dist,dmax
    integer, dimension(:,:), pointer     :: loop_index
    integer                              :: offset_loop_index
    integer                              :: next_loop_index
    integer                              :: nbp
    integer                              :: nb_sub_step
    nbp = size(valid)
    nb_sub_step = 2 ** ( ceiling( log2( real(nbp) - 1 ) ) + 1 )
    allocate( loop_index( 2, nb_sub_step ) )
    loop_index = -1
    loop_index( : , 1 ) = (/ 1, nbp /)
    ! print *, loop_index
    offset_loop_index = 1
    next_loop_index   = 1
    i1 = loop_index( 1, offset_loop_index )
    i2 = loop_index( 2, offset_loop_index )
    do while( (i1 /= -1) .and. (i2 /= -1) .and. (offset_loop_index <= nb_sub_step) )
      dmax = 0.D0
      imax = 1
      ! regarde les points intermediaires
      ! et calcule la distance perpenticulaire maximale
      DO ip = i1+1,i2-1
        dist = perpendicularDistance(v(:,ip), v(:,i1), v(:,i2))
        IF ( dist > dmax ) THEN
          imax = ip
          dmax = dist
        ENDIF
      ENDDO
      ! si la distance perpenticulaire maximale est plus grande que eps, 
      ! simplifie les 2 branches [i1,imax] et [imax,i2]
      IF ( dmax > eps ) THEN
        ! call DouglasPeuckerRecursive(v, i1, imax, eps, valid)
        next_loop_index = next_loop_index + 1
        loop_index( 1, next_loop_index ) = i1
        loop_index( 2, next_loop_index ) = imax
        ! call DouglasPeuckerRecursive(v, imax, i2, eps, valid)
        next_loop_index = next_loop_index + 1
        loop_index( 1, next_loop_index ) = imax
        loop_index( 2, next_loop_index ) = i2
      ! sinon on vire tous les points entre i1 et i2   
      ELSE
        valid(i1+1:i2-1) = .FALSE.
      ENDIF
      offset_loop_index = offset_loop_index + 1
      i1 = loop_index( 1, offset_loop_index )
      i2 = loop_index( 2, offset_loop_index )
    end do  ! END - do while
    deallocate(loop_index)
  end subroutine DouglasPeuckerIteratif
  !
  real function sim_degtor(x)
    implicit none
    real(kind=8),INTENT(IN)  :: x
    real pi
    parameter (pi=3.14159265359)
    sim_degtor=x*pi/180.
  end function sim_degtor

  real function sim_rtodeg(x)
    implicit none
    real(kind=8),INTENT(IN)  :: x
    real pi
    parameter (pi=3.14159265359)
    sim_rtodeg=x*180./pi
  end function sim_rtodeg
!
  real function sim_sindeg(x)
    implicit none
    real(kind=8),INTENT(IN)  :: x
    sim_sindeg=sin(sim_degtor(x))
  end function sim_sindeg

  real function sim_cosdeg(x)
    implicit none
    real(kind=8),INTENT(IN)  :: x
    sim_cosdeg=cos(sim_degtor(x))
  end function sim_cosdeg

  integer function getMeetPos(np,points,pos)
    implicit none
    integer np
    real(kind=8) :: points(2,np) ! latitude (deg)
    integer :: pos
    integer :: ii
    ii=pos+1
    do while (ii.gt.0.and.ii.le.np)
       if (points(1,pos).eq.points(1,ii).and.points(2,pos).eq.points(2,ii)) then
          getMeetPos=ii
          return
       end if
       ii=ii+1
    end do
    getMeetPos=-1
    return
  end function getMeetPos
  ! Does not work over 180-meridian
  subroutine simplify(np,latlon,eps)
    IMPLICIT NONE
    integer :: np                          ! number of points
    real(kind=8), allocatable :: latlon(:) ! latitude (deg)
    real(kind=8),INTENT(IN)  :: eps                      ! tolerance in km
    !
    integer :: ii,jj,nnp
    real :: pst,lat,lon
    INTEGER                                 :: nbp
    REAL(kind=8),DIMENSION(:,:),POINTER     :: points
    LOGICAL,DIMENSION(:),POINTER            :: valid
    real :: re=6371.0D0 ! earth radius in km
    logical :: meet
    integer :: pos,npos,fcnt,lcnt
    !
    ! For the ramer-douglas-peucker algorithm, the last point must be different
    ! of the first point to avoid a division by 0 in 'perpendicularDistance()'
    ! nbp = 10
    ! allocate(points(2,nbp))
    write(*,'(X,A,I0," (eps=",F0.1,"km)")')'SIMPLIFY Polygon started with: ',np,eps
    if (np.le.3) return
    allocate(points(2,np))
    allocate(valid(np))
    do ii=1,np
       points(1,ii)=sim_degtor(latlon(ii))*re                      ! latitude
       points(2,ii)=sim_degtor(latlon(np+ii))*re*sim_cosdeg(latlon(ii))  ! longitude
    end do
    valid = .TRUE.
    fcnt=0
    lcnt=0
    pos=1
    npos=getMeetPos(np,points,pos)
    meet=.false.
    do while (pos.gt.0)
       !write(*,*)'Simplify looping:',pos,npos
       fcnt=fcnt+1
       if (npos.gt.pos) then ! meet
          nbp=npos-1
       else
          nbp=np
       end if
       call DouglasPeuckerRecursive(points, pos, nbp, eps, valid)
       !call DouglasPeuckerIteratif(points, eps, valid)
       if (npos.eq.np) meet=.true.
       pos=npos
       npos=getMeetPos(np,points,pos)
    end do
    if (meet) then
       valid(np)=.true.
    end if
    nnp=0
    do ii=1,np
       if (valid(ii)) nnp=nnp+1
    end do
    pst=100.0D0*real(nnp)/real(max(1,np))
    write(*,'(X,A,I0," (",F0.1,"%)")')'SIMPLIFY Polygon ended with: ',nnp,pst
    ! compress data
    jj=0
    do ii=1,np
       if (valid(ii)) then
          lat=latlon(ii)
          jj=jj+1
          latlon(jj)=lat    ! latitude
       end if
    end do
    jj=0
    do ii=1,np
       if (valid(ii)) then
          lon=latlon(ii+np)
          jj=jj+1
          latlon(jj+nnp)=lon ! longitude
       end if
    end do
    ! auxiliary data...
    do ii=1,size(latlon)-2*np
       latlon(ii+2*nnp)=latlon(ii+2*np)
    end do
    np=nnp
    deallocate( valid )
    deallocate( points )
    return
  end subroutine simplify
  ! Does not work over 180-meridian
  subroutine simplifys(mpar,latlon,npar,eps)
    IMPLICIT NONE
    integer :: np                          ! number of points
    real(kind=8), allocatable :: latlon(:) ! latitude (deg)
    integer, allocatable :: npar(:)
    real(kind=8),INTENT(IN)  :: eps                      ! tolerance in km
    !
    integer :: ii,jj,ss,tt,nnp,mpar,dnp
    real :: pst,lat,lon,xlat,xlon
    INTEGER                                 :: nbp
    REAL(kind=8),DIMENSION(:,:),POINTER     :: points
    LOGICAL,DIMENSION(:),POINTER            :: valid
    real :: re=6371.0D0 ! earth radius in km
    logical :: meet
    integer :: pos,npos,fcnt,lcnt,istart,istop
    !
    ! For the ramer-douglas-peucker algorithm, the last point must be different
    ! of the first point to avoid a division by 0 in 'perpendicularDistance()'
    ! nbp = 10
    ! allocate(points(2,nbp))
    do ss=1,npar(1) ! loop over segments
       istart=npar(ss*2)
       istop=npar(ss*2+1)
       ! store last point
       xlat=latlon(istop)
       xlon=latlon(mpar+istop)
       np=istop-istart+1
       write(*,'(X,A,I0," (eps=",F0.1,"km)")')'SIMPLIFY Polygon started with: ',np,eps
       if (np.le.3) cycle
       ! create work-array
       allocate(points(2,np))
       allocate(valid(np))
       do ii=1,np
          points(1,ii)=sim_degtor(latlon(istart+ii-1))*re                      ! latitude
          points(2,ii)=sim_degtor(latlon(mpar+istart+ii-1))*re*&
               & sim_cosdeg(latlon(istart+ii-1))  ! longitude
       end do
       ! simplify segment
       valid = .TRUE.
       fcnt=0
       lcnt=0
       pos=1
       npos=getMeetPos(np,points,pos)
       meet=.false.
       do while (pos.gt.0)
          !write(*,*)'Simplify looping:',pos,npos
          fcnt=fcnt+1
          if (npos.gt.pos) then ! meet
             nbp=npos-1
          else
             nbp=np
          end if
          call DouglasPeuckerRecursive(points, pos, nbp, eps, valid)
          !call DouglasPeuckerIteratif(points, eps, valid)
          if (npos.eq.np) meet=.true.
          pos=npos
          npos=getMeetPos(np,points,pos)
       end do
       if (meet) then
          valid(np)=.true.
       end if
       ! get final number of points
       nnp=0
       do ii=1,np
          if (valid(ii)) nnp=nnp+1
       end do
       pst=100.0D0*real(nnp)/real(max(1,np))
       write(*,'(X,A,I0," (",F0.1,"%)")')'SIMPLIFY Polygon ended with: ',nnp,pst
       ! compress data (remove extra points)
       jj=0
       do ii=1,size(latlon)
          if (ii.ge.istart.and.ii.le.istop) then
             if (ii.lt.istart+nnp) then
                jj=jj+1
                latlon(jj)=latlon(ii)
             end if
          else if (ii.ge.mpar+istart.and.ii.le.mpar+istop) then
             if (ii.lt.mpar+istart+nnp) then
                jj=jj+1
                latlon(jj)=latlon(ii)
             end if
          else
             jj=jj+1
             latlon(jj)=latlon(ii)
          end if
       end do
       ! correct indexes
       dnp=np-nnp
       mpar=mpar-dnp
       npar(ss*2+1)=npar(ss*2+1)-dnp
       do tt=ss+1,npar(1)
          npar(tt*2)=npar(tt*2)-dnp
          npar(tt*2+1)=npar(tt*2+1)-dnp
       end do
       ! reset last point
       istart=npar(ss*2)
       istop=npar(ss*2+1)
       if (xlat.ne.latlon(istop) .or. &
            & xlon.ne.latlon(mpar+istop)) then
          latlon(istop)  =xlat
          latlon(mpar+istop)=xlon
       end if
       deallocate( valid )
       deallocate( points )
    end do
    return
  end subroutine simplifys

  subroutine writePolygon(poly350,np,latlon,eps,irc)
    implicit none
    character*350 :: poly350
    integer :: np
    real(kind=8), allocatable :: latlon(:) 
    integer :: irc
    real :: eps
    character*4 :: fmt4
    integer :: ndig
    integer :: lenp, unitw,ilat,ilon
    integer, external :: length, ftunit
    logical :: first
    CHARACTER*12 MYNAME
    DATA MYNAME /'WRITEPOLY'/
    call chop0(poly350,350)
    lenp=length(poly350,350,1)
    if (lenp.eq.0) then
       write(*,*)myname,"Missing file name."
       irc=123
       return
    end if
    ! format statement...
    ndig=ceiling(min(max(-log10(max(1.0D-9,eps*36.0D0/40.0D3)),1.0D0),9.0D0))
    write(fmt4,'("F0.",I0)',iostat=irc)ndig
    ! write(*,*)myname,'Format:',fmt4,ndig,eps,eps*36.0D0/40.0D3,-log10(max(1.0D-9,eps*36.0D0/40.0D3))
    if (irc.ne.0) then
       write(*,*) myname,"Error return from FMT4.",irc
       return
    end if
    ! open file
    unitw=ftunit(irc)
    if (irc.ne.0) then
       write(*,*) myname,"Error return from FTUNIT.",irc
       return
    end if
    write(*,*)myname,"Writing to file: "//poly350(1:lenp)
    OPEN ( UNIT=UNITW, STATUS="UNKNOWN", FORM="FORMATTED",&
         &        ACCESS="SEQUENTIAL",&
         &        IOSTAT=IRC, FILE=poly350(1:lenp) )
    IF (IRC.NE.0) THEN
       WRITE(*,*) MYNAME,'UNABLE TO OPEN:'//poly350(1:LENP)
       RETURN
    END IF
    write(unit=unitw,fmt='("[")',advance="no",iostat=irc)
    first=.true.
    do ilat=1,np
       ilon=ilat+np
       if (.not. first) write(unit=unitw,fmt='(",")',advance="no",iostat=irc)
       first=.false.
       write(unit=unitw,fmt='("[")',advance="no",iostat=irc)
       call writeNumber(unitw,latlon(ilat),fmt4,irc)
       write(unit=unitw,fmt='(",")',advance="no",iostat=irc)
       call writeNumber(unitw,latlon(ilon),fmt4,irc)
       write(unit=unitw,fmt='("]")',advance="no",iostat=irc)
    end do
    write(unit=unitw,fmt='("]")',iostat=irc)
    close(unit=unitw,iostat=irc)
    return
  end subroutine writePolygon
  subroutine writeNumber(unitw,x,fmt4,irc)
    integer unitw,irc
    real x,y
    character*4 :: fmt4
    y=abs(x)
    if (y<1.0D0) then
       if (x<0) then
          write(unit=unitw,fmt='("-0",'//fmt4//')',advance="no",iostat=irc)y
       else
          write(unit=unitw,fmt='("0",'//fmt4//')',advance="no",iostat=irc)y
       end if
    else
       if (x<0) then
          write(unit=unitw,fmt='("-",'//fmt4//')',advance="no",iostat=irc)y
       else
          write(unit=unitw,fmt='('//fmt4//')',advance="no",iostat=irc)y
       end if
    end if
    return
  end subroutine writeNumber
end module sim

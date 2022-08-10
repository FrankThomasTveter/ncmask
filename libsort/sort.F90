module sort
  IMPLICIT NONE
  !
  ! Global constants
  !
  logical     :: sort_bdeb=.false.
  !
  CONTAINS
  !
  !###############################################################################
  ! SORTING ROUTINES
  !###############################################################################
  !
  subroutine sort_heapsearch1r(maxnn,key,eps,nn,ind,tkey,left,right)
    !
    implicit none
    !
    integer :: maxnn
    real :: key(maxnn)
    real :: eps ! tolerance
    integer :: nn
    integer :: ind(nn)
    real :: tkey
    integer :: left
    integer :: right
    !
    real :: mid
    integer :: mfl,mcl,kfl,kcl,mch
    logical bdone
    !
    if (nn.eq.0) then
       left=-1                ! first element regardless of value
       return
    end if
    !
    left = 1
    right = nn
    do
       mid=float(left+right)/2.0D0
       mfl=floor(mid)
       mcl=ceiling(mid)
       kfl=sort_cmpr(tkey,key(ind(mfl)),eps)
       kcl=sort_cmpr(tkey,key(ind(mcl)),eps)
       !write(*,'(X,A,X,I3,F9.2,5(X,I3),3(X,F9.2),2(X,I5))')'sort_heapsearch:',left,mid,right,mfl,mcl,kfl,kcl,&
       !& tkey,key(ind(mfl)),key(ind(mcl)),ind(mfl),ind(mcl)
       if (kfl.eq.0) then        ! target is at ceiling => exit
          left=mfl
          right=mfl
          exit
       else if (kcl.eq.0) then   ! target is at floor => exit
          left=mcl
          right=mcl
          exit
       else if (kfl.gt.0) then   ! target is lower than floor
          IF (left.eq.right) then
             right=mfl-1
             exit ! out of bounds -> exit
          else
             right=mfl
          end if
       else if (kcl.lt.0) then   ! target is higher than ceiling
          if (left.eq.right) then
             left=mcl+1
             exit ! out of bounds -> exit
          else
             left=mcl
          end if
       else                      ! target is between floor and ceiling => exit
          left=mfl
          right=mcl
          exit
       end if
    end do
    IF (left > right) return
    !find first match...
    bdone=(left<2)
    do while (.not.bdone)
       mch=sort_cmpr(tkey, key(ind(left-1)),eps)
       if (mch == 0) then ! equal or target is below
          left=left-1
          bdone=(left<2)
       else
          bdone=.true.
       end if
    end do
    !find last match
    bdone=(right>nn-1)
    do while (.not.bdone)
       mch=sort_cmpr(tkey, key(ind(right+1)),eps)
       if (mch == 0) then ! equal or target is above
          right=right+1
          bdone=(right>nn-1)
       else
          bdone=.true.
       end if
    end do
    !
  end subroutine sort_heapsearch1r
  !
  subroutine sort_heapsearch1i(maxnn,key,nn,ind,tkey,left,right)
    !
    implicit none
    !
    integer :: maxnn
    integer, allocatable :: key(:)
    integer :: nn
    integer,allocatable :: ind(:)
    integer :: tkey
    integer :: left
    integer :: right
    !
    real :: mid
    integer :: mfl,mcl,kfl,kcl,mch
    logical bdone
    !
    if (nn.eq.0) then
       left=-1                ! first element regardless of value
       return
    end if
    !
    left = 1
    right = nn
    do
       mid=float(left+right)/2.0D0
       mfl=floor(mid)
       mcl=ceiling(mid)
       kfl=sort_cmpi(tkey,key(ind(mfl)))
       kcl=sort_cmpi(tkey,key(ind(mcl)))
       !write(*,'(X,A,X,I3,F0.2,5(X,I3),3(X,F0.2),2(X,I0))')'sort_heapsearch:',left,mid,right,mfl,mcl,kfl,kcl,&
       !& tkey,key(ind(mfl)),key(ind(mcl)),ind(mfl),ind(mcl)
       if (kfl.eq.0) then        ! target is at ceiling => exit
          left=mfl
          right=mfl
          exit
       else if (kcl.eq.0) then   ! target is at floor => exit
          left=mcl
          right=mcl
          exit
       else if (kfl.gt.0) then   ! target is lower than floor
          IF (left.eq.right) then
             right=mfl-1
             exit ! out of bounds -> exit
          else
             right=mfl
          end if
       else if (kcl.lt.0) then   ! target is higher than ceiling
          if (left.eq.right) then
             left=mcl+1
             exit ! out of bounds -> exit
          else
             left=mcl
          end if
       else                      ! target is between floor and ceiling => exit
          left=mfl
          right=mcl
          exit
       end if
    end do
    IF (left > right) return
    !find first match...
    bdone=(left<2)
    do while (.not.bdone)
       mch=sort_cmpi(tkey, key(ind(left-1)))
       if (mch == 0) then ! equal or target is below
          left=left-1
          bdone=(left<2)
       else
          bdone=.true.
       end if
    end do
    !find last match
    bdone=(right>nn-1)
    do while (.not.bdone)
       mch=sort_cmpi(tkey, key(ind(right+1)))
       if (mch == 0) then ! equal or target is above
          right=right+1
          bdone=(right>nn-1)
       else
          bdone=.true.
       end if
    end do
    !
  end subroutine sort_heapsearch1i
  !
  subroutine sort_heapsort1r(mm,key1,eps,newnn,nn,ind,uniq)
    !
    !! Generate sorted index for key1 
    !
    implicit none

    integer :: mm                ! Number of elements
    real :: key1(mm)             ! key
    real :: eps                  ! key tolerance (when are they equal)
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    logical uniq               ! Ignore duplicate records
    !
    integer :: ii,dmp

    if (nn.eq.0) then
       newnn=0
       return
    end if
    !
    do ii = nn/2, 1, -1
       call sort_pushdownr(ii, nn, mm,key1,eps,newnn,nn,ind)
    end do
    do ii = nn, 2, -1
       call sort_swap(ind(1), ind(ii))
       call sort_pushdownr(1, ii-1, mm,key1,eps,newnn,nn,ind)
    end do
    !
    if (uniq) then
       dmp=0
       newnn=1
       do ii=2,nn
          if (sort_cmpr(key1(ind(ii-1)),key1(ind(ii)),eps) /= 0) then
             ! Keep ind(ii)
             newnn = newnn+1
             ind(newnn) = ind(ii)
          else
             dmp=dmp+1
          end if
       end do
       if(sort_bdeb)write(*,*) "SORT_HEAPSORT dumped elements:",dmp
    else
       newnn=nn
    end if
    !
    !
  end subroutine sort_heapsort1r
  !
  subroutine sort_heapsort1i(mm,key1,newnn,nn,ind,uniq)
    !
    !! Generate sorted index for key1 
    !
    implicit none

    integer :: mm                ! Number of elements
    integer :: key1(mm)             ! key
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    logical uniq               ! Ignore duplicate records
    !
    integer :: ii,dmp

    if (nn.eq.0) then
       newnn=0
       return
    end if
    !
    do ii = nn/2, 1, -1
       call sort_pushdowni(ii, nn, mm,key1,newnn,nn,ind)
    end do
    do ii = nn, 2, -1
       call sort_swap(ind(1), ind(ii))
       call sort_pushdowni(1, ii-1, mm,key1,newnn,nn,ind)
    end do
    !
    if (uniq) then
       dmp=0
       newnn=1
       do ii=2,nn
          if (sort_cmpi(key1(ind(ii-1)),key1(ind(ii))) /= 0) then
             ! Keep ind(ii)
             newnn = newnn+1
             ind(newnn) = ind(ii)
          else
             dmp=dmp+1
          end if
       end do
       if(sort_bdeb)write(*,*) "SORT_HEAPSORT dumped elements:",dmp
    else
       newnn=nn
    end if
    !
    !
  end subroutine sort_heapsort1i
  !
  subroutine sort_pushdownr(first, last,mm,key1,eps,newnn,nn,ind)
    !
    integer :: first
    integer :: last
    integer :: mm                ! Number of elements
    real :: key1(mm)             ! key
    real :: eps                  ! key tolerance (when are they equal)
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    !
    integer :: r
    !
    r = first
    !
    MAINLOOP: do while (r <= last/2)
       if (last == 2*r) then
          if (sort_cmpr(key1(ind(r)),key1(ind( 2*r)),eps) > 0) then
             call sort_swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (sort_cmpr(key1(ind(r)),key1(ind(2*r)),eps) > 0 .and. &
               & sort_cmpr(key1(ind(2*r)),key1(ind(2*r+1)),eps) <= 0) then
             call sort_swap(ind(r), ind(2*r))
             r = 2*r
          else if (sort_cmpr(key1(ind(r)),key1(ind(2*r+1)),eps)>0 .and. &
               & sort_cmpr(key1(ind(2*r+1)),key1(ind(2*r)),eps)<0) then
             call sort_swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine sort_pushdownr
  !
  subroutine sort_pushdowni(first, last,mm,key1,newnn,nn,ind)
    !
    integer :: first
    integer :: last
    integer :: mm                ! Number of elements
    integer :: key1(mm)          ! key
    integer :: newnn             ! new number of keys
    integer :: nn                ! Number of elements
    integer :: ind(nn)           ! Resulting sorted index
    !
    integer :: r
    !
    r = first
    !
    MAINLOOP: do while (r <= last/2)
       if (last == 2*r) then
          if (sort_cmpi(key1(ind(r)),key1(ind( 2*r))) > 0) then
             call sort_swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (sort_cmpi(key1(ind(r)),key1(ind(2*r))) > 0 .and. &
               & sort_cmpi(key1(ind(2*r)),key1(ind(2*r+1))) <= 0) then
             call sort_swap(ind(r), ind(2*r))
             r = 2*r
          else if (sort_cmpi(key1(ind(r)),key1(ind(2*r+1)))>0 .and. &
               & sort_cmpi(key1(ind(2*r+1)),key1(ind(2*r)))<0) then
             call sort_swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine sort_pushdowni
  !
  !
  integer function sort_cmpr(a,b,eps)
    real :: a
    real :: b
    real :: eps
    if (abs(a-b) < eps) then
       sort_cmpr = 0
    else if (a < b) then
       sort_cmpr = 1
    else
       sort_cmpr = -1
    end if
  end function sort_cmpr
  !
  integer function sort_cmpi(a,b)
    integer :: a
    integer :: b
    if (a == b) then
       sort_cmpi = 0
    else if (a < b) then
       sort_cmpi = 1
    else
       sort_cmpi = -1
    end if
  end function sort_cmpi
  !
  !
  subroutine sort_swap(k1, k2)
    !
    implicit none
    !
    integer :: k1
    integer :: k2
    !
    integer :: tmp
    !
    tmp = k1
    k1 = k2
    k2 = tmp
    !
  end subroutine sort_swap
  !
  !
  ! E R R O R    R O U T I N E S
  !
  subroutine sort_errorappend(crc250,string)
    implicit none
    character*250 :: crc250
    character*(*) :: string
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    character*22 :: myname ="sort_errorappend"
    call chop0(crc250,250)
    lenc=length(crc250,250,10)
    lenb=len(trim(string))
    buff250=string(1:lenb)
    if (lenc.eq.0) then
       crc250=buff250(1:lenb)
    else
       crc250=crc250(1:lenc)//""//buff250(1:min(250-lenc-1,lenb))
    end if
  end subroutine sort_errorappend
  subroutine sort_errorappendi(crc250,inum)
    implicit none
    character*250 :: crc250
    integer :: inum
    character*250 :: buff250
    integer :: lenc, lenb
    integer, external :: length
    character*22 :: myname ="sort_errorappendi"
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
  end subroutine sort_errorappendi
end module sort

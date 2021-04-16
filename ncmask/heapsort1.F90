subroutine heapsort1(mm,key1,newnn,nn,ind,eps,uniq)
  !
  !     ! Generate sorted index for key1 
  !
  implicit none
  
  integer mm                ! Number of elements
  real    key1(mm)          ! First key
  integer newnn
  integer nn                ! Number of elements
  integer ind(nn)           ! Resulting sorted index
  real    eps               ! Delta key1
  logical uniq               ! Ignore duplicate records
  !
  integer ii,dmp
  
  if (nn.eq.0) then
     newnn=0
     return
  end if
  !
  do ii = nn/2, 1, -1
     call pushdown(ii, nn)
  end do
  do ii = nn, 2, -1
     call swap(ind(1), ind(ii))
     call pushdown(1, ii-1)
  end do
  !
  if (uniq) then
     dmp=0
     newnn=1
     do ii=2,nn
        if (cmp(ii-1,ii) /= 0) then
           ! Keep ind(ii)
           newnn = newnn+1
           ind(newnn) = ind(ii)
        else
           dmp=dmp+1
        end if
     end do
     !     write(*,*) "HEAPSORT dumped elements:",dmp
  else
     newnn=nn
  end if
  !
contains
  !
  !
  !
  subroutine pushdown(first, last)
    !
    integer first
    integer last
    !
    integer r
    !
    r = first
    !
    MAINLOOP: do while (r <= last/2)
       if (last == 2*r) then
          if (cmp(r, 2*r) > 0) then
             !  if (key1(ind(r)) > key1(ind(2*r))) then
             call swap(ind(r), ind(2*r))
          end if
          exit MAINLOOP
       else
          if (cmp(r,2*r) > 0 .and. cmp(2*r,2*r+1) <= 0) then
             call swap(ind(r), ind(2*r))
             r = 2*r
          else if (cmp(r,2*r+1)>0 .and. cmp(2*r+1,2*r)<0) then
             call swap(ind(r), ind(2*r+1))
             r = 2*r+1
          else
             exit MAINLOOP
          end if
       end if
    end do MAINLOOP
    !
  end subroutine pushdown
  !
  !
  integer function cmp(a, b)
    
    integer a
    integer b
    !
    if (abs(key1(ind(a))-key1(ind(b))) < eps) then
       cmp = 0
    else if (key1(ind(a)) < key1(ind(b))) then
       cmp = 1
    else
       cmp = -1
    end if
    !
  end function cmp
  !
  !
  subroutine swap(k1, k2)
    !
    implicit none
    !
    integer k1
    integer k2
    !
    integer tmp
    !
    tmp = k1
    k1 = k2
    k2 = tmp
    !
  end subroutine swap
  !
end subroutine heapsort1

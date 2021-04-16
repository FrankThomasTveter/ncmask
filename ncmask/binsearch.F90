subroutine binsearch(maxnn,nn,key,ind,tkey,eps,left,right)
  !
  implicit none
  !
  integer maxnn
  integer nn
  real    key(maxnn)
  integer ind(nn)
  real    tkey
  real    eps
  integer left
  integer right
  !
  integer mid
  logical bdone
  !
  left = 1
  right = nn
  if (nn.le.0) return
  do
     mid = (left+right)/2
     if (cmpr(tkey,key(ind(mid))) < 0) then
        right = mid-1
     else
        left = mid+1
     end if
     if (cmpr(tkey, key(ind(mid))) == 0 .or. left > right) exit
  end do
  if (cmpr(tkey, key(ind(mid))) == 0) then
     left = mid
     right = mid
  else
     left = 1
     right = 0
     return
  end if
  !
  bdone=(left<2)
  do while (.not.bdone)
     if (cmpr(tkey, key(ind(left-1))) == 0) then
        left=left-1
        bdone=(left<2)
     else
        bdone=.true.
     end if
  end do
  !
  bdone=(right>nn-1)
  do while (.not.bdone)
     if (cmpr(tkey, key(ind(right+1))) == 0) then
        right=right+1
        bdone=(right>nn-1)
     else
        bdone=.true.
     end if
  end do
  !
contains
  !
  integer function cmpr(a,b)
    !
    real a
    real b
    !
    if (abs(a-b) < eps) then
       cmpr = 0
    else if (a < b) then
       cmpr = -1
    else
       cmpr = 1
    end if
  end function cmpr
  !
end subroutine binsearch

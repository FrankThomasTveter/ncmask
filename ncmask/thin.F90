      module thin_module

      ! Define a tree node
      type node2d
         real*8 lon, lat             ! Point coordinates
         logical*4 enabled           ! Indicates whether point is deleted or not
         type(node2d), pointer :: l ! Left child node
         type(node2d), pointer :: r ! Right child node
      end type node2d
      
      ! Due to limitations in Fortran 90, we neeed this to make
      ! an array of pointers to tree nodes
      type node2dptr
         type(node2d), pointer :: p
      end type node2dptr
      
      ! Tree main data structure
      type tree2d
         integer*4 N                 ! Number of elements in tree
         type(node2dptr), pointer :: list(:) ! Data points, sorted cost-wise
         type(node2d), pointer :: root ! Pointer to tree root node
      end type tree2d
      
      contains

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

      recursive function node_insert(root, lon, lat, dir, irc) &
     &     result (new)
      
      implicit none
      
      type(node2d), pointer :: root
      real*8 lon
      real*8 lat
      integer*4 dir
      integer*4 irc
      type(node2d), pointer :: new
      
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
         root%enabled = .false.
         nullify(root%l, root%r)
         new => root
         return
      else
         newdir = mod(dir+1, 2)
      ! Recursively insert point in either left or right subtree
         if ((dir == 0 .and. lon < root%lon)  &
     &        .or.(dir == 1 .and. lat < root%lat)) then
            new => node_insert(root%l, lon, lat, newdir, irc)
         else
            new => node_insert(root%r, lon, lat, newdir, irc)
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

      subroutine tree_build(tree, ind, lon, lat, irc)
      
      implicit none
      
      type(tree2d), intent(inout) :: tree ! tree trunk information
      integer, intent(in) :: ind(:)
      real, intent(in) :: lon(:)
      real, intent(in) :: lat(size(lon))
      integer, intent(inout) :: irc
      
      integer*4 iind(size(ind))
      integer*4 tmpind(size(ind))
      integer*4 i, j, median
      type(node2d), pointer :: median_node
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
      median_node => node_insert(tree%root, lon(median), lat(median), 0, &
     &     irc)
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
         tree%list(iind(i))%p => node_insert(tree%root, lon(j), lat(j),&
     &        0, irc)
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
!!!        self - node with center coordinates
!!!        dist - max. radial distance (in degrees)
!!!               away from node pointed to by self
!!!        dir - 0->search in lon direction, 1->lat direction
!!!
!!! Returns: .true. if a node was found, .false. otherwise
!!!

      recursive logical function rangesearch(root, self, dist, dir)&
     &     result (success)
      
      implicit none
      
      type(node2d), pointer :: root
      type(node2d), pointer :: self
      real*8 lon
      real*8 lat
      real*8 dist
      integer*4 dir
      
      integer*4 newdir
      logical*4 d, goleft, goright
      
      if (associated(root)) then
         
      !!! Check current node for match
         if (.not.associated(root, self) .and. root%enabled .and.  &
     &        distance_deg(self%lon, self%lat, root%lon, root%lat) <= &
     &        dist) then
         success = .true.
         return
      end if
      
      !!! Determine which subtree(s) we must search in
      if (dir == 0) then
         d = distance_deg(self%lon, self%lat, root%lon, self%lat) <=&
     &        dist
         goleft = d .or. root%lon >= self%lon
         goright = d .or. root%lon <= self%lon
      else
         d = distance_deg(self%lon, self%lat, self%lon, root%lat) <=&
     &        dist
         goleft = d .or. root%lat >= self%lat
         goright = d .or. root%lat <= self%lat
      end if
      
      !!! Change sorting direction for next level
      newdir = mod(dir+1, 2)
      if (goleft) then
         success = rangesearch(root%l, self, dist, newdir)
         if (success) return
      end if
      if (goright) then
         success = rangesearch(root%r, self, dist, newdir)
      end if
      else
         success = .false.
      end if
      
      end function rangesearch
      
      end module thin_module





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Name: thin
!!!
!!! Description: Given the coordinates (lon(:), lat(:)), flag
!!!              all points with nearest neigbour within
!!!              distance dist as 'deleted'
!!!
!!! Input: n - size of arrays lon, lat, flags
!!!        lon, lat - arrays of points
!!!        enabled - whether a given point is included or not
!!!        cost - cost for each point
!!!        dist - min. distance to nearest neigbour
!!!
!!! Output: enabled - array of flags; .false. to flag a point as
!!!                   deleted, .true. to flag it as included
!!!         irc - error return code
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! File history:
!!!
!!! Date        Name                Comment
!!! -------------------------------------------------------------------
!!!
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine thin(n, lon, lat, enabled, cost, dist, irc)
   
      use thin_module
      implicit none

      integer*4 n                 ! Number of elements in lon/lat
      real*8 lon(n)               ! Longitudes
      real*8 lat(n)               ! Latitudes
      logical*4 enabled(n)        ! .false. if point is deleted
      real*8 cost(n)              ! Cost function.
      real*8 dist                 ! Max distance around each point
      integer*4 irc               ! Error return code
      

      type(tree2d) :: tree
      integer, allocatable :: ind(:)
      integer*4 i, j, newn
      character (len=15), parameter :: myname = 'SCATT_THIN'
      real*8 d
      
      
      ! Consider only datapoints for which enabled == .true.
      newn = count(enabled)
      if (newn <= 1) return     ! Nothing to do if we have 0 or 1 data point
      allocate(ind(newn), stat=irc)
      if (irc /= 0) then
         write(*,*) myname, "Memory allocation error: ", irc
         irc = 934
         return
      end if
      j = 1
      do i = 1, n
         if (enabled(i)) then
            ind(j) = i
            j = j+1
         end if
      end do
      
      ! Build a 2D search tree from the data
      call mergesort(ind, cost)
      call tree_build(tree, ind, lon, lat, irc)
      if (irc /= 0) then
         write(*,*) myname, 'Error building tree:', irc
         return
      end if
      
      ! Scan through all points, giving priority
      ! to the first elements (with lowest cost)
      do i = 1, newn
         if (.not.rangesearch(tree%root, tree%list(i)%p, dist, 0)) then
            tree%list(i)%p%enabled = .true.
         else
            enabled(ind(i)) = .false.
         end if
      end do
      
      !!! Deallocate memory
      call tree_destroy(tree)
      deallocate(ind)
      
      end subroutine thin


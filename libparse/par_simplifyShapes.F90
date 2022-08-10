subroutine par_simplifyShapes(tol20, crc250, irc)
  use parse
  implicit none
  character*20 :: tol20
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "simplifyShapes"
  !write(*,*) myname, 'Entering.',irc
  call parse_simplifyShapes(tol20, crc250,irc)
  if (irc.ne.0) then
     call parse_errorappend(crc250,myname)
     call parse_errorappend(crc250," Error return from parse_simplifyShapes.")
     call parse_errorappendi(crc250,irc)
     call parse_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine par_simplifyShapes

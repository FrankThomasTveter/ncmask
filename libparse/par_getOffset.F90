subroutine par_getoffset(off25,crc250, irc)
  use parse
  implicit none
  character*25 :: off25
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "getoffset"
  real :: offset
  !write(*,*) myname, 'Entering.',irc,filter250,
  offset=parse_gettimeoffset(crc250,irc)
  if (irc.ne.0) then
     call parse_errorappend(crc250,myname)
     call parse_errorappend(crc250," Error return from col_setobscache.")
     call parse_errorappendi(crc250,irc)
     call parse_errorappend(crc250,"\n")
     return
  end if
  write(off25,'(F0.2)')offset
  !write(*,*) myname,' Done.'
  return
end subroutine par_getoffset

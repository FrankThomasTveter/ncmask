subroutine par_setvalue(val,crc250, irc)
  use parse
  implicit none
  integer :: val
  character*250 :: crc250
  integer :: irc
  character*25 :: myname = "setvalue"
  !write(*,*) myname, 'Entering.',irc,filter250,
  call parse_setvalue(val,crc250,irc)
  if (irc.ne.0) then
     call parse_errorappend(crc250,myname)
     call parse_errorappend(crc250," Error return from col_setobscache.")
     call parse_errorappendi(crc250,irc)
     call parse_errorappend(crc250,"\n")
     return
  end if
  !write(*,*) myname,' Done.'
  return
end subroutine par_setvalue

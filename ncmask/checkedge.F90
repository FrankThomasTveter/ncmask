  subroutine checkEdge(lat,lon,nn,lata,lona,delta,inout)
    real :: lat,lon
    integer :: nn
    real :: lata(nn), lona(nn)
    real :: delta   ! in km
    integer :: inout
    integer :: ii
    !real, parameter :: PI= 3.141592653589793D+00 
    !real, parameter :: PIH= PI/2.0D0
    real :: dist
    real, parameter :: fact=360.0D0/40075.0D0
    real sindeg, cosdeg,acosdeg,asindeg
    external sindeg,cosdeg,acosdeg,asindeg
    real :: deltaf
    deltaf=delta*fact ! convert to degrees
    !write(*,*) 'checkEdge: ***',lat,lon,nn
    inout=-1 ! outside...
    do ii=1,nn-1
       dist=getLineDist(lat,lon,lata(ii),lona(ii),lata(ii+1),lona(ii+1)) ! get distance in degrees...
       if (dist < deltaf) then
          inout=0 ! inside...
          !write(*,'(X,A,I0,2(X,A,2(X,F5.2)),A,2(X,F4.2),X,I0)') &
          !     & 'Inside @@@ ',ii," a=",lata(ii),lona(ii),&
          !     & " b=",lata(ii+1),lona(ii+1),&
          !     & " dist=",dist,deltaf,inout
          return
       end if
       !write(*,'(X,A,I0,2(X,A,2(X,F5.2)),A,2(X,F4.2),X,I0)') &
       !     & 'Checkeds ??? ',ii," a=",lata(ii),lona(ii),&
       !     & " b=",lata(ii+1),lona(ii+1),&
       !     & " dist=",dist,deltaf,inout
    end do
    return
  contains
    real function getLineDist(LATC,LONC,LATA,LONA,LATB,LONB)
      ! get distance between C and the A-B line (in degrees)
      IMPLICIT NONE
      SAVE
      REAL   LONA,LATA,LONB,LATB,LATC,LONC
      REAL   dab,dac,dbc,pab,pac,pba,pbc,bac,abc,ha,hb
      ! get distance between points (== getDist)
      dab=getDist(lata,lona,latb,lonb)
      dac=getDist(lata,lona,latc,lonc)
      dbc=getDist(latb,lonb,latc,lonc)
      ! get angles between point arcs
      pab=getAng(lata,lona,latb,lonb,dab)
      pac=getAng(lata,lona,latc,lonc,dac)
      pba=getAng(latb,lonb,lata,lona,dab)
      pbc=getAng(latb,lonb,latc,lonc,dbc)
      bac=pab-pac
      if (bac.gt.180.0D0) bac=bac-360.0D0
      if (bac.lt.-180.0D0) bac=bac+360.0D0
      abc=pba-pbc
      if (abc.gt.180.0D0) abc=abc-360.0D0
      if (abc.lt.-180.0D0) abc=abc+360.0D0

      !write(*,'(10(X,A,X,F7.3))')'Angles bac:',bac,' abc:',abc," pab:",pab,&
      !     & " pac:",pac," pba:",pba," pbc:",abc,&
      !     & " dab:",dab," dac:",dac," dbc:",dbc

      ! check if both angles are -90<<90 (line is closest)
      if (abs(bac) .lt. 90.0D0 .and. abs(abc).lt.90.0D0) then
         ! get normal distance
         ha=getHeight(bac,dac)
         hb=getHeight(abc,dbc)
         getLineDist=abs(ha)
         !write(*,'(10(X,A,F7.2))')'Height bac:',bac,' dac:',dac,' h=',ha,' abc:',abc,' dbc:',dbc,' h=',hb
      else ! end points are closest...
      ! else use great angle from closest point
         getLineDist=min(abs(dac),abs(dbc))
         !write(*,*)'Point dac:',dac,' dbc:',dbc,' dist=',getLineDist
      end if
      return
    END FUNCTION GETLINEDIST
    real function getDist(LATA,LONA,LATB,LONB)
      ! get distance between A-B (in deg)
      IMPLICIT NONE
      SAVE
      REAL   LONA,LATA,LONB,LATB
      REAL   CDIFF
      CHARACTER*8 MYNAME 
      DATA MYNAME /'getDist'/
      CDIFF=SINDEG(LATA)*SINDEG(LATB) &
           & +COSDEG(LATA)*COSDEG(LATB)*COSDEG(LONB-LONA)
      CDIFF=MAX(-1.0D0,MIN(1.0D0,CDIFF)) ! handle truncation errors
      getDist=ACOSDEG(CDIFF)
      RETURN
    END function getDist
    real function getAng(LATA,LONA,LATB,LONB,DAB)
      ! get angle between pole-A-B
      IMPLICIT NONE
      SAVE
      REAL   LONA,LATA,LONB,LATB,DAB
      real sindeg, cosdeg,acosdeg
      external sindeg,cosdeg,acosdeg
      real :: sdab, slab, sang, ang, fang
      slab=SINDEG(LONB-LONA)
      sdab=SINDEG(DAB)
      if (abs(sdab).lt.1.0D-10) then
         sang=0.0D0
      else
         sang=cosdeg(latb)*slab/sdab
      end if
      SANG=MAX(-1.0D0,MIN(1.0D0,SANG)) ! handle truncation errors
      ang=ASINDEG(sang)
      fang=ang
      if (lata.gt.latb) then
         fang=180.0D0-ang
      end if
      if (fang.lt.0.0D0) then
         fang=360.0D0+fang
      end if
      getAng=fang
      !write(*,'(X,A,4(X,A,2(X,F7.2)),X,F7.2)')'getAng:'," a=",lata,lona," b=",latb,lonb,&
      !     & " ang=",ang,getAng," sin=",sang,slab,sdab
    END function getAng
    real function getHeight(ang,dab)
      ! get height
      IMPLICIT NONE
      SAVE
      real :: ang, dab
      real :: shgt
      shgt=sindeg(ang)*sindeg(dab)
      shgt=MAX(-1.0D0,MIN(1.0D0,shgt)) ! handle truncation errors
      getHeight=ASINDEG(shgt)
      return
    end function getHeight
  end subroutine checkEdge

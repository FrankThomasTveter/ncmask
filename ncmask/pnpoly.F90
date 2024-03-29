! SUBROUTINE PNPOLY C
! PURPOSE
! TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
!
! USAGE
! CALL PNPOLY (PX, PY, N, XX, YY, INOUT )
!
! DESCRIPTION OF THE PARAMETERS
!
! PX - X-COORDINATE OF POINT IN QUESTION.
!
! PY - Y-COORDINATE OF POINT IN QUESTION.
!
! N - NUMBER OF VERTICES IN THE POLYGON.
!
! XX - N LONG VECTOR CONTAINING X-COORDINATES OF VERTICES OF POLYGON.
!
! YY - N LONG VECTOR CONTAING Y-COORDINATES OF VERTICES OF POLYGON.
!
! INOUT - THE SIGNAL RETURNED:
!
! -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
!
! 0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
!
! 1 IF THE POINT IS INSIDE OF THE POLYGON.
!
! REMARKS
! THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.
! THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY
! OPTIONALLY BE INCREASED BY 1.
! THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
! OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
! OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
! N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
! INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
! THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM
! WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.
!
!
!     ..................................................................
!
!        SUBROUTINE PNPOLY
!
!        PURPOSE
!           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
!
!        USAGE
!           CALL PNPOLY (PX, PY, N, XX, YY, INOUT )
!
!        DESCRIPTION OF THE PARAMETERS
!           PX      - X-COORDINATE OF POINT IN QUESTION.
!           PY      - Y-COORDINATE OF POINT IN QUESTION.
!           N       - NUMBER OF VERTICES IN THE POLYGON.
!           XX      - N LONG VECTOR CONTAINING X-COORDINATES OF
!                     VERTICES OF POLYGON.
!           YY      - N LONG VECTOR CONTAING Y-COORDINATES OF
!                     VERTICES OF POLYGON.
!           INOUT   - THE SIGNAL RETURNED:
!                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
!                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
!                      1 IF THE POINT IS INSIDE OF THE POLYGON.
!
!        REMARKS
!           THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.
!           THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY
!           OPTIONALLY BE INCREASED BY 1.
!           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
!           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
!           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
!           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
!           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
!           THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM
!           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT
!           CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE
!           POINT IS INSIDE OF THE POLYGON.
!
!     ..................................................................
!
      SUBROUTINE PNPOLY(PX,PY,N,XX,YY,INOUT)
        implicit none
        integer n,inout
        REAL PX,PY,XX(N),YY(N)
        LOGICAL MX,MY,NX,NY
        INTEGER i,j
        real X(n),Y(n),mm
        INOUT=-1 ! point is outside
        if (n.le.1) return
        DO I=1,N
           X(I)=XX(I)-PX
           Y(I)=YY(I)-PY
        end do
        LOOP: DO I=1,N
           J=1+MOD(I,N)
           MX=X(I).GE.0.0
           NX=X(J).GE.0.0
           MY=Y(I).GE.0.0
           NY=Y(J).GE.0.0
           IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) CYCLE LOOP
           IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) then
              mm=(Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))
              IF(mm.lt.0) then
                 cycle loop
              else if (mm.eq.0) then
                 inout=0 ! point is on a vertex
                 return
              else if (mm.gt.0) then
                 INOUT=-INOUT
                 cycle loop
              end if
           end if
           INOUT=-INOUT
           CYCLE LOOP
        end do LOOP
        RETURN
      END SUBROUTINE PNPOLY

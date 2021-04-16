
C ---------------------------------------------------------------------
C   P O L Y G O N   P R E P R O C E S S I N G   R O U T I N E
C ---------------------------------------------------------------------


      SUBROUTINE PPPPAP (XCOP,YCOP,NCOP,NBTS)
C
C This routine may be called to pre-process a polygon that is to be
C used as input to one of the polygon-manipulation routines.  The
C polygon is defined by the points (XCOP(I),YCOP(I)), for I = 1 to
C NCOP.  NBTS is the number of significant bits to be left in the
C fractional parts of the point coordinates; you should probably not
C use a value less than about 10 (?) nor one greater than 24 on a
C machine with 32-bit reals or greater than 48 on a machine with
C 64-bit reals.  For most purposes, NBTS = 18 is probably okay.
C
        DIMENSION XCOP(NCOP),YCOP(NCOP)
C
C Reduce the number of significant bits in each point coordinate to
C NBTS by zeroing the remaining bits.  This is useful in avoiding a
C problem that occurs in the trapezoid-producing routines when there
C are edge segments that are very nearly, but not quite, horizontal.
C
        DO I=1,NCOP
          XCOP(I)=PPZBTS(XCOP(I),NBTS)
          YCOP(I)=PPZBTS(YCOP(I),NBTS)
        END DO
C
C Cull adjacent points points that are identical.  This step is
C probably unnecessary now, as I no longer know of any problem
C caused by adjacent identical points, but it does no harm.
C
        NOUT=1
C
        DO I=2,NCOP
          IF (XCOP(I).NE.XCOP(I-1).OR.YCOP(I).NE.YCOP(I-1)) THEN
            NOUT=NOUT+1
            IF (NOUT.NE.I) THEN
              XCOP(NOUT)=XCOP(I)
              YCOP(NOUT)=YCOP(I)
            END IF
          END IF
        END DO
C
        NCOP=NOUT
C
C Done.
C
        RETURN
C
      END


      FUNCTION PPZBTS (RVAL,NBTS)
C
C The function reference "PPZBTS(RVAL,NBTS)" has the value of the real
C number RVAL with all bits of its fraction except the first NBTS set
C to zero.  The Fortran 77 version is straightforward, but probably a
C bit inefficient; there may be a better way.  If and when Fortran 90
C becomes available, the code can be rewritten in a much more efficient
C way.
C
C If the input value is zero, return a zero.
C
        IF (RVAL.EQ.0.) THEN
          PPZBTS=0.
          RETURN
        END IF
C
C Otherwise, pick off the sign.
C
        RSGN=SIGN(1.,RVAL)
C
C Set up a temporary containing the absolute value of the real, ...
C
        RTMP=ABS(RVAL)
C
C Zero a counter.
C
        IVAL=0
C
C If the temporary is less than 1/2, use successive multiplies by
C two to make it bigger.
C
        IF (RTMP.LT..5) THEN
C
  101     RTMP=RTMP*2.
          IVAL=IVAL-1
          IF (RTMP.LT..5) GO TO 101
C
C If the temporary is greater than or equal to 1, use successive
C divides by two to make it smaller.
C
        ELSE IF (RTMP.GE.1.) THEN
C
  102     RTMP=RTMP/2.
          IVAL=IVAL+1
          IF (RTMP.GE.1.) GO TO 102
C
        END IF
C
C Once the temporary is in a known range, zero out its lower bits, put
C it back in a range commensurate with that of the input value, tack
C the sign back on, and return the result as the value of the function.
C
        PPZBTS=RSGN*(RTMP-MOD(RTMP,2.**(-NBTS)))*2.**IVAL
C
C Done.
C
        RETURN
C
      END


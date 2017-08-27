      SUBROUTINE VTGETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C IVAL is an integer variable in which the desired value is to be
C returned by VTGETI.
C
C
C Check for an uncleared prior error.
C
      IF (ICFELL('VTGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Use VTGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL VTGETR (WHCH,RVAL)
      IF (ICFELL('VTGETI',2).NE.0) RETURN
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END

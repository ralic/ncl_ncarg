      SUBROUTINE VTCVDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C This routine draws "curly vectors".
C
      DIMENSION IAMA(1)
C
      EXTERNAL VTDRPL
C
      DATA IAMA / 0 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('VTCVDR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
      CALL VTCVDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,VTDRPL)
      IF (ICFELL('VTCVDR',2).NE.0) RETURN
C
      RETURN
C
      END

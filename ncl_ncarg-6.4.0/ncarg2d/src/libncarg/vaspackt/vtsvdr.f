      SUBROUTINE VTSVDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C This routine draws simple vectors.
C
      DIMENSION IAMA(1)
C
      EXTERNAL VTDRPL
C
      DATA IAMA / 0 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('VTSVDR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
      CALL VTSVDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,VTDRPL)
      IF (ICFELL('VTSVDR',2).NE.0) RETURN
C
      RETURN
C
      END
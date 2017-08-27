      SUBROUTINE VTDREL (XCFR,YCFR,RADA,RADB,ROTD,DSTP,IDRW)
C
C This routine fills an ellipse.  The arguments are as follows:
C
C   XCFR and YCFR are the coordinates of the center of the ellipse, in
C   the fractional coordinate system.
C
C   RADA is the length of the semimajor axis of the ellipse (i.e., the
C   distance from the center of the ellipse to one of the two points on
C   the ellipse which are furthest from the center).  This is a distance
C   in the fractional coordinate system.
C
C   RADB is the length of the semiminor axis of the ellipse (i.e., the
C   distance from the center of the ellipse to one of the two points on
C   the ellipse which are nearest to the center).  This is a distance in
C   the fractional coordinate system.
C
C   ROTD is a rotation angle, in degrees.  If ROTD is 0, the major axis
C   of the ellipse is horizontal.  If ROTD is 90, the major axis is
C   vertical.
C
C   DSTP is the step size, in degrees, between any two consecutive
C   points used to draw the ellipse.  The actual value used will be
C   limited to the range from .1 degrees (3600 points used to draw
C   the ellipse) to 90 degrees (4 points used to draw the ellipse).
C
C   Set IDRW to 1 to just fill the ellipse, 2 to just draw the boundary
C   of the ellipse, 3 to do both.
C
C Declare work arrays to hold the coordinates.
C
      DIMENSION XCRA(3601),YCRA(3601)
C
C DTOR is pi over 180, used to convert an angle from degrees to radians.
C
      DATA DTOR / .017453292519943 /
C
C Get the rotation angle in radians.
C
      ROTR=DTOR*ROTD
C
C Compute the number of steps to be used to draw the ellipse and the
C actual number of degrees for each step.
C
      NSTP=MAX(4,MIN(3600,INT(360./MAX(.1,MIN(90.,DSTP)))))
      RSTP=360./NSTP
C
C Compute coordinates for the ellipse (just some trigonometry).
C
      DO 101 ISTP=0,NSTP
      ANGL=DTOR*REAL(ISTP)*RSTP
      XTMP=RADA*COS(ANGL)
      YTMP=RADB*SIN(ANGL)
      XCRA(ISTP+1)=CFUX(XCFR+XTMP*COS(ROTR)-YTMP*SIN(ROTR))
      YCRA(ISTP+1)=CFUY(YCFR+XTMP*SIN(ROTR)+YTMP*COS(ROTR))
  101 CONTINUE
C
C Fill it.
C
      IF (IDRW.EQ.1.OR.IDRW.EQ.3) CALL GFA (NSTP+1,XCRA,YCRA)
C
C Draw it.
C
      IF (IDRW.EQ.2.OR.IDRW.EQ.3) CALL GPL (NSTP+1,XCRA,YCRA)
C
C Done.
C
      RETURN
C
      END

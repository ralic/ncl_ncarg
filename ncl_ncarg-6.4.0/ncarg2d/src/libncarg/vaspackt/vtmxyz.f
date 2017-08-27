      SUBROUTINE VTMXYZ (IMAP,XINP,YINP,ZINP,XOTP,YOTP)
C
C Define the constant required to convert an angle from radians to
C degrees.
C
      DATA RTOD / 57.2957795130823 /
C
C If IMAP = 1, treat XINP, YINP, and ZINP as the coordinates of a point
C on the unit sphere.  Compute the latitude and longitude of that point
C and then use EZMAP to find its projection on a map.
C
      IF (IMAP.EQ.1) THEN
C
        RLAT=RTOD*ASIN(ZINP/SQRT(XINP*XINP+YINP*YINP+ZINP*ZINP))
C
        IF (XINP.EQ.0..AND.YINP.EQ.0.) THEN
          RLON=0.
        ELSE
          RLON=RTOD*ATAN2(YINP,XINP)
        END IF
C
        CALL MAPTRA (RLAT,RLON,XOTP,YOTP)
C
C If IMAP = -1, use EZMAP to see if a point on a map is the projection
C of some point on the globe.  (If not, 1.E12s are returned in XOTP and
C YOTP.)
C
      ELSE IF (IMAP.EQ.-1) THEN
C
        CALL MAPTRI (XINP,YINP,XOTP,YOTP)
C
C If IMAP = 2, call TDPACK to project the point (XINP,YINP,ZINP) into
C the projection plane.
C
      ELSE IF (IMAP.EQ.2) THEN
C
        CALL TDPRPT (XINP,YINP,ZINP,XOTP,YOTP)
C
C In all other cases, just do the identity mapping.
C
      ELSE
C
        XOTP=XINP
        YOTP=YINP
C
      END IF
C
      RETURN
C
      END

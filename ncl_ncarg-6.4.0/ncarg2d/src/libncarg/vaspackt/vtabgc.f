      FUNCTION VTABGC (ALAT,ALON,BLAT,BLON,CLAT,CLON)
C
C (VTABGC = VaspackT, Angle Between Great Circles)
C
C This function, given the latitudes and longitudes of points A, B, and
C C on the globe, returns the absolute value of the angle, in degrees,
C between the great circle from A to B and the great circle from A to C.
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
C Get XYZ coordinates for B and C.
C
      BVOX=COS(DTOR*BLAT)*COS(DTOR*BLON)
      BVOY=COS(DTOR*BLAT)*SIN(DTOR*BLON)
      BVOZ=SIN(DTOR*BLAT)
C
      CVOX=COS(DTOR*CLAT)*COS(DTOR*CLON)
      CVOY=COS(DTOR*CLAT)*SIN(DTOR*CLON)
      CVOZ=SIN(DTOR*CLAT)
C
C Rotate about the Z axis so as to put A on the prime meridian.
C
      CALL NGRITD (3,-ALON,BVOX,BVOY,BVOZ)
      CALL NGRITD (3,-ALON,CVOX,CVOY,CVOZ)
C
C Rotate about the Y axis so as to put A on the equator.
C
      CALL NGRITD (2,ALAT,BVOX,BVOY,BVOZ)
      CALL NGRITD (2,ALAT,CVOX,CVOY,CVOZ)
C
C Rotate about the X axis so as to put B on the equator.
C
      IF (BVOZ.NE.0..OR.BVOY.NE.0.) THEN
        ANGL=-RTOD*ATAN2(BVOZ,BVOY)
      ELSE
        ANGL=0.
      END IF
C
      CALL NGRITD (1,ANGL,CVOX,CVOY,CVOZ)
C
C Set the value of the function accordingly.
C
      IF (CVOZ.NE.0..OR.CVOY.NE.0.) THEN
        VTABGC=ABS(RTOD*ATAN2(CVOZ,CVOY))
      ELSE
        VTABGC=0.
      END IF
C
C Done.
C
      RETURN
C
      END

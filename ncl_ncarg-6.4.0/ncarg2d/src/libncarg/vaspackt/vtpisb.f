      SUBROUTINE VTPISB (XCPA,YCPA,XCPB,YCPB,XCPC,YCPC,XCPD,YCPD,
     +                                            XCOP,YCOP,IFLG)
C
C This function checks for overlap of the 2D line segments AB and CD;
C if they overlap, it adds the X and Y coordinates of the point of
C overlap to XCOP and YCOP and bumps the value of the counter IFLG.
C
C Compute a denominator needed below.  (Its value is zero if and only
C if the line segments are parallel.)
C
      DNOM=(XCPB-XCPA)*(YCPD-YCPC)-(XCPD-XCPC)*(YCPB-YCPA)
C
C If the line segments are parallel, they don't intersect.
C
      IF (DNOM.EQ.0.) RETURN
C
C Otherwise, find the values of S and T, in the parametric equations
C for line segments AB and CD, for which intersection occurs.
C
      S=((XCPC-XCPA)*(YCPD-YCPC)-(XCPD-XCPC)*(YCPC-YCPA))/DNOM
      T=((XCPC-XCPA)*(YCPB-YCPA)-(XCPB-XCPA)*(YCPC-YCPA))/DNOM
C
C If both S and T are between 0 and 1, the line segments intersect;
C otherwise, they don't.
C
      IF (S.GE.0..AND.S.LE.1..AND.T.GE.0..AND.T.LE.1.) THEN
        XCOP=XCOP+(XCPA+S*(XCPB-XCPA))
        YCOP=YCOP+(YCPA+S*(YCPB-YCPA))
        IFLG=IFLG+1
      END IF
C
C Done.
C
      RETURN
C
      END

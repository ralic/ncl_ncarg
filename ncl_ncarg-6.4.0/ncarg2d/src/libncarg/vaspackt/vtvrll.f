      SUBROUTINE VTVRLL (RADI,RLAT,RLON,NVEC,VMIN,VMAX,ICOL,CMIN,CMAX,
     +                   IAMA,RTPL)
C
      DIMENSION IAMA(*)
C
      EXTERNAL RTPL
C
C Draw a "vector rose" on a globe - centered at (0,0,0) and having a
C radius RADI - at a position specified by the latitude RLAT and the
C longitude RLON.  The zero line is assumed to point north.
C
C NVEC is the number of vectors to draw.
C
C VMIN and VMAX are minimum and maximum lengths of vectors to be drawn
C in each of the NVEC directions; if VMIN = VMAX, only one set of
C vectors will be drawn, except when both are zero, when the lengths
C used will be computed from the current value of the VASPACKT
C parameter 'VRL'.
C
C If ICOL is zero, the vectors are drawn in the color implied by the
C current value of the polyline color index, but, if ICOL is non-zero,
C the values in CMIN and CMAX are used to determine their colors; in
C either case, CMIN and CMAX must contain valid real values that will
C not cause arithmetic problems.
C
C IAMA is an array containing an area map against which the rose is to
C be masked.  If masking is not desired, set IAMA(1) = 0.
C
C RTPL is a routine to be called to draw the rose (when it is masked).
C
C Define a vector pointing at latitude 0, longitude 0.
C
      DCLU=1.
      DCLV=0.
      DCLW=0.
C
C Define a vector pointing north.
C
      DCZU=0.
      DCZV=0.
      DCZW=1.
C
C Rotate both the point and the vector to the desired position.
C
      CALL NGRITD (2,-RLAT,DCLU,DCLV,DCLW)
      CALL NGRITD (3,+RLON,DCLU,DCLV,DCLW)
C
      CALL NGRITD (2,-RLAT,DCZU,DCZV,DCZW)
      CALL NGRITD (3,+RLON,DCZU,DCZV,DCZW)
C
C Call VTVRAP to draw the vector rose.
C
      CALL VTVRAP (RADI*DCLU,RADI*DCLV,RADI*DCLW,DCLU,DCLV,DCLW,
     +             DCZU,DCZV,DCZW,NVEC,VMIN,VMAX,ICOL,CMIN,CMAX,
     +             IAMA,RTPL)
C
C Done.
C
      RETURN
C
      END

      SUBROUTINE VTSLDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,RTPL)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*),IAMA(*)
C
      EXTERNAL RTPL
C
C This routine draws streamlines.  It uses the capability of moving
C in a direction perpendicular to the velocity vectors to generate
C streamlines that are approximately the same distance apart and are
C such that the distance does not depend on the resolution of the mesh.
C
C Declare all of the VASPACKT common blocks.
C
C
C VTCOM1 contains integer and real variables.
C
      COMMON /VTCOM1/ AHAW,AHLN,AHLR,AHSP,AHSR
      COMMON /VTCOM1/ ANIL,ANM1,ANM2,ANZF,AVEL,CHWM,CXIL,CXZF
      COMMON /VTCOM1/ CYIL,CYZF,DCNU,DCNV,DCNW,DMAX
      COMMON /VTCOM1/ DMIN,DVAL  !  REUSE FOR FLOWMIN AND FLOWMAX?
      COMMON /VTCOM1/ EMAX,EPSI
      COMMON /VTCOM1/ IBIL,IBZF,ICIL,ICLR(255)
      COMMON /VTCOM1/ ICSG,ICST,ICTT,ICTV,ICZF,IDBG,IISP
      COMMON /VTCOM1/ IIWS(2),IIWU,ILBC,IMPF
      COMMON /VTCOM1/ INIL  !  NEEDED? (INFORMATIONAL LABEL INDEX)
      COMMON /VTCOM1/ INIT,IPAI,IPIS
      COMMON /VTCOM1/ IPIL,IPZF,IRNG,IRWS(2),IRWU,ISET,ISTA(625)
      COMMON /VTCOM1/ ISVT,ITBM,IWSO,IZFF,JODP,JOMA
      COMMON /VTCOM1/ JOTZ,LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3
      COMMON /VTCOM1/ LIWB,LIWK,LIWS(2),LNLG
      COMMON /VTCOM1/ LOEN,LOPN,LOTN
      COMMON /VTCOM1/ LRWK,LRWS(2)
      COMMON /VTCOM1/ LSDD,LSDL,LSDM,LTIL,LTZF,MIRO,NCLR
      COMMON /VTCOM1/ NDGL,NEDG,NEXL,NEXT,NEXU
      COMMON /VTCOM1/ NLBS  !  NEEDED? (NUMBER OF LABELS)
      COMMON /VTCOM1/ NLSD,NLZF,NOMF,NPNT
      COMMON /VTCOM1/ NR04  !  NEEDED? (LABEL-LIST MANAGEMENT)
      COMMON /VTCOM1/ NSDL,NSDR,NTRI,OORV,PCPX,PCPY,PCPZ
      COMMON /VTCOM1/ PITH  !  NEEDED? (STREAMLINE INTERPOLATION)
      COMMON /VTCOM1/ SCFS,SCFU  !  NEEDED? (SCALE FACTORS)
      COMMON /VTCOM1/ SLLN,SLLR,SLPS,SLPR,SLSP,SLSR,SVSP,SVSR
      COMMON /VTCOM1/ TTLL,TTLR,TTSP,TTSR,TVAL(0:256)
      COMMON /VTCOM1/ UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /VTCOM1/ UWDR,UWDT,VFRA,VRLN,VRLR,VRMG,VRMR
      COMMON /VTCOM1/ VVMM
      COMMON /VTCOM1/ WCIL,WCZF,WLIL,WLZF
      COMMON /VTCOM1/ WWIL,WWZF
      COMMON /VTCOM1/ XLBC,XMAX,XMIN,XVPL,XVPR,XWDL,XWDR
      COMMON /VTCOM1/ YLBC,YMAX,YMIN,YVPB,YVPT,YWDB,YWDT,ZMAX,ZMIN
C
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      SAVE   /VTCOM1/
C
C VTCOM2 holds character parameters.
C
      COMMON /VTCOM2/ CHEX,CTMA,CTMB,FRMT
      COMMON /VTCOM2/ TXIL,TXZF
      CHARACTER*13 CHEX
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*128 TXIL
      CHARACTER*64 TXZF
      SAVE   /VTCOM2/
C
C The following common block allows us to initialize the "seed" for the
C random number generator VTRAND that is used to offset the arrowheads.
C This makes it possible to generate the same set of arrowheads for a
C pair of stereo views of a streamline field.
C
      COMMON /VTSEED/ SEED
      DOUBLE PRECISION SEED
      SAVE   /VTSEED/
C
C Define a constant used to convert from degrees to radians.
C
      DATA DTOR / .017453292519943 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('VTSLDM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Compute "realized" values of various vector parameters.
C
      CALL VTRVSP
C
C Initialize the "seed" for the random number generator used to offset
C the arrowheads.
C
      SEED=0.D0
C
      DO 10001 I=1,IRNG
        TEMP=VTRAND()
10001 CONTINUE
C
C Get the required real and integer workspaces.
C
      CALL VTGRWS (RWRK,1,NTRI/LOTN,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('VTSLDM',2).NE.0) GO TO 104
C
      CALL VTGIWS (IWRK,1,NTRI/LOTN,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('VTSLDM',3).NE.0) GO TO 104
C
C Do an initialization loop through all of the triangle nodes.
C
      DO 10002 I=0,NTRI-LOTN,LOTN
C
C Zero the utility flags in the triangle node.  Bits B2-B0 of element 5
C contain a count of the number of streamlines that have passed through
C the triangle.  Bits B27-B3 record which of 25 sub-triangles have been
C previously crossed by a streamline.
C
        ITRI(I+5)=0
C
C Find the indices of the nodes of the vertices of the triangle (in no
C particular order).
C
        IPP1=IEDG(ITRI(I+1)+1)
        IPP2=IEDG(ITRI(I+1)+2)
        IPP3=IEDG(ITRI(I+2)+1)
        IF (IPP3.EQ.IPP1.OR.IPP3.EQ.IPP2) IPP3=IEDG(ITRI(I+2)+2)
C
C Compute the squares of the cosines of the angles between the velocity
C vectors at pairs of vertices of the triangle.  (We use the squares in
C order to avoid taking square roots, and, actually, we compute "signed
C squares" such that, as the angles range from 0 degrees to 180 degrees,
C the values we get range from -1 to +1.)  These values will be used to
C order the triangles from most desirable as the starting point of a
C streamline generator to least desirable.
C
        RWRK(IR01+I/LOTN+1)=1.
C
        DNM1=(RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)*
     +       (RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)
        IF (DNM1.EQ.0.) GO TO 104
        CSA1=(RPNT(IPP1+4)*RPNT(IPP2+4)+
     +        RPNT(IPP1+5)*RPNT(IPP2+5)+
     +        RPNT(IPP1+6)*RPNT(IPP2+6))
        CSA1=CSA1*ABS(CSA1)/DNM1
C
        DNM2=(RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)*
     +       (RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2)
        IF (DNM2.EQ.0.) GO TO 104
        CSA2=(RPNT(IPP2+4)*RPNT(IPP3+4)+
     +        RPNT(IPP2+5)*RPNT(IPP3+5)+
     +        RPNT(IPP2+6)*RPNT(IPP3+6))
        CSA2=CSA2*ABS(CSA2)/DNM2
C
        DNM3=(RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2)*
     +       (RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)
        IF (DNM3.EQ.0.) GO TO 104
        CSA3=(RPNT(IPP3+4)*RPNT(IPP1+4)+
     +        RPNT(IPP3+5)*RPNT(IPP1+5)+
     +        RPNT(IPP3+6)*RPNT(IPP1+6))
        CSA3=CSA3*ABS(CSA3)/DNM3
C
        RWRK(IR01+I/LOTN+1)=-MIN(CSA1,CSA2,CSA3)
C
10002 CONTINUE
C
C Generate an index array for the values in RWRK.
C
      CALL VTSORT (RWRK(IR01+1),NTRI/LOTN,IWRK(II01+1))
C
C Compute a maximum acceptable value of the measure that we used to
C order the triangles.
C
      TMAX=COS(DTOR*ANM1)
      TMAX=-TMAX*ABS(TMAX)
C
C Loop through the triangles in order from most desirable to least
C desirable.
C
      DO 10003 IIII=1,NTRI/LOTN
C
C Avoid using the triangle (or any following it) if the greatest angle
C between the velocity vectors at its vertices is too large.
C
        IF (RWRK(IR01+IWRK(II01+IIII)).GT.TMAX) GO TO 104
C
C Otherwise, compute the base index of the triangle.
C
        IBEG=(IWRK(II01+IIII)-1)*LOTN
C
C Avoid using the triangle if there are already streamlines passing
C through it.
C
        IF (ITRI(IBEG+5).NE.0) GO TO 103
C
C Otherwise, use the center point of the triangle as a starting point
C to generate a batch of streamlines that are "parallel" to one another.
C (The streamlines' starting points are distributed at regular intervals
C along a "streamline generator", which is traced in such a way as to
C be everywhere perpendicular to the flow field.)
C
C Update the count of the number of streamlines crossing the triangle.
C (This is a little premature, since we may not draw the streamline,
C but it also prevents the triangle from being considered again.)
C
        IF (IAND(ITRI(IBEG+5),7).NE.7) ITRI(IBEG+5)=ITRI(IBEG+5)+1
C
C Both the streamline generator and the first streamline start from the
C center of the triangle.
C
        RBEG=.333333333333333
        SBEG=.333333333333333
C
C Trace the streamline generator for a specified distance, first toward
C its beginning and then toward its end.  This does two things for us:
C 1) it tells us whether or not the first streamline is far enough away
C from previously-drawn streamlines and 2) it locates starting points
C for other streamlines to be drawn.
C
        CALL VTTPOM (RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               0,ICSG,SLSR,ITER,SLGB,ISTB,RSTB,SSTB,IAMA,RTPL)
        IF (SLGB.LE.TTLR) GO TO 103
C
        CALL VTTPOM (RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               1,ICSG,SLSR,ITER,SLGE,ISTE,RSTE,SSTE,IAMA,RTPL)
        IF (SLGE.LE.TTLR) GO TO 103
C
C Trace the first streamline toward its beginning (in a direction
C opposite to the direction of the flow field) ...
C
        CALL VTTSOM (0,RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               0,SLLR,ITER,SLTB,ISTD,RSTD,SSTD,IAMA,RTPL)
C
C ... and then toward its end (in the direction of the flow field).
C
        CALL VTTSOM (0,RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               1,SLLR,ITER,SLTE,ISTD,RSTD,SSTD,IAMA,RTPL)
C
C Draw the first streamline toward its beginning (in a direction
C opposite to the direction of the flow field) ...
C
        CALL VTTSOM (3,RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               0,SLTB,ITER,SLTD,ISTD,RSTD,SSTD,IAMA,RTPL)
C
C ... and then toward its end (in the direction of the flow field).
C
        CALL VTTSOM (3,RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               1,SLTE,ITER,SLTD,ISTD,RSTD,SSTD,IAMA,RTPL)
C
C Then, repositioning to the start of the next streamline toward the
C beginning of the streamline generator ...
C
  101   IF (SLGB.GT..999*SLSR) THEN
C
          ISTR=ISTB
          RSTR=RSTB
          SSTR=SSTB
C
C ... trace the streamline generator further toward its beginning
C (taking steps of a specified size), ...
C
          CALL VTTPOM (RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                 0,ICSG,SLSR,ITER,SLGB,ISTB,RSTB,SSTB,IAMA,RTPL)
C
C ... and then draw a streamline (in two parts).
C
          IF (SLGB.GT.TTLR) THEN
            IF (IAND(ITRI(ISTR+5),7).NE.7) ITRI(ISTR+5)=ITRI(ISTR+5)+1
            CALL VTTSOM (0,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                   0,SLLR,ITER,SLTB,ISTD,RSTD,SSTD,IAMA,RTPL)
            CALL VTTSOM (0,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                   1,SLLR,ITER,SLTE,ISTD,RSTD,SSTD,IAMA,RTPL)
            CALL VTTSOM (3,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                   0,SLTB,ITER,SLTD,ISTD,RSTD,SSTD,IAMA,RTPL)
            CALL VTTSOM (3,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                   1,SLTE,ITER,SLTD,ISTD,RSTD,SSTD,IAMA,RTPL)
            GO TO 101
          END IF
C
        END IF
C
C Then, repositioning to the start of the next streamline toward the
C end of the streamline generator ...
C
  102   IF (SLGE.GT..999*SLSR) THEN
C
          ISTR=ISTE
          RSTR=RSTE
          SSTR=SSTE
C
C ... trace the streamline generator further toward its end (taking
C steps of a specified size), ...
C
          CALL VTTPOM (RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                 1,ICSG,SLSR,ITER,SLGE,ISTE,RSTE,SSTE,IAMA,RTPL)
C
C ... and draw a streamline (in two parts).
C
          IF (SLGE.GT.TTLR) THEN
            IF (IAND(ITRI(ISTR+5),7).NE.7) ITRI(ISTR+5)=ITRI(ISTR+5)+1
            CALL VTTSOM (0,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                   0,SLLR,ITER,SLTB,IDUM,RDUM,SDUM,IAMA,RTPL)
            CALL VTTSOM (0,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                   1,SLLR,ITER,SLTE,IDUM,RDUM,SDUM,IAMA,RTPL)
            CALL VTTSOM (3,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                   0,SLTB,ITER,SLTD,IDUM,RDUM,SDUM,IAMA,RTPL)
            CALL VTTSOM (3,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,
     +                   1,SLTE,ITER,SLTD,IDUM,RDUM,SDUM,IAMA,RTPL)
            GO TO 102
          END IF
C
        END IF
C
  103 CONTINUE
10003 CONTINUE
C
C Release the real and integer workspaces acquired above.
C
  104 LR01=0
      LI01=0
C
C Done.
C
      RETURN
C
      END

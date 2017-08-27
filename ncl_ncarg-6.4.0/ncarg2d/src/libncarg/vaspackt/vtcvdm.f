      SUBROUTINE VTCVDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,RTPL)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*),IAMA(*)
C
      EXTERNAL RTPL
C
C This routine draws "curly vectors".
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
C random number generator VTRAND.  This makes it possible to generate
C the same set of curly vectors for a pair of stereo views of a field.
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
      IF (ICFELL('VTCVDM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Compute "realized" values of various vector parameters.
C
      CALL VTRVSP
C
C Initialize the "seed" for the random number generator used.
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
      IF (IWSE.NE.0.OR.ICFELL('VTCVDM',2).NE.0) GO TO 104
C
      CALL VTGIWS (IWRK,1,NTRI/LOTN,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('VTCVDM',3).NE.0) GO TO 104
C
C Do an initialization loop through all of the triangle nodes.
C
      DO 10002 I=0,NTRI-LOTN,LOTN
C
C Zero the utility flags in the triangle node.  Bits B2-B0 of element
C 5 contain a count of the number of curly vectors that have passed
C through the triangle.  Bits B27-B3 record which of 25 sub-triangles
C have been previously crossed by a curly vector.
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
C the values we get range from -1 to +1.)  We avoid starting a curly
C vector in any triangle where this measure is too large.
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
C Compute an index array putting the triangles in random order.
C
      NIND=NTRI/LOTN
C
      DO 10003 IIII=1,NIND
        IWRK(II01+IIII)=IIII
10003 CONTINUE
C
      DO 10004 IIII=1,NIND-1
        JJJJ=MAX(1,MIN(NIND,IIII+INT(REAL(NIND-IIII+1)*VTRAND())))
        IF (IIII.NE.JJJJ) THEN
          ITMP=IWRK(II01+IIII)
          IWRK(II01+IIII)=IWRK(II01+JJJJ)
          IWRK(II01+JJJJ)=ITMP
        END IF
10004 CONTINUE
C
C Compute a maximum acceptable value of the angular measures that were
C computed for the triangles above.
C
      TMAX=COS(DTOR*ANM1)
      TMAX=-TMAX*ABS(TMAX)
C
C Loop through the triangles in the specified random order.
C
      DO 10005 IIII=1,NIND
C
C Avoid using the triangle if the greatest angle between the velocity
C vectors at its vertices is too large.
C
        IF (RWRK(IR01+IWRK(II01+IIII)).GT.TMAX) GO TO 103
C
C Otherwise, compute the base index of the triangle.
C
        IBEG=(IWRK(II01+IIII)-1)*LOTN
C
C Avoid using the triangle if there are already curly vectors passing
C through it.
C
        IF (ITRI(IBEG+5).NE.0) GO TO 103
C
C Otherwise, use the center point of the triangle as the base point
C for a curly vector.
C
C Update the count of the number of curly vectors crossing the triangle.
C (This is a little premature, since we may not draw the vector, but it
C also prevents the triangle from being considered again.)
C
        IF (IAND(ITRI(IBEG+5),7).NE.7) ITRI(IBEG+5)=ITRI(IBEG+5)+1
C
C The curly vector starts from the center of the triangle.
C
        RBEG=.333333333333333
        SBEG=.333333333333333
C
C Trace a line orthogonal to the velocity-vector field, first in one
C direction and then the other, for a specified distance.  If another
C curly vector is encountered, skip the current triangle.
C
        CALL VTTPOM (RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               0,ICSG,TTLR,ITER,SLGB,ISTB,RSTB,SSTB,IAMA,RTPL)
        IF (SLGB.LE..999*TTLR) GO TO 103
C
        CALL VTTPOM (RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               1,ICSG,TTLR,ITER,SLGE,ISTE,RSTE,SSTE,IAMA,RTPL)
        IF (SLGE.LE..999*TTLR) GO TO 103
C
C Find the indices of the nodes of the vertices of the triangle (in no
C particular order).
C
        IPP1=IEDG(ITRI(IBEG+1)+1)
        IPP2=IEDG(ITRI(IBEG+1)+2)
        IPP3=IEDG(ITRI(IBEG+2)+1)
        IF (IPP3.EQ.IPP1.OR.IPP3.EQ.IPP2) IPP3=IEDG(ITRI(IBEG+2)+2)
C
C Find the magnitude of the flow field at the center of the triangle
C and use that to compute a vector length to be used.
C
        VMAG=(SQRT(RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)+
     +        SQRT(RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)+
     +        SQRT(RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2))
     +                                                             /3.
C
        VLEN=VFRA*VRLR+(1.-VFRA)*VRLR*(VMAG/VRMR)
C
        VLEN=VLEN/2.
C
        IF (VLEN.LE.SLPR) GO TO 103
C
C Trace the curly vector toward its beginning (in a direction opposite
C to the direction of the flow field), ...
C
        CALL VTTSOM (0,RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               0,VLEN,ITER,VRET,ISTD,RSTD,SSTD,IAMA,RTPL)
C
C ... skipping the triangle if the curly vector terminated early, ...
C
        IF (VRET.LT..999*VLEN) GO TO 103
C
C ... and then toward its end (in the direction of the flow field), ...
C
        CALL VTTSOM (0,RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               1,VLEN,ITER,VRET,ISTD,RSTD,SSTD,IAMA,RTPL)
C
C ... skipping the triangle if the curly vector terminated early.
C
        IF (VRET.LT..999*VLEN) GO TO 103
C
C Draw the curly vector toward its beginning (in a direction opposite
C to the direction of the flow field) ...
C
        CALL VTTSOM (1,RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               0,VLEN,ITER,VRET,ISTD,RSTD,SSTD,IAMA,RTPL)
C
C ... and then toward its end (in the direction of the flow field).
C
        CALL VTTSOM (2,RPNT,IEDG,ITRI,IBEG,RBEG,SBEG,
     +               1,VLEN,ITER,VRET,ISTD,RSTD,SSTD,IAMA,RTPL)
C
  103 CONTINUE
10005 CONTINUE
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

      SUBROUTINE VTMESH (RPNT,KPNT,KOPN,
     +                   IEDG,KEDG,KOEN,
     +                   ITRI,KTRI,KOTN,
     +                   RWRK,KRWK,
     +                   IWRK,KIWK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C The routine VTMESH is called to start the process of drawing a
C streamline plot, given data on a triangular mesh.
C
C RPNT is a one-dimensional array containing information about the
C points of the triangular mesh.
C
C KPNT is the index of the last element of RPNT containing data.
C
C KOPN is the length of a point node in RPNT.
C
C IEDG is a one-dimensional array containing information about the
C edges of the triangular mesh.
C
C KEDG is the index of the last element of IEDG.
C
C KOEN is the length of an edge node in IEDG.
C
C ITRI is a one-dimensional array containing information about the
C triangles of the triangular mesh.
C
C KTRI is the index of the last element of ITRI.
C
C KOTN is the length of a triangle node in ITRI.
C
C RWRK is a singly-subscripted real work array of length KRWK.
C
C KRWK is the dimension of RWRK.
C
C IWRK is a singly-subscripted integer work array of length KIWK.
C
C KIWK is the dimension of IWRK.
C
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
C Define a variable which will hold a single character.
C
      CHARACTER*1 SCHR
C
C IXOR(IONE,ITWO) is the exclusive OR of the 12-bit masks IONE and ITWO.
C
      IXOR(IONE,ITWO)=IAND(IOR(IONE,ITWO),4095-IAND(IONE,ITWO))
C
C ITBF(IARG) is non-zero if and only if a triangle with blocking-flag
C element IARG is blocked by the user.
C
      ITBF(IARG)=IAND(IAND(IXOR(IARG,ITBX),ITBA),1)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('VTMESH - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If no VASPACKT routine has been called before, initialize required
C constants.
C
      IF (INIT.EQ.0) THEN
        CALL VTINRC
        IF (ICFELL('VTMESH',2).NE.0) RETURN
      END IF
C
C Extract the values of ITBX and ITBA from ITBM.
C
      ITBX=IAND(ISHIFT(ITBM,-12),4095)
      ITBA=IAND(       ITBM     ,4095)
C
C Transfer the array dimensions and node lengths to variables in COMMON.
C
      NPNT=KPNT
      LOPN=KOPN
C
      NEDG=KEDG
      LOEN=KOEN
C
      NTRI=KTRI
      LOTN=KOTN
C
      LRWK=KRWK
C
      LIWK=KIWK
C
C Clear all the workspace block lengths.
C
      DO 10001 I=1,2
        LRWS(I)=0
10001 CONTINUE
C
      DO 10002 I=1,2
        LIWS(I)=0
10002 CONTINUE
C
C Zero the internal parameters which keep track of workspace usage.
C
      IIWU=0
      IRWU=0
C
C Compute the ranges of the X, Y, and Z coordinates, the flow field
C values, and the values of the 2D coordinates in the projection plane.
C
      ITM1=0
C
      XMIN=0.
      XMAX=0.
      YMIN=0.
      YMAX=0.
      ZMIN=0.
      ZMAX=0.
      DMIN=0.
      DMAX=0.
C
      ITM2=0
C
      UMIN=0.
      UMAX=0.
      VMIN=0.
      VMAX=0.
C
      DO 10003 I=0,NTRI-LOTN,LOTN
        IF (ITBF(ITRI(I+4)).EQ.0) THEN
          DO 10004 J=1,3
            DO 10005 K=1,2
              L=IEDG(ITRI(I+J)+K)
              IF (ITM1.EQ.0) THEN
                ITM1=1
                XMIN=RPNT(L+1)
                XMAX=RPNT(L+1)
                YMIN=RPNT(L+2)
                YMAX=RPNT(L+2)
                ZMIN=RPNT(L+3)
                ZMAX=RPNT(L+3)
                IF (ICTV.GE.4.AND.ICTV.LE.LOPN) THEN
                  TMIN=RPNT(L+ICTV)
                  TMAX=RPNT(L+ICTV)
                END IF
                DMIN=SQRT(RPNT(L+4)**2+RPNT(L+5)**2+RPNT(L+6)**2)
                DMAX=SQRT(RPNT(L+4)**2+RPNT(L+5)**2+RPNT(L+6)**2)
              ELSE
                XMIN=MIN(XMIN,RPNT(L+1))
                XMAX=MAX(XMAX,RPNT(L+1))
                YMIN=MIN(YMIN,RPNT(L+2))
                YMAX=MAX(YMAX,RPNT(L+2))
                ZMIN=MIN(ZMIN,RPNT(L+3))
                ZMAX=MAX(ZMAX,RPNT(L+3))
                IF (ICTV.GE.4.AND.ICTV.LE.LOPN) THEN
                  TMIN=MIN(TMIN,RPNT(L+ICTV))
                  TMAX=MAX(TMAX,RPNT(L+ICTV))
                END IF
                DMIN=MIN(DMIN,
     +                   SQRT(RPNT(L+4)**2+RPNT(L+5)**2+RPNT(L+6)**2))
                DMAX=MAX(DMAX,
     +                   SQRT(RPNT(L+4)**2+RPNT(L+5)**2+RPNT(L+6)**2))
              END IF
              IF (IMPF.EQ.0) THEN
                UTMP=RPNT(L+1)
                VTMP=RPNT(L+2)
              ELSE
                CALL HLUVTMXYZ (IMPF,RPNT(L+1),RPNT(L+2),RPNT(L+3),
     +                                                   UTMP,VTMP)
                IF (ICFELL('VTMESH',3).NE.0) RETURN
                IF (OORV.NE.0..AND.(UTMP.EQ.OORV.OR.VTMP.EQ.OORV))
     +                                                       GO TO 101
              END IF
              IF (ITM2.EQ.0) THEN
                ITM2=1
                UMIN=UTMP
                UMAX=UTMP
                VMIN=VTMP
                VMAX=VTMP
              ELSE
                UMIN=MIN(UMIN,UTMP)
                UMAX=MAX(UMAX,UTMP)
                VMIN=MIN(VMIN,VTMP)
                VMAX=MAX(VMAX,VTMP)
              END IF
  101       CONTINUE
10005       CONTINUE
10004     CONTINUE
        END IF
10003 CONTINUE
C
      EMAX=MAX(XMAX-XMIN,YMAX-YMIN,ZMAX-ZMIN)
C
C Initialize coloring of the streamlines.
C
      IF (ICTV.GT.0.AND.NCLR.NE.0) THEN
C
        IF (ICTV.EQ.1) THEN
          TMIN=XMIN
          TMAX=XMAX
        ELSE IF (ICTV.EQ.2) THEN
          TMIN=YMIN
          TMAX=YMAX
        ELSE IF (ICTV.EQ.3) THEN
          TMIN=ZMIN
          TMAX=ZMAX
        ELSE IF (ICTV.GT.LOPN) THEN
          TMIN=DMIN
          TMAX=DMAX
        END IF
C
        DO 10006 I=1,NCLR-1
          TVAL(I)=TMIN+REAL(I)*(TMAX-TMIN)/REAL(NCLR)
10006   CONTINUE
C
        TVAL(NCLR)=TMAX
C
      END IF
C
C Compute an average edge length over the unblocked portion of the mesh.
C
      ITMP=0
      AVEL=0.
C
      DO 10007 I=0,NEDG-LOEN,LOEN
        IFLL=0
        IF (IEDG(I+3).GE.0) THEN
          IF (ITBF(ITRI(LOTN*((IEDG(I+3)-1)/LOTN)+4)).EQ.0) IFLL=1
        END IF
        IFLR=0
        IF (IEDG(I+4).GE.0) THEN
          IF (ITBF(ITRI(LOTN*((IEDG(I+4)-1)/LOTN)+4)).EQ.0) IFLR=1
        END IF
        IF (IFLL.NE.0.OR.IFLR.NE.0) THEN
          ITMP=ITMP+1
          AVEL=AVEL+SQRT((RPNT(IEDG(I+1)+1)-RPNT(IEDG(I+2)+1))**2+
     +                   (RPNT(IEDG(I+1)+2)-RPNT(IEDG(I+2)+2))**2+
     +                   (RPNT(IEDG(I+1)+3)-RPNT(IEDG(I+2)+3))**2)
        END IF
10007 CONTINUE
C
      IF (ITMP.NE.0) AVEL=AVEL/REAL(ITMP)
C
C If the user has done a SET call, retrieve the arguments; if he hasn't
C done a SET call, do it for him.
C
      IF (ISET.EQ.0) THEN
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('VTMESH',4).NE.0) RETURN
C
      ELSE
C
        LNLG=1
C
        IF (UWDL.EQ.UWDR) THEN
          XWDL=UMIN
          XWDR=UMAX
        ELSE
          XWDL=UWDL
          XWDR=UWDR
        END IF
C
        IF (UWDB.EQ.UWDT) THEN
          YWDB=VMIN
          YWDT=VMAX
        ELSE
          YWDB=UWDB
          YWDT=UWDT
        END IF
C
        IF (UVPS.LT.0.) THEN
          RWTH=ABS(UVPS)
        ELSE IF (UVPS.EQ.0.) THEN
          RWTH=(UVPR-UVPL)/(UVPT-UVPB)
        ELSE IF (UVPS.LE.1.) THEN
          RWTH=ABS((XWDR-XWDL)/(YWDT-YWDB))
          IF (MIN(RWTH,1./RWTH).LT.UVPS) RWTH=(UVPR-UVPL)/(UVPT-UVPB)
        ELSE
          RWTH=ABS((XWDR-XWDL)/(YWDT-YWDB))
          IF (MAX(RWTH,1./RWTH).GT.UVPS) RWTH=1.
        END IF
C
        IF (RWTH.LT.(UVPR-UVPL)/(UVPT-UVPB)) THEN
          XVPL=.5*(UVPL+UVPR)-.5*(UVPT-UVPB)*RWTH
          XVPR=.5*(UVPL+UVPR)+.5*(UVPT-UVPB)*RWTH
          YVPB=UVPB
          YVPT=UVPT
        ELSE
          XVPL=UVPL
          XVPR=UVPR
          YVPB=.5*(UVPB+UVPT)-.5*(UVPR-UVPL)/RWTH
          YVPT=.5*(UVPB+UVPT)+.5*(UVPR-UVPL)/RWTH
        END IF
C
        CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('VTMESH',5).NE.0) RETURN
C
      END IF
C
C Set the flag MIRO, which indicates whether or not the transformations
C in effect cause mirror imaging.  To do this, we look for an unblocked
C triangle in the mesh, all of whose vertices are visible under the
C current mapping, and check to see if its vertices, after mapping, are
C still in counterclockwise order (in which case we set MIRO=0) or not
C (in which case we set MIRO=1).  (However, when 'MAP' = 2, saying that
C TDPACK is being called to do the transformation, MIRO is forced to 0;
C in that case, the transformation cannot cause mirror imaging.)
C
      MIRO=0
C
      IF (IMPF.NE.0.AND.IMPF.NE.2) THEN
C
        DO 10008 I=0,NTRI-LOTN,LOTN
C
C Use only triangles not blocked by the user.
C
          IF (ITBF(ITRI(I+4)).EQ.0) THEN
C
C Find the base index of the point that edges 1 and 2 have in common.
C
            IF (IEDG(ITRI(I+1)+1).EQ.IEDG(ITRI(I+2)+1).OR.IEDG(ITRI(I+1)
     ++1).EQ.IEDG(ITRI(I+2)+2)) THEN
              IPP1=IEDG(ITRI(I+1)+1)
            ELSE
              IPP1=IEDG(ITRI(I+1)+2)
            END IF
C
C Find the base index of the point that edges 2 and 3 have in common.
C
            IF (IEDG(ITRI(I+2)+1).EQ.IEDG(ITRI(I+3)+1).OR.IEDG(ITRI(I+2)
     ++1).EQ.IEDG(ITRI(I+3)+2)) THEN
              IPP2=IEDG(ITRI(I+2)+1)
            ELSE
              IPP2=IEDG(ITRI(I+2)+2)
            END IF
C
C Find the base index of the point that edges 3 and 1 have in common.
C
            IF (IEDG(ITRI(I+3)+1).EQ.IEDG(ITRI(I+1)+1).OR.IEDG(ITRI(I+3)
     ++1).EQ.IEDG(ITRI(I+1)+2)) THEN
              IPP3=IEDG(ITRI(I+3)+1)
            ELSE
              IPP3=IEDG(ITRI(I+3)+2)
            END IF
C
C Project point 1; if it's invisible, skip the triangle.
C
            CALL HLUVTMXYZ (IMPF,
     +                      RPNT(IPP1+1),RPNT(IPP1+2),RPNT(IPP1+3),
     +                                                      XCP1,YCP1)
            IF (ICFELL('VTMESH',6).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCP1.EQ.OORV.OR.YCP1.EQ.OORV))
     +                                                       GO TO 102
C
C Project point 2; if it's invisible, skip the triangle.
C
            CALL HLUVTMXYZ (IMPF,
     +                      RPNT(IPP2+1),RPNT(IPP2+2),RPNT(IPP2+3),
     +                                                      XCP2,YCP2)
            IF (ICFELL('VTMESH',7).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCP2.EQ.OORV.OR.YCP2.EQ.OORV))
     +                                                       GO TO 102
C
C Project point 3; if it's invisible, skip the triangle.
C
            CALL HLUVTMXYZ (IMPF,
     +                      RPNT(IPP3+1),RPNT(IPP3+2),RPNT(IPP3+3),
     +                                                      XCP3,YCP3)
            IF (ICFELL('VTMESH',8).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCP3.EQ.OORV.OR.YCP3.EQ.OORV))
     +                                                       GO TO 102
C
C If two points of the triangle are too close to each other, skip it.
C
            IF (ABS(XCP1-XCP2).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCP1-YCP2).LT..0001*ABS(YWDT-YWDB)) GO TO 102
            IF (ABS(XCP2-XCP3).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCP2-YCP3).LT..0001*ABS(YWDT-YWDB)) GO TO 102
            IF (ABS(XCP3-XCP1).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCP3-YCP1).LT..0001*ABS(YWDT-YWDB)) GO TO 102
C
C If two points of the triangle are too far apart, skip it.
C
            IF (ABS(XCP1-XCP2).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCP1-YCP2).GT..5*ABS(YWDT-YWDB)) GO TO 102
            IF (ABS(XCP2-XCP3).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCP2-YCP3).GT..5*ABS(YWDT-YWDB)) GO TO 102
            IF (ABS(XCP3-XCP1).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCP3-YCP1).GT..5*ABS(YWDT-YWDB)) GO TO 102
C
C Use this triangle to make the decision.  If point 1 is to the right
C of the vector from point 3 to point 2, then the mapping does not
C cause mirror imaging; otherwise, it does.
C
            IF (ABS(XCP2-XCP3).LT.ABS(YCP2-YCP3)) THEN
              IF (XCP1.LT.XCP3+((XCP2-XCP3)/(YCP2-YCP3))*(YCP1-YCP3)) TH
     +EN
                IF (YCP3.LT.YCP2) MIRO=1
                GO TO 103
              ELSE
                IF (YCP3.GT.YCP2) MIRO=1
                GO TO 103
              END IF
            ELSE
              IF (YCP1.LT.YCP3+((YCP2-YCP3)/(XCP2-XCP3))*(XCP1-XCP3)) TH
     +EN
                IF (XCP3.GT.XCP2) MIRO=1
                GO TO 103
              ELSE
                IF (XCP3.LT.XCP2) MIRO=1
                GO TO 103
              END IF
            END IF
C
          END IF
C
C End of loop through triangles.
C
  102   CONTINUE
10008   CONTINUE
C
      END IF
C
C Zero the count of label positions selected, the count of words used
C in real workspace number 4 (for informational and high/low label
C data), and the indices which indicate where the different kinds of
C labels are stored.
C
  103 NLBS=0
      NR04=0
      INIL=0
C
C Initialize the value of the scale factor used.
C
      IF (SCFS.LE.0.) THEN
        SCFU=1.
      ELSE
        SCFU=SCFS
      END IF
C
C If the flow field is (effectively) zero, set a flag to indicate that
C and force the scale factor back to 1.  Otherwise, clear the flag.
C
C Code here needs work ... ???
C
      IF (DMAX-DMIN.LE.10.*EPSI*ABS((DMIN+DMAX)/2.)) THEN
        IZFF=1
        SCFU=1.
      ELSE
        IZFF=0
      END IF
C
C Find the positions of the leftmost significant digits in the largest
C absolute value in the field and in the difference between the minimum
C and the maximum values in the field.  If the field is effectively
C zero, the latter value is set equal to the former.
C
      CALL VTNUMB (MAX(ABS(DMIN/SCFU),ABS(DMAX/SCFU)),1,-10000,-1,-1,
     +                    ' ',' ',' ',0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
      LSDM=IEVA-1
C
      IF (IZFF.EQ.0) THEN
        CALL VTNUMB ((DMAX-DMIN)/SCFU,1,-10000,-1,-1,
     +                    ' ',' ',' ',0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
        LSDD=IEVA-1
      ELSE
        LSDD=LSDM
      END IF
C
C Retrieve the current PLOTCHAR function code signal character.
C
      CALL PCGETC ('FC',SCHR)
      IF (ICFELL('VTMESH',9).NE.0) RETURN
C
C Set up the parameters used in generating numeric labels.  Set the
C number of significant digits to be used ...
C
      IF (NSDL.LT.0) THEN
        NDGL=ABS(NSDL)
      ELSE
        NDGL=MAX(0,LSDM-LSDD)+NSDL
      END IF
C
C ... the leftmost-significant digit flag ...
C
      IF (NLSD.EQ.0) THEN
        LSDL=-10000
      ELSE
        LSDL=LSDM
      END IF
C
C ... the numeric exponent type ...
C
      IF (NEXT.LE.0) THEN
        CHEX=' E '
        LEA1=1
        LEA2=1
        LEA3=1
        LEE1=0
        LEE2=1
        LEE3=0
      ELSE IF (NEXT.EQ.1) THEN
        CHEX=':L1:410:S::N:'
        IF (SCHR.NE.':') THEN
          CHEX( 1: 1)=SCHR
          CHEX( 4: 4)=SCHR
          CHEX( 8: 8)=SCHR
          CHEX(10:10)=SCHR
          CHEX(11:11)=SCHR
          CHEX(13:13)=SCHR
        END IF
        LEA1=5
        LEA2=5
        LEA3=3
        LEE1=1
        LEE2=2
        LEE3=0
      ELSE
        CHEX='x10** '
        LEA1=1
        LEA2=4
        LEA3=1
        LEE1=1
        LEE2=4
        LEE3=0
      END IF
C
C ... and the omission flags.
C
      JOMA=MOD(MAX(0,MIN(7,NOMF))/4,2)
      JODP=MOD(MAX(0,MIN(7,NOMF))/2,2)
      JOTZ=MOD(MAX(0,MIN(7,NOMF))  ,2)
C
C If the field is not zero and the scale factor is to be chosen
C here, do it now.  The parameter which specifies where the leftmost
C significant digit is assumed to be also must be updated.
C
      IF (IZFF.EQ.0.AND.SCFS.LE.0..AND.SCFS.GE.-3.) THEN
        ITMP=0
        IF (SCFS.EQ.0..OR.(SCFS.EQ.-3..AND.LSDM.LT.-1)) ITMP=LSDM+1
        IF (SCFS.EQ.-1.) ITMP=LSDM
        IF (SCFS.EQ.-2..OR.(SCFS.EQ.-3..AND.LSDM-NDGL.GE.0))
     +                                                ITMP=LSDM-NDGL+1
        SCFU=10.**ITMP
        IF (LSDL.NE.-10000) LSDL=LSDL-ITMP
      END IF
C
C Done.
C
      RETURN
C
      END

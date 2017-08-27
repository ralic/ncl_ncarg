      SUBROUTINE VTGRWS (RWRK,IOWS,LOWS,IERR)
C
      DIMENSION RWRK(*)
C
C This subroutine is called to get a block of space, of a specified
C size, in the user's real workspace array.  The block may or may not
C have been used before.
C
C IOWS is the index (into the arrays IRWS and LRWS) of the values
C saying where the block starts and how long it is.
C
C LOWS is the desired length.  The value 0 indicates that the maximum
C amount is desired; it will be replaced by the actual amount assigned.
C
C IERR is a returned error flag.  It will be 0 if no workspace overflow
C occurred, 1 if an overflow did occur.
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
C Check for argument error.
C
      IF (IOWS.LT.1.OR.IOWS.GT.2.OR.LOWS.LT.0) THEN
        CALL SETER ('VTGRWS - ARGUMENT ERROR - SEE SPECIALIST',1,1)
        RETURN
      END IF
C
C Clear error flag.
C
      IERR=0
C
C See if the desired amount of space is available.
C
      NLFT=LRWK
C
      DO 10001 I=1,2
        IF (I.NE.IOWS.AND.LRWS(I).GT.0) NLFT=NLFT-LRWS(I)
10001 CONTINUE
C
C If caller wants it all, arrange for that.
C
      IF (LOWS.LE.0) LOWS=NLFT
C
C Update the real-workspace-used parameter.
C
      IRWU=MAX(IRWU,LRWK-NLFT+LOWS)
C
C If too little space is available, take whatever action the user has
C specified.
C
      IF (NLFT.LT.LOWS) THEN
        IF (IWSO.LE.1)
     +    WRITE (I1MACH(4),'('' VTGRWS'',
     +                       I8,'' WORDS REQUESTED'',
     +                       I8,'' WORDS AVAILABLE'')') LOWS,NLFT
        IF (IWSO.LE.0) THEN
          CALL SETER ('VTGRWS - REAL WORKSPACE OVERFLOW',2,2)
          STOP
        ELSE IF (IWSO.GE.3) THEN
          CALL SETER ('VTGRWS - REAL WORKSPACE OVERFLOW',3,1)
        ELSE
          IERR=1
        END IF
        RETURN
      END IF
C
C It may be that a reduction in size has been requested.  That's easy.
C
      IF (LOWS.LE.LRWS(IOWS)) THEN
        LRWS(IOWS)=LOWS
        RETURN
      END IF
C
C Otherwise, what we do depends on whether the workspace associated
C with this index exists already.
C
      IF (LRWS(IOWS).LE.0) THEN
C
C It does not exist.  Find (or create) an area large enough.  First,
C check for an open space large enough.
C
        JRWS=0
10002   CONTINUE
          KRWS=LRWK
          IMIN=0
          DO 10003 I=1,2
            IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) TH
     +EN
              KRWS=IRWS(I)
              IMIN=I
            END IF
10003     CONTINUE
          IF (KRWS-JRWS.GE.LOWS) THEN
            IRWS(IOWS)=JRWS
            LRWS(IOWS)=LOWS
            RETURN
          END IF
          IF (IMIN.NE.0) THEN
            JRWS=IRWS(IMIN)+LRWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0)) GO TO 10002
C
C If no space large enough was found, pack all the existing blocks
C into the beginning of the array, which will leave enough space at
C the end of it.
C
        JRWS=0
10004   CONTINUE
          KRWS=LRWK
          IMIN=0
          DO 10005 I=1,2
            IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) TH
     +EN
              KRWS=IRWS(I)
              IMIN=I
            END IF
10005     CONTINUE
          IF (IMIN.NE.0) THEN
            IF (IRWS(IMIN).NE.JRWS) THEN
              DO 10006 I=1,LRWS(IMIN)
                RWRK(JRWS+I)=RWRK(IRWS(IMIN)+I)
10006         CONTINUE
              IRWS(IMIN)=JRWS
            END IF
            JRWS=JRWS+LRWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0)) GO TO 10004
C
        IRWS(IOWS)=JRWS
        LRWS(IOWS)=LOWS
        RETURN
C
      ELSE
C
C It exists.  Extend its length.  First, see if that can be done
C without moving anything around.
C
        JRWS=IRWS(IOWS)+LRWS(IOWS)
        KRWS=LRWK
        DO 10007 I=1,2
          IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) THEN
            KRWS=IRWS(I)
          END IF
10007   CONTINUE
        IF (KRWS-JRWS.GE.LOWS) THEN
          LRWS(IOWS)=LOWS
          RETURN
        END IF
C
C Blocks have to be moved.  Move those that precede the one to be
C lengthened and that one itself toward the beginning of the workspace.
C
        JRWS=0
10008   CONTINUE
          KRWS=LRWK
          IMIN=0
          DO 10009 I=1,2
            IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) TH
     +EN
              KRWS=IRWS(I)
              IMIN=I
            END IF
10009     CONTINUE
          IF (IMIN.NE.0) THEN
            IF (IRWS(IMIN).NE.JRWS) THEN
              DO 10010 I=1,LRWS(IMIN)
                RWRK(JRWS+I)=RWRK(IRWS(IMIN)+I)
10010         CONTINUE
              IRWS(IMIN)=JRWS
            END IF
            JRWS=JRWS+LRWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0.OR.IMIN.EQ.IOWS)) GO TO 10008
C
C Move blocks that follow the one to be lengthened toward the end of
C the workspace.
C
        KRWS=LRWK
10011   CONTINUE
          JRWS=IRWS(IOWS)+LRWS(IOWS)
          IMAX=0
          DO 10012 I=1,2
            IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) TH
     +EN
              JRWS=IRWS(I)+LRWS(I)
              IMAX=I
            END IF
10012     CONTINUE
          IF (IMAX.NE.0) THEN
            IF (JRWS.NE.KRWS) THEN
              DO 10013 I=LRWS(IMAX),1,-1
                RWRK(KRWS-LRWS(IMAX)+I)=RWRK(JRWS-LRWS(IMAX)+I)
10013         CONTINUE
              IRWS(IMAX)=KRWS-LRWS(IMAX)
            END IF
            KRWS=IRWS(IMAX)
          END IF
        IF (.NOT.(IMAX.EQ.0)) GO TO 10011
C
C There should now be room, so just update the length of the block.
C
        LRWS(IOWS)=LOWS
        RETURN
C
      END IF
C
      END

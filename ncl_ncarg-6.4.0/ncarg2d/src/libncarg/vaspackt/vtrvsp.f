      SUBROUTINE VTRVSP
C
C Compute "realized" values of all size parameters.
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
C Compute a "realized" value for the spacing of simple vectors.
C
      IF (IISP.EQ.0) THEN
        SVSR=SVSP
      ELSE
        SVSR=SVSP*EMAX
      END IF
C
C Compute a "realized" value of the vector reference length.
C
      IF (VRLN.LT.0.) THEN
        VRLR=ABS(VRLN)*AVEL
      ELSE IF (VRLN.GT.0.) THEN
        IF (IISP.EQ.0) THEN
          VRLR=VRLN
        ELSE
          VRLR=VRLN*EMAX
        END IF
      ELSE
        IF (ISVT.EQ.0.OR.SVSR.EQ.0.) THEN
          VRLR=AVEL*REAL(1+ISVT)
        ELSE
          VRLR=AVEL+2.*SVSR
        END IF
      END IF
C
C Compute a "realized" value of the vector reference magnitude.
C
      IF (VRMG.EQ.0.) THEN
        VRMR=DMAX
      ELSE
        VRMR=VRMG
      END IF
C
C Compute "realized" values of the arrowhead length and spacing.
C
      IF (AHLN.LE.0.) THEN
        AHLR=ABS(AHLN)*VRLR
      ELSE
        IF (IISP.EQ.0) THEN
          AHLR=AHLN
        ELSE
          AHLR=AHLN*EMAX
        END IF
      END IF
C
      IF (IISP.EQ.0) THEN
        AHSR=AHSP
      ELSE
        AHSR=AHSP*EMAX
      END IF
C
C Compute "realized" values of the streamline length, point spacing,
C streamline spacing, termination test length, and termination test
C spacing parameters.
C
      IF (IISP.EQ.0) THEN
        SLLR=SLLN
        SLPR=SLPS
        SLSR=SLSP
        TTLR=TTLL
        TTSR=TTSP
      ELSE
        SLLR=SLLN*EMAX
        SLPR=SLPS*EMAX
        SLSR=SLSP*EMAX
        TTLR=TTLL*EMAX
        TTSR=TTSP*EMAX
      END IF
C
C Done.
C
      RETURN
C
      END

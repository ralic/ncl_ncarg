      SUBROUTINE VTSETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to give a specified integer value to a
C specified parameter.
C
C WHCH is the name of the parameter whose value is to be set.
C
C IVAL is an integer variable containing the new value of the parameter.
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
      INTEGER ISHIFT
C
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to VTSETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either vtseti() or vtsetr(), as in:
C        CALL VTSETI ('xxx',-9999)
C     or
C        CALL VTSETR ('xxx',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the WHCH, then we delegate over to VTSETR.
C --------------------------------------------------------------------
C
C Check for an uncleared prior error.
C
      IF (ICFELL('VTSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(WHCH).LT.3) THEN
        CTMB(1:36)='VTSETI - PARAMETER NAME TOO SHORT - '
        CTMB(37:36+LEN(WHCH))=WHCH
        CALL SETER (CTMB(1:36+LEN(WHCH)),2,1)
        RETURN
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr') THEN
        IF (IPAI.LT.1.OR.IPAI.GT.NCLR) THEN
          GO TO 10002
        END IF
      END IF
C
      GO TO 10003
10002 CONTINUE
        CTMB(1:36)='VTSETI - SETTING XXX - PAI INCORRECT'
        CTMB(18:20)=WHCH(1:3)
        CALL SETER (CTMB(1:36),3,1)
        RETURN
10003 CONTINUE
C
C
C Set the appropriate parameter value.
C
      IF (WHCH(1:3).EQ.'CLR'.OR.WHCH(1:3).EQ.'clr') THEN
        ICLR(IPAI)=IVAL
      ELSE IF (WHCH(1:3).EQ.'CTV'.OR.WHCH(1:3).EQ.'ctv') THEN
        ICTV=IVAL
      ELSE IF (WHCH(1:3).EQ.'DBG'.OR.WHCH(1:3).EQ.'dbg') THEN
        IDBG=IVAL
      ELSE IF (WHCH(1:3).EQ.'ILB'.OR.WHCH(1:3).EQ.'ilb') THEN
        IBIL=MAX(0,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'ILC'.OR.WHCH(1:3).EQ.'ilc') THEN
        ICIL=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'ILP'.OR.WHCH(1:3).EQ.'ilp') THEN
        IPIL=MAX(-4,MIN(4,IVAL))
      ELSE IF (WHCH(1:3).EQ.'ISP'.OR.WHCH(1:3).EQ.'isp') THEN
        IISP=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:3).EQ.'IWB'.OR.WHCH(1:3).EQ.'iwb') THEN
        LIWB=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'LBC'.OR.WHCH(1:3).EQ.'lbc') THEN
        ILBC=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'MAP'.OR.WHCH(1:3).EQ.'map') THEN
        IMPF=MAX(0,IVAL)
      ELSE IF (WHCH(1:3).EQ.'NEL'.OR.WHCH(1:3).EQ.'nel') THEN
        NEXL=IVAL
      ELSE IF (WHCH(1:3).EQ.'NET'.OR.WHCH(1:3).EQ.'net') THEN
        NEXT=MAX(0,MIN(2,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NEU'.OR.WHCH(1:3).EQ.'neu') THEN
        NEXU=IVAL
      ELSE IF (WHCH(1:3).EQ.'NLS'.OR.WHCH(1:3).EQ.'nls') THEN
        NLSD=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NLV'.OR.WHCH(1:3).EQ.'nlv') THEN
        NCLR=MAX(0,MIN(255,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NLZ'.OR.WHCH(1:3).EQ.'nlz') THEN
        NLZF=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NOF'.OR.WHCH(1:3).EQ.'nof') THEN
        NOMF=MAX(0,MIN(7,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NSD'.OR.WHCH(1:3).EQ.'nsd') THEN
        NSDL=IVAL
      ELSE IF (WHCH(1:3).EQ.'PAI'.OR.WHCH(1:3).EQ.'pai') THEN
        IPAI=IVAL
      ELSE IF (WHCH(1:3).EQ.'PIS'.OR.WHCH(1:3).EQ.'pis') THEN
        IPIS=IVAL
      ELSE IF (WHCH(1:3).EQ.'RNG'.OR.WHCH(1:3).EQ.'rng') THEN
        IRNG=MAX(0,IVAL)
      ELSE IF (WHCH(1:3).EQ.'SET'.OR.WHCH(1:3).EQ.'set') THEN
        ISET=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:3).EQ.'SGC'.OR.WHCH(1:3).EQ.'sgc') THEN
        ICSG=MAX(0,IVAL)
      ELSE IF (WHCH(1:3).EQ.'STC'.OR.WHCH(1:3).EQ.'stc') THEN
        ICST=MAX(0,IVAL)
      ELSE IF (WHCH(1:3).EQ.'SVT'.OR.WHCH(1:3).EQ.'svt') THEN
        ISVT=MAX(0,MIN(10,IVAL))
      ELSE IF (WHCH(1:3).EQ.'TBA'.OR.WHCH(1:3).EQ.'tba') THEN
        ITBM=IOR(ISHIFT(ISHIFT(ITBM,-12),12),IAND(IVAL,4095))
      ELSE IF (WHCH(1:3).EQ.'TBX'.OR.WHCH(1:3).EQ.'tbx') THEN
        ITBM=IOR(ISHIFT(IAND(IVAL,4095),12),IAND(ITBM,4095))
      ELSE IF (WHCH(1:3).EQ.'TTC'.OR.WHCH(1:3).EQ.'ttc') THEN
        ICTT=MAX(0,IVAL)
      ELSE IF (WHCH(1:3).EQ.'WSO'.OR.WHCH(1:3).EQ.'wso') THEN
        IWSO=MAX(0,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'ZFB'.OR.WHCH(1:3).EQ.'zfb') THEN
        IBZF=MAX(0,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'ZFC'.OR.WHCH(1:3).EQ.'zfc') THEN
        ICZF=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'ZFP'.OR.WHCH(1:3).EQ.'zfp') THEN
        IPZF=MAX(-4,MIN(4,IVAL))
      ELSE
C         Float the integer value and pass it on to VTSETR.
        RVAL=REAL(IVAL)
        CALL VTSETR (WHCH,RVAL)
        IF (ICFELL('VTSETI',2).NE.0) RETURN
      END IF
C
C Done.
C
      RETURN
C
      END

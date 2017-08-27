      SUBROUTINE VTMVIW (IWKO,IWKN,LWKN)
C
      DIMENSION IWKO(LIWK),IWKN(LWKN)
C
C This subroutine is called to move what VASPACKT has in the integer
C workspace array to a new array.  IWKO is the old array, IWKN the
C new one.  LWKN is the length of the new array.
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
C Declare local versions of the arrays used to keep track of workspace
C usage.
C
      DIMENSION LCLI(2),LCLL(2)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('VTMVIW - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C First, zero the local pointers and lengths and, at the same time,
C compute the total space required in the new array.
C
      ITMP=0
C
      DO 10001 I=1,2
        LCLI(I)=0
        LCLL(I)=0
        ITMP=ITMP+LIWS(I)
10001 CONTINUE
C
C If there isn't enough space available in the new array, log an error
C and quit.
C
      IF (ITMP.GT.LWKN) THEN
        CALL SETER ('VTMVIW - NEW WORKSPACE ARRAY IS TOO SMALL',2,1)
        RETURN
      END IF
C
C Zero an index into the new workspace array.
C
      IINW=0
C
C Now, the trick is to move the stuff without stepping on our own toes
C if the user gives us the same array as both the old and the new array.
C We move the blocks closer to the beginning of the array first.
C
10002 CONTINUE
 
        ITM1=0
        ITM2=LIWK
C
        DO 10003 I=1,2
          IF (LIWS(I).NE.0.AND.IIWS(I).LT.ITM2) THEN
            ITM1=I
            ITM2=IIWS(I)
          END IF
10003   CONTINUE
C
        IF (ITM1.NE.0) THEN
          DO 10004 J=1,LIWS(ITM1)
            IWKN(IINW+J)=IWKO(IIWS(ITM1)+J)
10004     CONTINUE
          LCLI(ITM1)=IINW
          LCLL(ITM1)=LIWS(ITM1)
          IIWS(ITM1)=0
          LIWS(ITM1)=0
          IINW=IINW+LCLL(ITM1)
        END IF
C
      IF (.NOT.(ITM1.EQ.0)) GO TO 10002
C
C Now, copy the local set of pointers and lengths to common.
C
      DO 10005 I=1,2
        IIWS(I)=LCLI(I)
        LIWS(I)=LCLL(I)
10005 CONTINUE
C
C Update the variable that says how much integer workspace we have.
C
      LIWK=LWKN
C
C Done.
C
      RETURN
C
      END

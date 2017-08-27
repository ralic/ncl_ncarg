      SUBROUTINE CTMVRW (RWKO,RWKN,LWKN)
C
      DIMENSION RWKO(LRWK),RWKN(LWKN)
C
C This subroutine is called to move what CONPACKT has in the real
C workspace array to a new array.  RWKO is the old array, RWKN the
C new one.  LWKN is the length of the new array.
C
C Declare all of the CONPACKT common blocks.
C
C
C CTCOM1 contains integer and real variables.
C
      COMMON /CTCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CTCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CTCOM1/ CLDT(256),CLEV(256),CLWA(258),CXCF
      COMMON /CTCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DMAX
      COMMON /CTCOM1/ DMIN,DOPT,DVAL,EPSI,FNCM,GRAV,GRSD,GSDM,HCHL
      COMMON /CTCOM1/ HCHS,HLSR,IAIA(258),IAIB(256),IBCF,IBHL
      COMMON /CTCOM1/ IBIL,IBLL,ICAF,ICCF,ICCL(258),ICFF,ICHI
      COMMON /CTCOM1/ ICHL,ICIL,ICLL(256),ICLO,ICLP(256),ICLS
      COMMON /CTCOM1/ ICLU(258),ICLV,ICLW,IDUF,IGCL,IGLB,IGRM
      COMMON /CTCOM1/ IGRN,IGVS,IHCF,IHLE,IIWS(2),IIWU,ILBC
      COMMON /CTCOM1/ IMPF,INCX(8),INCY(8),INHL,INIL,INIT,INLL
      COMMON /CTCOM1/ IOCF,IOHL,IOLL,IPAI,IPCF,IPIC,IPIE,IPIL,IPLL
      COMMON /CTCOM1/ IRWS(4),IRWU,ISET,ITBM,IWSO,JODP,JOMA
      COMMON /CTCOM1/ JOTZ,LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3,LINS
      COMMON /CTCOM1/ LINT(10),LINU,LIWB,LIWK,LIWM,LIWS(2),LNLG
      COMMON /CTCOM1/ LOEN,LOPN,LOTN,LRWC,LRWG,LRWK,LRWM,LRWS(4)
      COMMON /CTCOM1/ LSDD,LSDL,LSDM,LTCF,LTHI,LTIL,LTLO,MIRO
      COMMON /CTCOM1/ NCLB(256),NCLV,NDGL,NEDG,NEXL,NEXT,NEXU
      COMMON /CTCOM1/ NLBS,NLSD,NLZF,NOMF,NOVS,NPNT,NR04,NSDL
      COMMON /CTCOM1/ NSDR,NTRI,OORV,PITH,SCFS,SCFU,SEGL,T2DS
      COMMON /CTCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CTCOM1/ UWDR,UWDT,WCCF,WCHL,WCIL,WCLL,WLCF,WLHL,WLIL
      COMMON /CTCOM1/ WLLL,WOCH,WODA,WTCD,WTGR,WTNC,WTOD,WWCF,WWHL
      COMMON /CTCOM1/ WWIL,WWLL,XLBC,XMAX,XMIN,XVPL,XVPR,XWDL,XWDR
      COMMON /CTCOM1/ YLBC,YMAX,YMIN,YVPB,YVPT,YWDB,YWDT,ZMAX,ZMIN
C
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CTCOM1/
C
C CTCOM2 holds character parameters.
C
      COMMON /CTCOM2/ CHEX,CLBL(256),CLDP(258),CTMA,CTMB,FRMT
      COMMON /CTCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CTCOM2/
C
C Declare local versions of the arrays used to keep track of workspace
C usage.
C
      DIMENSION LCLI(4),LCLL(4)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTMVRW - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C First, zero the local pointers and lengths and, at the same time,
C compute the total space required in the new array.
C
      ITMP=0
C
      DO 10001 I=1,4
        LCLI(I)=0
        LCLL(I)=0
        ITMP=ITMP+LRWS(I)
10001 CONTINUE
C
C If there isn't enough space available in the new array, log an error
C and quit.
C
      IF (ITMP.GT.LWKN) THEN
        CALL SETER ('CTMVRW - NEW WORKSPACE ARRAY IS TOO SMALL',2,1)
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
        ITM2=LRWK
C
        DO 10003 I=1,4
          IF (LRWS(I).NE.0.AND.IRWS(I).LT.ITM2) THEN
            ITM1=I
            ITM2=IRWS(I)
          END IF
10003   CONTINUE
C
        IF (ITM1.NE.0) THEN
          DO 10004 J=1,LRWS(ITM1)
            RWKN(IINW+J)=RWKO(IRWS(ITM1)+J)
10004     CONTINUE
          LCLI(ITM1)=IINW
          LCLL(ITM1)=LRWS(ITM1)
          IRWS(ITM1)=0
          LRWS(ITM1)=0
          IINW=IINW+LCLL(ITM1)
        END IF
C
      IF (.NOT.(ITM1.EQ.0)) GO TO 10002
C
C Now, copy the local set of pointers and lengths to common.
C
      DO 10005 I=1,4
        IRWS(I)=LCLI(I)
        LRWS(I)=LCLL(I)
10005 CONTINUE
C
C Update the variable that says how much real workspace we have.
C
      LRWK=LWKN
C
C Done.
C
      RETURN
C
      END

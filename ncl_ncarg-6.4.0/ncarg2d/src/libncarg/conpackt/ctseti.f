      SUBROUTINE CTSETI (WHCH,IVAL)
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
C ------------------------------------------------------------------
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to CTSETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either ctseti() or ctsetr(), as in:
C        CALL CTSETI ('xxx',-9999)
C     or
C        CALL CTSETR ('xxx',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the WHCH, then we delegate over to CTSETR.
C -------------------------------------------------------
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(WHCH).LT.3) THEN
        CTMB(1:36)='CTSETI - PARAMETER NAME TOO SHORT - '
        CTMB(37:36+LEN(WHCH))=WHCH
        CALL SETER (CTMB(1:36+LEN(WHCH)),2,1)
        RETURN
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (WHCH(1:3).EQ.'AIA'.OR.WHCH(1:3).EQ.'aia'.OR.WHCH(1:3).EQ.'CLC'
     +.OR.WHCH(1:3).EQ.'clc'.OR.WHCH(1:3).EQ.'CLD'.OR.WHCH(1:3).EQ.'cld'
     +.OR.WHCH(1:3).EQ.'CLL'.OR.WHCH(1:3).EQ.'cll'.OR.WHCH(1:3).EQ.'CLU'
     +.OR.WHCH(1:3).EQ.'clu') THEN
        IF (IPAI.GE.1.AND.IPAI.LE.NCLV) THEN
          JPAI=IPAI
        ELSE IF (IPAI.LE.-1.AND.IPAI.GE.-3) THEN
          JPAI=256+ABS(IPAI)
        ELSE
          GO TO 10002
        END IF
      ELSE IF ((WHCH(1:3).EQ.'AIB'.OR.WHCH(1:3).EQ.'aib'.OR.WHCH(1:3).EQ
     +.'CLV'.OR.WHCH(1:3).EQ.'clv'.OR.WHCH(1:3).EQ.'LLC'.OR.WHCH(1:3).EQ
     +.'llc').AND.(IPAI.LT.1.OR.IPAI.GT.NCLV)) THEN
        GO TO 10002
      ELSE IF ((WHCH(1:3).EQ.'CIT'.OR.WHCH(1:3).EQ.'cit'.OR.WHCH(1:3).EQ
     +.'LIT'.OR.WHCH(1:3).EQ.'lit').AND.(IPAI.LT.1.OR.IPAI.GT.10)) THEN
        GO TO 10002
      END IF
C
      GO TO 10005
10002 CONTINUE
        CTMB(1:36)='CTSETI - SETTING XXX - PAI INCORRECT'
        CTMB(18:20)=WHCH(1:3)
        CALL SETER (CTMB(1:36),3,1)
        RETURN
10005 CONTINUE
C
C Set the appropriate parameter value.
C
      IF (WHCH(1:3).EQ.'AIA'.OR.WHCH(1:3).EQ.'aia') THEN
        IAIA(JPAI)=IVAL
      ELSE IF (WHCH(1:3).EQ.'AIB'.OR.WHCH(1:3).EQ.'aib') THEN
        IAIB(IPAI)=IVAL
      ELSE IF (WHCH(1:3).EQ.'CAF'.OR.WHCH(1:3).EQ.'caf') THEN
        ICAF=IVAL
      ELSE IF (WHCH(1:3).EQ.'CFB'.OR.WHCH(1:3).EQ.'cfb') THEN
        IBCF=MAX(0,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'CFC'.OR.WHCH(1:3).EQ.'cfc') THEN
        ICCF=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'CFP'.OR.WHCH(1:3).EQ.'cfp') THEN
        IPCF=MAX(-4,MIN(4,IVAL))
      ELSE IF (WHCH(1:3).EQ.'CLC'.OR.WHCH(1:3).EQ.'clc') THEN
        ICCL(JPAI)=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'CLD'.OR.WHCH(1:3).EQ.'cld') THEN
        CLDP(JPAI)=' '
        ITMP=IVAL
        DO 10006 I=16,1,-1
          IF (IAND(ITMP,1).NE.0) THEN
            CLDP(JPAI)(I:I)='$'
          ELSE
            CLDP(JPAI)(I:I)=''''
          END IF
          ITMP=ISHIFT(ITMP,-1)
10006   CONTINUE
      ELSE IF (WHCH(1:3).EQ.'CLS'.OR.WHCH(1:3).EQ.'cls') THEN
        ICLS=IVAL
      ELSE IF (WHCH(1:3).EQ.'CLU'.OR.WHCH(1:3).EQ.'clu') THEN
        ICLU(JPAI)=MAX(0,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'DPU'.OR.WHCH(1:3).EQ.'dpu') THEN
        IDUF=IVAL
      ELSE IF (WHCH(1:3).EQ.'GIC'.OR.WHCH(1:3).EQ.'gic') THEN
        IGCL=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'GIL'.OR.WHCH(1:3).EQ.'gil') THEN
        IGLB=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'GIS'.OR.WHCH(1:3).EQ.'gis') THEN
        IGVS=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'HCF'.OR.WHCH(1:3).EQ.'hcf') THEN
        IHCF=MAX(-4,MIN(+4,IVAL))
      ELSE IF (WHCH(1:3).EQ.'HIC'.OR.WHCH(1:3).EQ.'hic') THEN
        ICHI=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'HLB'.OR.WHCH(1:3).EQ.'hlb') THEN
        IBHL=MAX(0,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'HLC'.OR.WHCH(1:3).EQ.'hlc') THEN
        ICHL=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'HLE'.OR.WHCH(1:3).EQ.'hle') THEN
        IHLE=MAX(0,IVAL)
      ELSE IF (WHCH(1:3).EQ.'HLO'.OR.WHCH(1:3).EQ.'hlo') THEN
        IOHL=MAX(0,MIN(15,IVAL))
      ELSE IF (WHCH(1:3).EQ.'ILB'.OR.WHCH(1:3).EQ.'ilb') THEN
        IBIL=MAX(0,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'ILC'.OR.WHCH(1:3).EQ.'ilc') THEN
        ICIL=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'ILP'.OR.WHCH(1:3).EQ.'ilp') THEN
        IPIL=MAX(-4,MIN(4,IVAL))
      ELSE IF (WHCH(1:3).EQ.'IWB'.OR.WHCH(1:3).EQ.'iwb') THEN
        LIWB=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'IWM'.OR.WHCH(1:3).EQ.'iwm') THEN
        LIWM=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'LBC'.OR.WHCH(1:3).EQ.'lbc') THEN
        ILBC=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'LIS'.OR.WHCH(1:3).EQ.'lis') THEN
        LINS=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'LIT'.OR.WHCH(1:3).EQ.'lit') THEN
        LINT(IPAI)=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'LLB'.OR.WHCH(1:3).EQ.'llb') THEN
        IBLL=MAX(0,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'LLC'.OR.WHCH(1:3).EQ.'llc') THEN
        ICLL(IPAI)=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'LLO'.OR.WHCH(1:3).EQ.'llo') THEN
        IOLL=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:3).EQ.'LLP'.OR.WHCH(1:3).EQ.'llp') THEN
        IPLL=MAX(-3,MIN(3,IVAL))
      ELSE IF (WHCH(1:3).EQ.'LOC'.OR.WHCH(1:3).EQ.'loc') THEN
        ICLO=MAX(-1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'MAP'.OR.WHCH(1:3).EQ.'map') THEN
        IMPF=MAX(0,IVAL)
      ELSE IF (WHCH(1:3).EQ.'NCL'.OR.WHCH(1:3).EQ.'ncl') THEN
        NCLV=IVAL
        IF (NCLV.LT.1.OR.NCLV.GT.256) THEN
          CALL SETER ('CTSETI - NCL LESS THAN 1 OR GREATER THAN 256',4,1
     +)
          RETURN
        END IF
      ELSE IF (WHCH(1:3).EQ.'NEL'.OR.WHCH(1:3).EQ.'nel') THEN
        NEXL=IVAL
      ELSE IF (WHCH(1:3).EQ.'NET'.OR.WHCH(1:3).EQ.'net') THEN
        NEXT=MAX(0,MIN(2,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NEU'.OR.WHCH(1:3).EQ.'neu') THEN
        NEXU=IVAL
      ELSE IF (WHCH(1:3).EQ.'NLS'.OR.WHCH(1:3).EQ.'nls') THEN
        NLSD=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NLZ'.OR.WHCH(1:3).EQ.'nlz') THEN
        NLZF=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NOF'.OR.WHCH(1:3).EQ.'nof') THEN
        NOMF=MAX(0,MIN(7,IVAL))
      ELSE IF (WHCH(1:3).EQ.'NSD'.OR.WHCH(1:3).EQ.'nsd') THEN
        NSDL=IVAL
      ELSE IF (WHCH(1:3).EQ.'NVS'.OR.WHCH(1:3).EQ.'nvs') THEN
        NOVS=MAX(0,IVAL)
      ELSE IF (WHCH(1:3).EQ.'PAI'.OR.WHCH(1:3).EQ.'pai') THEN
        IPAI=IVAL
      ELSE IF (WHCH(1:3).EQ.'PIC'.OR.WHCH(1:3).EQ.'pic') THEN
        IPIC=IVAL
      ELSE IF (WHCH(1:3).EQ.'PIE'.OR.WHCH(1:3).EQ.'pie') THEN
        IPIE=IVAL
      ELSE IF (WHCH(1:3).EQ.'RWC'.OR.WHCH(1:3).EQ.'rwc') THEN
        LRWC=MAX(5,IVAL)
      ELSE IF (WHCH(1:3).EQ.'RWG'.OR.WHCH(1:3).EQ.'rwg') THEN
        LRWG=MAX(1,IVAL)
      ELSE IF (WHCH(1:3).EQ.'RWM'.OR.WHCH(1:3).EQ.'rwm') THEN
        LRWM=MAX(2,IVAL)
      ELSE IF (WHCH(1:3).EQ.'SET'.OR.WHCH(1:3).EQ.'set') THEN
        ISET=MAX(0,MIN(1,IVAL))
      ELSE IF (WHCH(1:3).EQ.'TBA'.OR.WHCH(1:3).EQ.'tba') THEN
        ITBM=IOR(ISHIFT(ISHIFT(ITBM,-12),12),IAND(IVAL,4095))
      ELSE IF (WHCH(1:3).EQ.'TBX'.OR.WHCH(1:3).EQ.'tbx') THEN
        ITBM=IOR(ISHIFT(IAND(IVAL,4095),12),IAND(ITBM,4095))
      ELSE IF (WHCH(1:3).EQ.'WSO'.OR.WHCH(1:3).EQ.'wso') THEN
        IWSO=MAX(0,MIN(3,IVAL))
      ELSE
C         Float the integer value and pass it on to CTSETR.
        RVAL=REAL(IVAL)
        CALL CTSETR (WHCH,RVAL)
        IF (ICFELL('CTSETI',2).NE.0) RETURN
      END IF
C
C Done.
C
      RETURN
C
      END

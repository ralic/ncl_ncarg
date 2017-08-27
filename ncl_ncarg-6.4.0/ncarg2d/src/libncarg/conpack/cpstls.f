      SUBROUTINE CPSTLS (ZDAT,RWRK,IWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C The routine CPSTLS is called to set the label-size parameters.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C
C Declare all of the CONPACK common blocks.
C
C
C CPCOM1 contains integer and real variables.
C
      COMMON /CPCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CPCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CPCOM1/ CLDT(256),CLEV(256),CLWA(259),CXCF
      COMMON /CPCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DOPT
      COMMON /CPCOM1/ EPSI,FNCM,GRAV,GRSD,GSDM,HCHL,HCHS,IAIA(259)
      COMMON /CPCOM1/ IAIB(256),IBCF,IBHL,IBIL,IBLL,ICAF,ICCF
      COMMON /CPCOM1/ ICCL(259),ICFF,ICHI,ICHL,ICIL,ICLL(256)
      COMMON /CPCOM1/ ICLO,ICLP(256),ICLS,ICLU(259),ICLV,ICLW
      COMMON /CPCOM1/ IDUF,IGCL,IGLB,IGRM,IGRN,IGVS,IHCF,IHLE,IHLX
      COMMON /CPCOM1/ IHLY,IIWS(2),IIWU,ILBC,IMPF,INCX(8),INCY(8)
      COMMON /CPCOM1/ INHL,INIL,INIT,INLL,IOCF,IOHL,IOLL,IPAI,IPCF
      COMMON /CPCOM1/ IPIC,IPIE,IPIL,IPLL,IRWS(4),IRWU,ISET,IWSO
      COMMON /CPCOM1/ IZD1,IZDM,IZDN,IZDS,JODP,JOMA,JOTZ,LCTM,LEA1
      COMMON /CPCOM1/ LEA2,LEA3,LEE1,LEE2,LEE3,LINS,LINT(10),LINU
      COMMON /CPCOM1/ LIWK,LIWM,LIWS(2),LNLG,LRWC,LRWG,LRWK
      COMMON /CPCOM1/ LRWM,LRWS(4),LSDD,LSDL,LSDM,LTCF,LTHI
      COMMON /CPCOM1/ LTIL,LTLO,MIRO,NCLB(256),NCLV,NDGL,NEXL
      COMMON /CPCOM1/ NEXT,NEXU,NLBS,NLSD,NLZF,NOMF,NOVS,NR04,NSDL
      COMMON /CPCOM1/ NSDR,OORV,PITH,SCFS,SCFU,SEGL,SVAL,T2DS,T3DS
      COMMON /CPCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CPCOM1/ UWDR,UWDT,UXA1,UXAM,UYA1,UYAN,WCCF,WCHL,WCIL
      COMMON /CPCOM1/ WCLL,WLCF,WLHL,WLIL,WLLL,WOCH,WODA,WTCD,WTGR
      COMMON /CPCOM1/ WTNC,WTOD,WWCF,WWHL,WWIL,WWLL,XAT1,XATM,XLBC
      COMMON /CPCOM1/ XVPL,XVPR,XWDL,XWDR,YAT1,YATN,YLBC,YVPB,YVPT
      COMMON /CPCOM1/ YWDB,YWDT,ZDVL,ZMAX,ZMIN
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CPCOM1/
C
C CPCOM2 holds character parameters.
C
      COMMON /CPCOM2/ CHEX,CLBL(256),CLDP(259),CTMA,CTMB,FRMT
      COMMON /CPCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CPCOM2/
C
C Fill in the internal parameters giving the number of characters in
C each label and its extent in four directions.
C
      DO 10001 ICLV=1,NCLV
        IF (MOD(ICLU(ICLV)/2,2).NE.0.AND.NCLB(ICLV).LE.0) THEN
          KCLB=MAX(1,ABS(NCLB(ICLV)))
          NCLB(ICLV)=KCLB
          XLBC=(XWDL+XWDR)/2.
          YLBC=(YWDB+YWDT)/2.
          SIZE=CHWM*WCLL*(XVPR-XVPL)
          WWSP=CHWM*WWLL*(XVPR-XVPL)
          CALL PCGETI ('TE',ITMP)
          IF (ICFELL('CPSTLS',1).NE.0) RETURN
          CALL PCSETI ('TE',1)
          IF (ICFELL('CPSTLS',2).NE.0) RETURN
          LCTM=KCLB
          CTMA(1:LCTM)=CLBL(ICLV)(1:KCLB)
          CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),SIZE,360.,0.)
          IF (ICFELL('CPSTLS',3).NE.0) RETURN
          CALL PCGETR ('DB',DSTB)
          IF (ICFELL('CPSTLS',4).NE.0) RETURN
          CALL PCGETR ('DL',DSTL)
          IF (ICFELL('CPSTLS',5).NE.0) RETURN
          CALL PCGETR ('DR',DSTR)
          IF (ICFELL('CPSTLS',6).NE.0) RETURN
          CALL PCGETR ('DT',DSTT)
          IF (ICFELL('CPSTLS',7).NE.0) RETURN
          CALL PCSETI ('TE',ITMP)
          IF (ICFELL('CPSTLS',8).NE.0) RETURN
          CLDB(ICLV)=DSTB+WWSP
          CLDL(ICLV)=DSTL+WWSP
          CLDR(ICLV)=DSTR+WWSP
          CLDT(ICLV)=DSTT+WWSP
        END IF
10001 CONTINUE
C
C Done.
C
      RETURN
C
      END
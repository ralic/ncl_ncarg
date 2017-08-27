      SUBROUTINE CPGETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C RVAL is a real variable in which the desired value is to be returned
C by CPGETR.
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
C Check for an uncleared prior error.
C
      IF (ICFELL('CPGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(WHCH).LT.3) THEN
        CTMB(1:36)='CPGETR - PARAMETER NAME TOO SHORT - '
        CTMB(37:36+LEN(WHCH))=WHCH
        CALL SETER (CTMB(1:36+LEN(WHCH)),2,1)
        RETURN
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (WHCH(1:3).EQ.'AIA'.OR.WHCH(1:3).EQ.'aia'.OR.WHCH(1:3).EQ.'CLC'
     +.OR.WHCH(1:3).EQ.'clc'.OR.WHCH(1:3).EQ.'CLL'.OR.WHCH(1:3).EQ.'cll'
     +.OR.WHCH(1:3).EQ.'CLU'.OR.WHCH(1:3).EQ.'clu') THEN
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
        CTMB(1:36)='CPGETR - GETTING XXX - PAI INCORRECT'
        CTMB(18:20)=WHCH(1:3)
        CALL SETER (CTMB(1:36),3,1)
        RETURN
10005 CONTINUE
C
C Get the appropriate parameter value.  (09/15/00) Because of a compiler
C problem on certain systems, the following long IF statement has been
C broken in two: we check for parameter names in the first half of the
C alphabet in one IF and for parameter names in the second half of the
C alphabet in another IF.
C
      IF (WHCH(1:3).EQ.'AIA'.OR.WHCH(1:3).EQ.'aia') THEN
        RVAL=REAL(IAIA(JPAI))
      ELSE IF (WHCH(1:3).EQ.'AIB'.OR.WHCH(1:3).EQ.'aib') THEN
        RVAL=REAL(IAIB(IPAI))
      ELSE IF (WHCH(1:3).EQ.'CAF'.OR.WHCH(1:3).EQ.'caf') THEN
        RVAL=REAL(ICAF)
      ELSE IF (WHCH(1:3).EQ.'CFA'.OR.WHCH(1:3).EQ.'cfa') THEN
        RVAL=ANCF
      ELSE IF (WHCH(1:3).EQ.'CFB'.OR.WHCH(1:3).EQ.'cfb') THEN
        RVAL=REAL(IBCF)
      ELSE IF (WHCH(1:3).EQ.'CFC'.OR.WHCH(1:3).EQ.'cfc') THEN
        RVAL=REAL(ICCF)
      ELSE IF (WHCH(1:3).EQ.'CFF'.OR.WHCH(1:3).EQ.'cff') THEN
        RVAL=ICFF
      ELSE IF (WHCH(1:3).EQ.'CFL'.OR.WHCH(1:3).EQ.'cfl') THEN
        RVAL=WLCF
      ELSE IF (WHCH(1:3).EQ.'CFP'.OR.WHCH(1:3).EQ.'cfp') THEN
        RVAL=REAL(IPCF)
      ELSE IF (WHCH(1:3).EQ.'CFS'.OR.WHCH(1:3).EQ.'cfs') THEN
        RVAL=WCCF
      ELSE IF (WHCH(1:3).EQ.'CFW'.OR.WHCH(1:3).EQ.'cfw') THEN
        RVAL=WWCF
      ELSE IF (WHCH(1:3).EQ.'CFX'.OR.WHCH(1:3).EQ.'cfx') THEN
        RVAL=CXCF
      ELSE IF (WHCH(1:3).EQ.'CFY'.OR.WHCH(1:3).EQ.'cfy') THEN
        RVAL=CYCF
      ELSE IF (WHCH(1:3).EQ.'CIS'.OR.WHCH(1:3).EQ.'cis') THEN
        RVAL=CINS
      ELSE IF (WHCH(1:3).EQ.'CIT'.OR.WHCH(1:3).EQ.'cit') THEN
        RVAL=CINT(IPAI)
      ELSE IF (WHCH(1:3).EQ.'CIU'.OR.WHCH(1:3).EQ.'ciu') THEN
        RVAL=CINU
      ELSE IF (WHCH(1:3).EQ.'CLC'.OR.WHCH(1:3).EQ.'clc') THEN
        RVAL=REAL(ICCL(JPAI))
      ELSE IF (WHCH(1:3).EQ.'CLL'.OR.WHCH(1:3).EQ.'cll') THEN
        RVAL=CLWA(JPAI)
      ELSE IF (WHCH(1:3).EQ.'CLS'.OR.WHCH(1:3).EQ.'cls') THEN
        RVAL=REAL(ICLS)
      ELSE IF (WHCH(1:3).EQ.'CLU'.OR.WHCH(1:3).EQ.'clu') THEN
        RVAL=REAL(ICLU(JPAI))
      ELSE IF (WHCH(1:3).EQ.'CLV'.OR.WHCH(1:3).EQ.'clv') THEN
        RVAL=CLEV(IPAI)
      ELSE IF (WHCH(1:3).EQ.'CMN'.OR.WHCH(1:3).EQ.'cmn') THEN
        RVAL=UCMN
      ELSE IF (WHCH(1:3).EQ.'CMX'.OR.WHCH(1:3).EQ.'cmx') THEN
        RVAL=UCMX
      ELSE IF (WHCH(1:3).EQ.'CWM'.OR.WHCH(1:3).EQ.'cwm') THEN
        RVAL=CHWM
      ELSE IF (WHCH(1:3).EQ.'DPS'.OR.WHCH(1:3).EQ.'dps') THEN
        RVAL=WOCH
      ELSE IF (WHCH(1:3).EQ.'DPU'.OR.WHCH(1:3).EQ.'dpu') THEN
        RVAL=REAL(IDUF)
      ELSE IF (WHCH(1:3).EQ.'DPV'.OR.WHCH(1:3).EQ.'dpv') THEN
        RVAL=WODA
      ELSE IF (WHCH(1:3).EQ.'GIC'.OR.WHCH(1:3).EQ.'gic') THEN
        RVAL=REAL(IGCL)
      ELSE IF (WHCH(1:3).EQ.'GIL'.OR.WHCH(1:3).EQ.'gil') THEN
        RVAL=REAL(IGLB)
      ELSE IF (WHCH(1:3).EQ.'GIS'.OR.WHCH(1:3).EQ.'gis') THEN
        RVAL=REAL(IGVS)
      ELSE IF (WHCH(1:3).EQ.'HCF'.OR.WHCH(1:3).EQ.'hcf') THEN
        RVAL=REAL(IHCF)
      ELSE IF (WHCH(1:3).EQ.'HCL'.OR.WHCH(1:3).EQ.'hcl') THEN
        RVAL=HCHL
      ELSE IF (WHCH(1:3).EQ.'HCS'.OR.WHCH(1:3).EQ.'hcs') THEN
        RVAL=HCHS
      ELSE IF (WHCH(1:3).EQ.'HIC'.OR.WHCH(1:3).EQ.'hic') THEN
        RVAL=REAL(ICHI)
      ELSE IF (WHCH(1:3).EQ.'HLA'.OR.WHCH(1:3).EQ.'hla') THEN
        RVAL=ANHL
      ELSE IF (WHCH(1:3).EQ.'HLB'.OR.WHCH(1:3).EQ.'hlb') THEN
        RVAL=REAL(IBHL)
      ELSE IF (WHCH(1:3).EQ.'HLC'.OR.WHCH(1:3).EQ.'hlc') THEN
        RVAL=REAL(ICHL)
      ELSE IF (WHCH(1:3).EQ.'HLE'.OR.WHCH(1:3).EQ.'hle') THEN
        RVAL=REAL(IHLE)
      ELSE IF (WHCH(1:3).EQ.'HLL'.OR.WHCH(1:3).EQ.'hll') THEN
        RVAL=WLHL
      ELSE IF (WHCH(1:3).EQ.'HLO'.OR.WHCH(1:3).EQ.'hlo') THEN
        RVAL=REAL(IOHL)
      ELSE IF (WHCH(1:3).EQ.'HLS'.OR.WHCH(1:3).EQ.'hls') THEN
        RVAL=WCHL
      ELSE IF (WHCH(1:3).EQ.'HLW'.OR.WHCH(1:3).EQ.'hlw') THEN
        RVAL=WWHL
      ELSE IF (WHCH(1:3).EQ.'HLX'.OR.WHCH(1:3).EQ.'hlx') THEN
        RVAL=REAL(IHLX)
      ELSE IF (WHCH(1:3).EQ.'HLY'.OR.WHCH(1:3).EQ.'hly') THEN
        RVAL=REAL(IHLY)
      ELSE IF (WHCH(1:3).EQ.'ILA'.OR.WHCH(1:3).EQ.'ila') THEN
        RVAL=ANIL
      ELSE IF (WHCH(1:3).EQ.'ILB'.OR.WHCH(1:3).EQ.'ilb') THEN
        RVAL=REAL(IBIL)
      ELSE IF (WHCH(1:3).EQ.'ILC'.OR.WHCH(1:3).EQ.'ilc') THEN
        RVAL=REAL(ICIL)
      ELSE IF (WHCH(1:3).EQ.'ILL'.OR.WHCH(1:3).EQ.'ill') THEN
        RVAL=WLIL
      ELSE IF (WHCH(1:3).EQ.'ILP'.OR.WHCH(1:3).EQ.'ilp') THEN
        RVAL=REAL(IPIL)
      ELSE IF (WHCH(1:3).EQ.'ILS'.OR.WHCH(1:3).EQ.'ils') THEN
        RVAL=WCIL
      ELSE IF (WHCH(1:3).EQ.'ILW'.OR.WHCH(1:3).EQ.'ilw') THEN
        RVAL=WWIL
      ELSE IF (WHCH(1:3).EQ.'ILX'.OR.WHCH(1:3).EQ.'ilx') THEN
        RVAL=CXIL
      ELSE IF (WHCH(1:3).EQ.'ILY'.OR.WHCH(1:3).EQ.'ily') THEN
        RVAL=CYIL
      ELSE IF (WHCH(1:3).EQ.'IWM'.OR.WHCH(1:3).EQ.'iwm') THEN
        RVAL=REAL(LIWM)
      ELSE IF (WHCH(1:3).EQ.'IWU'.OR.WHCH(1:3).EQ.'iwu') THEN
        RVAL=REAL(IIWU)
      ELSE IF (WHCH(1:3).EQ.'LBC'.OR.WHCH(1:3).EQ.'lbc') THEN
        RVAL=REAL(ILBC)
      ELSE IF (WHCH(1:3).EQ.'LBX'.OR.WHCH(1:3).EQ.'lbx') THEN
        RVAL=XLBC
      ELSE IF (WHCH(1:3).EQ.'LBY'.OR.WHCH(1:3).EQ.'lby') THEN
        RVAL=YLBC
      ELSE IF (WHCH(1:3).EQ.'LIS'.OR.WHCH(1:3).EQ.'lis') THEN
        RVAL=REAL(LINS)
      ELSE IF (WHCH(1:3).EQ.'LIT'.OR.WHCH(1:3).EQ.'lit') THEN
        RVAL=REAL(LINT(IPAI))
      ELSE IF (WHCH(1:3).EQ.'LIU'.OR.WHCH(1:3).EQ.'liu') THEN
        RVAL=REAL(LINU)
      ELSE IF (WHCH(1:3).EQ.'LLA'.OR.WHCH(1:3).EQ.'lla') THEN
        RVAL=ANLL
      ELSE IF (WHCH(1:3).EQ.'LLB'.OR.WHCH(1:3).EQ.'llb') THEN
        RVAL=REAL(IBLL)
      ELSE IF (WHCH(1:3).EQ.'LLC'.OR.WHCH(1:3).EQ.'llc') THEN
        RVAL=REAL(ICLL(IPAI))
      ELSE IF (WHCH(1:3).EQ.'LLL'.OR.WHCH(1:3).EQ.'lll') THEN
        RVAL=WLLL
      ELSE IF (WHCH(1:3).EQ.'LLO'.OR.WHCH(1:3).EQ.'llo') THEN
        RVAL=REAL(IOLL)
      ELSE IF (WHCH(1:3).EQ.'LLP'.OR.WHCH(1:3).EQ.'llp') THEN
        RVAL=REAL(IPLL)
      ELSE IF (WHCH(1:3).EQ.'LLS'.OR.WHCH(1:3).EQ.'lls') THEN
        RVAL=WCLL
      ELSE IF (WHCH(1:3).EQ.'LLW'.OR.WHCH(1:3).EQ.'llw') THEN
        RVAL=WWLL
      ELSE IF (WHCH(1:3).EQ.'LOC'.OR.WHCH(1:3).EQ.'loc') THEN
        RVAL=REAL(ICLO)
      ELSE IF (WHCH(1:3).EQ.'MAP'.OR.WHCH(1:3).EQ.'map') THEN
        RVAL=REAL(IMPF)
      ELSE
        GO TO 101
      END IF
C
C Done.
C
      RETURN
C
C Check parameter names in the second half of the alphabet.
C
  101 IF (WHCH(1:3).EQ.'NCL'.OR.WHCH(1:3).EQ.'ncl') THEN
        RVAL=REAL(NCLV)
      ELSE IF (WHCH(1:3).EQ.'NEL'.OR.WHCH(1:3).EQ.'nel') THEN
        RVAL=REAL(NEXL)
      ELSE IF (WHCH(1:3).EQ.'NET'.OR.WHCH(1:3).EQ.'net') THEN
        RVAL=REAL(NEXT)
      ELSE IF (WHCH(1:3).EQ.'NEU'.OR.WHCH(1:3).EQ.'neu') THEN
        RVAL=REAL(NEXU)
      ELSE IF (WHCH(1:3).EQ.'NLS'.OR.WHCH(1:3).EQ.'nls') THEN
        RVAL=REAL(NLSD)
      ELSE IF (WHCH(1:3).EQ.'NLZ'.OR.WHCH(1:3).EQ.'nlz') THEN
        RVAL=REAL(NLZF)
      ELSE IF (WHCH(1:3).EQ.'NOF'.OR.WHCH(1:3).EQ.'nof') THEN
        RVAL=REAL(NOMF)
      ELSE IF (WHCH(1:3).EQ.'NSD'.OR.WHCH(1:3).EQ.'nsd') THEN
        RVAL=REAL(NSDL)
      ELSE IF (WHCH(1:3).EQ.'NVS'.OR.WHCH(1:3).EQ.'nvs') THEN
        RVAL=REAL(NOVS)
      ELSE IF (WHCH(1:3).EQ.'ORV'.OR.WHCH(1:3).EQ.'orv') THEN
        RVAL=OORV
      ELSE IF (WHCH(1:3).EQ.'PAI'.OR.WHCH(1:3).EQ.'pai') THEN
        RVAL=REAL(IPAI)
      ELSE IF (WHCH(1:3).EQ.'PC1'.OR.WHCH(1:3).EQ.'pc1') THEN
        RVAL=GSDM
      ELSE IF (WHCH(1:3).EQ.'PC2'.OR.WHCH(1:3).EQ.'pc2') THEN
        RVAL=FNCM
      ELSE IF (WHCH(1:3).EQ.'PC3'.OR.WHCH(1:3).EQ.'pc3') THEN
        RVAL=CDMX
      ELSE IF (WHCH(1:3).EQ.'PC4'.OR.WHCH(1:3).EQ.'pc4') THEN
        RVAL=DOPT
      ELSE IF (WHCH(1:3).EQ.'PC5'.OR.WHCH(1:3).EQ.'pc5') THEN
        RVAL=DFLD
      ELSE IF (WHCH(1:3).EQ.'PC6'.OR.WHCH(1:3).EQ.'pc6') THEN
        RVAL=DBLM
      ELSE IF (WHCH(1:3).EQ.'PIC'.OR.WHCH(1:3).EQ.'pic') THEN
        RVAL=REAL(IPIC)
      ELSE IF (WHCH(1:3).EQ.'PIE'.OR.WHCH(1:3).EQ.'pie') THEN
        RVAL=REAL(IPIE)
      ELSE IF (WHCH(1:3).EQ.'PIT'.OR.WHCH(1:3).EQ.'pit') THEN
        RVAL=PITH
      ELSE IF (WHCH(1:3).EQ.'PW1'.OR.WHCH(1:3).EQ.'pw1') THEN
        RVAL=WTGR
      ELSE IF (WHCH(1:3).EQ.'PW2'.OR.WHCH(1:3).EQ.'pw2') THEN
        RVAL=WTNC
      ELSE IF (WHCH(1:3).EQ.'PW3'.OR.WHCH(1:3).EQ.'pw3') THEN
        RVAL=WTCD
      ELSE IF (WHCH(1:3).EQ.'PW4'.OR.WHCH(1:3).EQ.'pw4') THEN
        RVAL=WTOD
      ELSE IF (WHCH(1:3).EQ.'RC1'.OR.WHCH(1:3).EQ.'rc1') THEN
        RVAL=DBLF
      ELSE IF (WHCH(1:3).EQ.'RC2'.OR.WHCH(1:3).EQ.'rc2') THEN
        RVAL=DBLN
      ELSE IF (WHCH(1:3).EQ.'RC3'.OR.WHCH(1:3).EQ.'rc3') THEN
        RVAL=DBLV
      ELSE IF (WHCH(1:3).EQ.'RWC'.OR.WHCH(1:3).EQ.'rwc') THEN
        RVAL=REAL(LRWC)
      ELSE IF (WHCH(1:3).EQ.'RWG'.OR.WHCH(1:3).EQ.'rwg') THEN
        RVAL=REAL(LRWG)
      ELSE IF (WHCH(1:3).EQ.'RWM'.OR.WHCH(1:3).EQ.'rwm') THEN
        RVAL=REAL(LRWM)
      ELSE IF (WHCH(1:3).EQ.'RWU'.OR.WHCH(1:3).EQ.'rwu') THEN
        RVAL=REAL(IRWU)
      ELSE IF (WHCH(1:3).EQ.'SET'.OR.WHCH(1:3).EQ.'set') THEN
        RVAL=REAL(ISET)
      ELSE IF (WHCH(1:3).EQ.'SFS'.OR.WHCH(1:3).EQ.'sfs') THEN
        RVAL=SCFS
      ELSE IF (WHCH(1:3).EQ.'SFU'.OR.WHCH(1:3).EQ.'sfu') THEN
        RVAL=SCFU
      ELSE IF (WHCH(1:3).EQ.'SPV'.OR.WHCH(1:3).EQ.'spv') THEN
        RVAL=SVAL
      ELSE IF (WHCH(1:3).EQ.'SSL'.OR.WHCH(1:3).EQ.'ssl') THEN
        RVAL=SEGL
      ELSE IF (WHCH(1:3).EQ.'T2D'.OR.WHCH(1:3).EQ.'t2d') THEN
        RVAL=T2DS
      ELSE IF (WHCH(1:3).EQ.'T3D'.OR.WHCH(1:3).EQ.'t3d') THEN
        RVAL=T3DS
      ELSE IF (WHCH(1:3).EQ.'VPB'.OR.WHCH(1:3).EQ.'vpb') THEN
        RVAL=UVPB
      ELSE IF (WHCH(1:3).EQ.'VPL'.OR.WHCH(1:3).EQ.'vpl') THEN
        RVAL=UVPL
      ELSE IF (WHCH(1:3).EQ.'VPR'.OR.WHCH(1:3).EQ.'vpr') THEN
        RVAL=UVPR
      ELSE IF (WHCH(1:3).EQ.'VPS'.OR.WHCH(1:3).EQ.'vps') THEN
        RVAL=UVPS
      ELSE IF (WHCH(1:3).EQ.'VPT'.OR.WHCH(1:3).EQ.'vpt') THEN
        RVAL=UVPT
      ELSE IF (WHCH(1:3).EQ.'WDB'.OR.WHCH(1:3).EQ.'wdb') THEN
        RVAL=UWDB
      ELSE IF (WHCH(1:3).EQ.'WDL'.OR.WHCH(1:3).EQ.'wdl') THEN
        RVAL=UWDL
      ELSE IF (WHCH(1:3).EQ.'WDR'.OR.WHCH(1:3).EQ.'wdr') THEN
        RVAL=UWDR
      ELSE IF (WHCH(1:3).EQ.'WDT'.OR.WHCH(1:3).EQ.'wdt') THEN
        RVAL=UWDT
      ELSE IF (WHCH(1:3).EQ.'WSO'.OR.WHCH(1:3).EQ.'wso') THEN
        RVAL=REAL(IWSO)
      ELSE IF (WHCH(1:3).EQ.'XC1'.OR.WHCH(1:3).EQ.'xc1') THEN
        RVAL=UXA1
      ELSE IF (WHCH(1:3).EQ.'XCM'.OR.WHCH(1:3).EQ.'xcm') THEN
        RVAL=UXAM
      ELSE IF (WHCH(1:3).EQ.'YC1'.OR.WHCH(1:3).EQ.'yc1') THEN
        RVAL=UYA1
      ELSE IF (WHCH(1:3).EQ.'YCN'.OR.WHCH(1:3).EQ.'ycn') THEN
        RVAL=UYAN
      ELSE IF (WHCH(1:3).EQ.'ZD1'.OR.WHCH(1:3).EQ.'zd1') THEN
        RVAL=REAL(IZD1)
      ELSE IF (WHCH(1:3).EQ.'ZDM'.OR.WHCH(1:3).EQ.'zdm') THEN
        RVAL=REAL(IZDM)
      ELSE IF (WHCH(1:3).EQ.'ZDN'.OR.WHCH(1:3).EQ.'zdn') THEN
        RVAL=REAL(IZDN)
      ELSE IF (WHCH(1:3).EQ.'ZDS'.OR.WHCH(1:3).EQ.'zds') THEN
        RVAL=REAL(IZDS)
      ELSE IF (WHCH(1:3).EQ.'ZDV'.OR.WHCH(1:3).EQ.'zdv') THEN
        RVAL=ZDVL
      ELSE IF (WHCH(1:3).EQ.'ZMN'.OR.WHCH(1:3).EQ.'zmn') THEN
        RVAL=ZMIN
      ELSE IF (WHCH(1:3).EQ.'ZMX'.OR.WHCH(1:3).EQ.'zmx') THEN
        RVAL=ZMAX
      ELSE
        CTMB(1:36)='CPGETR - PARAMETER NAME NOT KNOWN - '
        CTMB(37:39)=WHCH(1:3)
        CALL SETER (CTMB(1:39),4,1)
        RETURN
      END IF
C
C Done.
C
      RETURN
C
      END

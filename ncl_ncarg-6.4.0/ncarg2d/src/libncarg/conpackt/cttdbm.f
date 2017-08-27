      SUBROUTINE CTTDBM (IHBX,IEBX,IWBX,IUBX,IHBA,IEBA,IWBA,IUBA)
C
C This routine assumes that TDPACK routines are being used to map the
C triangular mesh from 3-space into 2-space.  It sets the triangle
C blocking mask parameter ITBM as directed by the user.  All arguments
C have one of the two values 0 or 1 and are as follows:
C
C IHBX, IEBX, IWBX, and IUBX are set to zero to leave a particular bit
C of a blocking flag unchanged or to one to toggle that bit.  IHBX
C is associated with the bit that says a triangle is hidden by other
C triangles, IEBX with the bit that says a triangle is nearly edge-on
C to the line of sight, IWBX with the bit that says a triangle is on
C the "wrong" side of the mesh, and IUBX with the bit that says a
C triangle is blocked by the user.
C
C IHBA, IEBA, IWBA, and IUBA are set to zero to ignore a particular bit
C of a blocking flag unchanged or to one to examine that bit.  IHBA
C is associated with the bit that says a triangle is hidden by other
C triangles, IEBA with the bit that says a triangle is nearly edge-on
C to the line of sight, IWBA with the bit that says a triangle is on
C the "wrong" side of the mesh, and IUBA with the bit that says a
C triangle is blocked by the user.
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
C
C The variables in the following common block define TDPACK's mapping
C from 3-space to 2-space.
C
      COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
      COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
      COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
      SAVE   /TDCOM1/
C
C Create the proper values of ITBX and ITBA, depending on which eye is
C in use.
C
      IF (OE.LT.0.) THEN
        ITBX=64*IAND(IHBX,1)+
     +       32*IAND(IEBX,1)+
     +       16*IAND(IWBX,1)+
     +          IAND(IUBX,1)
        ITBA=64*IAND(IHBA,1)+
     +       32*IAND(IEBA,1)+
     +       16*IAND(IWBA,1)+
     +          IAND(IUBA,1)
      ELSE
        ITBX= 8*IAND(IHBX,1)+
     +        4*IAND(IEBX,1)+
     +        2*IAND(IWBX,1)+
     +          IAND(IUBX,1)
        ITBA= 8*IAND(IHBA,1)+
     +        4*IAND(IEBA,1)+
     +        2*IAND(IWBA,1)+
     +          IAND(IUBA,1)
      END IF
C
C Pack the parameter values into the variable that holds them.
C
      ITBM=IOR(ISHIFT(ITBX,12),ITBA)
C
C Done.
C
      RETURN
C
      END

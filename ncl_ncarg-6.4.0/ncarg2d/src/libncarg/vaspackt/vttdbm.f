      SUBROUTINE VTTDBM (IHBX,IEBX,IWBX,IUBX,IHBA,IEBA,IWBA,IUBA)
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

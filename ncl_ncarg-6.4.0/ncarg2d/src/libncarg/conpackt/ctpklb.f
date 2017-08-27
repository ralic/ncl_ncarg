      SUBROUTINE CTPKLB (RPNT,IEDG,ITRI,RWRK,IWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C The routine CTPKLB is called to pick the labels to be associated with
C the contour levels.
C
C RPNT is an array of nodes defining vertices of triangles.
C
C IEDG is an array of nodes defining edges of triangles.
C
C ITRI is an array of nodes defining triangles.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
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
C
C SCHR is a one-character temporary variable.
C
      CHARACTER*1 SCHR
C
C SCHX is a thirteen-character temporary variable.
C
      CHARACTER*13 SCHX
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTPKLB - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTPKLB - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C If the constant-field flag is set, do nothing.
C
      IF (ICFF.NE.0) RETURN
C
C If no contour levels are defined, try to define them.
C
      IF (NCLV.LE.0) THEN
        CALL CTPKCL (RPNT,IEDG,ITRI,RWRK,IWRK)
        IF (ICFELL('CTPKLB',3).NE.0) RETURN
      END IF
C
C Get indices for the contour levels in ascending order.
C
      IF (NCLV.GT.0) CALL CTSORT (CLEV,NCLV,ICLP)
C
C Find the positions of the leftmost and rightmost digits in the
C character representations of all the contour levels.
C
      MINI=+10000
      MAXI=-10000
      ITMP=0
C
      DO 10001 ICLV=1,NCLV
        IF (MOD(ICLU(ICLV)/2,2).NE.0.AND.CLBL(ICLV).EQ.' ') THEN
          ITMP=ITMP+1
          CALL CTNUMB (CLEV(ICLV)/SCFU,NDGL,-10000,-1,-1,' ',' ',' ',
     +                                0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
          IF (SCHR.NE.'0') THEN
            MINI=MIN(MINI,IEVA-NDGS)
            MAXI=MAX(MAXI,IEVA-1)
          END IF
        END IF
10001 CONTINUE
C
C If no unset contour labels were found, quit.  There are no labels to
C be filled in and no information on which to base the selection of a
C scale factor.  CTPKLB has probably been called needlessly for a second
C time.
C
      IF (ITMP.EQ.0) RETURN
C
C If no meaningful information was found about the position of digits
C in the contour levels (which probably means there was only one
C unspecified label and it should be just a zero), find the position
C of the leftmost digit in the minimum and maximum values and use it.
C
      IF (MINI.GT.MAXI) THEN
        CALL CTNUMB (MAX(ABS(DMIN),ABS(DMAX))/SCFU,NDGL,-10000,-1,-1,
     +                    ' ',' ',' ',0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
        MINI=IEVA
        MAXI=IEVA
      END IF
C
C If the leftmost digit in the contour levels is too far to the right
C of the digit to be considered the leftmost significant digit while
C generating labels, increase the number of digits to be used from that
C point rightward.  This may result in recomputing the scale factor and
C other dependent quantities.
C
      IF (MAXI.LT.LSDL-1) THEN
        NDGL=NDGL+LSDL-MAXI
        IF (SCFS.LE.0..AND.SCFS.GE.-3.) THEN
          SCFO=SCFU
          ITMP=0
          IF (SCFS.EQ.0..OR.(SCFS.EQ.-3..AND.LSDM.LT.-1)) ITMP=LSDM+1
          IF (SCFS.EQ.-1.) ITMP=LSDM
          IF (SCFS.EQ.-2..OR.(SCFS.EQ.-3..AND.LSDM-NDGL.GE.0))
     +                                                ITMP=LSDM-NDGL+1
          SCFU=10.**ITMP
          LSDL=LSDM-ITMP
          ITMP=NINT(ALOG10(SCFO/SCFU))
          MINI=MINI+ITMP
          MAXI=MAXI+ITMP
        END IF
      END IF
C
C Determine the number of significant digits to be used for the contour
C labels.
C
      NSDU=MIN(MAX(LSDL,MAXI)-MINI+1,NDGL)
C
C If the scale factor is to be based on contour-level values, compute
C it now.
C
      IF (SCFS.EQ.-4.) THEN
        IF (MINI*(MAXI+1).GT.0) THEN
          SCFU=10.**MINI
          IF (LSDL.NE.-10000) LSDL=LSDL-MINI
        END IF
      END IF
C
C Generate labels for those contour lines which will be labelled.
C
      ISNX=0
      IF (ABS(IPLL).EQ.1.AND.NEXT.EQ.1.AND.IDUF.GT.0) THEN
        ISNX=1
        NEXT=0
        SCHX=CHEX
        CHEX=' E '
        LEA1=1
        LEA2=1
        LEA3=1
        LEE1=0
        LEE2=1
        LEE3=0
      END IF
C
      DO 10002 ICLV=1,NCLV
        IF (MOD(ICLU(ICLV)/2,2).NE.0.AND.CLBL(ICLV).EQ.' ') THEN
          CALL CTNUMB (CLEV(ICLV)/SCFU,NSDU,LSDL,NEXU,NEXL,
     +                 CHEX(1:LEA1),CHEX(LEA1+1:LEA1+LEA2),
     +                 CHEX(LEA1+LEA2+1:LEA1+LEA2+LEA3),LEE1,LEE2,
     +                 LEE3,JOMA,JODP,JOTZ,CLBL(ICLV),NCHS,NDGS,
     +                                                     IEVA)
          NCLB(ICLV)=-NCHS
        END IF
10002 CONTINUE
C
      IF (ISNX.NE.0) THEN
        NEXT=1
        CHEX=SCHX
        LEA1=5
        LEA2=5
        LEA3=3
        LEE1=1
        LEE2=2
        LEE3=0
      END IF
C
C Done.
C
      RETURN
C
      END
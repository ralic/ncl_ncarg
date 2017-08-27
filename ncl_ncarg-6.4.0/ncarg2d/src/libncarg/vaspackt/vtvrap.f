      SUBROUTINE VTVRAP (UCEN,VCEN,WCEN,DCLU,DCLV,DCLW,DCZU,DCZV,DCZW,
     +                   NVEC,VMIN,VMAX,ICOL,CMIN,CMAX,IAMA,RTPL)
C
      DIMENSION IAMA(*)
C
      EXTERNAL RTPL
C
C Draw a "vector rose" at an arbitrary position in 3-space - centered
C at (UCEN,VCEN,WCEN), perpendicular to a line with direction cosines
C (DCNU,DCNV,DCNW), and having a zero line with direction cosines
C (DCZU,DCZV,DCZW).
C
C NVEC specifies the number of vectors to draw.
C
C VMIN and VMAX are minimum and maximum lengths of vectors to be
C drawn in each of the NVEC directions; if VMIN = VMAX, only one set
C of vectors will be drawn, except when both are zero, when the lengths
C used will be computed from the current value of the VASPACKT
C parameter 'VRL'.
C
C If ICOL is zero, the vectors drawn in each of the NVEC directions; if
C VMIN = VMAX, only one set of vectors will be drawn.  If ICOL is zero,
C the vectors are drawn in the color implied by the current value of
C the polyline color index, but, if ICOL is non-zero, the values in
C CMIN and CMAX are used to determine their colors; in either case,
C CMIN and CMAX must contain valid real values that will not cause
C arithmetic problems.
C
C IAMA is an array containing an area map against which the rose is to
C be masked.  If masking is not desired, set IAMA(1) = 0.
C
C RTPL is a routine to be called to draw the rose (when it is masked).
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
C Declare arrays for use in calling VTCUDR.
C
      DIMENSION UCRV(2),VCRV(2),WCRV(2),CCRV(2)
C
C Define a constant used to convert from degrees to radians.
C
      DATA DTOR / .017453292519943 /
C
C Compute "realized" values of various vector parameters.
C
      CALL VTRVSP
C
C Transfer the direction cosines of the normal to COMMON for use by
C TDCURV.
C
      DCNU=DCLU
      DCNV=DCLV
      DCNW=DCLW
C
C Compute the direction cosines of a vector perpendicular to both the
C normal and the zero line of the wind rose.
C
      DCPU=DCZV*DCLW-DCZW*DCLV
      DCPV=DCZW*DCLU-DCZU*DCLW
      DCPW=DCZU*DCLV-DCZV*DCLU
C
C The first point for each vector drawn is the same and the colors to
C be used are the same.
C
      UCRV(1)=UCEN
      VCRV(1)=VCEN
      WCRV(1)=WCEN
      CCRV(1)=CMAX
      CCRV(2)=CMAX
C
C Determine "realized" values of VMIN and VMAX.
C
      IF (VMIN.EQ.0..AND.VMAX.EQ.0.) THEN
        RMIN=VFRA*VRLR
        RMAX=VRLR
      ELSE
        RMIN=VMIN
        RMAX=VMAX
      END IF
C
C Loop to draw the NVEC vectors of maximum length.
C
      DO 101 I=1,NVEC
      ANGD=(REAL(I-1)/REAL(NVEC))*360.
      ANGR=DTOR*ANGD
      SINA=SIN(ANGR)
      COSA=COS(ANGR)
      UCRV(2)=UCEN+RMAX*(SINA*DCPU+COSA*DCZU)
      VCRV(2)=VCEN+RMAX*(SINA*DCPV+COSA*DCZV)
      WCRV(2)=WCEN+RMAX*(SINA*DCPW+COSA*DCZW)
      CALL VTCUDR (UCRV,VCRV,WCRV,CCRV,2,ICOL,1,IAMA,RTPL)
  101 CONTINUE
C
C If requested, loop to draw the NVEC vectors of minimum length.
C
      IF (RMIN.NE.RMAX) THEN
C
        CCRV(1)=CMIN
        CCRV(2)=CMIN
C
        DO 102 I=1,NVEC
        ANGD=(REAL(I-1)/REAL(NVEC))*360.
        ANGR=DTOR*ANGD
        SINA=SIN(ANGR)
        COSA=COS(ANGR)
        UCRV(2)=UCEN+RMIN*(SINA*DCPU+COSA*DCZU)
        VCRV(2)=VCEN+RMIN*(SINA*DCPV+COSA*DCZV)
        WCRV(2)=WCEN+RMIN*(SINA*DCPW+COSA*DCZW)
        CALL VTCUDR (UCRV,VCRV,WCRV,CCRV,2,ICOL,1,IAMA,RTPL)
  102   CONTINUE
C
      END IF
C
C Done.
C
      RETURN
C
      END

      SUBROUTINE VTCUDR (UCRV,VCRV,WCRV,CCRV,NCRV,ICOL,IARH,IAMA,RTPL)
C
      DIMENSION UCRV(NCRV),VCRV(NCRV),WCRV(NCRV),CCRV(NCRV),IAMA(*)
C
      EXTERNAL RTPL
C
C Draw the projection of a curve in 3-space, as defined by the points
C (UCRV(I),VCRV(I),WCRV(I)), for I from 1 to NCRV.  For each I from 1
C to NCRV, CCRV(I) is the value of some real quantity associated with
C point I of the curve.
C
C If ICOL is zero, the entire curve is drawn in the color implied by
C the current value of the polyline color index, but, if ICOL is
C non-zero, the values in CCRV are used to determine its color; in
C either case, CCRV must contain valid real values that will not cause
C arithmetic problems.
C
C If IARH is non-zero, VTCUDR will draw the projection of an arrowhead
C at one end of the curve (at the beginning of it if IARH is negative,
C or at the end of it if IARH is positive).  What is used is a simple
C arrowhead, of length AHLR and half-width AHHR (both of which are
C computed from AHLN and AHAW, in COMMON), lying in a plane
C perpendicular to both the unit vector having components DCNU, DCNV,
C and DCNW, and to the terminal segment of the curve.
C
C IAMA is an array containing an area map against which the curve is to
C be masked.  If masking is not desired, set IAMA(1) = 0.
C
C RTPL is a routine to be called to draw the curve (when it is masked).
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
C Declare local arrays for user-system polyline coordinates.
C
      PARAMETER (MCPL=100)
      DIMENSION XCPL(MCPL),YCPL(MCPL)
C
C Declare local arrays to use in drawing masked polylines.
C
      PARAMETER (MCPF=MCPL,MNOG=64)
      DIMENSION XCPF(MCPF),YCPF(MCPF),IAAI(MNOG),IAGI(MNOG)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('VTCUDR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Compute the value of AHHR.
C
      AHHR=AHLR*TAN(.017453292519943*AHAW/2.)
C
C If the curve is to be colored according to the values in CCRV, save
C the current polyline color.
C
      IF (.NOT.(ICOL.NE.0.AND.ICTV.NE.0.AND.NCLR.NE.0)) GO TO 10001
        CALL GQPLCI (IGER,IPCS)
        IF (.NOT.(IGER.NE.0)) GO TO 10002
          CALL SETER ('VTCUDR - ERROR EXIT FROM GQPLCI',2,1)
          RETURN
10002   CONTINUE
        IPCC=IPCS
        ICVL=(NCLR+1)/2
      GO TO 10003
10001 CONTINUE
        IPCS=-1
        IPCC=-1
        IPCD=-1
10003 CONTINUE
C
C Set some tolerances.
C
      EPSX=ABS(XWDR-XWDL)*EPSI
      EPSY=ABS(YWDT-YWDB)*EPSI
C
      SMLX=.01*ABS(XWDR-XWDL)
      SMLY=.01*ABS(YWDT-YWDB)
C
      PITX=PITH*ABS(XWDR-XWDL)
      PITY=PITH*ABS(YWDT-YWDB)
C
C Extract the data coordinates of the first point on the curve.
C
      UCND=UCRV(1)
      VCND=VCRV(1)
      WCND=WCRV(1)
      CCND=CCRV(1)
C
C Map the point (UCND,VCND,WCND) to the position (XCNU,YCNU).
C
      L10005=    1
      GO TO 10005
10004 CONTINUE
C
C Zero the number of points in the coordinate arrays and zero the
C variable that keeps track of the ratio of segment length in the user
C coordinate system to segment length in the data coordinate system.
C
      NCPL=0
      RUDN=0.
C
C Process the rest of the points on the curve.
C
        IPNT = 2
        GO TO 10008
10006   CONTINUE
        IPNT =IPNT +1
10008   CONTINUE
        IF (IPNT .GT.(NCRV)) GO TO 10007
C
C Save the coordinates of the previous point on the curve and compute
C coordinates of a new one.
C
        UCOD=UCND
        VCOD=VCND
        WCOD=WCND
        CCOD=CCND
C
        XCOU=XCNU
        YCOU=YCNU
C
        IVOU=IVNU
C
        UCND=UCRV(IPNT)
        VCND=VCRV(IPNT)
        WCND=WCRV(IPNT)
        CCND=CCRV(IPNT)
C
C Map the point (UCND,VCND,WCND) to the position (XCNU,YCNU).
C
        L10005=    2
        GO TO 10005
10009   CONTINUE
C
C Deal with the line segment from the previous point to the new one.
C
        L10011=    1
        GO TO 10011
10010   CONTINUE
C
C Loop back to find the next point on the curve.
C
      GO TO 10006
10007 CONTINUE
C
C Process any remaining portion of the curve.
C
      L10013=    1
      GO TO 10013
10012 CONTINUE
C
C If an arrowhead is to be drawn at one end of the curve, do that.
C
      IF (.NOT.(NCRV.GE.2.AND.IARH.NE.0)) GO TO 10014
        IF (.NOT.(IARH.LT.0)) GO TO 10015
          UEND=UCRV(1)
          VEND=VCRV(1)
          WEND=WCRV(1)
          UBEG=UCRV(2)
          VBEG=VCRV(2)
          WBEG=WCRV(2)
          CCOD=CCRV(1)
          CCND=CCRV(1)
        GO TO 10016
10015   CONTINUE
          UEND=UCRV(NCRV)
          VEND=VCRV(NCRV)
          WEND=WCRV(NCRV)
          UBEG=UCRV(NCRV-1)
          VBEG=VCRV(NCRV-1)
          WBEG=WCRV(NCRV-1)
          CCOD=CCRV(NCRV)
          CCND=CCRV(NCRV)
10016   CONTINUE
        DNOM=SQRT((UEND-UBEG)**2+(VEND-VBEG)**2+(WEND-WBEG)**2)
        IF (.NOT.(DNOM.NE.0.)) GO TO 10017
          UDCC=(UEND-UBEG)/DNOM
          VDCC=(VEND-VBEG)/DNOM
          WDCC=(WEND-WBEG)/DNOM
          UDCP=VDCC*DCNW-WDCC*DCNV
          VDCP=WDCC*DCNU-UDCC*DCNW
          WDCP=UDCC*DCNV-VDCC*DCNU
          UACC=UEND-AHLR*UDCC
          VACC=VEND-AHLR*VDCC
          WACC=WEND-AHLR*WDCC
          UCND=UACC-AHHR*UDCP
          VCND=VACC-AHHR*VDCP
          WCND=WACC-AHHR*WDCP
          L10005=    3
          GO TO 10005
10018     CONTINUE
          UCOD=UCND
          VCOD=VCND
          WCOD=WCND
          XCOU=XCNU
          YCOU=YCNU
          IVOU=IVNU
          UCND=UEND
          VCND=VEND
          WCND=WEND
          L10005=    4
          GO TO 10005
10019     CONTINUE
          L10011=    2
          GO TO 10011
10020     CONTINUE
          UCOD=UCND
          VCOD=VCND
          WCOD=WCND
          XCOU=XCNU
          YCOU=YCNU
          IVOU=IVNU
          UCND=UACC+AHHR*UDCP
          VCND=VACC+AHHR*VDCP
          WCND=WACC+AHHR*WDCP
          L10005=    5
          GO TO 10005
10021     CONTINUE
          L10011=    3
          GO TO 10011
10022     CONTINUE
10017   CONTINUE
        L10013=    2
        GO TO 10013
10023   CONTINUE
10014 CONTINUE
C
C If the curve was colored according to the values in CCRV, restore the
C saved polyline color.
C
  101 IF (IPCS.GE.0) CALL GSPLCI (IPCS)
C
C Done.
C
      RETURN
C
C The following internal procedure, given a line segment, adds visible
C portions of it to the coordinate arrays.
C
10011 CONTINUE
C
C If point interpolation is turned on, do the first IPIS segments.
C
        IF (.NOT.(IPIS.NE.0)) GO TO 10024
          USOD=UCOD
          VSOD=VCOD
          WSOD=WCOD
          CSOD=CCOD
          USND=UCND
          VSND=VCND
          WSND=WCND
          CSND=CCND
          XSNU=XCNU
          YSNU=YCNU
          ISNU=IVNU
            INTP = 1
            GO TO 10027
10025       CONTINUE
            INTP =INTP +1
10027       CONTINUE
            IF (INTP .GT.(ABS(IPIS))) GO TO 10026
            UCND=USOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(USND-USOD)
            VCND=VSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(VSND-VSOD)
            WCND=WSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(WSND-WSOD)
            CCND=CSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(CSND-CSOD)
            L10005=    6
            GO TO 10005
10028       CONTINUE
            IF (.NOT.(IPIS.GT.0.OR.IVNU.NE.IVOU)) GO TO 10029
              L10031=    1
              GO TO 10031
10030         CONTINUE
              UCOD=UCND
              VCOD=VCND
              WCOD=WCND
              CCOD=CCND
              XCOU=XCNU
              YCOU=YCNU
              IVOU=IVNU
10029       CONTINUE
          GO TO 10025
10026     CONTINUE
          UCND=USND
          VCND=VSND
          WCND=WSND
          CCND=CSND
          XCNU=XSNU
          YCNU=YSNU
          IVNU=ISNU
10024   CONTINUE
C
C Finish off the job.
C
        L10031=    2
        GO TO 10031
10032   CONTINUE
C
      GO TO (10010,10020,10022) , L10011
C
C The following internal procedure examines the points (UCOD,VCOD,WCOD),
C which projects into (XCOU,YCOU), and (UCND,VCND,WCND), which projects
C into (XCNU,YCNU), either of which may be visible or invisible in the
C projection space, and adds visible portions of the line segment
C between them to the polyline being built.
C
10031 CONTINUE
C
        IF (.NOT.(UCND.NE.UCOD.OR.VCND.NE.VCOD.OR.WCND.NE.WCOD))
     +  GO TO 10033
C
          IF (.NOT.(NCPL.EQ.0)) GO TO 10034
            IF (.NOT.(IVOU.NE.0)) GO TO 10035
              IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10036
                UCLD=UCOD
                VCLD=VCOD
                WCLD=WCOD
                CCLD=CCOD
                XCLU=XCOU
                YCLU=YCOU
10036         CONTINUE
              NCPL=1
              XCPL(1)=XCOU
              YCPL(1)=YCOU
              IF (.NOT.(IPCS.GE.0)) GO TO 10037
                CVAL=CCOD
                L10039=    1
                GO TO 10039
10038           CONTINUE
                IPCD=IPCI
10037         CONTINUE
            GO TO 10040
10035       CONTINUE
            IF (.NOT.(IVNU.NE.0)) GO TO 10041
              UCID=UCOD
              VCID=VCOD
              WCID=WCOD
              CCID=CCOD
              UCVD=UCND
              VCVD=VCND
              WCVD=WCND
              CCVD=CCND
              XCVU=XCNU
              YCVU=YCNU
              L10043=    1
              GO TO 10043
10042         CONTINUE
              L10045=    1
              GO TO 10045
10044         CONTINUE
              UCOD=UCVD
              VCOD=VCVD
              WCOD=WCVD
              CCOD=CCVD
              XCOU=XCVU
              YCOU=YCVU
              IVOU=1
10040       CONTINUE
10041       CONTINUE
          GO TO 10046
10034     CONTINUE
          IF (.NOT.(NCPL.EQ.MCPL)) GO TO 10047
            L10049=    1
            GO TO 10049
10048       CONTINUE
10046     CONTINUE
10047     CONTINUE
C
          IF (.NOT.(IVNU.NE.0)) GO TO 10050
            L10052=    1
            GO TO 10052
10051       CONTINUE
          GO TO 10053
10050     CONTINUE
          IF (.NOT.(IVOU.NE.0)) GO TO 10054
            UCVD=UCOD
            VCVD=VCOD
            WCVD=WCOD
            CCVD=CCOD
            XCVU=XCOU
            YCVU=YCOU
            UCID=UCND
            VCID=VCND
            WCID=WCND
            CCID=CCND
            L10043=    2
            GO TO 10043
10055       CONTINUE
            UKND=UCND
            VKND=VCND
            WKND=WCND
            CKND=CCND
            XKNU=XCNU
            YKNU=YCNU
            UCND=UCVD
            VCND=VCVD
            WCND=WCVD
            CCND=CCVD
            XCNU=XCVU
            YCNU=YCVU
            L10052=    2
            GO TO 10052
10056       CONTINUE
            UCND=UKND
            VCND=VKND
            WCND=WKND
            CCND=CKND
            XCNU=XKNU
            YCNU=YKNU
            L10013=    3
            GO TO 10013
10057       CONTINUE
10053     CONTINUE
10054     CONTINUE
C
10033   CONTINUE
C
      GO TO (10030,10032) , L10031
C
C The following internal procedure outputs the next point; if mapping
C is being done and there is a sufficiently large jump in the mapped
C position of the point, we check for a discontinuity in the mapping.
C Similarly, if mapping is being done and point interpolation is
C activated, we check for a large enough jump in the mapped position
C to justify interpolating points.
C
10052 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.(UCND.NE.UCOD.OR.VCND.NE.VCOD.OR.WCND.NE
     +.WCOD))) GO TO 10058
          RUDO=RUDN
          RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +         (ABS(UCND-UCOD)+ABS(VCND-VCOD)+ABS(WCND-WCOD))
          IF (.NOT.(RUDN.GT.2.*RUDO)) GO TO 10059
            L10061=    1
            GO TO 10061
10060       CONTINUE
10059     CONTINUE
          IF (.NOT.(PITH.GT.0.)) GO TO 10062
            UCTD=UCND
            VCTD=VCND
            WCTD=WCND
            CCTD=CCND
            XCTU=XCNU
            YCTU=YCNU
            L10064=    1
            GO TO 10064
10063       CONTINUE
10062     CONTINUE
10058   CONTINUE
        NCPL=NCPL+1
        XCPL(NCPL)=XCNU
        YCPL(NCPL)=YCNU
        IF (.NOT.(IPCS.GE.0)) GO TO 10065
          CVAL=CCND
          L10039=    2
          GO TO 10039
10066     CONTINUE
          IF (.NOT.(IPCI.NE.IPCD)) GO TO 10067
            L10049=    2
            GO TO 10049
10068       CONTINUE
            IPCD=IPCI
10067     CONTINUE
10065   CONTINUE
      GO TO (10051,10056) , L10052
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the curve is seen.  It
C checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10061 CONTINUE
        UC1D=UCOD
        VC1D=VCOD
        WC1D=WCOD
        CC1D=CCOD
        XC1U=XCOU
        YC1U=YCOU
        UC2D=UCND
        VC2D=VCND
        WC2D=WCND
        CC2D=CCND
        XC2U=XCNU
        YC2U=YCNU
        ITMP=0
10069   CONTINUE
          DSTO=ABS(XC2U-XC1U)+ABS(YC2U-YC1U)
          UC3D=(UC1D+UC2D)/2.
          VC3D=(VC1D+VC2D)/2.
          WC3D=(WC1D+WC2D)/2.
          CC3D=(CC1D+CC2D)/2.
          CALL HLUCTMXYZ (IMPF,UC3D,VC3D,WC3D,XC3U,YC3U)
          IF (ICFELL('VTCUDR',3).NE.0) GO TO 101
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10070
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(MIN(DST1,DST2).GT.DSTO)) GO TO 10071
              ITMP=1000
              GO TO 10072
10071       CONTINUE
            IF (.NOT.(DST1.LT.DST2)) GO TO 10073
              IF (UC3D.EQ.UC1D.AND.VC3D.EQ.VC1D.AND.WC3D.EQ.WC1D) GO TO
     +10072
              UC1D=UC3D
              VC1D=VC3D
              WC1D=WC3D
              CC1D=CC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10074
10073       CONTINUE
              IF (UC3D.EQ.UC2D.AND.VC3D.EQ.VC2D.AND.WC3D.EQ.WC2D) GO TO
     +10072
              UC2D=UC3D
              VC2D=VC3D
              WC2D=WC3D
              CC2D=CC3D
              XC2U=XC3U
              YC2U=YC3U
10074       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10072
          GO TO 10075
10070     CONTINUE
            UCVD=UCOD
            VCVD=VCOD
            WCVD=WCOD
            CCVD=CCOD
            XCVU=XCOU
            YCVU=YCOU
            UCID=UC3D
            VCID=VC3D
            WCID=WC3D
            CCID=CC3D
            L10043=    3
            GO TO 10043
10076       CONTINUE
            L10045=    2
            GO TO 10045
10077       CONTINUE
            L10013=    4
            GO TO 10013
10078       CONTINUE
            UCID=UC3D
            VCID=VC3D
            WCID=WC3D
            CCID=CC3D
            UCVD=UCND
            VCVD=VCND
            WCVD=WCND
            CCVD=CCND
            XCVU=XCNU
            YCVU=YCNU
            L10043=    4
            GO TO 10043
10079       CONTINUE
            L10045=    3
            GO TO 10045
10080       CONTINUE
            ITMP=1000
            GO TO 10072
10075     CONTINUE
        GO TO 10069
10072   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10081
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10082
            UCTD=UC1D
            VCTD=VC1D
            WCTD=WC1D
            CCTD=CC1D
            XCTU=XC1U
            YCTU=YC1U
            L10064=    2
            GO TO 10064
10083       CONTINUE
10082     CONTINUE
          NCPL=NCPL+1
          XCPL(NCPL)=XC1U
          YCPL(NCPL)=YC1U
          L10013=    5
          GO TO 10013
10084     CONTINUE
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10085
            UCLD=UC2D
            VCLD=VC2D
            WCLD=WC2D
            CCLD=CC2D
            XCLU=XC2U
            YCLU=YC2U
10085     CONTINUE
          NCPL=1
          XCPL(1)=XC2U
          YCPL(1)=YC2U
          IF (.NOT.(IPCS.GE.0)) GO TO 10086
            CVAL=CC2D
            L10039=    3
            GO TO 10039
10087       CONTINUE
            IPCD=IPCI
10086     CONTINUE
10081   CONTINUE
      GO TO (10060) , L10061
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10043 CONTINUE
        ITMP=0
10088   CONTINUE
          UCHD=(UCVD+UCID)/2.
          VCHD=(VCVD+VCID)/2.
          WCHD=(WCVD+WCID)/2.
          CCHD=(CCVD+CCID)/2.
          CALL HLUCTMXYZ (IMPF,UCHD,VCHD,WCHD,XCHU,YCHU)
          IF (ICFELL('VTCUDR',4).NE.0) GO TO 101
          IF (.NOT.(XCHU.NE.OORV.AND.YCHU.NE.OORV)) GO TO 10089
            IF (UCHD.EQ.UCVD.AND.VCHD.EQ.VCVD.AND.WCHD.EQ.WCVD) GO TO 10
     +090
            UCVD=UCHD
            VCVD=VCHD
            WCVD=WCHD
            CCVD=CCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10091
10089     CONTINUE
            IF (UCHD.EQ.UCID.AND.VCHD.EQ.VCID.AND.WCHD.EQ.WCID) GO TO 10
     +090
            UCID=UCHD
            VCID=VCHD
            WCID=WCHD
            CCID=CCHD
10091     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10090
        GO TO 10088
10090   CONTINUE
      GO TO (10042,10055,10076,10079) , L10043
C
C The following internal procedure outputs a visible edge point found
C by the previous internal procedure.
C
10045 CONTINUE
        IF (.NOT.(PITH.GT.0.)) GO TO 10092
          IF (.NOT.(NCPL.EQ.0)) GO TO 10093
            UCLD=UCVD
            VCLD=VCVD
            WCLD=WCVD
            CCLD=CCVD
            XCLU=XCVU
            YCLU=YCVU
          GO TO 10094
10093     CONTINUE
            UCTD=UCVD
            VCTD=VCVD
            WCTD=WCVD
            CCTD=CCVD
            XCTU=XCVU
            YCTU=YCVU
            L10064=    3
            GO TO 10064
10095       CONTINUE
10094     CONTINUE
10092   CONTINUE
        NCPL=NCPL+1
        XCPL(NCPL)=XCVU
        YCPL(NCPL)=YCVU
        IF (.NOT.(IPCS.GE.0)) GO TO 10096
          CVAL=CCVD
          L10039=    4
          GO TO 10039
10097     CONTINUE
          IF (.NOT.(IPCI.NE.IPCD)) GO TO 10098
            L10049=    3
            GO TO 10049
10099       CONTINUE
            IPCD=IPCI
10098     CONTINUE
10096   CONTINUE
      GO TO (10044,10077,10080) , L10045
C
C The following internal procedure is invoked when mapping is being
C done and a new point is about to be added to the polyline buffer.
C It checks for a jump (using a user-defined threshold value) in
C the mapped coordinates of the point and, if such a jump is found,
C interpolates some points in between.  The assumption is made that
C all points in between are visible; if that is found not to be the
C case, no attempt is made to rectify the situation: the user probably
C screwed up the definition of the mapping function.
C
10064 CONTINUE
10100   CONTINUE
        IF (.NOT.(ABS(XCTU-XCLU).GT.PITX.OR.ABS(YCTU-YCLU).GT.PITY))
     +  GO TO 10101
          IFND=0
          UCQD=0.
          VCQD=0.
          WCQD=0.
          CCQD=0.
          RDST=.50
          RSTP=.25
10102     CONTINUE
            UCPD=UCLD+RDST*(UCTD-UCLD)
            VCPD=VCLD+RDST*(VCTD-VCLD)
            WCPD=WCLD+RDST*(WCTD-WCLD)
            CCPD=CCLD+RDST*(CCTD-CCLD)
            CALL HLUCTMXYZ (IMPF,UCPD,VCPD,WCPD,XCPU,YCPU)
            IF (ICFELL('VTCUDR',5).NE.0) GO TO 101
            IF (OORV.NE.0..AND.(XCPU.EQ.OORV.OR.YCPU.EQ.OORV)) GO TO 101
     +03
            IF (.NOT.(ABS(XCPU-XCLU).LT.PITX.AND.ABS(YCPU-YCLU).LT.PITY)
     +)     GO TO 10104
              IFND=1
              UCQD=UCPD
              VCQD=VCPD
              WCQD=WCPD
              CCQD=CCPD
              XCQU=XCPU
              YCQU=YCPU
              IF (ABS(XCQU-XCLU).GT..5*PITX.OR.ABS(YCQU-YCLU).GT..5*PITY
     +)       GO TO 10103
              RDST=RDST+RSTP
            GO TO 10105
10104       CONTINUE
              RDST=RDST-RSTP
10105       CONTINUE
            RSTP=RSTP/2.
            IF (RSTP.LT..0001) GO TO 10103
          GO TO 10102
10103     CONTINUE
          IF (.NOT.(IFND.NE.0.AND.(UCQD.NE.UCLD.OR.VCQD.NE.VCLD.OR.WCQD.
     +NE.WCLD))) GO TO 10106
            NCPL=NCPL+1
            XCPL(NCPL)=XCQU
            YCPL(NCPL)=YCQU
            IF (.NOT.(IPCS.GE.0)) GO TO 10107
              CVAL=CCQD
              L10039=    5
              GO TO 10039
10108         CONTINUE
              IF (.NOT.(IPCI.NE.IPCD)) GO TO 10109
                L10049=    4
                GO TO 10049
10110           CONTINUE
                IPCD=IPCI
10109         CONTINUE
10107       CONTINUE
            IF (.NOT.(NCPL.EQ.MCPL)) GO TO 10111
              L10049=    5
              GO TO 10049
10112         CONTINUE
10111       CONTINUE
            UCLD=UCQD
            VCLD=VCQD
            WCLD=WCQD
            CCLD=CCQD
            XCLU=XCQU
            YCLU=YCQU
          GO TO 10113
10106     CONTINUE
            UCLD=UCTD
            VCLD=VCTD
            WCLD=WCTD
            CCLD=CCTD
            XCLU=XCTU
            YCLU=YCTU
10113     CONTINUE
        GO TO 10100
10101   CONTINUE
        UCLD=UCTD
        VCLD=VCTD
        WCLD=WCTD
        CCLD=CCTD
        XCLU=XCTU
        YCLU=YCTU
      GO TO (10063,10083,10095) , L10064
C
C The following internal procedure is given the data-system coordinates
C of a point (UCND,VCND,WCND) and computes the user-system coordinates
C of the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10005 CONTINUE
C
        IF (.NOT.(IMPF.EQ.0)) GO TO 10114
          XCNU=UCND
          YCNU=VCND
          IVNU=1
        GO TO 10115
10114   CONTINUE
          CALL HLUCTMXYZ (IMPF,UCND,VCND,WCND,XCNU,YCNU)
          IF (ICFELL('VTCUDR',6).NE.0) GO TO 101
          IF (.NOT.((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)))
     +    GO TO 10116
            IVNU=0
          GO TO 10117
10116     CONTINUE
            IVNU=1
10117     CONTINUE
10115   CONTINUE
C
      GO TO (10004,10009,10018,10019,10021,10028) , L10005
C
C The following internal procedure, given a value (CVAL), computes a
C polyline color index (IPCI) to be used to get a desired color for a
C streamline being drawn.
C
10039 CONTINUE
10118   CONTINUE
        IF (.NOT.(ICVL.GT.1.AND.CVAL.LT.TVAL(ICVL))) GO TO 10119
          ICVL=ICVL-1
        GO TO 10118
10119   CONTINUE
10120   CONTINUE
        IF (.NOT.(ICVL.LT.NCLR.AND.CVAL.GE.TVAL(ICVL+1))) GO TO 10121
          ICVL=ICVL+1
        GO TO 10120
10121   CONTINUE
        IPCI=ICLR(ICVL)
      GO TO (10038,10066,10087,10097,10108) , L10039
C
C The following internal procedure draws the part of the curve defined
C by the contents of the polyline buffer and then removes all but the
C last point from the buffer.  IPCC is the polyline color currently
C in use and IPCD the polyline color desired for the curve.
C
10049 CONTINUE
C
        I=1
C
10122   CONTINUE
          I=I+1
          IF (I.GT.NCPL) GO TO 10123
          IF (.NOT.(ABS(XCPL(I)-XCPL(I-1)).LT.EPSX.AND.ABS(YCPL(I)-YCPL(
     +I-1)).LT.EPSY)) GO TO 10124
            IF (.NOT.(I.NE.NCPL)) GO TO 10125
              DO 10126 J=I+1,NCPL
                XCPL(J-1)=XCPL(J)
                YCPL(J-1)=YCPL(J)
10126         CONTINUE
            GO TO 10127
10125       CONTINUE
              XCPL(NCPL-1)=XCPL(NCPL)
              YCPL(NCPL-1)=YCPL(NCPL)
10127       CONTINUE
            I=I-1
            NCPL=NCPL-1
10124     CONTINUE
        GO TO 10122
10123   CONTINUE
C
        IF (.NOT.(NCPL.GT.1)) GO TO 10128
          IF (.NOT.(IPCC.NE.IPCD)) GO TO 10129
            CALL GSPLCI (IPCD)
            IPCC=IPCD
10129     CONTINUE
          IF (.NOT.(IAMA(1).EQ.0)) GO TO 10130
            CALL CURVE (XCPL,YCPL,NCPL)
          GO TO 10131
10130     CONTINUE
            CALL ARDRLN (IAMA,XCPL,YCPL,NCPL,
     +                        XCPF,YCPF,MCPF,
     +                        IAAI,IAGI,MNOG,RTPL)
10131     CONTINUE
10128   CONTINUE
C
        XCPL(1)=XCPL(NCPL)
        YCPL(1)=YCPL(NCPL)
        NCPL=1
C
      GO TO (10048,10068,10099,10110,10112) , L10049
C
C The following internal procedure draws the part of the curve defined
C by the contents of the polyline buffer and then clears the buffer.
C IPCC is the polyline color currently in use and IPCD the polyline
C color desired for the curve.
C
10013 CONTINUE
C
        I=1
C
10132   CONTINUE
          I=I+1
          IF (I.GT.NCPL) GO TO 10133
          IF (.NOT.(ABS(XCPL(I)-XCPL(I-1)).LT.EPSX.AND.ABS(YCPL(I)-YCPL(
     +I-1)).LT.EPSY)) GO TO 10134
            IF (.NOT.(I.NE.NCPL)) GO TO 10135
              DO 10136 J=I+1,NCPL
                XCPL(J-1)=XCPL(J)
                YCPL(J-1)=YCPL(J)
10136         CONTINUE
            GO TO 10137
10135       CONTINUE
              XCPL(NCPL-1)=XCPL(NCPL)
              YCPL(NCPL-1)=YCPL(NCPL)
10137       CONTINUE
            I=I-1
            NCPL=NCPL-1
10134     CONTINUE
        GO TO 10132
10133   CONTINUE
C
        IF (.NOT.(NCPL.GT.1)) GO TO 10138
          IF (.NOT.(IPCC.NE.IPCD)) GO TO 10139
            CALL GSPLCI (IPCD)
            IPCC=IPCD
10139     CONTINUE
          IF (.NOT.(IAMA(1).EQ.0)) GO TO 10140
            CALL CURVE (XCPL,YCPL,NCPL)
          GO TO 10141
10140     CONTINUE
            CALL ARDRLN (IAMA,XCPL,YCPL,NCPL,
     +                        XCPF,YCPF,MCPF,
     +                        IAAI,IAGI,MNOG,RTPL)
10141     CONTINUE
10138   CONTINUE
C
        NCPL=0
        RUDN=0.
C
      GO TO (10012,10023,10057,10078,10084) , L10013
C
      END

      SUBROUTINE CPTRCL (ZDAT,RWRK,IWRK,CLVL,IJMP,IRW1,IRW2,NRWK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C Given ZDAT (an array of data), RWRK (a real workspace), IWRK (an
C integer workspace), and CLVL (a particular contour level), CPTRCL
C finds the beginning of each contour line at the level CLVL and then
C traces it.  Control is passed back to the caller to process the
C line segments generated.
C
C ZDAT is the doubly-dimensioned array of data being contoured.
C
C RWRK is a real workspace array.
C
C IWRK is an integer workspace array.
C
C CLVL is the contour level being worked on.
C
C IJMP is initially set to zero by the caller.  Upon return, it will be
C zero if all segments have been traced and processed, non-zero if the
C caller is expected to process a segment and recall CPTRCL.
C
C IRW1 and IRW2 are output variables.  If IJMP is non-zero, they are
C base indices of X and Y coordinate arrays in RWRK.
C
C NRWK is an output variable.  If IJMP is non-zero, NRWK is the number
C of coordinates to be processed by the caller.
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
C
C Because of the way this routine is entered and reentered, we need to
C save every variable it uses.
C
      SAVE
C
C Define an interpolation function.
C
      FRCT(ZDT1,ZDT2)=(CLVL-ZDT1)/(ZDT2-ZDT1)
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (105,103,104,106,107,108) , IJMP
C
C Otherwise, compute some needed conversion constants.
C
      RZDM=(XATM-XAT1)/REAL(IZDM-1)
      RZDN=(YATN-YAT1)/REAL(IZDN-1)
C
C Assign space to use for storing the coordinates of points on contour
C lines.
C
      IF (.NOT.(T2DS.EQ.0.)) GO TO 10001
        CALL CPGRWS (RWRK,1,2*LRWC,IWSE)
      GO TO 10002
10001 CONTINUE
        CALL CPGRWS (RWRK,1,7*LRWC,IWSE)
10002 CONTINUE
C
      IF (IWSE.NE.0.OR.ICFELL('CPTRCL',1).NE.0) GO TO 102
C
C Set the offset from one portion of the real workspace to the next.
C
      MPLS=LRWC
C
C Set some tolerances.
C
      IF (.NOT.(T2DS.LT.0.)) GO TO 10003
        DBPI=ABS(XATM-XAT1)*SEGL
        EPSX=ABS(XATM-XAT1)*EPSI
        EPSY=ABS(YATN-YAT1)*EPSI
      GO TO 10004
10003 CONTINUE
        DBPI=ABS(XWDR-XWDL)*SEGL
        EPSX=ABS(XWDR-XWDL)*EPSI
        EPSY=ABS(YWDT-YWDB)*EPSI
10004 CONTINUE
C
      SMLX=.01*ABS(XWDR-XWDL)
      SMLY=.01*ABS(YWDT-YWDB)
C
      PITX=PITH*ABS(XWDR-XWDL)
      PITY=PITH*ABS(YWDT-YWDB)
C
C Zero the count of horizontal segments seen so far.
C
      NHSS=0
C
C Initialize four variables to prevent the code from blowing up.  (The
C values only have to be legal values on the machine; they are used in
C one clause of a block-IF, the other clause of which makes the value
C immaterial.)
C
      XBFS=0.
      YBFS=0.
      XELS=0.
      YELS=0.
C
C Set IOCF to indicate that we are looking for open contours.
C
      IOCF=0
C
C Search the bottom edge of the grid.
C
      IVBY=1
        IVBX = 2
        GO TO 10007
10005   CONTINUE
        IVBX =IVBX +1
10007   CONTINUE
        IF (IVBX .GT.(IZDM)) GO TO 10006
        IF (.NOT.(ZDAT(IVBX-1,1).LT.CLVL.AND.ZDAT(IVBX,1).GE.CLVL))
     +  GO TO 10008
          INCI=1
          L10010=    1
          GO TO 10010
10009     CONTINUE
10008   CONTINUE
      GO TO 10005
10006 CONTINUE
C
C Search the right edge of the grid.
C
      IVBX=IZDM
        IVBY = 2
        GO TO 10013
10011   CONTINUE
        IVBY =IVBY +1
10013   CONTINUE
        IF (IVBY .GT.(IZDN)) GO TO 10012
        IF (.NOT.(ZDAT(IZDM,IVBY-1).LT.CLVL.AND.ZDAT(IZDM,IVBY).GE.CLVL)
     +) GO TO 10014
          INCI=7
          L10010=    2
          GO TO 10010
10015     CONTINUE
10014   CONTINUE
      GO TO 10011
10012 CONTINUE
C
C Search the top edge of the grid.
C
      IVBY=IZDN
        IVBX = IZDM-1
        GO TO 10018
10016   CONTINUE
        IVBX =IVBX -1
10018   CONTINUE
        IF (IVBX .LT.(1)) GO TO 10017
        IF (.NOT.(ZDAT(IVBX+1,IZDN).LT.CLVL.AND.ZDAT(IVBX,IZDN).GE.CLVL)
     +) GO TO 10019
          INCI=5
          L10010=    3
          GO TO 10010
10020     CONTINUE
10019   CONTINUE
      GO TO 10016
10017 CONTINUE
C
C Search the left edge of the grid.
C
      IVBX=1
        IVBY = IZDN-1
        GO TO 10023
10021   CONTINUE
        IVBY =IVBY -1
10023   CONTINUE
        IF (IVBY .LT.(1)) GO TO 10022
        IF (.NOT.(ZDAT(1,IVBY+1).LT.CLVL.AND.ZDAT(1,IVBY).GE.CLVL))
     +  GO TO 10024
          INCI=3
          L10010=    4
          GO TO 10010
10025     CONTINUE
10024   CONTINUE
      GO TO 10021
10022 CONTINUE
C
C Set IOCF to indicate that we are looking for closed contours.
C
      IOCF=1
C
C Search the interior of the grid.
C
        IVBY = 2
        GO TO 10028
10026   CONTINUE
        IVBY =IVBY +1
10028   CONTINUE
        IF (IVBY .GT.(IZDN-1)) GO TO 10027
          IVBX = 2
          GO TO 10031
10029     CONTINUE
          IVBX =IVBX +1
10031     CONTINUE
          IF (IVBX .GT.(IZDM)) GO TO 10030
          IF (.NOT.(ZDAT(IVBX-1,IVBY).LT.CLVL.AND.ZDAT(IVBX,IVBY).GE.CLV
     +L)) GO TO 10032
            IPXY=IZDN*IVBX+IVBY
            DO 10033 I=1,NHSS
              IF (IPXY.EQ.IWRK(II01+I)) GO TO 101
10033       CONTINUE
            IF (.NOT.(NHSS.GE.LI01)) GO TO 10034
              CALL CPGIWS (IWRK,1,LI01+100,IWSE)
              IF (IWSE.NE.0.OR.ICFELL('CPTRCL',2).NE.0) GO TO 102
10034       CONTINUE
            NHSS=NHSS+1
            IWRK(II01+NHSS)=IPXY
            INCI=1
            L10010=    5
            GO TO 10010
10035       CONTINUE
  101     CONTINUE
10032     CONTINUE
        GO TO 10029
10030   CONTINUE
      GO TO 10026
10027 CONTINUE
C
C Done.
C
  102 LI01=0
      LR01=0
      IJMP=0
      RETURN
C
C Line-following algorithm.  This internal routine moves the line-
C following vector (defined by the base point (IVBX,IVBY) and the
C components INCX(INCI) and INCY(INCI)) along a contour line.  The
C points defining the contour line are thereby determined.  The
C process stops when either the starting point or the edge of the
C grid is encountered.
C
10010 CONTINUE
C
C Save the variables defining the original position of the line-
C following vector.
C
        MVBX=IVBX
        MVBY=IVBY
        MNCI=INCI
C
C Set variables defining the position of the end of the vector.
C
        IVEX=IVBX+INCX(INCI)
        IVEY=IVBY+INCY(INCI)
C
C Compute the coordinates, in the data-index coordinate system, of the
C starting position of the contour line.  If the point is very close to
C a grid intersection, put it at the intersection; this avoids problems
C caused by very short line segments.  Also, be careful to compute the
C value of XFRA using code exactly like that in CPTREG and CPTRES, thus
C ensuring that points they interpolate where contour lines intersect
C the edge of the grid or the edges of special-value areas will match
C the points generated here.
C
        IF (.NOT.(IVEX.GT.IVBX)) GO TO 10036
          XFRA=FRCT(ZDAT(IVBX,IVBY),ZDAT(IVEX,IVEY))
          IF (XFRA.LE..00001) XFRA=0.
          IF (XFRA.GE..99999) XFRA=1.
          XCND=REAL(IVBX)+XFRA
          YCND=REAL(IVBY)
        GO TO 10037
10036   CONTINUE
        IF (.NOT.(IVEX.LT.IVBX)) GO TO 10038
          XFRA=FRCT(ZDAT(IVEX,IVEY),ZDAT(IVBX,IVBY))
          IF (XFRA.LE..00001) XFRA=0.
          IF (XFRA.GE..99999) XFRA=1.
          XCND=REAL(IVEX)+XFRA
          YCND=REAL(IVEY)
        GO TO 10037
10038   CONTINUE
        IF (.NOT.(IVEY.GT.IVBY)) GO TO 10039
          XCND=REAL(IVBX)
          YFRA=FRCT(ZDAT(IVBX,IVBY),ZDAT(IVEX,IVEY))
          IF (YFRA.LE..00001) YFRA=0.
          IF (YFRA.GE..99999) YFRA=1.
          YCND=REAL(IVBY)+YFRA
        GO TO 10037
10039   CONTINUE
          XCND=REAL(IVEX)
          YFRA=FRCT(ZDAT(IVEX,IVEY),ZDAT(IVBX,IVBY))
          IF (YFRA.LE..00001) YFRA=0.
          IF (YFRA.GE..99999) YFRA=1.
          YCND=REAL(IVEY)+YFRA
10037   CONTINUE
C
C Map the coordinates (XCND,YCND) into user coordinates (XCNU,YCNU).
C
        L10041=    1
        GO TO 10041
10040   CONTINUE
C
C Zero the number of points in the coordinate arrays, initialize the
C flag that indicates we're working on the first segment, and zero
C the variable that keeps track of the ratio of segment length in
C the user system to segment length in the data-index system.
C
        NPLS=0
        IFSF=1
        RUDN=0.
C
C Loop, moving the line-following vector as dictated by the positions
C of its end points.
C
10042   CONTINUE
C
C At this point, we know that the base of the line-following vector is
C on the high side of the contour and that the end of it is on the other
C side.  Move the vector clockwise and see what the situation is then.
C
          INCI=INCI+1
          IF (INCI.GT.8) INCI=INCI-8
          IVEX=IVBX+INCX(INCI)
          IVEY=IVBY+INCY(INCI)
C
C Exit the loop if we've hit the edge.
C
          IF (IVEX.LT.1.OR.IVEX.GT.IZDM.OR.IVEY.LT.1.OR.IVEY.GT.IZDN)
     +    GO TO 10043
C
C If the end of the line-following vector is now on the same side of
C the contour line as its base ...
C
          IF (.NOT.(ZDAT(IVEX,IVEY).GE.CLVL)) GO TO 10044
C
C flip it end-for-end and continue the loop.
C
            IVBX=IVEX
            IVBY=IVEY
            INCI=INCI+4
C
C Otherwise, if the line-following vector is currently horizontal or
C vertical, we have another point to add to the contour line ...
C
          GO TO 10045
10044     CONTINUE
          IF (.NOT.((INCI/2)*2.NE.INCI)) GO TO 10046
C
C so save the coordinates of the old point and compute the coordinates
C of the new one.
C
            XCOD=XCND
            YCOD=YCND
            XCOU=XCNU
            YCOU=YCNU
            IVOU=IVNU
C
            IF (.NOT.(IVEX.GT.IVBX)) GO TO 10047
              XFRA=FRCT(ZDAT(IVBX,IVBY),ZDAT(IVEX,IVEY))
              IF (XFRA.LE..00001) XFRA=0.
              IF (XFRA.GE..99999) XFRA=1.
              XCND=REAL(IVBX)+XFRA
              YCND=REAL(IVBY)
            GO TO 10048
10047       CONTINUE
            IF (.NOT.(IVEX.LT.IVBX)) GO TO 10049
              XFRA=FRCT(ZDAT(IVEX,IVEY),ZDAT(IVBX,IVBY))
              IF (XFRA.LE..00001) XFRA=0.
              IF (XFRA.GE..99999) XFRA=1.
              XCND=REAL(IVEX)+XFRA
              YCND=REAL(IVEY)
            GO TO 10048
10049       CONTINUE
            IF (.NOT.(IVEY.GT.IVBY)) GO TO 10050
              XCND=REAL(IVBX)
              YFRA=FRCT(ZDAT(IVBX,IVBY),ZDAT(IVEX,IVEY))
              IF (YFRA.LE..00001) YFRA=0.
              IF (YFRA.GE..99999) YFRA=1.
              YCND=REAL(IVBY)+YFRA
            GO TO 10048
10050       CONTINUE
              XCND=REAL(IVEX)
              YFRA=FRCT(ZDAT(IVEX,IVEY),ZDAT(IVBX,IVBY))
              IF (YFRA.LE..00001) YFRA=0.
              IF (YFRA.GE..99999) YFRA=1.
              YCND=REAL(IVEY)+YFRA
10048       CONTINUE
C
C Map the coordinates (XCND,YCND) into user coordinates (XCNU,YCNU).
C
            L10041=    2
            GO TO 10041
10051       CONTINUE
C
C Save the coordinates of the point.  Special values complicate things.
C
            IF (.NOT.(SVAL.EQ.0.)) GO TO 10052
              L10054=    1
              GO TO 10054
10053         CONTINUE
            GO TO 10055
10052       CONTINUE
              IF (.NOT.(INCI.EQ.1)) GO TO 10056
                INCP=9
              GO TO 10057
10056         CONTINUE
                INCP=INCI
10057         CONTINUE
              IVCX=IVBX+INCX(INCP-1)
              IVCY=IVBY+INCY(INCP-1)
              IVDX=IVBX+INCX(INCP-2)
              IVDY=IVBY+INCY(INCP-2)
              IF (.NOT.(ZDAT(IVBX,IVBY).EQ.SVAL.OR.ZDAT(IVCX,IVCY).EQ.SV
     +AL.OR.ZDAT(IVDX,IVDY).EQ.SVAL.OR.ZDAT(IVEX,IVEY).EQ.SVAL)) GO TO 1
     +0058
                IF (.NOT.(NPLS.GT.1)) GO TO 10059
                  L10061=    1
                  GO TO 10061
10060             CONTINUE
10059           CONTINUE
              GO TO 10062
10058         CONTINUE
                L10054=    2
                GO TO 10054
10063           CONTINUE
10062         CONTINUE
10055       CONTINUE
C
C If we just crossed a horizontal grid line in the upwards direction,
C save that information.
C
            IF (.NOT.(INCI.EQ.1)) GO TO 10064
              IF (.NOT.(NHSS.GE.LI01)) GO TO 10065
                CALL CPGIWS (IWRK,1,LI01+100,IWSE)
                IF (IWSE.NE.0.OR.ICFELL('CPTRCL',3).NE.0) GO TO 102
10065         CONTINUE
              NHSS=NHSS+1
              IWRK(II01+NHSS)=IZDN*IVBX+IVBY
10064       CONTINUE
C
C If we just arrived at our starting point, quit the loop.
C
            IF (IVBX.EQ.MVBX.AND.IVBY.EQ.MVBY.AND.INCI.EQ.MNCI) GO TO 10
     +043
C
10045     CONTINUE
10046     CONTINUE
C
        GO TO 10042
10043   CONTINUE
C
C Process any remaining portion of the contour line.
C
        IF (.NOT.(NPLS.GT.1)) GO TO 10066
          L10061=    2
          GO TO 10061
10067     CONTINUE
10066   CONTINUE
C
C Reset the vector base to its original position.
C
        IVBX=MVBX
        IVBY=MVBY
C
C Done.
C
      GO TO (10009,10015,10020,10025,10035) , L10010
C
C The following internal procedure, given a line segment, adds visible
C portions of it to the coordinate arrays.
C
10054 CONTINUE
C
C If point interpolation is turned on, do the first IPIC segments.
C
        IF (.NOT.(IPIC.NE.0)) GO TO 10068
          XSOD=XCOD
          YSOD=YCOD
          XSND=XCND
          YSND=YCND
          XSNU=XCNU
          YSNU=YCNU
          ISNU=IVNU
            INTP = 1
            GO TO 10071
10069       CONTINUE
            INTP =INTP +1
10071       CONTINUE
            IF (INTP .GT.(ABS(IPIC))) GO TO 10070
            XCND=XSOD+(REAL(INTP)/REAL(ABS(IPIC)+1))*(XSND-XSOD)
            YCND=YSOD+(REAL(INTP)/REAL(ABS(IPIC)+1))*(YSND-YSOD)
            L10041=    3
            GO TO 10041
10072       CONTINUE
            IF (.NOT.(IPIC.GT.0.OR.IVNU.NE.IVOU)) GO TO 10073
              L10075=    1
              GO TO 10075
10074         CONTINUE
              XCOD=XCND
              YCOD=YCND
              XCOU=XCNU
              YCOU=YCNU
              IVOU=IVNU
10073       CONTINUE
          GO TO 10069
10070     CONTINUE
          XCND=XSND
          YCND=YSND
          XCNU=XSNU
          YCNU=YSNU
          IVNU=ISNU
10068   CONTINUE
C
C Finish off the job.
C
        L10075=    2
        GO TO 10075
10076   CONTINUE
C
      GO TO (10053,10063) , L10054
C
C The following internal procedure examines the points (XCOD,YCOD),
C which projects into (XCOU,YCOU), and (XCND,YCND), which projects into
C (XCNU,YCNU), either of which may be visible or invisible in the
C projection space, and adds visible portions of the line segment
C between them to the polyline being built.
C
10075 CONTINUE
C
        IF (.NOT.(XCND.NE.XCOD.OR.YCND.NE.YCOD)) GO TO 10077
C
          IF (.NOT.(NPLS.EQ.0)) GO TO 10078
            IF (.NOT.(IVOU.NE.0)) GO TO 10079
              IF (.NOT.(IMPF.NE.0.AND.T2DS.GE.0..AND.PITH.GT.0.)) GO TO
     +10080
                XCLD=XCOD
                YCLD=YCOD
                XCLU=XCOU
                YCLU=YCOU
10080         CONTINUE
              RWRK(IR01+1)=XCOU
              RWRK(IR01+MPLS+1)=YCOU
              NPLS=1
            GO TO 10081
10079       CONTINUE
            IF (.NOT.(IVNU.NE.0)) GO TO 10082
              XCID=XCOD
              YCID=YCOD
              XCVD=XCND
              YCVD=YCND
              XCVU=XCNU
              YCVU=YCNU
              L10084=    1
              GO TO 10084
10083         CONTINUE
              L10086=    1
              GO TO 10086
10085         CONTINUE
              XCOD=XCVD
              YCOD=YCVD
              XCOU=XCVU
              YCOU=YCVU
              IVOU=1
10081       CONTINUE
10082       CONTINUE
          GO TO 10087
10078     CONTINUE
          IF (.NOT.(NPLS.EQ.MPLS)) GO TO 10088
            XSAV=RWRK(IR01+NPLS)
            YSAV=RWRK(IR01+MPLS+NPLS)
            L10061=    3
            GO TO 10061
10089       CONTINUE
            RWRK(IR01+1)=XSAV
            RWRK(IR01+MPLS+1)=YSAV
            NPLS=1
10087     CONTINUE
10088     CONTINUE
C
          IF (.NOT.(IVNU.NE.0)) GO TO 10090
            L10092=    1
            GO TO 10092
10091       CONTINUE
          GO TO 10093
10090     CONTINUE
          IF (.NOT.(IVOU.NE.0)) GO TO 10094
            XCVD=XCOD
            YCVD=YCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XCND
            YCID=YCND
            L10084=    2
            GO TO 10084
10095       CONTINUE
            XKND=XCND
            YKND=YCND
            XKNU=XCNU
            YKNU=YCNU
            XCND=XCVD
            YCND=YCVD
            XCNU=XCVU
            YCNU=YCVU
            L10092=    2
            GO TO 10092
10096       CONTINUE
            XCND=XKND
            YCND=YKND
            XCNU=XKNU
            YCNU=YKNU
            L10061=    4
            GO TO 10061
10097       CONTINUE
10093     CONTINUE
10094     CONTINUE
C
10077   CONTINUE
C
      GO TO (10074,10076) , L10075
C
C The following internal procedure outputs the next point; if mapping
C is being done and there is a sufficiently large jump in the mapped
C position of the point, we check for a discontinuity in the mapping.
C Similarly, if mapping is being done and point interpolation is
C activated, we check for a large enough jump in the mapped position
C to justify interpolating points.
C
10092 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.T2DS.GE.0..AND.(XCND.NE.XCOD.OR.YCND.NE.
     +YCOD))) GO TO 10098
          RUDO=RUDN
          RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +         (ABS(XCND-XCOD)+ABS(YCND-YCOD))
          IF (.NOT.(RUDN.GT.2.*RUDO)) GO TO 10099
            L10101=    1
            GO TO 10101
10100       CONTINUE
10099     CONTINUE
          IF (.NOT.(PITH.GT.0.)) GO TO 10102
            XCTD=XCND
            YCTD=YCND
            XCTU=XCNU
            YCTU=YCNU
            L10104=    1
            GO TO 10104
10103       CONTINUE
10102     CONTINUE
10098   CONTINUE
        NPLS=NPLS+1
        RWRK(IR01+NPLS)=XCNU
        RWRK(IR01+MPLS+NPLS)=YCNU
      GO TO (10091,10096) , L10092
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the contour line is seen.
C It checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10101 CONTINUE
        XC1D=XCOD
        YC1D=YCOD
        XC1U=XCOU
        YC1U=YCOU
        XC2D=XCND
        YC2D=YCND
        XC2U=XCNU
        YC2U=YCNU
        ITMP=0
10105   CONTINUE
          DSTO=ABS(XC2U-XC1U)+ABS(YC2U-YC1U)
          XC3D=(XC1D+XC2D)/2.
          YC3D=(YC1D+YC2D)/2.
          CALL HLUCPMPXY (IMPF,XAT1+RZDM*(XC3D-1.),
     +                         YAT1+RZDN*(YC3D-1.),
     +                                   XC3U,YC3U)
          IF (ICFELL('CPTRCL',4).NE.0) GO TO 102
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10106
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(MIN(DST1,DST2).GT.DSTO)) GO TO 10107
              ITMP=1000
              GO TO 10108
10107       CONTINUE
            IF (.NOT.(DST1.LT.DST2)) GO TO 10109
              IF (XC3D.EQ.XC1D.AND.YC3D.EQ.YC1D) GO TO 10108
              XC1D=XC3D
              YC1D=YC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10110
10109       CONTINUE
              IF (XC3D.EQ.XC2D.AND.YC3D.EQ.YC2D) GO TO 10108
              XC2D=XC3D
              YC2D=YC3D
              XC2U=XC3U
              YC2U=YC3U
10110       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10108
          GO TO 10111
10106     CONTINUE
            XCVD=XCOD
            YCVD=YCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XC3D
            YCID=YC3D
            L10084=    3
            GO TO 10084
10112       CONTINUE
            L10086=    2
            GO TO 10086
10113       CONTINUE
            L10061=    5
            GO TO 10061
10114       CONTINUE
            XCID=XC3D
            YCID=YC3D
            XCVD=XCND
            YCVD=YCND
            XCVU=XCNU
            YCVU=YCNU
            L10084=    4
            GO TO 10084
10115       CONTINUE
            L10086=    3
            GO TO 10086
10116       CONTINUE
            ITMP=1000
            GO TO 10108
10111     CONTINUE
        GO TO 10105
10108   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10117
          IF (.NOT.(IMPF.NE.0.AND.T2DS.GE.0..AND.PITH.GT.0.)) GO TO 1011
     +8
            XCTD=XC1D
            YCTD=YC1D
            XCTU=XC1U
            YCTU=YC1U
            L10104=    2
            GO TO 10104
10119       CONTINUE
10118     CONTINUE
          NPLS=NPLS+1
          RWRK(IR01+NPLS)=XC1U
          RWRK(IR01+MPLS+NPLS)=YC1U
          L10061=    6
          GO TO 10061
10120     CONTINUE
          IF (.NOT.(IMPF.NE.0.AND.T2DS.GE.0..AND.PITH.GT.0.)) GO TO 1012
     +1
            XCLD=XC2D
            YCLD=YC2D
            XCLU=XC2U
            YCLU=YC2U
10121     CONTINUE
          RWRK(IR01+1)=XC2U
          RWRK(IR01+MPLS+1)=YC2U
          NPLS=1
10117   CONTINUE
      GO TO (10100) , L10101
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10084 CONTINUE
        ITMP=0
10122   CONTINUE
          XCHD=(XCVD+XCID)/2.
          YCHD=(YCVD+YCID)/2.
          CALL HLUCPMPXY (IMPF,XAT1+RZDM*(XCHD-1.),
     +                         YAT1+RZDN*(YCHD-1.),
     +                                   XCHU,YCHU)
          IF (ICFELL('CPTRCL',5).NE.0) GO TO 102
          IF (.NOT.(XCHU.NE.OORV.AND.YCHU.NE.OORV)) GO TO 10123
            IF (XCHD.EQ.XCVD.AND.YCHD.EQ.YCVD) GO TO 10124
            XCVD=XCHD
            YCVD=YCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10125
10123     CONTINUE
            IF (XCHD.EQ.XCID.AND.YCHD.EQ.YCID) GO TO 10124
            XCID=XCHD
            YCID=YCHD
10125     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10124
        GO TO 10122
10124   CONTINUE
      GO TO (10083,10095,10112,10115) , L10084
C
C The following internal procedure outputs a visible edge point found
C by the previous internal procedure.
C
10086 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.T2DS.GE.0..AND.PITH.GT.0.)) GO TO 10126
          IF (.NOT.(NPLS.EQ.0)) GO TO 10127
            XCLD=XCVD
            YCLD=YCVD
            XCLU=XCVU
            YCLU=YCVU
          GO TO 10128
10127     CONTINUE
            XCTD=XCVD
            YCTD=YCVD
            XCTU=XCVU
            YCTU=YCVU
            L10104=    3
            GO TO 10104
10129       CONTINUE
10128     CONTINUE
10126   CONTINUE
        NPLS=NPLS+1
        RWRK(IR01+NPLS)=XCVU
        RWRK(IR01+MPLS+NPLS)=YCVU
      GO TO (10085,10113,10116) , L10086
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
10104 CONTINUE
10130   CONTINUE
        IF (.NOT.(ABS(XCTU-XCLU).GT.PITX.OR.ABS(YCTU-YCLU).GT.PITY))
     +  GO TO 10131
          IFND=0
          XCQD=0.
          YCQD=0.
          RDST=.50
          RSTP=.25
10132     CONTINUE
            XCPD=XCLD+RDST*(XCTD-XCLD)
            YCPD=YCLD+RDST*(YCTD-YCLD)
            CALL HLUCPMPXY (IMPF,XAT1+RZDM*(XCPD-1.),
     +                           YAT1+RZDN*(YCPD-1.),
     +                                     XCPU,YCPU)
            IF (ICFELL('CPTRCL',6).NE.0) GO TO 102
            IF (OORV.NE.0..AND.(XCPU.EQ.OORV.OR.YCPU.EQ.OORV)) GO TO 101
     +33
            IF (.NOT.(ABS(XCPU-XCLU).LT.PITX.AND.ABS(YCPU-YCLU).LT.PITY)
     +)     GO TO 10134
              IFND=1
              XCQD=XCPD
              YCQD=YCPD
              XCQU=XCPU
              YCQU=YCPU
              IF (ABS(XCQU-XCLU).GT..5*PITX.OR.ABS(YCQU-YCLU).GT..5*PITY
     +)       GO TO 10133
              RDST=RDST+RSTP
            GO TO 10135
10134       CONTINUE
              RDST=RDST-RSTP
10135       CONTINUE
            RSTP=RSTP/2.
            IF (RSTP.LT..0001) GO TO 10133
          GO TO 10132
10133     CONTINUE
          IF (.NOT.(IFND.NE.0.AND.(XCQD.NE.XCLD.OR.YCQD.NE.YCLD)))
     +    GO TO 10136
            NPLS=NPLS+1
            RWRK(IR01+NPLS)=XCQU
            RWRK(IR01+MPLS+NPLS)=YCQU
            IF (.NOT.(NPLS.EQ.MPLS)) GO TO 10137
              XSAV=RWRK(IR01+NPLS)
              YSAV=RWRK(IR01+MPLS+NPLS)
              L10061=    7
              GO TO 10061
10138         CONTINUE
              RWRK(IR01+1)=XSAV
              RWRK(IR01+MPLS+1)=YSAV
              NPLS=1
10137       CONTINUE
            XCLD=XCQD
            YCLD=YCQD
            XCLU=XCQU
            YCLU=YCQU
          GO TO 10139
10136     CONTINUE
            XCLD=XCTD
            YCLD=YCTD
            XCLU=XCTU
            YCLU=YCTU
10139     CONTINUE
        GO TO 10130
10131   CONTINUE
        XCLD=XCTD
        YCLD=YCTD
        XCLU=XCTU
        YCLU=YCTU
      GO TO (10103,10119,10129) , L10104
C
C The following internal procedure is given the data-system coordinates
C of a point (XCND,YCND) and computes the user-system coordinates of
C the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10041 CONTINUE
C
        XCNU=XAT1+RZDM*(XCND-1.)
        YCNU=YAT1+RZDN*(YCND-1.)
        IVNU=1
C
        IF (.NOT.(IMPF.NE.0.AND.T2DS.GE.0.)) GO TO 10140
          XTMP=XCNU
          YTMP=YCNU
          CALL HLUCPMPXY (IMPF,XTMP,YTMP,XCNU,YCNU)
          IF (ICFELL('CPTRCL',7).NE.0) GO TO 102
          IF ((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)) IVNU=0
10140   CONTINUE
C
      GO TO (10040,10051,10072) , L10041
C
C The following internal procedure processes a complete line segment.
C If the 2D smoother is turned on, the routines MSKRV1 and MSKRV2 are
C called to smooth the segment.
C
C
10061 CONTINUE
C
        I=1
C
10141   CONTINUE
          I=I+1
          IF (I.GT.NPLS) GO TO 10142
          IF (.NOT.(ABS(RWRK(IR01+I)-RWRK(IR01+I-1)).LT.EPSX.AND.ABS(RWR
     +K(IR01+MPLS+I)-RWRK(IR01+MPLS+I-1)).LT.EPSY)) GO TO 10143
            IF (.NOT.(I.NE.NPLS)) GO TO 10144
              DO 10145 J=I+1,NPLS
                RWRK(IR01+J-1)=RWRK(IR01+J)
                RWRK(IR01+MPLS+J-1)=RWRK(IR01+MPLS+J)
10145         CONTINUE
            GO TO 10146
10144       CONTINUE
              RWRK(IR01     +NPLS-1)=RWRK(IR01     +NPLS)
              RWRK(IR01+MPLS+NPLS-1)=RWRK(IR01+MPLS+NPLS)
10146       CONTINUE
            I=I-1
            NPLS=NPLS-1
10143     CONTINUE
        GO TO 10141
10142   CONTINUE
C
        IF (.NOT.(NPLS.GT.1)) GO TO 10147
C
          IF (.NOT.(T2DS.EQ.0.)) GO TO 10148
C
            IJMP=1
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
C
10148     CONTINUE
C
            IF (.NOT.(NPLS.GT.3.AND.ABS(RWRK(IR01+NPLS)-RWRK(IR01+1)).LT
     +.EPSX.AND.ABS(RWRK(IR01+MPLS+NPLS)-RWRK(IR01+MPLS+1)).LT.EPSY))
     +      GO TO 10149
              ISLP=4
            GO TO 10150
10149       CONTINUE
            IF (.NOT.(IFSF.EQ.0.AND.ABS(RWRK(IR01+1)-XELS).LT.EPSX.AND.A
     +BS(RWRK(IR01+MPLS+1)-YELS).LT.EPSY)) GO TO 10151
              ISLP=1
              SLP1=SELS
              IF (.NOT.(ABS(RWRK(IR01+NPLS)-XBFS).LT.EPSX.AND.ABS(RWRK(I
     +R01+MPLS+NPLS)-YBFS).LT.EPSY)) GO TO 10152
                ISLP=0
                SLPN=SBFS
10152         CONTINUE
            GO TO 10150
10151       CONTINUE
              ISLP=3
10150       CONTINUE
C
            CALL MSKRV1 (NPLS,RWRK(IR01+1),RWRK(IR01+MPLS+1),
     +                   SLP1,SLPN,RWRK(IR01+2*MPLS+1),
     +                   RWRK(IR01+3*MPLS+1),RWRK(IR01+5*MPLS+1),
     +                   RWRK(IR01+4*MPLS+1),ABS(T2DS),ISLP)
            IF (ICFELL('CPTRCL',8).NE.0) GO TO 102
C
            NINT=MAX(3,1+INT(RWRK(IR01+4*MPLS+NPLS)/DBPI))
C
            NOUT=0
            TUDN=0.
C
              IINT = 0
              GO TO 10155
10153         CONTINUE
              IINT =IINT +1
10155         CONTINUE
              IF (IINT .GT.(NINT)) GO TO 10154
C
              IF (.NOT.(IINT.EQ.0)) GO TO 10156
                XTMP=RWRK(IR01+1)
                YTMP=RWRK(IR01+MPLS+1)
              GO TO 10157
10156         CONTINUE
              IF (.NOT.(IINT.NE.NINT)) GO TO 10158
                CALL MSKRV2 (REAL(IINT)/REAL(NINT),XTMP,YTMP,NPLS,
     +                       RWRK(IR01+1),RWRK(IR01+MPLS+1),
     +                       RWRK(IR01+2*MPLS+1),RWRK(IR01+3*MPLS+1),
     +                       RWRK(IR01+4*MPLS+1),ABS(T2DS),0,DUMI)
                IF (ICFELL('CPTRCL',9).NE.0) GO TO 102
              GO TO 10157
10158         CONTINUE
                XTMP=RWRK(IR01+NPLS)
                YTMP=RWRK(IR01+MPLS+NPLS)
10157         CONTINUE
C
              IF (.NOT.(IMPF.EQ.0.OR.T2DS.GT.0.)) GO TO 10159
                NOUT=NOUT+1
                RWRK(IR01+5*MPLS+NOUT)=XTMP
                RWRK(IR01+6*MPLS+NOUT)=YTMP
              GO TO 10160
10159         CONTINUE
                IF (.NOT.(IINT.NE.0)) GO TO 10161
                  XTOD=XTND
                  YTOD=YTND
                  XTOU=XTNU
                  YTOU=YTNU
                  IVSO=IVSN
10161           CONTINUE
                XTND=XTMP
                YTND=YTMP
                CALL HLUCPMPXY (IMPF,XTND,YTND,XTNU,YTNU)
                IF (ICFELL('CPTRCL',10).NE.0) GO TO 102
                IVSN=1
                IF (OORV.NE.0..AND.
     +              (XTNU.EQ.OORV.OR.YTNU.EQ.OORV)) IVSN=0
                IF (.NOT.(NOUT.EQ.0)) GO TO 10162
                  IF (.NOT.(IVSN.NE.0)) GO TO 10163
                    IF (.NOT.(IINT.NE.0)) GO TO 10164
                      XTID=XTOD
                      YTID=YTOD
                      XTVD=XTND
                      YTVD=YTND
                      XTVU=XTNU
                      YTVU=YTNU
                      L10166=    1
                      GO TO 10166
10165                 CONTINUE
                      L10168=    1
                      GO TO 10168
10167                 CONTINUE
                      XTOD=XTVD
                      YTOD=YTVD
                      XTOU=XTVU
                      YTOU=YTVU
                      IVSO=1
10164               CONTINUE
                    IF (.NOT.(PITH.GT.0.)) GO TO 10169
                      IF (.NOT.(NOUT.EQ.0)) GO TO 10170
                        XTLD=XTND
                        YTLD=YTND
                        XTLU=XTNU
                        YTLU=YTNU
                      GO TO 10171
10170                 CONTINUE
                        XTTD=XTND
                        YTTD=YTND
                        XTTU=XTNU
                        YTTU=YTNU
                        L10173=    1
                        GO TO 10173
10172                   CONTINUE
10171                 CONTINUE
10169               CONTINUE
                    NOUT=NOUT+1
                    RWRK(IR01+5*MPLS+NOUT)=XTNU
                    RWRK(IR01+6*MPLS+NOUT)=YTNU
10163             CONTINUE
                GO TO 10174
10162           CONTINUE
                IF (.NOT.(IVSN.NE.0)) GO TO 10175
                  L10177=    1
                  GO TO 10177
10176             CONTINUE
                GO TO 10174
10175           CONTINUE
                  XTVD=XTOD
                  YTVD=YTOD
                  XTVU=XTOU
                  YTVU=YTOU
                  XTID=XTND
                  YTID=YTND
                  L10166=    2
                  GO TO 10166
10178             CONTINUE
                  XQND=XTND
                  YQND=YTND
                  XQNU=XTNU
                  YQNU=YTNU
                  XTND=XTVD
                  YTND=YTVD
                  XTNU=XTVU
                  YTNU=YTVU
                  L10177=    2
                  GO TO 10177
10179             CONTINUE
                  XTND=XQND
                  YTND=YQND
                  XTNU=XQNU
                  YTNU=YQNU
                  IJMP=2
                  IRW1=IR01+5*MPLS
                  IRW2=IR01+6*MPLS
                  NRWK=NOUT
                  RETURN
  103             NOUT=0
                  TUDN=0.
10174           CONTINUE
10160         CONTINUE
C
              IF (.NOT.((IINT.EQ.NINT.OR.NOUT.EQ.MPLS).AND.NOUT.NE.0))
     +        GO TO 10180
                XTMP=RWRK(IR01+5*MPLS+NOUT)
                YTMP=RWRK(IR01+6*MPLS+NOUT)
                IJMP=3
                IRW1=IR01+5*MPLS
                IRW2=IR01+6*MPLS
                NRWK=NOUT
                RETURN
  104           RWRK(IR01+5*MPLS+1)=XTMP
                RWRK(IR01+6*MPLS+1)=YTMP
                NOUT=1
10180         CONTINUE
C
            GO TO 10153
10154       CONTINUE
C
            IF (.NOT.(IFSF.NE.0)) GO TO 10181
              IFSF=0
              XBFS=RWRK(IR01+1)
              YBFS=RWRK(IR01+MPLS+1)
              CALL MSKRV2 (0.,XTMP,YTMP,NPLS,RWRK(IR01+1),
     +                     RWRK(IR01+MPLS+1),RWRK(IR01+2*MPLS+1),
     +                     RWRK(IR01+3*MPLS+1),RWRK(IR01+4*MPLS+1),
     +                     ABS(T2DS),1,SBFS)
              IF (ICFELL('CPTRCL',11).NE.0) GO TO 102
10181       CONTINUE
C
            XELS=RWRK(IR01+NPLS)
            YELS=RWRK(IR01+MPLS+NPLS)
            CALL MSKRV2 (1.,XTMP,YTMP,NPLS,RWRK(IR01+1),
     +                   RWRK(IR01+MPLS+1),RWRK(IR01+2*MPLS+1),
     +                   RWRK(IR01+3*MPLS+1),RWRK(IR01+4*MPLS+1),
     +                   ABS(T2DS),1,SELS)
            IF (ICFELL('CPTRCL',12).NE.0) GO TO 102
C
C
10147   CONTINUE
C
  105   NPLS=0
        RUDN=0.
C
C Done.
C
      GO TO (10060,10067,10089,10097,10114,10120,10138) , L10061
C
C The following internal procedure outputs the next point; if mapping
C is being done and there is a sufficiently large jump in the mapped
C position of the point, we check for a discontinuity in the mapping.
C Similarly, if mapping is being done and point interpolation is
C is activated, we check for a large enough jump in the mapped position
C to justify interpolating points.
C
10177 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.(XTND.NE.XTOD.OR.YTND.NE.YTOD)))
     +  GO TO 10182
          TUDO=TUDN
          TUDN=(ABS(XTNU-XTOU)+ABS(YTNU-YTOU))/
     +         (ABS(XTND-XTOD)+ABS(YTND-YTOD))
          IF (.NOT.(TUDN.GT.2.*TUDO)) GO TO 10183
            L10185=    1
            GO TO 10185
10184       CONTINUE
10183     CONTINUE
10182   CONTINUE
        IF (.NOT.(PITH.GT.0.)) GO TO 10186
          XTTD=XTND
          YTTD=YTND
          XTTU=XTNU
          YTTU=YTNU
          L10173=    2
          GO TO 10173
10187     CONTINUE
10186   CONTINUE
        NOUT=NOUT+1
        RWRK(IR01+5*MPLS+NOUT)=XTNU
        RWRK(IR01+6*MPLS+NOUT)=YTNU
      GO TO (10176,10179) , L10177
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the contour line is seen.
C It checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10185 CONTINUE
        XT1D=XTOD
        YT1D=YTOD
        XT1U=XTOU
        YT1U=YTOU
        XT2D=XTND
        YT2D=YTND
        XT2U=XTNU
        YT2U=YTNU
        ITMP=0
10188   CONTINUE
          DSTO=ABS(XT2U-XT1U)+ABS(YT2U-YT1U)
          XT3D=(XT1D+XT2D)/2.
          YT3D=(YT1D+YT2D)/2.
          CALL HLUCPMPXY (IMPF,XT3D,YT3D,XT3U,YT3U)
          IF (ICFELL('CPTRCL',13).NE.0) GO TO 102
          IF (.NOT.(OORV.EQ.0..OR.(XT3U.NE.OORV.AND.YT3U.NE.OORV)))
     +    GO TO 10189
            DST1=ABS(XT3U-XT1U)+ABS(YT3U-YT1U)
            DST2=ABS(XT3U-XT2U)+ABS(YT3U-YT2U)
            IF (.NOT.(MIN(DST1,DST2).GT.DSTO)) GO TO 10190
              ITMP=1000
              GO TO 10191
10190       CONTINUE
            IF (.NOT.(DST1.LT.DST2)) GO TO 10192
              IF (XT3D.EQ.XT1D.AND.YT3D.EQ.YT1D) GO TO 10191
              XT1D=XT3D
              YT1D=YT3D
              XT1U=XT3U
              YT1U=YT3U
            GO TO 10193
10192       CONTINUE
              IF (XT3D.EQ.XT2D.AND.YT3D.EQ.YT2D) GO TO 10191
              XT2D=XT3D
              YT2D=YT3D
              XT2U=XT3U
              YT2U=YT3U
10193       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10191
          GO TO 10194
10189     CONTINUE
            XTVD=XTOD
            YTVD=YTOD
            XTVU=XTOU
            YTVU=YTOU
            XTID=XT3D
            YTID=YT3D
            L10166=    3
            GO TO 10166
10195       CONTINUE
            L10168=    2
            GO TO 10168
10196       CONTINUE
            IJMP=4
            IRW1=IR01+5*MPLS
            IRW2=IR01+6*MPLS
            NRWK=NOUT
            RETURN
  106       NOUT=0
            TUDN=0.
            XTID=XT3D
            YTID=YT3D
            XTVD=XTND
            YTVD=YTND
            XTVU=XTNU
            YTVU=YTNU
            L10166=    4
            GO TO 10166
10197       CONTINUE
            L10168=    3
            GO TO 10168
10198       CONTINUE
            ITMP=1000
            GO TO 10191
10194     CONTINUE
        GO TO 10188
10191   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XT1U-XT2U).GT.SMLX.OR.ABS(YT1U-Y
     +T2U).GT.SMLY))) GO TO 10199
          IF (.NOT.(PITH.GT.0.)) GO TO 10200
            XTTD=XT1D
            YTTD=YT1D
            XTTU=XT1U
            YTTU=YT1U
            L10173=    3
            GO TO 10173
10201       CONTINUE
10200     CONTINUE
          NOUT=NOUT+1
          RWRK(IR01+5*MPLS+NOUT)=XT1U
          RWRK(IR01+6*MPLS+NOUT)=YT1U
          IJMP=5
          IRW1=IR01+5*MPLS
          IRW2=IR01+6*MPLS
          NRWK=NOUT
          RETURN
  107     RWRK(IR01+5*MPLS+1)=XT2U
          RWRK(IR01+6*MPLS+1)=YT2U
          NOUT=1
          IF (.NOT.(PITH.GT.0.)) GO TO 10202
            XTLD=XT2D
            YTLD=YT2D
            XTLU=XT2U
            YTLU=YT2U
10202     CONTINUE
          TUDN=0.
10199   CONTINUE
      GO TO (10184) , L10185
C
C Given two points in the unmapped user coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10166 CONTINUE
        ITMP=0
10203   CONTINUE
          XTHD=(XTVD+XTID)/2.
          YTHD=(YTVD+YTID)/2.
          CALL HLUCPMPXY (IMPF,XTHD,YTHD,XTHU,YTHU)
          IF (ICFELL('CPTRCL',14).NE.0) GO TO 102
          IF (.NOT.(XTHU.NE.OORV.AND.YTHU.NE.OORV)) GO TO 10204
            IF (XTHD.EQ.XTVD.AND.YTHD.EQ.YTVD) GO TO 10205
            XTVD=XTHD
            YTVD=YTHD
            XTVU=XTHU
            YTVU=YTHU
          GO TO 10206
10204     CONTINUE
            IF (XTHD.EQ.XTID.AND.YTHD.EQ.YTID) GO TO 10205
            XTID=XTHD
            YTID=YTHD
10206     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10205
        GO TO 10203
10205   CONTINUE
      GO TO (10165,10178,10195,10197) , L10166
C
C The following internal procedure outputs a visible edge point found
C by the previous internal procedure.
C
10168 CONTINUE
        IF (.NOT.(PITH.GT.0.)) GO TO 10207
          IF (.NOT.(NOUT.EQ.0)) GO TO 10208
            XTLD=XTVD
            YTLD=YTVD
            XTLU=XTVU
            YTLU=YTVU
          GO TO 10209
10208     CONTINUE
            XTTD=XTVD
            YTTD=YTVD
            XTTU=XTVU
            YTTU=YTVU
            L10173=    4
            GO TO 10173
10210       CONTINUE
10209     CONTINUE
10207   CONTINUE
        NOUT=NOUT+1
        RWRK(IR01+5*MPLS+NOUT)=XTVU
        RWRK(IR01+6*MPLS+NOUT)=YTVU
      GO TO (10167,10196,10198) , L10168
C
C The following internal procedure is invoked when mapping is being
C done and a new point is about to be added to the polyline buffer.
C It checks for a jump greater than a user-defined threshold value in
C the mapped coordinates of the point and, if such a jump is found,
C interpolates some points in between.  The assumption is made that
C all points in between are visible; if that is found not to be the
C case, no attempt is made to rectify the situation: the user probably
C screwed up the definition of the mapping function.
C
10173 CONTINUE
10211   CONTINUE
        IF (.NOT.(ABS(XTTU-XTLU).GT.PITX.OR.ABS(YTTU-YTLU).GT.PITY))
     +  GO TO 10212
          IFND=0
          XTQD=0.
          YTQD=0.
          RDST=.50
          RSTP=.25
10213     CONTINUE
            XTPD=XTLD+RDST*(XTTD-XTLD)
            YTPD=YTLD+RDST*(YTTD-YTLD)
            CALL HLUCPMPXY (IMPF,XTPD,YTPD,XTPU,YTPU)
            IF (ICFELL('CPTRCL',15).NE.0) GO TO 102
            IF (OORV.NE.0..AND.(XTPU.EQ.OORV.OR.YTPU.EQ.OORV)) GO TO 102
     +14
            IF (.NOT.(ABS(XTPU-XTLU).LT.PITX.AND.ABS(YTPU-YTLU).LT.PITY)
     +)     GO TO 10215
              IFND=1
              XTQD=XTPD
              YTQD=YTPD
              XTQU=XTPU
              YTQU=YTPU
              IF (ABS(XTQU-XTLU).GT..5*PITX.OR.ABS(YTQU-YTLU).GT..5*PITY
     +)       GO TO 10214
              RDST=RDST+RSTP
            GO TO 10216
10215       CONTINUE
              RDST=RDST-RSTP
10216       CONTINUE
            RSTP=RSTP/2.
            IF (RSTP.LT..0001) GO TO 10214
          GO TO 10213
10214     CONTINUE
          IF (.NOT.(IFND.NE.0.AND.(XTQD.NE.XTLD.OR.YTQD.NE.YTLD)))
     +    GO TO 10217
            NOUT=NOUT+1
            RWRK(IR01+5*MPLS+NOUT)=XTQU
            RWRK(IR01+6*MPLS+NOUT)=YTQU
            IF (.NOT.(NOUT.EQ.MPLS)) GO TO 10218
              XTMP=RWRK(IR01+5*MPLS+NOUT)
              YTMP=RWRK(IR01+6*MPLS+NOUT)
              IJMP=6
              IRW1=IR01+5*MPLS
              IRW2=IR01+6*MPLS
              NRWK=NOUT
              RETURN
  108         RWRK(IR01+5*MPLS+1)=XTMP
              RWRK(IR01+6*MPLS+1)=YTMP
              NOUT=1
10218       CONTINUE
            XTLD=XTQD
            YTLD=YTQD
            XTLU=XTQU
            YTLU=YTQU
          GO TO 10219
10217     CONTINUE
            XTLD=XTTD
            YTLD=YTTD
            XTLU=XTTU
            YTLU=YTTU
10219     CONTINUE
        GO TO 10211
10212   CONTINUE
        XTLD=XTTD
        YTLD=YTTD
        XTLU=XTTU
        YTLU=YTTU
      GO TO (10172,10187,10201,10210) , L10173
C
      END

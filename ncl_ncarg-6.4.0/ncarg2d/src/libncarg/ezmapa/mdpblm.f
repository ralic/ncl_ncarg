      SUBROUTINE MDPBLM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      INTEGER IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
C
C Declare required common blocks.
C
      COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                 SROT,SIN1,TOPI,TSRT
      DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                 SROT,SIN1,TOPI,TSRT
      SAVE   /MAPCM0/
C
      COMMON /MAPCM1/  COSO,COSR,SINO,SINR,IPRJ,IROD
      DOUBLE PRECISION COSO,COSR,SINO,SINR
      INTEGER          IPRJ,IROD
      SAVE   /MAPCM1/
C
      COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
      DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG
      INTEGER          ISSL
      SAVE   /MAPCM2/
C
      COMMON /MAPCM3/  ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,
     +                 SLOG,PNTS(200),IDOS(4)
      INTEGER          ITPN,NOUT,NPTS,IGID,IDLS,IDRS,IDOS
      REAL             BLAG,SLAG,BLOG,SLOG,PNTS
      SAVE   /MAPCM3/
C
      COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                 PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                 SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                 ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
      DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                 PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                 SRCH,XLOW,XROW,YBOW,YTOW
      INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
      LOGICAL          ELPF,INTF,LBLF,PRMF
      SAVE   /MAPCM4/
C
      COMMON /MAPCMC/  IGI1,IGI2,NCRA,NOVS,XCRA(100),YCRA(100)
      INTEGER          IGI1,IGI2,NCRA,NOVS
      REAL             XCRA,YCRA
      SAVE   /MAPCMC/
C
      COMMON /MAPCMW/  CSLS,CSLT,SLTD,ISLT
      DOUBLE PRECISION CSLS,CSLT,SLTD
      INTEGER ISLT
      SAVE   /MAPCMW/
C
      COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
      DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
      SAVE   /MAPSAT/
C
      COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
      DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
      INTEGER IPRF
      SAVE   /USGSC1/
C
C Declare local variables.
C
      INTEGER          I,IGIS,IPEN,IVIS,IWGF,J,K,NSEG
C
      DOUBLE PRECISION ALPH,CLAT,CLON,CRAD,COSA,COSB,COSL,COSP,DEPS,
     +                 DIST,DLAT,DLON,DR,DS,SINA,SINB,SINL,SINP,RLAT,
     +                 RLON,RVTU,SSLT,TEMP,U,UCIR,UEDG,UNS1,UOLD,URAD,
     +                 V,VCIR,VEDG,VNS1,VOLD,X,XANP,XAS1,XAS2,XCRD,
     +                 YANP,YAS1,YAS2,YCRD
C
C Declare a couple of temporary arrays to hold coordinates of a circle.
C
      DOUBLE PRECISION TLAT(361),TLON(361)
C
C Dimension the arrays needed to define some lines across the map.
C
      REAL             XCR(2),YCR(2)
C
C Declare arithmetic statement functions.
C
      DOUBLE PRECISION CEIL,FLOR
C
C Declare external functions.
C
      DOUBLE PRECISION RBGDFE,RBGLEN
C
C The arithmetic statement functions FLOR and CEIL give, respectively,
C the "floor" of X - the largest integer less than or equal to X - and
C the "ceiling" of X - the smallest integer greater than or equal to X.
C
      FLOR(X)=DINT(X+1.D4)-1.D4
      CEIL(X)=-FLOR(-X)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MDPBLM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MDPINT
      IF (ICFELL('MDPBLM',2).NE.0) RETURN
10000 CONTINUE
C
C If the perimeter is to be drawn ...
C
      IF (.NOT.(PRMF)) GO TO 10001
C
C ... reset the color index and dash pattern for the perimeter ...
C
      CALL MDPCHM (1,IOR(ISHIFT(32767,1),1),
     +                              IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',3).NE.0) RETURN
C
C .. and draw the perimeter.
C
      IF (.NOT.(ELPF)) GO TO 10002
      TEMP=.9998D0
10003 CONTINUE
      U=URNG
      V=0.D0
      XCRD=UCEN+TEMP*U
      YCRD=VCEN
      L10005=    1
      GO TO 10005
10004 CONTINUE
      I = 1
      GO TO 10008
10006 CONTINUE
      I =I +1
10008 CONTINUE
      IF (I .GT.(360)) GO TO 10007
      U=URNG*COS(DTOR*DBLE(I))
      V=URNG*SIN(DTOR*DBLE(I))
      XCRD=UCEN+TEMP*U
      YCRD=VCEN+TEMP*V*VRNG/URNG
      L10010=    1
      GO TO 10010
10009 CONTINUE
      GO TO 10006
10007 CONTINUE
      IF (TEMP.EQ.1.0002D0) GO TO 10011
      TEMP=1.0002D0
      GO TO 10003
10011 CONTINUE
      GO TO 10012
10002 CONTINUE
      XCRD=UMIN
      YCRD=VMIN
      L10005=    2
      GO TO 10005
10013 CONTINUE
      XCRD=UMAX
      YCRD=VMIN
      L10010=    2
      GO TO 10010
10014 CONTINUE
      XCRD=UMAX
      YCRD=VMAX
      L10010=    3
      GO TO 10010
10015 CONTINUE
      XCRD=UMIN
      YCRD=VMAX
      L10010=    4
      GO TO 10010
10016 CONTINUE
      XCRD=UMIN
      YCRD=VMIN
      L10010=    5
      GO TO 10010
10017 CONTINUE
10012 CONTINUE
      L10019=    1
      GO TO 10019
10018 CONTINUE
C
C Restore the color index and dash pattern.
C
      CALL MDPCHM (-1,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',4).NE.0) RETURN
C
10001 CONTINUE
C
C Reset the color index and dash pattern for limb lines.
C
      CALL MDPCHM (4,IOR(ISHIFT(32767,1),1),
     +                                IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',5).NE.0) RETURN
C
C Draw the limb line.
C
C Projection:   US  LC  ST  OR  LE  GN  AE
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (arbitrary)
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (fast-path)
C                       RM
C
      GO TO (100,101,108,102,103,108,104,
     +           107,107,105,107,107,109,110,110,107,
     +           107,107,105,107,107,109,110,110,107,
     +               107                            ) , IPRJ+1
C
C USGS transformations.
C
  100 CONTINUE
      IF (.NOT.(IPRF.EQ.3.OR.IPRF.EQ.4.OR.IPRF.EQ.5.OR.IPRF.EQ.7.OR.IPRF
     +.EQ.8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.IPRF.EQ.19.OR.I
     +PRF.EQ.21)) GO TO 10020
      IF (.NOT.(IPRF.EQ.3.OR.IPRF.EQ.8.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.IP
     +RF.EQ.21)) GO TO 10021
      DLON=GRDR
      RLAT=-89.998D0
      K=CEIL(360.D0/DLON)
      DO 10022 I=1,2
      RLON=UTPA(5)-180.D0
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',6).NE.0) RETURN
      DO 10023 J=1,K-1
      RLON=RLON+DLON
      CALL MDPITM (RLAT,RLON,1,
     +                        IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',7).NE.0) RETURN
10023 CONTINUE
      RLON=UTPA(5)+180.D0
      CALL MDPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',8).NE.0) RETURN
      CALL MDPIQM (            IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',9).NE.0) RETURN
      RLAT=89.998D0
10022 CONTINUE
      L10019=    2
      GO TO 10019
10024 CONTINUE
10021 CONTINUE
      IF (.NOT.(IPRF.EQ.7)) GO TO 10025
      DLON= 89.999999D0
      GO TO 10026
10025 CONTINUE
      DLON=179.999999D0
10026 CONTINUE
      DLAT=GRDR
      RLON=UTPA(5)+DLON
      K=CEIL(180.D0/DLAT)
      DO 10027 I=1,2
      RLAT=-90.D0
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',10).NE.0) RETURN
      DO 10028 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',11).NE.0) RETURN
10028 CONTINUE
      RLAT=90.D0
      CALL MDPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',12).NE.0) RETURN
      CALL MDPIQM (            IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',13).NE.0) RETURN
      RLON=UTPA(5)-DLON
10027 CONTINUE
      L10019=    3
      GO TO 10019
10029 CONTINUE
      GO TO 108
10020 CONTINUE
      IF (.NOT.(IPRF.EQ.9)) GO TO 10030
      DLON=GRDR
      RLAT=-.001D0
      K=CEIL(180.D0/DLON)
      DO 10031 I=1,2
      RLON=UTPA(5)+90.D0
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',14).NE.0) RETURN
      DO 10032 J=1,K-1
      RLON=RLON+DLON
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',15).NE.0) RETURN
10032 CONTINUE
      RLON=UTPA(5)+270.D0
      CALL MDPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',16).NE.0) RETURN
      CALL MDPIQM (            IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',17).NE.0) RETURN
      RLAT=.001D0
10031 CONTINUE
      L10019=    4
      GO TO 10019
10033 CONTINUE
      GO TO 108
10030 CONTINUE
      IF (.NOT.(IPRF.EQ.11.OR.IPRF.EQ.12.OR.IPRF.EQ.14.OR.IPRF.EQ.15.OR.
     +IPRF.EQ.23)) GO TO 10034
      IF (.NOT.(IPRF.EQ.11.OR.IPRF.EQ.12)) GO TO 10035
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=179.95D0
      GO TO 10036
10035 CONTINUE
      IF (.NOT.(IPRF.EQ.14)) GO TO 10037
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=89.999D0
      GO TO 10036
10037 CONTINUE
      IF (.NOT.(IPRF.EQ.15)) GO TO 10038
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=RTOD*ACOS(UTPA(1)/(UTPA(1)+UTPA(3)))-.001D0
      GO TO 10036
10038 CONTINUE
      IF (.NOT.(IPRF.EQ.23)) GO TO 10039
      CLAT=  64.D0
      CLON=-152.D0
      CRAD=  29.999D0
10036 CONTINUE
10039 CONTINUE
      CALL MDGCOG (CLAT,CLON,CRAD,TLAT,TLON,361)
      CALL MDPITM (TLAT(1),TLON(1),0,
     +                              IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',18).NE.0) RETURN
      DO 10040 I=2,360
      CALL MDPITM (TLAT(I),TLON(I),1,
     +                            IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',19).NE.0) RETURN
10040 CONTINUE
      CALL MDPITM (TLAT(361),TLON(361),2,
     +                              IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',20).NE.0) RETURN
      CALL MDPIQM (                 IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',21).NE.0) RETURN
      GO TO 108
10034 CONTINUE
      IF (.NOT.(IPRF.EQ.20)) GO TO 10041
      ALPH=DTOR*(180.D0-UTPA(4))
      CALL MDPTRN (+90.D0,0.D0,XANP,YANP)
      IF (ICFELL('MDPBLM',22).NE.0) RETURN
      CALL MDPTRN (-90.D0,0.D0,XAS1,YAS1)
      IF (ICFELL('MDPBLM',23).NE.0) RETURN
      UNS1=(XAS1-XANP)*COS(ALPH)+(YAS1-YANP)*SIN(ALPH)
      VNS1=(YAS1-YANP)*COS(ALPH)-(XAS1-XANP)*SIN(ALPH)
      XAS2=XANP+VNS1*SIN(ALPH)+UNS1*COS(ALPH)
      YAS2=YANP+UNS1*SIN(ALPH)-VNS1*COS(ALPH)
      DIST=SQRT((XAS2-XAS1)*(XAS2-XAS1)+(YAS2-YAS1)*(YAS2-YAS1))
      IF (.NOT.(VNS1.LT.0.D0)) GO TO 10042
      DEPS=-.001D0*DIST
      GO TO 10043
10042 CONTINUE
      DEPS=+.001D0*DIST
10043 CONTINUE
      DIST=2.D0*DIST
      XCR(1)=REAL(XAS1-DIST*COS(ALPH)+DEPS*SIN(ALPH))
      YCR(1)=REAL(YAS1-DIST*SIN(ALPH)-DEPS*COS(ALPH))
      XCR(2)=REAL(XAS1+DIST*COS(ALPH)+DEPS*SIN(ALPH))
      YCR(2)=REAL(YAS1+DIST*SIN(ALPH)-DEPS*COS(ALPH))
      CALL ARDRLN (IAM,XCR,YCR,2,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',24).NE.0) RETURN
      XCR(1)=REAL(XAS2-DIST*COS(ALPH)-DEPS*SIN(ALPH))
      YCR(1)=REAL(YAS2-DIST*SIN(ALPH)+DEPS*COS(ALPH))
      XCR(2)=REAL(XAS2+DIST*COS(ALPH)-DEPS*SIN(ALPH))
      YCR(2)=REAL(YAS2+DIST*SIN(ALPH)+DEPS*COS(ALPH))
      CALL ARDRLN (IAM,XCR,YCR,2,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',25).NE.0) RETURN
      GO TO 108
10041 CONTINUE
      GO TO 108
C
C Lambert conformal conic.
C
  101 DLAT=GRDR
      RLON=PLNO+179.999999D0
      K=CEIL(180.D0/DLAT)
      DO 10044 I=1,2
      RLAT=-90.D0
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',26).NE.0) RETURN
      DO 10045 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',27).NE.0) RETURN
10045 CONTINUE
      RLAT=RLAT+DLAT
      CALL MDPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',28).NE.0) RETURN
      RLON=PLNO-179.999999D0
10044 CONTINUE
      L10019=    5
      GO TO 10019
10046 CONTINUE
      GO TO 108
C
C Orthographic (or satellite-view).
C
  102 CONTINUE
      IF (.NOT.(ABS(SALT).LE.1.D0.OR.ALFA.EQ.0.D0)) GO TO 10047
      URAD=1.D0
      RVTU=1.D0
      GO TO 106
10047 CONTINUE
      SSLT=SALT
      SALT=-ABS(SALT)
      DR=.9998D0
10048 CONTINUE
      IPEN=0
      DO 10049 I=1,361
      COSB=COS(DTOR*DBLE(I-1))
      SINB=SIN(DTOR*DBLE(I-1))
      IF (.NOT.(DR.LT.1.D0)) GO TO 10050
      COSA=(DR*DR*ABS(SALT)+SSMO*SQRT(1.D0-DR*DR))/
     +                                 (DR*DR+SSMO)
      GO TO 10051
10050 CONTINUE
      DS=2.D0-DR
      COSA=(DS*DS*ABS(SALT)-SSMO*SQRT(1.D0-DS*DS))/
     +                                 (DS*DS+SSMO)
10051 CONTINUE
      SINA=SQRT(1.D0-COSA*COSA)
      SINL=SINA*SINB
      COSL=COSA*COSO-SINA*SINO*COSB
      COSP=SQRT(SINL*SINL+COSL*COSL)
      IF (.NOT.(COSP.NE.0.D0)) GO TO 10052
      SINL=SINL/COSP
      COSL=COSL/COSP
10052 CONTINUE
      IF (.NOT.(ABS(SINO).GT..000001D0)) GO TO 10053
      SINP=(COSA-COSP*COSL*COSO)/SINO
      GO TO 10054
10053 CONTINUE
      SINP=SINA*COSB
10054 CONTINUE
      RLAT=RTOD*ATAN2(SINP,COSP)
      RLON=PLNO+RTOD*ATAN2(SINA*SINB,
     +                     COSA*COSO-SINA*SINO*COSB)
      IF (ABS(RLON).GT.180.D0) RLON=RLON-SIGN(360.D0,RLON)
      CALL MDPITM (RLAT,RLON,IPEN,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',29).NE.0) RETURN
      IPEN=1
      IF (I.EQ.360) IPEN=2
10049 CONTINUE
      L10019=    6
      GO TO 10019
10055 CONTINUE
      IF (DR.EQ.1.0002D0) GO TO 10056
      DR=1.0002D0
      GO TO 10048
10056 CONTINUE
      SALT=SSLT
      GO TO 108
C
C Lambert equal-area.  Note:  The constant "1.999999500000" is the real
C effective radius of the limb of the Lambert equal area projection, as
C determined by the test at statement number 106 in the routine MDPTRN.
C
  103 URAD=1.999999500000D0
      RVTU=1.D0
      GO TO 106
C
C Azimuthal equidistant.  Note:  The constant "3.140178439909" is the
C real effective radius of the limb of the azimuthal equidistant
C projection, as determined by the test at statement number 108 in the
C routine MDPTRN.
C
  104 URAD=3.140178439909D0
      RVTU=1.D0
      GO TO 106
C
C Mollweide type.
C
  105 URAD=2.D0
      RVTU=0.5D0
      GO TO 106
C
C Aitoff.
C
  109 URAD=3.14159265358979D0
      RVTU=.5D0
      GO TO 106
C
C Hammer and true Mollweide.
C
  110 URAD=2.82842712474619D0
      RVTU=.5D0
      GO TO 106
C
  106 IF (ELPF.AND.ABS(UCEN).LT..0001D0.AND.
     +             ABS(VCEN).LT..0001D0.AND.
     +             ABS(URNG-URAD).LT..0001D0.AND.
     +             ABS(VRNG/URNG-RVTU).LT..0001D0) GO TO 108
C
      TEMP=.9998D0
C
10057 CONTINUE
      IVIS=-1
      I = 1
      GO TO 10060
10058 CONTINUE
      I =I +1
10060 CONTINUE
      IF (I .GT.(361)) GO TO 10059
      UCIR=TEMP*URAD*COS(DTOR*DBLE(I-1))
      VCIR=TEMP*URAD*SIN(DTOR*DBLE(I-1))
      U=UCIR-UOFF
      V=RVTU*VCIR-VOFF
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10061
      IF (.NOT.(IVIS.EQ.1)) GO TO 10062
      CALL MDPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=    6
      GO TO 10010
10063 CONTINUE
10062 CONTINUE
      IVIS=0
      GO TO 10064
10061 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.D0)
     +)) GO TO 10065
      IF (.NOT.(IVIS.EQ.1)) GO TO 10066
      CALL MDPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=    7
      GO TO 10010
10067 CONTINUE
10066 CONTINUE
      IVIS=0
      GO TO 10064
10065 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10068
      XCRD=U
      YCRD=V
      L10005=    3
      GO TO 10005
10069 CONTINUE
      IVIS=1
      GO TO 10070
10068 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10071
      IF (.NOT.ELPF) CALL MDPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MDPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    4
      GO TO 10005
10072 CONTINUE
      IVIS=1
10071 CONTINUE
      XCRD=U
      YCRD=V
      L10010=    8
      GO TO 10010
10073 CONTINUE
10070 CONTINUE
10064 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10058
10059 CONTINUE
      L10019=    7
      GO TO 10019
10074 CONTINUE
      IF (TEMP.EQ.1.0002D0) GO TO 10075
      TEMP=1.0002D0
      GO TO 10057
10075 CONTINUE
      GO TO 108
C
C Cylindrical equidistant, Mercator, Robinson, cylindrical equal-area,
C Winkel tripel.
C
  107 TEMP=.9998D0
C
10076 CONTINUE
      IVIS=-1
      RLAT=-90.D0
      RLON=-180.D0
      I = 1
      GO TO 10079
10077 CONTINUE
      I =I +1
10079 CONTINUE
      IF (I .GT.(361)) GO TO 10078
      IF (.NOT.(IPRJ.EQ.7.OR.IPRJ.EQ.16)) GO TO 10080
      U=TEMP*RLON-UOFF
      V=TEMP*RLAT/CSLT-VOFF
      GO TO 10081
10080 CONTINUE
      IF (.NOT.(IPRJ.EQ.8.OR.IPRJ.EQ.17.OR.IPRJ.EQ.25)) GO TO 10082
      U=TEMP*DTOR*RLON-UOFF
      V=TEMP*LOG(TAN((MAX(-89.999999D0,
     +                MIN(+89.999999D0,RLAT))+90.D0)*DTRH))-VOFF
      IF (.NOT.(IPRJ.EQ.25)) GO TO 10083
      UTMP=U*COSR+V*SINR
      VTMP=V*COSR-U*SINR
      U=UTMP
      V=VTMP
10083 CONTINUE
      GO TO 10081
10082 CONTINUE
      IF (.NOT.(IPRJ.EQ.11.OR.IPRJ.EQ.20)) GO TO 10084
      U=TEMP*DTOR*RLON-UOFF
      V=TEMP*SIN(DTOR*RLAT)/CSLS-VOFF
      GO TO 10081
10084 CONTINUE
      IF (.NOT.(IPRJ.EQ.15.OR.IPRJ.EQ.24)) GO TO 10085
      CALL WTPROJ (TEMP*DTOR*RLAT,TEMP*DTOR*RLON,U,V,CSLT)
      U=U-UOFF
      V=V-VOFF
      GO TO 10081
10085 CONTINUE
      U=TEMP*(RLON/180.D0)*RBGLEN(RLAT)-UOFF
      V=TEMP*RBGDFE(RLAT)-VOFF
10081 CONTINUE
      IF (.NOT.(I.LE.90)) GO TO 10086
      RLON=RLON+4.D0
      GO TO 10087
10086 CONTINUE
      IF (.NOT.(I.LE.180)) GO TO 10088
      RLAT=RLAT+2.D0
      GO TO 10087
10088 CONTINUE
      IF (.NOT.(I.LE.270)) GO TO 10089
      RLON=RLON-4.D0
      GO TO 10087
10089 CONTINUE
      IF (.NOT.(I.LE.360)) GO TO 10090
      RLAT=RLAT-2.D0
10087 CONTINUE
10090 CONTINUE
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10091
      IF (.NOT.(IVIS.EQ.1)) GO TO 10092
      CALL MDPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=    9
      GO TO 10010
10093 CONTINUE
10092 CONTINUE
      IVIS=0
      GO TO 10094
10091 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.D0)
     +)) GO TO 10095
      IF (.NOT.(IVIS.EQ.1)) GO TO 10096
      CALL MDPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   10
      GO TO 10010
10097 CONTINUE
10096 CONTINUE
      IVIS=0
      GO TO 10094
10095 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10098
      XCRD=U
      YCRD=V
      L10005=    5
      GO TO 10005
10099 CONTINUE
      IVIS=1
      GO TO 10100
10098 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10101
      IF (.NOT.ELPF) CALL MDPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MDPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    6
      GO TO 10005
10102 CONTINUE
      IVIS=1
10101 CONTINUE
      XCRD=U
      YCRD=V
      L10010=   11
      GO TO 10010
10103 CONTINUE
10100 CONTINUE
10094 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10077
10078 CONTINUE
      L10019=    8
      GO TO 10019
10104 CONTINUE
      IF (TEMP.EQ.1.0002D0) GO TO 10105
      TEMP=1.0002D0
      GO TO 10076
10105 CONTINUE
C
C Restore the color index and dash pattern.
C
  108 CALL MDPCHM (-4,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',30).NE.0) RETURN
C
C If the selected outline type is "NONE", quit; no outlines need to
C be drawn.
C
      IF (NOUT.LE.0) RETURN
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each outline point group for intersection with
C the window.
C
      IWGF=0
      IF (BLAM-SLAM.GT.179.D0.AND.BLOM-SLOM.GT.359.D0) IWGF=1
C
C IGIS keeps track of changes in the group identifier, so that the
C color index can be changed when necessary.
C
      IGIS=0
C
C Position to the user-selected portion of the outline dataset.
C
      CALL MDPIO (1)
      IF (ICFELL('MDPBLM',31).NE.0) RETURN
      NSEG=0
C
C Read the next record (group of points).
C
  301 CALL MDPIO (2)
      IF (ICFELL('MDPBLM',32).NE.0) RETURN
      NSEG=NSEG+1
C
C Check for the end of the desired data.
C
      IF (NPTS.EQ.0) GO TO 303
C
C If less than the whole globe is shown by the projection, do a quick
C check for intersection of the box surrounding the point group with
C the area shown.
C
      IF (.NOT.(IWGF.EQ.0)) GO TO 10106
      IF (DBLE(SLAG).GT.BLAM.OR.DBLE(BLAG).LT.SLAM) GO TO 301
      IF ((DBLE(SLOG     ).GT.BLOM.OR.
     +     DBLE(BLOG     ).LT.SLOM).AND.
     +    (DBLE(SLOG-360.).GT.BLOM.OR.
     +     DBLE(BLOG-360.).LT.SLOM).AND.
     +    (DBLE(SLOG+360.).GT.BLOM.OR.
     +     DBLE(BLOG+360.).LT.SLOM)) GO TO 301
10106 CONTINUE
C
C See if the user wants to omit this point group.
C
      CALL HLUMAPEOD (NOUT,NSEG,IDOS(NOUT)+IDLS,
     +                          IDOS(NOUT)+IDRS,NPTS,PNTS)
      IF (ICFELL('MDPBLM',33).NE.0) RETURN
      IF (NPTS.LE.1) GO TO 301
C
C If we've switched to a new group, set the color index, dotting, and
C dash pattern for the group.
C
      IF (.NOT.(IGID.NE.IGIS)) GO TO 10107
      IF (.NOT.(IGIS.NE.0)) GO TO 10108
      CALL MDPCHM (-4-IGIS,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',34).NE.0) RETURN
10108 CONTINUE
      CALL MDPCHM (4+IGID,IOR(ISHIFT(32767,1),1),
     +                              IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',35).NE.0) RETURN
      IGIS=IGID
10107 CONTINUE
C
C Plot the group.
C
      CALL MAPITM (PNTS(1),PNTS(2),0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',36).NE.0) RETURN
C
      DO 10109 K=2,NPTS-1
      CALL MAPITM (PNTS(2*K-1),PNTS(2*K),1,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',37).NE.0) RETURN
10109 CONTINUE
C
      CALL MAPITM (PNTS(2*NPTS-1),PNTS(2*NPTS),2,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',38).NE.0) RETURN
C
C Force a buffer dump.
C
      L10019=    9
      GO TO 10019
10110 CONTINUE
C
C Go get another group.
C
      GO TO 301
C
C Reset the color index, dotting, and dash pattern, if necessary.
C
  303 CONTINUE
      IF (.NOT.(IGIS.NE.0)) GO TO 10111
      CALL MDPCHM (-4-IGIS,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',39).NE.0) RETURN
10111 CONTINUE
C
C Done.
C
      RETURN
C
C The following internal procedure is invoked to start a line.
C
10005 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10112
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',40).NE.0) RETURN
10112 CONTINUE
      XCRA(1)=REAL(XCRD)
      YCRA(1)=REAL(YCRD)
      NCRA=1
      GO TO (10004,10013,10069,10072,10099,10102) , L10005
C
C The following internal procedure is invoked to continue a line.
C
10010 CONTINUE
      IF (.NOT.(NCRA.EQ.100)) GO TO 10113
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',41).NE.0) RETURN
      XCRA(1)=XCRA(100)
      YCRA(1)=YCRA(100)
      NCRA=1
10113 CONTINUE
      NCRA=NCRA+1
      XCRA(NCRA)=REAL(XCRD)
      YCRA(NCRA)=REAL(YCRD)
      GO TO (10009,10014,10015,10016,10017,10063,10067,10073,10093,10097
     +,10103) , L10010
C
C The following internal procedure is invoked to terminate a line.
C
10019 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10114
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',42).NE.0) RETURN
      NCRA=0
10114 CONTINUE
      GO TO (10018,10024,10029,10033,10046,10055,10074,10104,10110) , L1
     +0019
C
      END

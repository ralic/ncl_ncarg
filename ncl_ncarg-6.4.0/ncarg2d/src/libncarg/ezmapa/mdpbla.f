      SUBROUTINE MDPBLA (IAMP)
C
      INTEGER IAMP(*)
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
      COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(19),
     +                 PDCL(19)
      CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
      SAVE   /MAPCM5/
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
      DOUBLE PRECISION ALPH,CLAT,CLON,CRAD,COSA,COSB,COSL,COSP,DEPS,
     +                 DIST,DLAT,DLON,DR,DS,SINA,SINB,SINL,SINP,RLAT,
     +                 RLON,RVTU,SSLT,TEMP,U,UCIR,UEDG,UNS1,UOLD,URAD,
     +                 UVAL,V,VCIR,VEDG,VNS1,VOLD,VVAL,X,XANP,XAS1,
     +                 XAS2,XCRD,YANP,YAS1,YAS2,YCRD
C
      INTEGER          I,IAID,IAM5,IDIV,IDLT,IDRT,IFDE,IGRP,IPEN,IPSS,
     +                 ISTA,IVIS,IWGF,J,K,MCHR,NCHR,NCOL,NROW,NSEG,
     +                 NTMS
C
C Declare a couple of temporary arrays to hold coordinates of a circle.
C
      DOUBLE PRECISION TLAT(361),TLON(361)
C
C Dimension the arrays needed to define some lines across the map.
C
      REAL             XCR(2),YCR(2)
C
C Declare an array in which to construct a file name.
C
      CHARACTER*128 FLNM
C
C Declare an array to use as an input buffer in reading characters.
C
      CHARACTER*1 CHRS(512)
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
      IF (ICFELL('MDPBLA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MDPINT
      IF (ICFELL('MDPBLA',2).NE.0) RETURN
10000 CONTINUE
C
C Put the perimeter and the limb line into the area map (in group 1 and,
C perhaps, in group 2).
C
      IPSS=1
      IGRP=IGI1
C
10001 CONTINUE
C
C Perimeter.
C
      IDLT=0
      IDRT=-1
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
      XCRD=UMIN-1.0002D0*(UMAX-UMIN)
      YCRD=VMIN-1.0002D0*(UMAX-UMIN)
      L10005=    2
      GO TO 10005
10013 CONTINUE
      XCRD=UMAX+1.0002D0*(UMAX-UMIN)
      YCRD=VMIN-1.0002D0*(UMAX-UMIN)
      L10010=    2
      GO TO 10010
10014 CONTINUE
      XCRD=UMAX+1.0002D0*(UMAX-UMIN)
      YCRD=VMAX+1.0002D0*(UMAX-UMIN)
      L10010=    3
      GO TO 10010
10015 CONTINUE
      XCRD=UMIN-1.0002D0*(UMAX-UMIN)
      YCRD=VMAX+1.0002D0*(UMAX-UMIN)
      L10010=    4
      GO TO 10010
10016 CONTINUE
      XCRD=UMIN-1.0002D0*(UMAX-UMIN)
      YCRD=VMIN-1.0002D0*(UMAX-UMIN)
      L10010=    5
      GO TO 10010
10017 CONTINUE
      XCRD=UMIN+.9998D0*(UMAX-UMIN)
      YCRD=VMIN+.9998D0*(VMAX-VMIN)
      L10005=    3
      GO TO 10005
10018 CONTINUE
      XCRD=UMAX-.9998D0*(UMAX-UMIN)
      YCRD=VMIN+.9998D0*(VMAX-VMIN)
      L10010=    6
      GO TO 10010
10019 CONTINUE
      XCRD=UMAX-.9998D0*(UMAX-UMIN)
      YCRD=VMAX-.9998D0*(VMAX-VMIN)
      L10010=    7
      GO TO 10010
10020 CONTINUE
      XCRD=UMIN+.9998D0*(UMAX-UMIN)
      YCRD=VMAX-.9998D0*(VMAX-VMIN)
      L10010=    8
      GO TO 10010
10021 CONTINUE
      XCRD=UMIN+.9998D0*(UMAX-UMIN)
      YCRD=VMIN+.9998D0*(VMAX-VMIN)
      L10010=    9
      GO TO 10010
10022 CONTINUE
10012 CONTINUE
      L10024=    1
      GO TO 10024
10023 CONTINUE
C
C Don't put the limb line in group 2.
C
      IF (IPSS.EQ.2) GO TO 108
C
C Limb line.
C
C Projection:     US  LC  ST  OR  LE  GN  AE
C                     CE  ME  MT  RO  EA  AI  HA  MO  WT  (arbitrary)
C                     CE  ME  MT  RO  EA  AI  HA  MO  WT  (fast-path)
C                         ME
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
     +PRF.EQ.21)) GO TO 10025
      IF (.NOT.(IPRF.EQ.3.OR.IPRF.EQ.8.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.IP
     +RF.EQ.21)) GO TO 10026
      DLON=GRDR
      RLAT=-89.998D0
      IDLT=0
      IDRT=-1
      K=CEIL(360.D0/DLON)
      DO 10027 I=1,2
      RLON=UTPA(5)-180.D0
      CALL MDPITA (RLAT,RLON,0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',3).NE.0) RETURN
      DO 10028 J=1,K-1
      RLON=RLON+DLON
      CALL MDPITA (RLAT,RLON,1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',4).NE.0) RETURN
10028 CONTINUE
      RLON=UTPA(5)+180.D0
      CALL MDPITA (RLAT,RLON,2,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',5).NE.0) RETURN
      CALL MDPIQA (            IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',6).NE.0) RETURN
      RLAT=89.998D0
      IDLT=-1
      IDRT=0
10027 CONTINUE
      L10024=    2
      GO TO 10024
10029 CONTINUE
10026 CONTINUE
      IF (.NOT.(IPRF.EQ.7)) GO TO 10030
      DLON= 89.999999D0
      GO TO 10031
10030 CONTINUE
      DLON=179.999999D0
10031 CONTINUE
      DLAT=GRDR
      RLON=UTPA(5)+DLON
      IDLT=0
      IDRT=-1
      K=CEIL(180.D0/DLAT)
      DO 10032 I=1,2
      RLAT=-90.D0
      CALL MDPITA (RLAT,RLON,0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',7).NE.0) RETURN
      DO 10033 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MDPITA (RLAT,RLON,1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',8).NE.0) RETURN
10033 CONTINUE
      RLAT=90.D0
      CALL MDPITA (RLAT,RLON,2,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',9).NE.0) RETURN
      CALL MDPIQA (            IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',10).NE.0) RETURN
      RLON=UTPA(5)-DLON
      IDLT=-1
      IDRT=0
10032 CONTINUE
      L10024=    3
      GO TO 10024
10034 CONTINUE
      GO TO 108
10025 CONTINUE
      IF (.NOT.(IPRF.EQ.9)) GO TO 10035
      DLON=GRDR
      RLAT=-.001D0
      IDLT=-1
      IDRT=0
      K=CEIL(180.D0/DLON)
      DO 10036 I=1,2
      RLON=UTPA(5)+90.D0
      CALL MDPITA (RLAT,RLON,0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',11).NE.0) RETURN
      DO 10037 J=1,K-1
      RLON=RLON+DLON
      CALL MDPITA (RLAT,RLON,1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',12).NE.0) RETURN
10037 CONTINUE
      RLON=UTPA(5)+270.D0
      CALL MDPITA (RLAT,RLON,2,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',13).NE.0) RETURN
      CALL MDPIQA (            IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',14).NE.0) RETURN
      RLAT=.001D0
      IDLT=0
      IDRT=-1
10036 CONTINUE
      L10024=    4
      GO TO 10024
10038 CONTINUE
      GO TO 108
10035 CONTINUE
      IF (.NOT.(IPRF.EQ.11.OR.IPRF.EQ.12.OR.IPRF.EQ.14.OR.IPRF.EQ.15.OR.
     +IPRF.EQ.23)) GO TO 10039
      IF (.NOT.(IPRF.EQ.11.OR.IPRF.EQ.12)) GO TO 10040
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=179.95D0
      GO TO 10041
10040 CONTINUE
      IF (.NOT.(IPRF.EQ.14)) GO TO 10042
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=89.999D0
      GO TO 10041
10042 CONTINUE
      IF (.NOT.(IPRF.EQ.15)) GO TO 10043
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=RTOD*ACOS(UTPA(1)/(UTPA(1)+UTPA(3)))-.001D0
      GO TO 10041
10043 CONTINUE
      IF (.NOT.(IPRF.EQ.23)) GO TO 10044
      CLAT=  64.D0
      CLON=-152.D0
      CRAD=  29.999D0
10041 CONTINUE
10044 CONTINUE
      CALL MDGCOG (CLAT,CLON,CRAD,TLAT,TLON,361)
      CALL MDPITA (TLAT(1),TLON(1),0,IAMP,IGRP,0,-1)
      IF (ICFELL('MDPBLA',15).NE.0) RETURN
      DO 10045 I=2,360
      CALL MDPITA (TLAT(I),TLON(I),1,IAMP,IGRP,0,-1)
      IF (ICFELL('MDPBLA',16).NE.0) RETURN
10045 CONTINUE
      CALL MDPITA (TLAT(361),TLON(361),2,IAMP,IGRP,0,-1)
      IF (ICFELL('MDPBLA',17).NE.0) RETURN
      CALL MDPIQA (                      IAMP,IGRP,0,-1)
      IF (ICFELL('MDPBLA',18).NE.0) RETURN
      GO TO 108
10039 CONTINUE
      IF (.NOT.(IPRF.EQ.20)) GO TO 10046
      ALPH=DTOR*(180.D0-UTPA(4))
      CALL MDPTRN (+90.D0,0.D0,XANP,YANP)
      IF (ICFELL('MDPBLA',19).NE.0) RETURN
      CALL MDPTRN (-90.D0,0.D0,XAS1,YAS1)
      IF (ICFELL('MDPBLA',20).NE.0) RETURN
      UNS1=(XAS1-XANP)*COS(ALPH)+(YAS1-YANP)*SIN(ALPH)
      VNS1=(YAS1-YANP)*COS(ALPH)-(XAS1-XANP)*SIN(ALPH)
      XAS2=XANP+VNS1*SIN(ALPH)+UNS1*COS(ALPH)
      YAS2=YANP+UNS1*SIN(ALPH)-VNS1*COS(ALPH)
      DIST=SQRT((XAS2-XAS1)*(XAS2-XAS1)+(YAS2-YAS1)*(YAS2-YAS1))
      IF (.NOT.(VNS1.LT.0.D0)) GO TO 10047
      DEPS=-.001D0*DIST
      IDLT=-1
      IDRT= 0
      GO TO 10048
10047 CONTINUE
      DEPS=+.001D0*DIST
      IDLT= 0
      IDRT=-1
10048 CONTINUE
      DIST=2.D0*DIST
      XCR(1)=REAL(XAS1-DIST*COS(ALPH)+DEPS*SIN(ALPH))
      YCR(1)=REAL(YAS1-DIST*SIN(ALPH)-DEPS*COS(ALPH))
      XCR(2)=REAL(XAS1+DIST*COS(ALPH)+DEPS*SIN(ALPH))
      YCR(2)=REAL(YAS1+DIST*SIN(ALPH)-DEPS*COS(ALPH))
      CALL AREDAM (IAMP,XCR,YCR,2,IGRP,IDRT,IDLT)
      IF (ICFELL('MDPBLA',21).NE.0) RETURN
      XCR(1)=REAL(XAS2-DIST*COS(ALPH)-DEPS*SIN(ALPH))
      YCR(1)=REAL(YAS2-DIST*SIN(ALPH)+DEPS*COS(ALPH))
      XCR(2)=REAL(XAS2+DIST*COS(ALPH)-DEPS*SIN(ALPH))
      YCR(2)=REAL(YAS2+DIST*SIN(ALPH)+DEPS*COS(ALPH))
      CALL AREDAM (IAMP,XCR,YCR,2,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',22).NE.0) RETURN
      GO TO 108
10046 CONTINUE
      GO TO 108
C
C Lambert conformal conic.
C
  101 DLAT=GRDR
      RLON=PLNO+179.999999D0
      IDLT=0
      IDRT=-1
      K=CEIL(180.D0/DLAT)
      DO 10049 I=1,2
      RLAT=-90.D0
      CALL MDPITA (RLAT,RLON,0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',23).NE.0) RETURN
      DO 10050 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MDPITA (RLAT,RLON,1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',24).NE.0) RETURN
10050 CONTINUE
      RLAT=90.D0
      CALL MDPITA (RLAT,RLON,2,IAMP,IGRP,IDLT,IDRT)
      CALL MDPIQA (            IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',25).NE.0) RETURN
      RLON=PLNO-179.999999D0
      IDLT=-1
      IDRT=0
10049 CONTINUE
      L10024=    5
      GO TO 10024
10051 CONTINUE
      GO TO 108
C
C Orthographic (or satellite-view).
C
  102 CONTINUE
      IF (.NOT.(ABS(SALT).LE.1.D0.OR.ALFA.EQ.0.D0)) GO TO 10052
      URAD=1.D0
      RVTU=1.D0
      GO TO 106
10052 CONTINUE
      SSLT=SALT
      SALT=-ABS(SALT)
      IDLT=-1
      IDRT=0
      DR=.9998D0
10053 CONTINUE
      IPEN=0
      DO 10054 I=1,361
      COSB=COS(DBLE(DTOR*DBLE(I-1)))
      SINB=SIN(DBLE(DTOR*DBLE(I-1)))
      IF (.NOT.(DR.LT.1.D0)) GO TO 10055
      COSA=(DR*DR*ABS(SALT)+SSMO*SQRT(1.D0-DR*DR))/
     +                                 (DR*DR+SSMO)
      GO TO 10056
10055 CONTINUE
      DS=2.D0-DR
      COSA=(DS*DS*ABS(SALT)-SSMO*SQRT(1.D0-DS*DS))/
     +                                 (DS*DS+SSMO)
10056 CONTINUE
      SINA=SQRT(1.D0-COSA*COSA)
      SINL=SINA*SINB
      COSL=COSA*COSO-SINA*SINO*COSB
      COSP=SQRT(SINL*SINL+COSL*COSL)
      IF (.NOT.(COSP.NE.0.D0)) GO TO 10057
      SINL=SINL/COSP
      COSL=COSL/COSP
10057 CONTINUE
      IF (.NOT.(ABS(SINO).GT..000001D0)) GO TO 10058
      SINP=(COSA-COSP*COSL*COSO)/SINO
      GO TO 10059
10058 CONTINUE
      SINP=SINA*COSB
10059 CONTINUE
      RLAT=RTOD*ATAN2(SINP,COSP)
      RLON=PLNO+RTOD*ATAN2(SINA*SINB,
     +               COSA*COSO-SINA*SINO*COSB)
      IF (ABS(RLON).GT.180.D0) RLON=RLON-SIGN(360.D0,RLON)
      CALL MDPITA (RLAT,RLON,IPEN,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',26).NE.0) RETURN
      IPEN=1
      IF (I.EQ.360) IPEN=2
10054 CONTINUE
      L10024=    6
      GO TO 10024
10060 CONTINUE
      IF (DR.EQ.1.0002D0) GO TO 10061
      DR=1.0002D0
      GO TO 10053
10061 CONTINUE
      SALT=SSLT
      GO TO 108
C
C Lambert equal-area.  Note:  The constant "1.999999500000" is the real
C effective radius of the limb of the Lambert equal area projection, as
C determined by the test at statement number 106 in the routine MDPTRN.
C
  103 URAD=1.9999995000000D0
      RVTU=1.D0
      GO TO 106
C
C Azimuthal equidistant.  Note:  The constant "3.140178439909" is the
C real effective radius of the limb of the azimuthal equidistant
C projection, as determined by the test at statement number 108 in the
C routine MDPTRN.
C
  104 URAD=3.1401784399095D0
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
10062 CONTINUE
      IDLT=0
      IDRT=-1
      IVIS=-1
      I = 1
      GO TO 10065
10063 CONTINUE
      I =I +1
10065 CONTINUE
      IF (I .GT.(361)) GO TO 10064
      UCIR=TEMP*URAD*COS(DTOR*DBLE(I-1))
      VCIR=TEMP*URAD*SIN(DTOR*DBLE(I-1))
      U=UCIR-UOFF
      V=RVTU*VCIR-VOFF
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10066
      IF (.NOT.(IVIS.EQ.1)) GO TO 10067
      CALL MDPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   10
      GO TO 10010
10068 CONTINUE
10067 CONTINUE
      IVIS=0
      GO TO 10069
10066 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.D0)
     +)) GO TO 10070
      IF (.NOT.(IVIS.EQ.1)) GO TO 10071
      CALL MDPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   11
      GO TO 10010
10072 CONTINUE
10071 CONTINUE
      IVIS=0
      GO TO 10069
10070 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10073
      XCRD=U
      YCRD=V
      L10005=    4
      GO TO 10005
10074 CONTINUE
      IVIS=1
      GO TO 10075
10073 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10076
      IF (.NOT.ELPF) CALL MDPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MDPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    5
      GO TO 10005
10077 CONTINUE
      IVIS=1
10076 CONTINUE
      XCRD=U
      YCRD=V
      L10010=   12
      GO TO 10010
10078 CONTINUE
10075 CONTINUE
10069 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10063
10064 CONTINUE
      L10024=    7
      GO TO 10024
10079 CONTINUE
      IF (TEMP.EQ.1.0002D0) GO TO 10080
      TEMP=1.0002D0
      GO TO 10062
10080 CONTINUE
      GO TO 108
C
C Cylindrical equidistant, Mercator, Robinson, cylindrical equal-area,
C Winkel tripel.
C
  107 TEMP=.9998D0
C
10081 CONTINUE
      IDLT=0
      IDRT=-1
      IVIS=-1
      RLAT=-90.D0
      RLON=-180.D0
      I = 1
      GO TO 10084
10082 CONTINUE
      I =I +1
10084 CONTINUE
      IF (I .GT.(361)) GO TO 10083
      IF (.NOT.(IPRJ.EQ.7.OR.IPRJ.EQ.16)) GO TO 10085
      U=TEMP*RLON-UOFF
      V=TEMP*RLAT/CSLT-VOFF
      GO TO 10086
10085 CONTINUE
      IF (.NOT.(IPRJ.EQ.8.OR.IPRJ.EQ.17.OR.IPRJ.EQ.25)) GO TO 10087
      U=TEMP*DTOR*RLON-UOFF
      V=TEMP*LOG(TAN((MAX(-89.999999D0,
     +                MIN(+89.999999D0,RLAT))+
     +                                      90.D0)*DTRH))-VOFF
      IF (.NOT.(IPRJ.EQ.25)) GO TO 10088
      UTMP=U*COSR+V*SINR
      VTMP=V*COSR-U*SINR
      U=UTMP
      V=VTMP
10088 CONTINUE
      GO TO 10086
10087 CONTINUE
      IF (.NOT.(IPRJ.EQ.11.OR.IPRJ.EQ.20)) GO TO 10089
      U=TEMP*DTOR*RLON-UOFF
      V=TEMP*SIN(DTOR*RLAT)/CSLS-VOFF
      GO TO 10086
10089 CONTINUE
      IF (.NOT.(IPRJ.EQ.15.OR.IPRJ.EQ.24)) GO TO 10090
      CALL WTPROJ (TEMP*DTOR*RLAT,TEMP*DTOR*RLON,U,V,CSLT)
      U=U-UOFF
      V=V-VOFF
      GO TO 10086
10090 CONTINUE
      U=TEMP*(RLON/180.D0)*RBGLEN(RLAT)-UOFF
      V=TEMP*RBGDFE(RLAT)-VOFF
10086 CONTINUE
      IF (.NOT.(I.LE.90)) GO TO 10091
      RLON=RLON+4.D0
      GO TO 10092
10091 CONTINUE
      IF (.NOT.(I.LE.180)) GO TO 10093
      RLAT=RLAT+2.D0
      GO TO 10092
10093 CONTINUE
      IF (.NOT.(I.LE.270)) GO TO 10094
      RLON=RLON-4.D0
      GO TO 10092
10094 CONTINUE
      IF (.NOT.(I.LE.360)) GO TO 10095
      RLAT=RLAT-2.D0
10092 CONTINUE
10095 CONTINUE
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10096
      IF (.NOT.(IVIS.EQ.1)) GO TO 10097
      CALL MDPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   13
      GO TO 10010
10098 CONTINUE
10097 CONTINUE
      IVIS=0
      GO TO 10099
10096 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.D0)
     +)) GO TO 10100
      IF (.NOT.(IVIS.EQ.1)) GO TO 10101
      CALL MDPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   14
      GO TO 10010
10102 CONTINUE
10101 CONTINUE
      IVIS=0
      GO TO 10099
10100 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10103
      XCRD=U
      YCRD=V
      L10005=    6
      GO TO 10005
10104 CONTINUE
      IVIS=1
      GO TO 10105
10103 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10106
      IF (.NOT.ELPF) CALL MDPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MDPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    7
      GO TO 10005
10107 CONTINUE
      IVIS=1
10106 CONTINUE
      XCRD=U
      YCRD=V
      L10010=   15
      GO TO 10010
10108 CONTINUE
10105 CONTINUE
10099 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10082
10083 CONTINUE
      L10024=    8
      GO TO 10024
10109 CONTINUE
      IF (TEMP.EQ.1.0002D0) GO TO 10110
      TEMP=1.0002D0
      GO TO 10081
10110 CONTINUE
C
  108 CONTINUE
      IF (IGRP.EQ.IGI2.OR.NOVS.LE.0) GO TO 10111
C
      IPSS=2
      IGRP=IGI2
C
      GO TO 10001
10111 CONTINUE
C
C Add lines to group 2 to create vertical strips.
C
      IF (.NOT.(NOVS.GT.1)) GO TO 10112
C
      IDLT=0
      IDRT=0
C
      YCR(1)=VMIN
      YCR(2)=VMAX
C
      DO 10113 I=1,NOVS-1
      XCR(1)=REAL(UMIN+(DBLE(I)/DBLE(NOVS))*(UMAX-UMIN))
      XCR(2)=XCR(1)
      CALL AREDAM (IAMP,XCR,YCR,2,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',27).NE.0) RETURN
10113 CONTINUE
C
10112 CONTINUE
C
C If the selected outline type is "NONE", quit; no outlines need be
C added to the area map.
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
C Position to the user-selected portion of the outline dataset.
C
      IGRP=IGI1
      CALL MDPIO (1)
      IF (ICFELL('MDPBLA',28).NE.0) RETURN
      NSEG=0
C
C Save the pointer that will tell us whether anything actually got
C put into the area map, so that, if not, we can take remedial action.
C
      IAM5=IAMP(5)
C
C Read the next record (group of points).
C
  301 CALL MDPIO (2)
      IF (ICFELL('MDPBLA',29).NE.0) RETURN
      IDLT=IDOS(NOUT)+IDLS
      IDRT=IDOS(NOUT)+IDRS
      NSEG=NSEG+1
C
C If the end of the desired data has been reached, quit reading.
C
      IF (NPTS.EQ.0) GO TO 302
C
C If less than the whole globe is shown by the projection, do a quick
C check for intersection of the box surrounding the point group with
C the area shown.
C
      IF (.NOT.(IWGF.EQ.0)) GO TO 10114
      IF (DBLE(SLAG).GT.BLAM.OR.DBLE(BLAG).LT.SLAM) GO TO 301
      IF ((DBLE(SLOG     ).GT.BLOM.OR.
     +     DBLE(BLOG     ).LT.SLOM).AND.
     +    (DBLE(SLOG-360.).GT.BLOM.OR.
     +     DBLE(BLOG-360.).LT.SLOM).AND.
     +    (DBLE(SLOG+360.).GT.BLOM.OR.
     +     DBLE(BLOG+360.).LT.SLOM)) GO TO 301
10114 CONTINUE
C
C See if the user wants to omit this point group.
C
      CALL HLUMAPEOD (NOUT,NSEG,IDLT,IDRT,NPTS,PNTS)
      IF (ICFELL('MDPBLA',30).NE.0) RETURN
      IF (NPTS.LE.1) GO TO 301
C
C Put the group into the area map.
C
      CALL MAPITA (PNTS(1),PNTS(2),0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',31).NE.0) RETURN
C
      DO 10115 K=2,NPTS-1
      CALL MAPITA (PNTS(2*K-1),PNTS(2*K),1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',32).NE.0) RETURN
10115 CONTINUE
C
      CALL MAPITA (PNTS(2*NPTS-1),PNTS(2*NPTS),2,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',33).NE.0) RETURN
C
C Force a buffer dump.
C
      L10024=    9
      GO TO 10024
10116 CONTINUE
C
C Go get another group.
C
      GO TO 301
C
C See if anything was actually put into the area map and, if not, take
C action to supply AREAS with a correct area identifier.
C
  302 CONTINUE
      IF (.NOT.(IAMP(5).EQ.IAM5)) GO TO 10117
      CALL MPDBDI (FLNM,ISTA)
      IF (ISTA.EQ.-1) GO TO 309
      DO 303 I=1,111
      IF (.NOT.(FLNM(I:I).EQ.CHAR(0))) GO TO 10118
      FLNM(I:I+17)='/EzmapAreaInfo.'//DDCT(NOUT+1)//CHAR(0)
      GO TO 304
10118 CONTINUE
  303 CONTINUE
      GO TO 309
  304 CALL NGOFRO (FLNM,IFDE,ISTA)
      IF (ISTA.NE.0) GO TO 309
      NTMS=0
      MCHR=0
      NCHR=0
      DO 307 IDIV=0,9
      NROW=2**IDIV
      NCOL=2*NROW
      DO 306 J=1,NROW
      RLAT=-90.D0+(DBLE(J)-.5D0)*(180.D0/DBLE(NROW))
      DO 305 I=1,NCOL
      RLON=-180.D0+(DBLE(I)-.5D0)*(360.D0/DBLE(NCOL))
      IF (.NOT.(NTMS.EQ.0)) GO TO 10119
      CALL MDRDNM (IFDE,CHRS,512,MCHR,NCHR,NTMS)
      IF (MCHR.EQ.0) GO TO 308
      CALL MDRDNM (IFDE,CHRS,512,MCHR,NCHR,IAID)
      IF (MCHR.EQ.0) GO TO 308
10119 CONTINUE
      CALL MDPTRA (RLAT,RLON,UVAL,VVAL)
      IF (.NOT.(UVAL.NE.1.D12)) GO TO 10120
      XCR(1)=REAL(UMIN)
      XCR(2)=REAL(UMAX)
      YCR(1)=REAL(VMIN)
      YCR(2)=REAL(VMAX)
      CALL AREDAM (IAMP,XCR,YCR,2,IGRP,IAID,IAID)
      GO TO 308
10120 CONTINUE
      NTMS=NTMS-1
  305 CONTINUE
  306 CONTINUE
  307 CONTINUE
  308 CALL NGCLFI (IFDE)
C
10117 CONTINUE
C
C Done.
C
  309 RETURN
C
C The following internal procedure is invoked to start a line.
C
10005 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10121
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',34).NE.0) RETURN
10121 CONTINUE
      XCRA(1)=REAL(XCRD)
      YCRA(1)=REAL(YCRD)
      NCRA=1
      GO TO (10004,10013,10018,10074,10077,10104,10107) , L10005
C
C The following internal procedure is invoked to continue a line.
C
10010 CONTINUE
      IF (.NOT.(NCRA.EQ.100)) GO TO 10122
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',35).NE.0) RETURN
      XCRA(1)=XCRA(100)
      YCRA(1)=YCRA(100)
      NCRA=1
10122 CONTINUE
      NCRA=NCRA+1
      XCRA(NCRA)=REAL(XCRD)
      YCRA(NCRA)=REAL(YCRD)
      GO TO (10009,10014,10015,10016,10017,10019,10020,10021,10022,10068
     +,10072,10078,10098,10102,10108) , L10010
C
C The following internal procedure is invoked to terminate a line.
C
10024 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10123
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPBLA',36).NE.0) RETURN
      NCRA=0
10123 CONTINUE
      GO TO (10023,10029,10034,10038,10051,10060,10079,10109,10116) , L1
     +0024
C
      END
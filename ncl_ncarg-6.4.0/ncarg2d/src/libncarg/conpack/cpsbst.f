      SUBROUTINE CPSBST (CHSI,CHSO,NCHO)
C
      CHARACTER*(*) CHSI,CHSO
C
C The routine CPSBST is called to perform substitution of numeric values
C for parameter names.  The contents of the string CHSI are copied to
C the string CHSO.  Certain substrings of the form '$xxx$' are replaced
C by strings representing numeric values; in particular, '$ZDV$' is
C replaced by a string representing the numeric value of ZDVL.  The
C length of the resulting string is returned as the value of NCHO.
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
C Find the length of the input character string.
C
      NCHI=LEN(CHSI)
C
C Find the length of the output character-string variable, blank-fill
C it, and initialize the count of characters put into it.
C
      MCHO=LEN(CHSO)
      CHSO=' '
      NCHO=0
C
C Do the copy.  Each time a dollar sign is encountered, see if it
C introduces one of the parameter names to be replaced and, if so,
C do the replacement.
C
      KCHI=0
10001 CONTINUE
      IF (.NOT.(KCHI.LT.NCHI)) GO TO 10002
        KCHI=KCHI+1
        IF (.NOT.(NCHO.LT.MCHO)) GO TO 10003
          NCHO=NCHO+1
          CHSO(NCHO:NCHO)=CHSI(KCHI:KCHI)
          IF (.NOT.(CHSI(KCHI:KCHI).EQ.'$'.AND.KCHI+4.LE.NCHI)) GO TO 10
     +004
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'ZDV')) GO TO 10005
              VALU=ZDVL
              L10007=    1
              GO TO 10007
10006         CONTINUE
            GO TO 10008
10005       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'ZMN')) GO TO 10009
              VALU=ZMIN
              L10007=    2
              GO TO 10007
10010         CONTINUE
            GO TO 10008
10009       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'ZMX')) GO TO 10011
              VALU=ZMAX
              L10007=    3
              GO TO 10007
10012         CONTINUE
            GO TO 10008
10011       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'CIU')) GO TO 10013
              VALU=CINU
              L10015=    1
              GO TO 10015
10014         CONTINUE
            GO TO 10008
10013       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'CMN')) GO TO 10016
              IF (.NOT.(NCLV.LE.0)) GO TO 10017
                VALU=0.
              GO TO 10018
10017         CONTINUE
                VALU=CLEV(ICLP(1))
10018         CONTINUE
              L10015=    2
              GO TO 10015
10019         CONTINUE
            GO TO 10008
10016       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'CMX')) GO TO 10020
              IF (.NOT.(NCLV.LE.0)) GO TO 10021
                VALU=0.
              GO TO 10022
10021         CONTINUE
                VALU=CLEV(ICLP(NCLV))
10022         CONTINUE
              L10015=    3
              GO TO 10015
10023         CONTINUE
            GO TO 10008
10020       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'SFU')) GO TO 10024
              VALU=SCFU
              LMSD=-10000
              IEXP=1
              LEXP=0
              IOMA=1
              IODP=1
              IOTZ=1
              L10026=    1
              GO TO 10026
10025         CONTINUE
10008       CONTINUE
10024       CONTINUE
10004     CONTINUE
10003   CONTINUE
      GO TO 10001
10002 CONTINUE
C
C Done.
C
      RETURN
C
C The following internal procedure determines whether to treat $CIU$,
C $CMN$, and $CMX$ as unrounded or rounded numbers.
C
10015 CONTINUE
        IF (.NOT.(ICLS.LT.0)) GO TO 10027
          L10007=    4
          GO TO 10007
10028     CONTINUE
        GO TO 10029
10027   CONTINUE
          L10031=    1
          GO TO 10031
10030     CONTINUE
10029   CONTINUE
      GO TO (10014,10019,10023) , L10015
C
C The following internal procedure is used to handle numbers known not
C to have been rounded to nice values.
C
10007 CONTINUE
        IF (CHSI(KCHI+4:KCHI+4).NE.'U') VALU=VALU/SCFU
        LMSD=LSDL
        IEXP=NEXU
        LEXP=NEXL
        IOMA=JOMA
        IODP=JODP
        IOTZ=JOTZ
        L10026=    2
        GO TO 10026
10032   CONTINUE
      GO TO (10006,10010,10012,10028) , L10007
C
C The following internal procedure is used to handle numbers which are
C likely to have been rounded to nice values, so that it is probably a
C good idea to trim off trailing zeroes.
C
10031 CONTINUE
        IF (CHSI(KCHI+4:KCHI+4).NE.'U') VALU=VALU/SCFU
        LMSD=LSDL
        IEXP=NEXU
        LEXP=NEXL
        IOMA=JOMA
        IODP=JODP
        IOTZ=1
        L10026=    3
        GO TO 10026
10033   CONTINUE
      GO TO (10030) , L10031
C
C The following internal procedure generates, in the output string, the
C representation of a numeric value.  It then updates the pointers into
C the input and output character strings.
C
10026 CONTINUE
        CALL CPNUMB (VALU,NDGL,LMSD,IEXP,LEXP,CHEX(1:LEA1),
     +               CHEX(LEA1+1:LEA1+LEA2),
     +               CHEX(LEA1+LEA2+1:LEA1+LEA2+LEA3),
     +               LEE1,LEE2,LEE3,IOMA,IODP,IOTZ,
     +               CHSO(NCHO:MCHO),NCHS,NDGS,IEVA)
        NCHO=NCHO+NCHS-1
        KCHI=KCHI+4
        IF (CHSI(KCHI:KCHI).NE.'$') KCHI=KCHI+1
      GO TO (10025,10032,10033) , L10026
C
      END

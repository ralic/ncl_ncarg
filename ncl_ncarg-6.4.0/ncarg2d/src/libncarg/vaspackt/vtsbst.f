      SUBROUTINE VTSBST (CHSI,CHSO,NCHO)
C
      CHARACTER*(*) CHSI,CHSO
C
C The routine VTSBST is called to perform substitution of numeric values
C for parameter names.  The contents of the string CHSI are copied to
C the string CHSO.  Certain substrings of the form '$xxx$' are replaced
C by strings representing numeric values; in particular, '$DVA$' is
C replaced by a string representing the numeric value of DVAL.  The
C length of the resulting string is returned as the value of NCHO.
C
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
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'DVA')) GO TO 10005
              VALU=DVAL
              L10007=    1
              GO TO 10007
10006         CONTINUE
            GO TO 10008
10005       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'DMN')) GO TO 10009
              VALU=DMIN
              L10007=    2
              GO TO 10007
10010         CONTINUE
            GO TO 10008
10009       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'DMX')) GO TO 10011
              VALU=DMAX
              L10007=    3
              GO TO 10007
10012         CONTINUE
            GO TO 10008
10011       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'SFU')) GO TO 10013
              VALU=SCFU
              LMSD=-10000
              IEXP=1
              LEXP=0
              IOMA=1
              IODP=1
              IOTZ=1
              L10015=    1
              GO TO 10015
10014         CONTINUE
10008       CONTINUE
10013       CONTINUE
10004     CONTINUE
10003   CONTINUE
      GO TO 10001
10002 CONTINUE
C
C Done.
C
      RETURN
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
        L10015=    2
        GO TO 10015
10016   CONTINUE
      GO TO (10006,10010,10012) , L10007
C
C The following internal procedure generates, in the output string, the
C representation of a numeric value.  It then updates the pointers into
C the input and output character strings.
C
10015 CONTINUE
        CALL VTNUMB (VALU,NDGL,LMSD,IEXP,LEXP,CHEX(1:LEA1),
     +               CHEX(LEA1+1:LEA1+LEA2),
     +               CHEX(LEA1+LEA2+1:LEA1+LEA2+LEA3),
     +               LEE1,LEE2,LEE3,IOMA,IODP,IOTZ,
     +               CHSO(NCHO:MCHO),NCHS,NDGS,IEVA)
        NCHO=NCHO+NCHS-1
        KCHI=KCHI+4
        IF (CHSI(KCHI:KCHI).NE.'$') KCHI=KCHI+1
      GO TO (10014,10016) , L10015
C
      END

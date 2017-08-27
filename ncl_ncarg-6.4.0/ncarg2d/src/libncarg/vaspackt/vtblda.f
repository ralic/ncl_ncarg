      SUBROUTINE VTBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
      RETURN
C
      END
      BLOCKDATA VTBLDAX
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
C Below are descriptions of all the COMMON variables and default values
C for those which require defaults.
C
C AHAW and AHLN are the parameters 'AHA' and 'AHL', which are the
C angular width and length, respectively, of the arrowheads to be
C used on a streamline.  AHLR is the "realized" length of an arrowhead,
C computed as needed.
C
      DATA AHAW,AHLN / 30.,.04 /
C
C AHSP is the parameter 'AHS', which is the spacing of arrowheads along
C a streamline.  AHSR is a "realized" value of AHSP and is computed as
C necessary.
C
      DATA AHSP / .16 /
C
C ANIL is the parameter 'ILA', which is the angle, in degrees, at which
C the informational label is to be written.
C
      DATA ANIL / 0. /
C
C ANM1 and ANM2 are the parameters 'AM1' and 'AM2', maximum angles to
C be allowed between any pair of velocity vectors at the vertices of a
C triangle being considered.  The first serves to limit the placement
C of curly vectors and the starting positions of streamline generators
C and streamlines, while the second serves to terminate streamline
C generators and streamlines.
C
      DATA ANM1,ANM2 / 90. , 0. /
C
C ANZF is the parameter 'ZFA', which is the angle, in degrees, at which
C the zero-field label is to be written.
C
      DATA ANZF / 0. /
C
C AVEL is the parameter 'AEL' (for retrieval only), which is the average
C edge length in the triangular mesh.
C
      DATA AVEL / 0. /
C
C CHEX is used to hold the character string which stands between the
C mantissa and the exponent of a numeric value.
C
C CHWM is the parameter 'CWM', the character-width multiplier.
C
      DATA CHWM / 1. /
C
C CTMA and CTMB are character-variable temporaries, used for various
C purposes throughout the code.  CTMA is the parameter 'CTM'.
C
      DATA CTMA,CTMB / ' ',' ' /
C
C CXZF and CYZF are the parameters 'ZFX' and 'ZFY', which are the X and
C Y coordinates of a basepoint relative to which the zero-field
C label is to be positioned.  These coordinates are given in a
C fractional coordinate system superimposed on the user-system window.
C
      DATA CXZF,CYZF / .50,.50 /
C
C CXIL and CYIL are the parameters 'ILX' and 'ILY', which are the X and
C Y coordinates of a basepoint relative to which the informational label
C is to be positioned.  These coordinates are given in a fractional
C coordinate system superimposed on the user-system window.
C
      DATA CXIL,CYIL / .98,-.02 /
C
C DCNU, DCNV, and DCNW are the direction cosines for the normal to the
C plane of the triangle, computed for use by the routine VTCUDR when it
C is asked to draw arrowheads (so that they will lie in the plane of
C the triangle).
C
      DATA DCNU,DCNV,DCNW / 0.,0.,1. /
C
C DMAX and DMIN are the parameters 'DMX' and 'DMN', the maximum and
C minimum velocity-vector magnitudes in the user's array of data.
C
      DATA DMAX,DMIN / 0.,0. /
C
C DVAL is the parameter 'DVA', which holds a data value.
C
      DATA DVAL / 0. /
C
C EMAX is equal to MAX(XMAX-XMIN,YMAX-YMIN,ZMAX-ZMIN) and is computed
C by VTMESH.  It is given a default value only because its value can
C be retrieved as the value of the parameter 'EOM'.
C
      DATA EMAX / 0. /
C
C EPSI is a machine "epsilon", whose real value is computed as required.
C
C FRMT is a format to be used by the routine VTNUMB.  It is constructed
C as needed by the routine VTINRC.
C
C IBIL is the parameter 'ILB', which is zero if no box is to be drawn
C around the informational label.  Adding 1 to the value causes the box
C to be drawn and adding 2 to it causes the box to be filled.
C
      DATA IBIL / 0 /
C
C IBZF is the parameter 'ZFB', which is zero if no box is to be drawn
C around the zero-field label.  Adding 1 to the value causes the box
C to be drawn and adding 2 to it causes the box to be filled.
C
      DATA IBZF / 0 /
C
C ICIL is the parameter 'ILC', which determines the color of the
C informational label.
C
      DATA ICIL / -1 /
C
C ICZF is the parameter 'ZFC', which determines the color of the
C zero-field label.
C
      DATA ICZF / -1 /
C
C ICLR is the parameter array 'CLR', which holds color indices for use
C on streamlines.
C
      DATA ICLR / 255*1 /
C
C ICSG and ICTT are the parameters 'SGC', 'STC', and 'TTC', which
C determine the colors of streamline generator lines, subtriangle
C lines,  and termination test lines, respectively.  (These are
C drawn only when debugging is turned on.)
C
      DATA ICSG,ICST,ICTT / 1 , 1 , 1 /
C
C ICTV is the parameter 'CTV', which is the "Color Value Threshold
C Control", saying whether streamlines are to be colored and, if so,
C how the colors are to be determined.
C
      DATA ICTV / 0 /
C
C IDBG is the parameter 'DBG', a debug flag.  This is mostly for use
C by the developer.  When set non-zero, it causes debug information to
C be plotted.
C
      DATA IDBG / 0 /
C
C IISP is the parameter 'ISP', which says how to interpret the size
C parameters.  If 'ISP' = 0, all size parameters are treated as actual
C values in 3-space, commensurate with the coordinates of the mesh
C points themselves; if 'ISP' = 1, they are treated as multiples of
C the maximum extent of the mesh in 3-space.
C
      DATA IISP / 0 /
C
C IIWS is an array of base indices in the integer work array.  LIWS is
C an associated array of lengths.  For each I for which LIWS(I) is not
C zero, IIWS(I)+1 is the index of the first word, and IIWS(I)+LIWS(I)
C the index of the last word, of a portion of the integer work array
C reserved for some particular purpose.
C
      DATA IIWS,LIWS / 2*0 , 2*0 /
C
C IIWU is the parameter 'IWU', which may be used to find out how much
C space was used in the integer workspace.
C
      DATA IIWU / 0 /
C
C ILBC is the parameter 'LBC', the color-index specifier for area fill
C of label boxes.
C
      DATA ILBC / 0 /
C
C IMPF is the parameter 'MAP', the mapping flag.
C
      DATA IMPF / 0 /
C
C INIL is used to save the index of the informational label in the list
C of labels.
C
C INIT is a flag indicating whether some necessary constants have been
C computed yet or not.
C
      DATA INIT / 0 /
C
C IPAI is the parameter 'PAI', which is the index for parameter arrays.
C
      DATA IPAI / 0 /
C
C IPIS is the parameter 'PIS', which indicates the number of points to
C interpolate between each pair of points defining a streamline.
C
      DATA IPIS / 0 /
C
C IPIL is the parameter 'ILP', specifying how the informational label
C is to be positioned.
C
      DATA IPIL / 4 /
C
C IPZF is the parameter 'ZFP', specifying how the zero-field label
C is to be positioned.
C
      DATA IPZF / 0 /
C
C IRNG is the parameter 'RNG', specifying by how many steps the random
C number generator is to be spun up during initialization of VTCVDM,
C VTSLDM, and VTSVDM.
C
      DATA IRNG / 0 /
C
C IRWS is an array of base indices in the real work array.  LRWS is an
C associated array of lengths.  For each I for which LRWS(I) is not
C zero, IRWS(I)+1 is the index of the first word, and IRWS(I)+LRWS(I)
C the index of the last word, of a portion of the real work array
C reserved for some particular purpose.
C
      DATA IRWS,LRWS / 2*0 , 2*0 /
C
C IRWU is the parameter 'RWU', which may be used to find out how much
C space was used in the real workspace.
C
      DATA IRWU / 0 /
C
C ISET is the parameter 'SET', which says whether or not VASPACKT is to
C call SET.
C
      DATA ISET / 1 /
C
C ISTA is a table, dimensioned 25x25, of subtriangle bit masks for use
C by the routines VTTSOM and VTTPOM.  If I and J are the indices of two
C subtriangles of a triangle (1.LE.I,J.LE.25), then ISTA(I,J) contains
C 1 bits for triangles forming a connected path from subtriangle I to
C subtriangle J.  (See the file "CreateTable.f".)
C
      DATA (ISTA(I),I=  1, 70) /
     +        1,       3,       7,      15,      31,      63,     127,
     +      255,     511,     515,    1539,    2063,    6159,   14351,
     +    30735,   57407,   67075,  198147,  269827,  794115, 1079311,
     +  2295299, 6489603, 9373187,23266819,       3,       2,       6,
     +       14,      30,      62,     126,     254,     510,     514,
     +     1538,    2062,    6158,   14350,   30734,   57406,   67074,
     +   198146,  269826,  794114, 1079310, 2295298, 6489602, 9182722,
     + 23266818,       7,       6,       4,      12,      28,      60,
     +      124,     252,     508,     518,    3084,    2060,    6156,
     +     8252,   24636,   57404,   68620,  199692,  268300,  792588/
      DATA (ISTA(I),I= 71,140) /
     +  1079308, 2296844, 6690828, 9181196,23268364,      15,      14,
     +       12,       8,      24,      56,     120,     248,     504,
     +     3592,    3080,    2056,    6152,    8248,   24632,   57400,
     +    68616,  399368,  268296,  792584, 1079304, 2496520, 6690824,
     +  9181192,23468040,      31,      30,      28,      24,      16,
     +       48,     112,     240,     496,    3608,    3096,    2072,
     +    12336,    8240,   24624,   57392,   68632,  399384,  274480,
     +   798768, 1073200, 2496536,13381680, 9187376,30158896,      63,
     +       62,      60,      56,      48,      32,      96,     224,
     +      480,    3640,   15392,   14368,   12320,    8224,   24608/
      DATA (ISTA(I),I=141,210) /
     +    57376,   80928,  405536,  274464, 1597472, 1073184, 2502688,
     + 13381664, 9986080,30158880,     127,     126,     124,     120,
     +      112,      96,      64,     192,     448,    3704,   15456,
     +    14432,   12384,    8288,   49344,   32960,   80992,  405600,
     +   274528, 1597536, 1097920, 2502752,14180448, 9986144,30957664,
     +      255,     254,     252,     248,     240,     224,     192,
     +      128,     384,   16096,   15584,   14560,   61568,   57472,
     +    49280,   32896,  520320,  454784,  323712, 1622144, 1097856,
     +  4112512,14205056,10010752,30982272,     511,     510,     508,
     +      504,     496,     480,     448,     384,     256,    4088/
      DATA (ISTA(I),I=211,280) /
     +    15840,   14816,   61824,   57728,   49536,   33152,  520576,
     +   455040,  323968, 1622400, 1098112, 4112768,14205312,10011008,
     + 30982528,     515,     514,     518,     526,     542,    3640,
     +     3704,    3832,    4088,     512,    1536,    3584,    7680,
     +    15872,   32256,   65024,   67072,  198144,  269824,  794112,
     +  1842688, 2295296, 6489600, 9373184,23266816,    1539,    1538,
     +     1542,    3080,    3096,    3128,    3192,   15584,   15840,
     +     1536,    1024,    3072,    7168,   15360,   31744,   64512,
     +    66560,  197632,  269312,  793600, 1842176, 2294784, 6489088,
     +  9372672,23266304,    3587,    3586,    2060,    2056,    2072/
      DATA (ISTA(I),I=281,350) /
     +    14368,   14432,   14560,   14816,    3584,    3072,    2048,
     +     6144,   14336,   30720,   63488,   68608,  199680,  268288,
     +   792576, 1841152, 2296832, 6690816, 9181184,23468032,    7683,
     +     7682,    6156,    6152,    6168,   12320,   12384,   61568,
     +    61824,    7680,    7168,    6144,    4096,   12288,   28672,
     +    61440,  462848,  397312,  266240,  790528, 1839104, 2494464,
     +  6688768, 9179136,23465984,   14351,   14350,   14348,   14344,
     +     8240,    8224,    8288,   57472,   57728,   15872,   15360,
     +    14336,   12288,    8192,   24576,   57344,  471040,  405504,
     +   274432, 1597440, 1073152, 2502656,13381632, 9986048,30158848/
      DATA (ISTA(I),I=351,420) /
     +    30735,   30734,   30732,   30728,   24624,   24608,   49344,
     +    49280,   49536,   32256,   31744,   30720,   28672,   24576,
     +    16384,   49152,  487424, 1982464, 1851392, 1589248, 1064960,
     +  4079616,14172160, 9977856,30949376,   57407,   63502,   57404,
     +    57400,   57392,   57376,   32960,   32896,   33152,   65024,
     +    64512,   63488,   61440,   57344,   49152,   32768,  520192,
     +  2015232, 1884160, 1622016, 1097728, 4112384,14204928,10010624,
     + 30982144,   67075,   67074,   67078,   68616,   68632,   80928,
     +    80992,   81120,   81376,   67072,   66560,   68608,   72704,
     +    80896,  487424,  520192,   65536,  196608,  458752,  983040/
      DATA (ISTA(I),I=421,490) /
     +  2031616, 2293760, 6488064,14876672,23265280,  198147,  198146,
     +   199692,  199688,  399384,  405536,  405600,  454784,  455040,
     +   198144,  197632,  199680,  397312,  405504,  421888,  454656,
     +   196608,  131072,  393216,  917504, 1966080, 2228224, 6422528,
     + 14811136,23199744,  269827,  269826,  268300,  268296,  268312,
     +   274464,  274528,  323712,  323968,  460288,  459776,  268288,
     +   266240,  274432,  290816,  323584,  458752,  393216,  262144,
     +   786432, 1835008, 2490368, 6684672, 9175040,23461888,  794115,
     +   794114,  792588,  792584,  798768,  798752, 1597536, 1622144,
     +  1622400,  984576,  984064,  792576,  790528,  798720, 1589248/
      DATA (ISTA(I),I=491,560) /
     +  1622016,  983040,  917504,  786432,  524288, 1572864, 3014656,
     + 13107200, 8912896,29884416, 1842691, 1842690, 1079308, 1079304,
     +  1073200, 1073184, 1073248, 1097856, 1098112, 1842688, 1842176,
     +  1841152, 1839104, 1073152, 1064960, 1097728, 2031616, 1966080,
     +  1835008, 1572864, 1048576,16252928,14155776, 9961472,30932992,
     +  2295299, 2295298, 2296844, 2296840, 2496536, 2502688, 2502752,
     +  2551936, 4112768, 2295296, 2294784, 2296832, 2494464, 2502656,
     +  4079616, 4112384, 2293760, 2228224, 2490368, 3014656, 4063232,
     +  2097152, 6291456,14680064,23068672, 6489603, 6489602, 6491148,
     +  6690824, 6690840,13381664,13381728,14205056,14205312, 6489600/
      DATA (ISTA(I),I=561,625) /
     +  6489088, 6690816, 6688768,13381632,14172160,14204928, 6488064,
     +  6422528, 6684672,13107200,14155776, 6291456, 4194304,12582912,
     + 20971520, 9373187, 9373186, 9181196, 9181192, 9187376, 9187360,
     +  9986144,10010752,10011008, 9373184, 9372672, 9181184, 9179136,
     +  9187328, 9977856,10010624,14876672,14811136, 9175040, 8912896,
     +  9961472,14680064,12582912, 8388608,29360128,23266819,23266818,
     + 23268364,23468040,23468056,30158880,30957664,30982272,30982528,
     + 23266816,23266304,23468032,23465984,30158848,30949376,30982144,
     + 23265280,23199744,23461888,29884416,30932992,23068672,20971520,
     + 29360128,16777216/
C
C ISVT is the parameter 'SVT', which turns on the algorithm for
C thinning simple vectors and helps determine how it works.
C
      DATA ISVT / 5 /
C
C ITBM contains the parameters 'TBX' and 'TBA', which are used to mask
C triangle blocking flags.  It has the form 4096*ITBX+ITBA; both ITBX
C and ITBA are 12-bit masks.  If ITBF is the triangle blocking flag for
C some triangle of the triangular mesh, then, in general, the triangle
C will be blocked if and only if the value of AND(XOR(ITBF,ITBX),ITBA)
C is non-zero.  The default values are such as to block only triangles
C having the low-order blocking flag bit set (ITBX = 0, ITBA = 1).
C
      DATA ITBM / 1 /
C
C IWSO is the parameter 'WSO', which says what to do when workspace
C overflow occurs.
C
      DATA IWSO / 1 /
C
C IZFF is the parameter 'ZFF' (output only) which is non-zero if the
C flow field being dealt with is essentially zero.
C
      DATA IZFF / 0 /
C
C JODP, JOMA, and JOTZ are used to hold 0/1 flags extracted from the
C parameter 'NOF'.  Each is non-zero if and only if some extraneous
C portion of a numeric label may be omitted.
C
C LCTM is the length of the character string in CTMA.
C
      DATA LCTM / 1 /
C
C LEA1, LEA2, and LEA3 are the actual lengths of the three portions of
C the character string CHEX.
C
C LEE1, LEE2, and LEE3 are the effective lengths of the three portions
C of the character string CHEX.
C
C LIWB is the parameter 'IWB', which is the length of the integer
C workspace to be made available to the routine VTTDBF (called to
C set the blocking flags for triangles being mapped by TDPACK).
C
      DATA LIWB / 2500 /
C
C LIWK is the length of the user's integer workspace array, as declared
C in the last call to VTMESH.
C
C LIWS is described with IIWS, above.
C
C LNLG is the linear/log flag for the SET call defining the mapping
C from the current viewport to the window and vice-versa.
C
C LOEN, LOPN, and LOTN are the lengths of an edge node, a point node,
C and a triangle node, respectively, as set by the routine VTMESH.
C
C LRWK is the length of the user's real workspace array, as declared in
C the last call to VTMESH.
C
C LRWS is described with IRWS, above.
C
C LSDD is set by VTMESH to indicate the position of the leftmost
C significant digit in ABS(DMAX-DMIN).
C
C LSDL is used for the leftmost-significant-digit argument of VTNUMB,
C which is based on, but not identical with, the leftmost-significant-
C digit parameter 'NLS'.
C
C LSDM is set by VTMESH to indicate the position of the leftmost
C significant digit in MAX(ABS(DMIN),ABS(DMAX)).
C
C LTIL is the length of the informational label, before substitution.
C
      DATA LTIL / 21 /
C
C LTZF is the length of the zero-field label, before substitution.
C
      DATA LTZF / 10 /
C
C MIRO is a flag used to signal that the coordinate transformations in
C effect will cause mirror imaging.
C
      DATA MIRO / 0 /
C
C NCLR is the parameter 'NLV', which is the number of colors currently
C defined for use on streamlines.
C
      DATA NCLR / 0 /
C
C NDGL is used for the number-of-significant-digits argument of VTNUMB,
C which is based on, but not identical with, the number-of-significant-
C digits parameter 'NSD'.
C
C NEDG/LOEN is the number of edges in the triangular mesh (set by
C VTMESH).
C
C NEXL is the parameter 'NEL', which specifies the desired length of
C exponents in numeric labels.  A value which is zero or negative
C indicates that exponents should be written in the shortest possible
C form.  A positive value "n" indicates that a sign should be used (+
C or -) and that the length should be padded, if necessary, to n digits
C with leading zeroes.
C
      DATA NEXL / 0 /
C
C NEXT is the parameter 'NET', which is the numeric exponent type,
C specifying what characters are to be used between the mantissa of a
C numeric label and the exponent.  The value 0 implies the use of an
C E, as in FORTRAN "E format", the value 1 implies the use of function
C codes, as expected by the utility routine PLOTCHAR, to generate
C "x10n", where n is a superscript exponent, and the value 2 implies
C the use of "x10**".
C
      DATA NEXT / 1 /
C
C NEXU is the parameter 'NEU', the numeric exponent use flag.  A value
C less than or equal to zero forces the use of the exponential form in
C all numeric labels.  A positive value n indicates that the form
C without an exponent should be used as long as it requires no more
C than n characters; otherwise the form requiring the fewest characters
C should be used.
C
      DATA NEXU / 5 /
C
C NLBS specifies the current number of entries in the list of labels.
C
      DATA NLBS / 0 /
C
C NLSD is the parameter 'NLS', the leftmost-significant-digit flag.
C The value zero indicates that the leftmost non-zero digit of a
C number represented by a numeric label is to be considered its first
C significant digit.  A non-zero value indicates that the digit in the
C same digit position as the leftmost non-zero digit of the largest
C number (in absolute value) in the field is to be considered the
C leftmost significant digit.  This tends to make the numeric labels
C more consistent with one another.  Consider the following example,
C using three significant digits:
C
C    'NLS'=0:  .500  1.00  1.50  ...  9.50  10.5  ...
C    'NLS'=1:  .5    1.0   1.5   ...  9.5   10.5  ...
C
      DATA NLSD / 1 /
C
C NLZF is the parameter 'NLZ', which may be set non-zero to force a
C zero preceding the decimal point in no-exponent representations of
C numbers.
C
      DATA NLZF / 0 /
C
C NOMF is the parameter 'NOF', which specifies the numeric omission
C flags, which say what parts of a numeric label may be omitted.  The
C value 0 says that no part may be omitted.  Adding a 4 indicates that
C a leading "1" or "1." which is unnecessary (as in "1x10**13") may be
C omitted, adding a 2 indicates that a trailing decimal point (as in
C "13.") may be omitted, and adding a 1 indicates that trailing zeroes
C (as in "46.200") may be omitted.
C
      DATA NOMF / 6 /
C
C NPNT/LOPN is the number of points in the triangular mesh (set by
C VTMESH).
C
C NR04 is the current number of words of real work space devoted to the
C list of labels which are not line labels (the informational label and
C high/low labels).
C
C NSDL is the parameter 'NSD', which specifies the maximum number of
C significant digits to be used in numeric labels representing contour
C field values.  A negative value "-n" indicates that n significant
C digits should be used.  A positive value "n" indicates that m+n digits
C should be used, where m is the number of digits that are the same for
C all values in the field.  (For example, if the minimum value is 1163.6
C and the maximum value is 1165.9, then the value of m is 3.)
C
      DATA NSDL / 4 /
C
C NSDR is the number of significant digits in a real number, which is
C computed as required by VASPACKT itself.
C
C NTRI/LOTN is the number of triangles in the triangular mesh (set by
C VTMESH).
C
C OORV is the parameter 'ORV', an out-of-range value to be returned by
C VTMPXY for both coordinates of a point which is invisible.
C
      DATA OORV / 0. /
C
C (PCPX,PCPY,PCPZ) is a "projection center point" for the mesh, used in
C the process of projecting each velocity vector into the plane of its
C associated triangle.  The three coordinates are the parameters 'PCX',
C 'PCY', and 'PCZ'.
C
      DATA PCPX,PCPY,PCPZ / 0.,0.,0. /
C
C PITH is the parameter 'PIT', the "point interpolation threshold".  In
C routines that map polylines using VTMPXY, this value is used to check
C whether two points have mapped so far apart that some interpolated
C points should be inserted.  A value less than or equal to zero (like
C the default) says that no such checks are to be performed.  A value
C greater than zero represents a fraction of the height or width of the
C window in the user coordinate system.
C
      DATA PITH / 0. /
C
C SCFS is the parameter 'SFS', the scale factor selector.
C
      DATA SCFS / 1. /
C
C SCFU is the parameter 'SFU', the scale factor in use.
C
      DATA SCFU / 1. /
C
C SLLN is the parameter 'SLL', the maximum length of any streamline,
C while SLLR is a "realized" value of SLLN, computed as needed.
C
      DATA SLLN / 8. /
C
C SLPS is the parameter 'SLP', the desired distance between points used
C to draw the streamlines, while SLPR is a "realized" value of SLPS,
C computed as needed.
C
      DATA SLPS / .001 /
C
C SLSP is the parameter 'SLS', the desired spacing of the streamlines,
C while SLSR is a "realized" value of SLSP, computed as needed.
C
      DATA SLSP / .072 /
C
C SVSP is the parameter 'SVS', which is the desired spacing of simple
C vectors.  Its value is used by the algorithm that attempts to cull
C some of the simple vectors.  SVSR is a "realized" value of this
C parameter, computed from it as needed.
C
      DATA SVSP / 0. /
C
C TTLL is the parameter 'TTL', which is the length of each termination
C test line, expressed in the same coordinate system used for the
C triangular mesh, while TTLR is a "realized" value of TTLL, computed
C as required.
C
      DATA TTLL / .018 /
C
C TTSP is the parameter 'TTS', which says how far apart termination
C tests are to be spaced along streamlines, expressed in the same
C coordinate system used for the triangular mesh, while TTSR is a
C "realized" value of TTSP, computed as required.
C
      DATA TTSP / .036 /
C
C TVAL is the parameter array 'TVL', which is an array of threshold
C values for use in determining what color indices from 'CLR' are to
C be used to color streamlines.
C
      DATA TVAL / 257*0. /
C
C TXIL is the parameter 'ILT', the text of the informational label.
C
      DATA TXIL / 'SCALE FACTOR IS $SFU$' /
C
C TXZF is the parameter 'ZFT', the text of the zero-field label.
C
      DATA TXZF / 'ZERO FIELD' /
C
C UVPL, UVPR, UVPB, and UVPT are the parameters 'VPL', 'VPR', 'VPB',
C and 'VPT', specifying the edges of an area in which the viewport is
C to lie.  Each is expressed as a fraction of the distance from left to
C right, or from bottom to top, in the plotter frame.
C
      DATA UVPL,UVPR,UVPB,UVPT / .05,.95,.05,.95 /
C
C UVPS is the parameter 'VPS', specifying the desired shape of the
C viewport.
C
      DATA UVPS / .25 /
C
C UWDL, UWDR, UWDB, and UWDT are the parameters 'WDL', 'WDR', 'WDB',
C and 'WDT', specifying the user-coordinate-system values at the left,
C right, bottom, and top edges of the window.  These are used when
C VASPACKT is asked to do the call to SET; they become arguments 5
C through 8 in the call.
C
      DATA UWDL,UWDR,UWDB,UWDT / 0.,0.,0.,0. /
C
C VFRA is the parameter 'VFR', which is the minimum length of a simple
C vector or a curly vector as a fraction of 'VRL'.
C
      DATA VFRA / 0. /
C
C VRLN is the parameter 'VRL', which is the "vector reference length",
C given in the native coordinate system of the triangular mesh, which
C is the length to be used for a curly vector representing magnitude
C 'VRM'.  VRLR is a "vector reference length, realized", which is
C computed as needed, as implied by the value of VRLN.
C
      DATA VRLN / 0. /
C
C VRMG is the parameter 'VRM' - the "vector reference magnitude".  A
C simple vector or curly vector of this magnitude will be shown with
C length 'VRL'.  The value zero specifies the use of the actual maximum
C magnitude in the data field.  VRMR is a "vector reference magnitude,
C realized", which is computed as needed, as implied by the value of
C VRMG.
C
      DATA VRMG / 0. /
C
C VVMM is the parameter 'VVM', which is the velocity vector magnitude
C minimum, below which the velocity is considered to be zero.
C
      DATA VVMM / 0. /
C
C WCIL is the parameter 'ILS', which specifies the width of a character
C in the informational label, as a fraction of the viewport width.
C
      DATA WCIL / .012 /
C
C WCZF is the parameter 'ZFS', which specifies the width of a character
C in the zero-field label, as a fraction of the viewport width.
C
      DATA WCZF / .012 /
C
C WLIL is the parameter 'ILL', a line-width specifier for the box
C around an informational label.
C
      DATA WLIL / 0. /
C
C WLZF is the parameter 'ZFL', a line-width specifier for the box
C around a zero-field label.
C
      DATA WLZF / 0. /
C
C WWIL is the parameter 'ILW', which specifies the width of the white
C space around the informational label, as a fraction of the viewport
C width.
C
      DATA WWIL / .005 /
C
C WWZF is the parameter 'ZFW', which specifies the width of the white
C space around the zero-field label, as a fraction of the viewport
C width.
C
      DATA WWZF / .005 /
C
C XLBC is the parameter 'LBX', which may be retrieved in any of the
C change routines and specifies the X position of the label's center,
C in the current user coordinate system.
C
      DATA XLBC / 0. /
C
C XMAX and XMIN are the parameters 'XMX' and 'XMN', the maximum and
C minimum values among the user's X coordinate data.
C
      DATA XMAX,XMIN / 0.,0. /
C
C XVPL and XVPR specify the positions of the current viewport's left
C and right edges.  Both values are between 0. and 1.
C
C XWDL and XWDR are the values at the left and right edges of the
C current window in the user coordinate system.
C
C YLBC is the parameter 'LBY', which may be retrieved in any of the
C change routines and specifies the Y position of the label's center,
C in the current user coordinate system.
C
      DATA YLBC / 0. /
C
C YMAX and YMIN are the parameters 'YMX' and 'YMN', the maximum and
C minimum values among the user's Y coordinate data.
C
      DATA YMAX,YMIN / 0.,0. /
C
C YVPB and YVPT specify the positions of the current viewport's bottom
C and top edges.  Both values are between 0. and 1.
C
C YWDB and YWDT are the values at the bottom and top edges of the
C current window in the user coordinate system.
C
C ZMAX and ZMIN are the parameters 'ZMX' and 'ZMN', the maximum and
C minimum values among the user's Z coordinate data.
C
      DATA ZMAX,ZMIN / 0.,0. /
C
      END

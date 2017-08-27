      SUBROUTINE SFBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
      RETURN
C
      END
      BLOCKDATA SFBLDAX
C
C Specify default values for the internal parameters.
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,RDS,IDC,LCH,LDP(8,8)
C
C AID is the angle at which fill lines are to be drawn - a real value,
C in degrees.
C
      DATA AID / 0. /
C
C DBL is the distance between fill lines - a real value, in normalized
C device coordinate units, between 0 and 1.  Values less than zero or
C greater than 1 act the same as .00125, which is also the default.
C
      DATA DBL / .00125 /
C
C ITY is a flag specifying the type of fill to be done by the routine
C SFSGFA.
C
      DATA ITY / 0 /
C
C LPA is the pattern selector - the value 0 means that solid lines are
C to be used, the value 1 that dotted lines are to be used (the dots to
C be arranged according to the pattern specified by "LDP") and the dots
C are to be drawn using calls to the SPPS routine POINTS, and the value
C -1 means the same as 1, except that the dotted lines are to be drawn
C using calls to the routine NGDOTS, in which case RDS specifies the
C diameter of each dot and IDC specifies the color of each dot (a color
C index value).
C
      DATA LPA,RDS,IDC / 0 , .01 , 1 /
C
C If LCH is 0, the dots in a dotted line will be actual dots.  If LCH
C is non-zero, the character specified by LCH will be used instead of
C the dots.  See the plot package routine "POINTS".
C
      DATA LCH / 0 /
C
C LDP contains a set of 0s and 1s defining the current dot pattern, in
C an 8x8 pixel.  The value 1 turns a dot on, the value 0 turns it off.
C
      DATA LDP / 64*1 /
C
      END

      FUNCTION VTRAND ()
C
C This function generates repeatable sequences of pseudo-random
C numbers.  Initially, the calling routine zeroes the seed value X in
C the labelled common block VTSEED, calls VTRAND N times (where N is
C zero or greater), and then saves the resulting value of X.  When it
C is desired to repeat the sequence, the value of X is restored.  It
C is probably not a good idea to use an arbitrary seed value for X, as
C the algorithm seems to depend on starting with X = e.
C
      COMMON /VTSEED/ X
      DOUBLE PRECISION X
      SAVE   /VTSEED/
C
      IF (X.EQ.0.) X=2.718281828459045D0
C
      X=MOD(9821.D0*X+.211327D0,1.D0)
      VTRAND=REAL(X)
C
      RETURN
C
      END

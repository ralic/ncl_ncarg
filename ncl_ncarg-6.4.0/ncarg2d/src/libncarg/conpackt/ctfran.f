      FUNCTION CTFRAN ()
C
C Pseudo-random-number generator.
C
      DOUBLE PRECISION X
      SAVE X
C
      DATA X / 2.718281828459045D0 /
C
      X=MOD(9821.D0*X+.211327D0,1.D0)
      CTFRAN=REAL(X)
C
      RETURN
C
      END

      SUBROUTINE CTHLSO (RWRK,NORN,LORN,ISES,IWRK)
C
      DIMENSION RWRK(*),IWRK(*)
C
C This is a modification of a sort routine from TDPACK.  I think the
C original came from Fred Clare.
C
C Given NORN nodes, each consisting of LORN reals, in an array RWRK, a
C sort element selector ISEL [= ABS(ISES)] and an integer array IWRK of
C length NORN, CTHLSO returns in IWRK an array of base indices of nodes
C of RWRK such that if M and N are both in [1,NORN] and M is less than
C or equal to N, then RWRK(IWRK(M)+ISEL) is less than or equal to
C RWRK(IWRK(N)+ISEL).  The base indices returned in IWRK allow one
C to step through the nodes of RWRK in increasing order of the node
C element ISEL.
C
C If the input value of ISES is positive, IWRK is initialized to contain
C the base indices of the first NORN nodes (of length LORN) in RWRK, but
C if the input value of ISES is negative, the initialization of IWRK is
C skipped; it is assumed that the user has initialized IWRK himself and
C that the NORN nodes being sorted on constitute a noncontiguous subset
C of all the nodes (of length LORN) in RWRK.
C
C If requested, generate base indices in the array IWRK.  In any case,
C set the sort element selector ISEL.
C
      IF (ISES.LT.0) THEN
        ISEL=-ISES
      ELSE
        ISEL=+ISES
        DO 10001 I=1,NORN
          IWRK(I)=(I-1)*LORN
10001   CONTINUE
      END IF
C
C Sort the NORN nodes into increasing order.
C
      K=0
C
  101 IF (3*K+1.LT.NORN) THEN
        K=3*K+1
        GO TO 101
      END IF
C
  102 IF (K.GT.0) THEN
C
        DO 104 I=1,NORN-K
C
        J=I
C
  103   IF (RWRK(IWRK(J)+ISEL).LE.RWRK(IWRK(J+K)+ISEL)) GO TO 104
        ITMP=IWRK(J)
        IWRK(J)=IWRK(J+K)
        IWRK(J+K)=ITMP
        J=J-K
        IF (J.LT.1) GO TO 104
        GO TO 103
C
  104   CONTINUE
C
        K=(K-1)/3
C
        GO TO 102
C
      END IF
C
C Done.
C
      RETURN
C
      END

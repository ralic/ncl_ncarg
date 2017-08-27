      FUNCTION HLUICAEDG (IPP1,IPP2,IEDG,LOEN,IPPE,MPPE,NPPE,RPNT)
C
      DIMENSION IEDG(LOEN,MPPE),IPPE(2,MPPE),RPNT(*)
      INTEGER *8 IKEY1,IKEY2
C
C      integer totaldepth
C      save maxdepth,itdepth
C
C modified by dbrown to use mangled integer keys. This fixes a problem
C of the edges centers occasionally giving false equalities with
C different edges. Also improves performance. 11/30/2012
C
C This function, given the base indices, in the point list, of the two
C points defining an edge, searches the edge list for an edge matching
C it.  If such an edge exists, its index is returned; if not, such an
C edge is created and its index is returned.  The search is effected
C using a tree-sort technique, the pointers for which are kept in the
C array IPPE.
C
C If there are any edges in the edge list at all, ...
C
C      if (nppe .eq. 0) then
C         itdepth = 0
C      end if
      IF (NPPE.NE.0) THEN
C
C search it.  First, order the pointers to the points in a consistent
C manner and then find the X, Y, and Z coordinates of the edge's
C midpoint, which we use to determine the order of the edges in the
C list.  (Using the values of ITM1 and ITM2 results in very bad
C behavior by the tree-sort.)
C
        ITM1=MIN(IPP1,IPP2)
        ITM2=MAX(IPP1,IPP2)
C
C
C Instead of finding the edge center and using floating point numbers
C for comparision, bit-mangle the two point ids into a single int*8
C value that will be unique but distribute in the tree in a semi-
C random manner. The 2 ids always mangle to the same value.
C This eliminates the possible error due to float imprecision
C MANGLE is a C routine in libncarg_c
C
        CALL MANGLE(ITM1,ITM2,IKEY1)
C
C Initialize a search index to point to the first element in the sort
C list.
C
        ITMP=1
C
C Loop.  If the search index now points at the edge we want, return
C its index.
C
C        idepth = 0
  101   IF (ITM1.EQ.IEDG(1,ITMP).AND.ITM2.EQ.IEDG(2,ITMP)) THEN
C
          HLUICAEDG=ITMP
C
          RETURN
C
        END IF
C
C Find the X, Y, and Z coordinates of this edge's midpoint for
C comparison with the one we seek.
C
C        idepth = idepth + 1
        CALL MANGLE(IEDG(1,ITMP),IEDG(2,ITMP),IKEY2)
C
C If the edge we want would precede the one pointed at by the search
C index, reset the search index to look at lesser elements (if any),
C and loop back to continue the search.  If the pointer is null, reset
C it to point to a new element that we will create.
C
C  this commented out code can be used if integer*8 is not portable
C        if ((ikey1(1) .lt. ikey2(1)) .or.
C     +      (ikey1(1) .eq. ikey2(1) .and. ikey1(2) .lt. ikey2(2))) then
C
        IF (IKEY1 .LT. IKEY2) THEN
C
          IF (IPPE(1,ITMP).NE.0) THEN
            ITMP=IPPE(1,ITMP)
            GO TO 101
          END IF
C
          IPPE(1,ITMP)=NPPE+1
C
C If the edge we want would follow the one pointed at by the search
C index, reset the search index to look at greater elements (if any),
C and loop back to continue the search.  If the pointer is null, reset
C it to point to a new element that we will create.
C
C        else if ((ikey1(1) .gt. ikey2(1)) .or.
C     +       (ikey1(1) .eq. ikey2(1) .and. ikey1(2) .gt. ikey2(2))) the
C
        ELSE IF (IKEY1 .GT. IKEY2) THEN
C
          IF (IPPE(2,ITMP).NE.0) THEN
            ITMP=IPPE(2,ITMP)
            GO TO 101
          END IF
C
          IPPE(2,ITMP)=NPPE+1
C
        ELSE
C
          CALL SETER ('ICAEDG - LOGIC ERROR',1,1)
          HLUICAEDG=-1
          RETURN
C
        END IF
C
      END IF
C
C      output for monitoring performance of the key method
C
C      itdepth = itdepth + idepth
C      if (idepth .gt. maxdepth) then
C         maxdepth = idepth
C         write(*,*) 'current maxdepth:', maxdepth,
C     +              ' avg depth: ', itdepth / (nppe + 1),
C     +              'edge count is: ', (nppe + 1)
C      else if (mod((nppe+1),100) .eq. 0) then
C         write(*,*) ' avg depth: ', itdepth, itdepth / (nppe + 1)
C      end if
C
C Create a new edge in the edge list (if there's room, of course), and
C return its index to the caller.
C
      IF (NPPE.GE.MPPE) THEN
C
        CALL SETER ('ICAEDG - EDGE ARRAY IS TOO SMALL',1,1)
        HLUICAEDG=-1
        RETURN
C
      ELSE
C
        NPPE=NPPE+1
C
        IPPE(1,NPPE)=0
        IPPE(2,NPPE)=0
C
        IEDG(1,NPPE)=MIN(IPP1,IPP2)
        IEDG(2,NPPE)=MAX(IPP1,IPP2)
        IEDG(3,NPPE)=-1
        IEDG(4,NPPE)=-1
        IEDG(5,NPPE)=0
C
        HLUICAEDG=NPPE
C
      END IF
C
      RETURN
C
      END

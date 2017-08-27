      SUBROUTINE HLUCPCHHL (IFLG)
C
C This routine stands between CONPACK and the user call-back routine
C CPCHHL.  When HLUs are not in use, this version of the routine gets
C loaded, so that CPCHHL is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CPCHHL.
C
      CALL CPCHHL (IFLG)
C
      RETURN
C
      END
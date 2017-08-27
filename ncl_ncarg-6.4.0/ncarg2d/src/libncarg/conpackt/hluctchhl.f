      SUBROUTINE HLUCTCHHL (IFLG)
C
C This routine stands between CONPACKT and the user call-back routine
C CTCHHL.  When HLUs are not in use, this version of the routine gets
C loaded, so that CTCHHL is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CTCHHL.
C
      CALL CTCHHL (IFLG)
C
      RETURN
C
      END

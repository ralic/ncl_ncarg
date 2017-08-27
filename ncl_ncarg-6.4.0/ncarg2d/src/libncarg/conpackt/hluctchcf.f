      SUBROUTINE HLUCTCHCF (IFLG)
C
C This routine stands between CONPACKT and the user call-back routine
C CTCHCF.  When HLUs are not in use, this version of the routine gets
C loaded, so that CTCHCF is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CTCHCF.
C
      CALL CTCHCF (IFLG)
C
      RETURN
C
      END

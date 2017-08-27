      SUBROUTINE HLUVTCHZF (IFLG)
C
C This routine stands between VASPACKT and the user call-back routine
C VTCHZF.  When HLUs are not in use, this version of the routine gets
C loaded, so that VTCHZF is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls VTCHZF.
C
      CALL VTCHZF (IFLG)
C
      RETURN
C
      END

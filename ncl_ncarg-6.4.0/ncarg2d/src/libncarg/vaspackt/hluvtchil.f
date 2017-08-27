      SUBROUTINE HLUVTCHIL (IFLG)
C
C This routine stands between VASPACKT and the user call-back routine
C VTCHIL.  When HLUs are not in use, this version of the routine gets
C loaded, so that VTCHIL is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls VTCHIL.
C
      CALL VTCHIL (IFLG)
C
      RETURN
C
      END

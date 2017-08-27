      SUBROUTINE CTCHCL (IFLG)
C
C This routine is a dummy.  It is called just before and just after each
C contour line is drawn.  A user version may be substituted to change
C dash pattern, color, and/or line width.
C
C IFLG is +1 if a contour line is about to be drawn, -1 if a contour
C line has just been drawn.
C
C When CTCHCL is called, the internal parameter 'PAI' will have been
C set to the index of the appropriate contour level.  Thus, parameters
C associated with that level may easily be retrieved by calls to CTGETx.
C
      RETURN
C
      END

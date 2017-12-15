      Subroutine SkipLines(NLines,Unit)

C###############################################################################
C
C     Se salta NLines de el archivo dado
C
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################

      Implicit None

C------------------------- External.

      Integer*4::NLines,Unit

C------------------------- Internal.
      Integer*4::I

C------------------------- First executable statment.
      Do I=1,NLines
          Read(Unit,*)
      End Do !Do I=1,NLines

      End Subroutine SkipLines
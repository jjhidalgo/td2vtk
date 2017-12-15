      Subroutine Make_File_Name(Ext,Filename,Num,Root,Suffix)

c########################################################################
c                                                                       #
c     Construye el nombre de un archivo a partir de un número, un       #
c     sufijo y una extensión.                                           #
c                                                                       #
c                                                                       #
C     Autor: JHG                                                        #
C     Fecha: Julio, 2007                                                #
C                                                                       #
C########################################################################
      Implicit None

C------------------------- External

      Integer*4::Num
      Character*3::Ext
      Character*40::Filename
      Character*20::Root
      Character*7::Suffix

C-------------------- Internal

      Integer*4::Ind

C------------------------- First executable estatement.

      Select Case (Num)

          Case(0:9)

              Write(Filename,10) Num
   10         Format(I1)


          Case(10:99)

              Write(Filename,20) Num
   20         Format(I2)

          Case(100:999)

              Write(Filename,30) Num
   30         Format(I3)

          Case(1000:9999)

              Write(Filename,40) Num
   40         Format(I4)

          Case Default

C------------------------- Removes underscore from Suffix '_'

              Ind = Index(Suffix,'_') - 1
              Suffix = Suffix(1:Ind)

          Write(Filename,99) 
   99     Format(1X)

      End Select

      Filename = Trim(Root) // Trim(Suffix) // Trim(Filename) // '.'
     &          // Ext


      End Subroutine Make_File_Name

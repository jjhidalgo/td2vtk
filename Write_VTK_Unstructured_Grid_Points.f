      Subroutine Write_VTK_Unstructured_Grid_Points
     &          (COORD    ,NUMNP    ,Title    ,VTKUnit)

C###############################################################################
C
C     Escribe una malla en formato VTK Legacy
C
C     La malla está formada sólo por puntos. Es decir, las celdas son los
C     propios nudos.
C
C     Autor: JHG
C     Fecha: Junio, 2008
C
C###############################################################################


      Implicit None

C------------------------- External

      Integer*4::NUMNP 

      Integer*4::VTKUnit

      Character*80::Title

      Real*8::COORD(NUMNP,3)

C------------------------- Internal

      Integer*4::I,ListSize

C------------------------- 
C-------------------------


C------------------------- Escribe la cabecera


      Write(VTKUnit,10) Title

   10 Format('# vtk DataFile Version 1.0'
     &      ,/,A80
     &      ,/,'ASCII'
     &      ,/
     &      ,/,'DATASET UNSTRUCTURED_GRID')

C-------------------------
C------------------------- NUDOS
C------------------------- 

C------------------------- Escribe los nudos


      Write(VTKUnit,20) NUMNP
   20 Format('POINTS',1X,I6,1X,'double',/)

      Do I=1,NUMNP

          Write(VTKUnit,30) Coord(I,1),Coord(I,2),Coord(I,3)
   30     Format (3(F15.5,1X))

      End Do !I=1,NUMNP

C-------------------------
C------------------------- Celdas
C------------------------- 

C------------------------- Calcula los n�meros que hacen falta para
C------------------------- describir las celdas.

      ListSize = 2*NUMNP

C------------------------- Escribe las celdas (conectividades)

      Write(VTKUnit,40) NUMNP,ListSize
   40 Format (/,'CELLS',1X,I6,1X,I9,/)

      

      Do I=1,NUMNP

          Write(VTKUnit,50) 1, I - 1 !Los nudos se numeran a partir de 0 en vtk
   50     Format(I5,1X,I6)

      End Do !L=1,NUMNP

C------------------------- Escribe el tipo de celdas


      Write(VTKUnit,60) NUMNP
   60 Format(/,'CELL_TYPES',1X,I6)


      Do I=1,NUMNP
      
          Write(VTKUnit,70) 1
   70     Format(I5)

      End Do !L=1,NUMNP

      End Subroutine Write_VTK_Unstructured_Grid_Points

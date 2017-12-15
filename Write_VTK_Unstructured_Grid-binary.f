      Subroutine Write_VTK_Unstructured_Grid
     &          (COORD    ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &          ,NUMNP    ,Title    ,VTKUnit)

C###############################################################################
C
C     Escribe una malla en formto VTK Legacy
C
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################


      Implicit None

C------------------------- External

      Integer*4::LMXNDL   ,NUMEL    ,NUMNP 

      Integer*4::VTKUnit

      Integer*4::GetVTKCellType

      Character*80::Title

      Integer*4::KXX(LMXNDL,NUMEL),LNNDEL(NUMEL),LTYPE(NUMEL)

      Real*8::COORD(NUMNP,3)

C------------------------- Internal

      Integer*4::CellType,I,L,ListSize,NNUD

      Character::strFmt1*20

      Integer*4::Conect(6)

      Character*1::lf
      Character*50::straux

      lf = char(10)
C------------------------- 
C-------------------------


C------------------------- Escribe la cabecera


      Write(VTKUnit) "# vtk DataFile Version 3.0",lf
      Write(VTKUnit) Title,lf
      Write(VTKUnit) "BINARY",lf
      Write(VTKUnit) "DATASET UNSTRUCTURED_GRID",lf

   10 Format('# vtk DataFile Version 1.0'
     &      ,/,A80
     &      ,/,'BINARY'
     &      ,/
     &      ,/,'DATASET UNSTRUCTURED_GRID')

C-------------------------
C------------------------- NUDOS
C------------------------- 

C------------------------- Escribe los nudos


      Write(straux,20) NUMNP
      write(VTKUnit) straux//lf

   20 Format('POINTS',1X,I6,1X,'double')

      Write(VTKUnit) Coord(:,:)

C-------------------------
C------------------------- Celdas
C------------------------- 

C------------------------- Calcula los números que hacen falta para
C------------------------- describir las celdas.

      ListSize = SUM(LNNDEL) + NUMEL

C------------------------- Escribe las celdas (conectividades)

      Write(straux,40) NUMEL,ListSize
      write(VTKUnit) lf//straux//lf

   40 Format ('CELLS',1X,I6,1X,I9)

      

      Do L=1,NUMEL

          NNUD = LNNDEL(L)

          Conect(1:NNUD) = KXX(1:NNUD,L) - 1 !Los nudos se numeran a partir de 0 en vtk

          strFmt1 = ''
          Write(strFmt1,*) NNUD
          strFmt1 = '(I5,'//Trim(AdjustL(strFmt1))//'(1X,I6))'

          Write(straux,strFmt1) NNUD,Conect(1:NNUD)
          write(VTKUnit) straux
C   50     Format(I5,<NNUD>(1X,I6))

      End Do !L=1,NUMEL

C------------------------- Escribe el tipo de celdas


      Write(straux,60) NUMEL
      write(VTKUnit) lf//straux//lf
   60 Format('CELL_TYPES',1X,I6)


      Do L=1,NUMEL
      
          CellType = GetVTKCellType(LTYPE(L))

          Write(straux,70) CellType
          write(VTKUnit) straux//lf
   70     Format(I5)

      End Do !L=1,NUMEL

      End Subroutine Write_VTK_Unstructured_Grid

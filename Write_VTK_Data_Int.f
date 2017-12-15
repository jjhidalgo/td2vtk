      Subroutine Write_VTK_Data_Int
     &          (IDimVals ,NumVals  ,Values   ,VarName  ,VarType
     &          ,VTKUnit)

C###############################################################################
C
C     Escribe valores escalares o vectoriales por punto o celda en el formato
C     VTK Legacy
C
C     VarType:  1 -> POINT_DATA - SCALAR
C               2 -> CELL_DATA  - SCALAR
C               3 -> POINT_DATA - VECTOR
C               4 -> CELL_DATA - VECTOR
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################


      Implicit None

C------------------------- External

      Integer*4::IDimVals ,NumVals  ,VarType 

      Integer*4::VTKUnit

      Character*2::VarName

      Integer*4::Values(IDimVals,NumVals)


C------------------------- Internal

      Integer*4::I

      Character::strFmt1*20

C------------------------- 
C-------------------------


      If (VarType.EQ.1 .OR. VarType.EQ.3) Then

          Write(VTKUnit,10) NumVals
   10     Format('POINT_DATA',1X,I6)

      Else If(VarType.EQ.2 .OR. VarType.EQ.4) Then

          Write(VTKUnit,11) NumVals
   11     Format('CELL_DATA',1X,I6)

      End If !VarType.EQ.1 .OR. VarType.EQ.3 ...
     

      If (VarType.EQ.1 .OR. VarType.EQ.2) Then

          Write(VTKUnit,20) VarName
   20     Format('SCALARS',1X,A2,1X,'int ',1X,'1')

          Write(VTKUnit,21)
   21     Format('LOOKUP_TABLE default')

      Else If(VarType.EQ.3 .OR. VarType.EQ.4) Then

          Write(VTKUnit,22) VarName
   22     Format('VECTORS',1X,A2,1X,'double ')
          
      End If !VarType.EQ.1 .OR. VarType.EQ.2 ...
      

      strFmt1 = ''
      Write(strFmt1,*) IDimVals
      strFmt1 = '('//Trim(AdjustL(strFmt1))//'(I15,1X))'

      Do I=1,NumVals

		If (VarType.EQ.1 .OR. VarType.EQ.2) Then

			Write(VTKUnit,30) Values(1,I)
   30			Format (I15)

		Else If(VarType.EQ.3 .OR. VarType.EQ.4) Then

			Write(VTKUnit,strFmt1) Values(1:IDimVals,I)
C   31			Format (<IDimVals>(I15,1X))
          
		End If !VarType.EQ.1 .OR. VarType.EQ.2 ...

      End Do !I=1,NUMNP


      End Subroutine Write_VTK_Data_Int

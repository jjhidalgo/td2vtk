      Subroutine Write_Var_Real
     &          (COORD    ,GridType ,IDimVals ,IOWri1D  ,ITime
     &          ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &          ,NUMNP    ,NumVals  ,Root     ,Values   ,VarName
     &          ,VarTime  ,VarType  ,VTKUnit)


C###############################################################################
C
C     Escribe una variable asociada a nudo o elemento en formato VTK Legacy.
C
C     VarType:  1 -> POINT_DATA - SCALAR
C               2 -> CELL_DATA  - SCALAR
C               3 -> POINT_DATA - VECTOR
C               4 -> CELL_DATA - VECTOR
C
C
C     GridType: 1 -> Las celdas son los nudos
C               2 -> Las celdas son los elementos 
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################


      Implicit None

C------------------------- External

      Integer*4::GridType ,IDimVals ,IOWri1D  ,ITime    ,LMXNDL   ,NUMEL
     &          ,NUMNP    ,NumVals  ,VarType

      Integer*4::VTKUnit

      Real*8::VarTime

      Character::Root*20  ,VarName*6

      Integer*4::KXX(LMXNDL,NUMEL),LNNDEL(NUMEL),LTYPE(NUMEL)

      Real*8::COORD(NUMNP,3), Values(IDimVals,NumVals)


C------------------------- Internal

      Integer*4::IDim,NewNumVals,Num1D
      Character*3::Ext*3,Filename*40,Suffix*7,Title*80

      Real*8,Allocatable::NewValues(:,:)

C------------------------- 
C-------------------------

      Ext = 'vtk'
      Suffix = Trim(VarName) // '_'


C------------------------- Construye el nombre del archivo

      Call Make_File_Name(Ext,Filename,ITime,Root,Suffix)

      Write (Title, 10) Trim(Root),VarName, VarTime
   10 Format(A20,1X,A2,' Time: ',G25.8E3)

      Open (VTKUnit, File=Filename, Status='UNKNOWN')

      If (GridType.EQ.1) Then

          Call Write_VTK_Unstructured_Grid_Points
     &        (COORD    ,NUMNP    ,Title    ,VTKUnit)

      Else If (GridType.EQ.2) Then

          Call Write_VTK_Unstructured_Grid
     &        (COORD    ,IOWri1D  ,KXX      ,LMXNDL   ,LNNDEL
     &        ,LTYPE    ,NUMEL    ,NUMNP    ,Title    ,VTKUnit)

      End If !GridType.EQ.1,2
      
      If ((VarType.EQ.2 .OR. VarType.EQ.4) .AND. IOWri1D.LT.1) Then

          Num1D = COUNT(LTYPE.EQ.1)
          NewNumVals = NumVals - Num1D

          Allocate(NewValues(IDimVals,NewNumVals))
          NewValues = 0

          Do IDim=1,IDimVals
            
              NewValues(IDim,:) = pack(Values(IDim,:),LTYPE.NE.1)

          End Do

          Call Write_VTK_Data_Real
     &        (IDimVals ,NewNumVals  ,NewValues   ,VarName  ,VarType
     &        ,VTKUnit)
      Else

          Call Write_VTK_Data_Real
     &        (IDimVals ,NumVals  ,Values   ,VarName  ,VarType
     &        ,VTKUnit)

      End If

      Close(VTKUnit)


      End Subroutine Write_Var_Real

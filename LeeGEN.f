      Subroutine LeeGEN
     &          (COORD    ,FileGEN  ,IFmt     ,IOWri1D  ,IRegimen
     &          ,ISOLEQ   ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE
     &          ,NINT     ,GenUnit  ,NameGen  ,NUMEL    ,NumGen
     &          ,NUMNP    ,NumVals  ,NWarns   ,OutUnit  ,Root
     &          ,Times    ,VarType  ,VTKUnit)

C###############################################################################
C
C     Lee del archivos genericos por nudos o elementos. Los archivos
C     deben tener formato Mxx. Se supone que sólo hay un problema y que
C     se escribe la variable en todos los tiempo, es decir,
C     IOMCC = IOMHH = 1.
C
C     IVar = 1 --> Nudo
C            2 --> Elemento
C
C     IRegimen = 0 --> Se resuelve estacionario
C                1 --> Transitorio con condiciones iniciales prescritas
C                2 --> Transitorio con condiciones iniciales estacionarias
C
C     VarType:  1 -> POINT_DATA - SCALAR
C               2 -> CELL_DATA  - SCALAR
C               3 -> POINT_DATA - VECTOR
C               4 -> CELL_DATA - VECTOR
C
C     Autor: JHG
C     Fecha: Enero, 2009
C
C###############################################################################

      Implicit None

C------------------------- External

      Integer*4::IFmt     ,IRegimen ,LMXNDL   ,NINT     ,NUMEL
     &          ,NumGen   ,NUMNP    ,NumVals  ,NWarns   ,VarType

      Integer*4::GenUnit,IOWri1D,OutUnit,VTKUnit

      Logical::IOExistGen

      Character::FileGEN(NumGen)*50,NameGen(NumGen)*3,Root*20


      Integer*4::ISOLEQ(NINT,4)  ,KXX(LMXNDL,NUMEL)
     &          ,LNNDEL(NUMEL)   ,LTYPE(NUMEL)

      Real*8::COORD(NUMNP,3),Times(NINT)


C------------------------- Internal

      Integer*4::IGen

      Character::VarName*6

      Real*8,Allocatable::Values(:)

C------------------------- 
C------------------------- 

      Allocate(Values(NumVals))
      Values = 0d0

      Do IGen=1,NumGEN

          Inquire (FILE=FileGEN(IGen), EXIST=IOExistGEN)

          If (IOExistGEN) Then

              VarName = NameGen(IGen)//'   '

              Call LeeMxx
     &            (COORD    ,FileGEN(IGen)      ,IFmt     ,1
     &            ,IOWri1D  ,IRegimen ,ISOLEQ   ,3        ,KXX
     &            ,LMXNDL   ,LNNDEL   ,LTYPE    ,NINT     ,GenUnit
     &            ,NUMEL    ,NUMNP    ,1        ,NumVals  ,OutUnit
     &            ,Root     ,Times    ,Values   ,VarName  ,VarType
     &            ,VTKUnit)

          Else

              Write (OutUnit,99) FileGEN(IGen)
              Write (*,99)
   99         Format (/,'AVISO: El archivo generico ',A50
     &                     ,' no existe.')

                  NWarns = NWarns + 1

          End If !IOExistGEN

      End Do !I=1,NumGEN

      DeAllocate(Values)

      End Subroutine LeeGEN

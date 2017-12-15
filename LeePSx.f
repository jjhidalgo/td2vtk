      Subroutine LeePSx
     &          (COORD    ,FilePSx  ,IOWri1D  ,IVar     ,KXX
     &          ,LMXNDL   ,LNNDEL   ,LTYPE    ,NINT     ,PSxUnit
     &          ,NPAR     ,NUMEL    ,NUMNP    ,OutUnit  ,Root
     &          ,Times    ,VarType  ,VTKUnit)

C###############################################################################
C
C     Lee del archivo PSx (PSH o PSC) los valores de la sensibilidad de la
C     variable de estado respecto de cada parámetro para los tiempos en los que
C     se haya escrito.
C
C     IVar = 1 --> Niveles (PSH)
C            2 --> Concentraciones (PSC)
C
C     VarType:  1 -> POINT_DATA - SCALAR
C               2 -> CELL_DATA  - SCALAR
C               3 -> POINT_DATA - VECTOR
C               4 -> CELL_DATA - VECTOR
C
C     Autor: JHG
C     Fecha: Marzo, 2008
C
C###############################################################################

      Implicit None

C------------------------- External

      Integer*4::IVar     ,LMXNDL   ,NINT
     &          ,NPAR     ,NUMEL    ,NUMNP    ,VarType

      Integer*4::IOWri1D,PSxUnit,OutUnit,VTKUnit

      Character::FilePSx*50,Root*20,VarName*6
      

      Integer*4::KXX(LMXNDL,NUMEL),LNNDEL(NUMEL),LTYPE(NUMEL)

      Real*8::COORD(NUMNP,3),Times(NINT)

C------------------------- Internal

      Integer*4::I      ,IPar     ,ITime    ,J

      Real*8::VarTime

      Logical::EOF

      Character::Preffix*2,strFmt1*20,strFmt2*20

      Real*8,Allocatable::Values(:,:)

      Character,Allocatable::ParNames(:)*10

C------------------------- 
C------------------------- 

      
      Open (UNIT=PSxUnit, FILE=FilePSx, STATUS='OLD')

      If (IVar.EQ.1) Then

          Write (OutUnit,11)
          Write (*,11)
   11 Format (/,'Procesando el archivo PSH...')

          Preffix = 'SH'

      Else If (IVAR.EQ.2) Then

          Write (OutUnit,12)
          Write (*,12)
   12 Format (/,'Procesando el archivo PSC...')

          Preffix = 'SC'

      End If !IVar.EQ.1 ...

      EOF=.FALSE.

      ITime = 0

      Allocate(ParNames(NPAR))
      Allocate(Values(NUMNP,NPAR))

      ParNames = ''
      Values = 0d0

C------------------------- Salta la primera línea

      Read(PSxUnit,*)

C------------------------- Lee los nombres de los parámetros

      strFmt1 = ''
      Write(strFmt1,*) NPAR
      strFmt1 = '('//Trim(AdjustL(strFmt1))//'(A4,1X))'

      strFmt2 = ''
      Write(strFmt2,*) NPAR
      strFmt2 = '('//Trim(AdjustL(strFmt2))//'F15.5)'

      Read(PSxUnit,strFmt1) ParNames(1:NPAR)

C   10 Format(<NPAR>(A4,1X))

C------------------------- Lee los valores para cada tiempo


      Do While (.NOT.EOF)

          ITime = ITime + 1

C------------------------- Salta la línea del paso de  tiempo

          Read (PSxUnit,*,END=999)
          
C------------------------- Lee los valores por nudo para el tiempo ITime

          Do I=1,NUMNP

              Read (PSxUnit,strFmt2) (Values(I,J),J=1,NPAR)

          End Do !I=1,NUMNP

C   20     Format(<NPAR>F15.5)

C------------------------- Escribe los valores por nudo para el tiempo ITime

          Do IPar = 1, NPAR
           
              VarName = Preffix // ParNames(IPar)

              Call Write_Var_Real
     &            (COORD   ,2        ,1        ,IOWri1D ,ITime
     &            ,KXX     ,LMXNDL  ,LNNDEL  ,LTYPE    ,NUMEL
     &            ,NUMNP   ,NUMNP   ,Root    ,Values(1,IPar)
     &            ,VarName ,VarTime ,VarType ,VTKUnit)

          End Do !IPar = 1, NPAR

           Cycle

  999      EOF = .TRUE.

      End Do ! While Not(EOF)

      Close (PSxUnit)

      DeAllocate(Values)
      DeAllocate(ParNames)

      End Subroutine LeePSx

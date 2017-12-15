      Subroutine LeeMxx
     &          (COORD    ,FileMxx  ,IFmt     ,IOMxx    ,IOWri1D
     &          ,IRegimen ,ISOLEQ   ,IVar     ,KXX      ,LMXNDL
     &          ,LNNDEL   ,LTYPE    ,NINT     ,MxxUnit  ,NUMEL
     &          ,NUMNP    ,NumProbs ,NumVals  ,OutUnit  ,Root
     &          ,Times    ,Values   ,VarName  ,VarType  ,VTKUnit)
C###############################################################################
C
C     Lee del archivo Mxx (MHH o MCC) los valores del variable de estado para
C     los tiempos en los que se haya escrito.
C
C     La subrutina copia la estructura de escritura de Transdens y
C     Transin para leer los archivos.
C
C     Se ha generalizado para leer variables por elemento contenidas en archivos
C     tipo Mxx.
C
C     IVar = 1 --> Niveles (MHH)
C            2 --> Concentraciones (MCC)
C            3 --> Genérica
C
C     IRegimen = 0 --> Se resuelve estacionario
C                1 --> Transitorio con condiciones iniciales prescritas
C                2 --> Transitorio con condiciones iniciales estacionarias
C
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

      Integer*4::IFmt     ,IOMxx   ,IOWri1D  ,IRegimen ,IVar     ,LMXNDL
     &          ,NINT    ,NUMEL    ,NUMNP    ,NumProbs ,NumVals
     &          ,VarType

      Integer*4::MxxUnit,OutUnit,VTKUnit

      Character::FileMxx*50,Root*20,VarName*6


      Integer*4::ISOLEQ(NINT,4)  ,KXX(LMXNDL,NUMEL)
     &          ,LNNDEL(NUMEL)   ,LTYPE(NUMEL)

      Real*8::COORD(NUMNP,3),Times(NINT),Values(NumVals)


C------------------------- Internal

      Integer*4::I      ,IDummy ,INI    ,IPROB  ,IT     ,JT

      Character::RootPb*20,strFmt1*20

C-------------------------
C-------------------------

      Open (UNIT=MxxUnit, FILE=FileMxx, STATUS='OLD')

      If (IVar.EQ.1) Then

          Write (OutUnit,11)
          Write (*,11)
   11 Format (/,'Procesando el archivo MHH...')

      Else If (IVar.EQ.2) Then

          Write (OutUnit,12)
          Write (*,12)
   12 Format (/,'Procesando el archivo MCC...')

      Else If (IVar.EQ.3) Then

          Write (OutUnit,13) VarName
          Write (*,13) VarName
   13 Format (/,'Procesando archivo generico ',A6,' ...')

      End If !IVar.EQ.1 ...

      strFmt1 = ''
      Write(strFmt1,*) 4+IFmt
      strFmt1 = '(I'//Trim(AdjustL(strFmt1))//',F15.8)'

      RootPb = Root


      Do IProb = 1, NumProbs

C------------------------- Si hay más de un problema modifica la ráiz
C------------------------- para incluir el número de problema.

          If (NumProbs .GT. 1) Then

              Write(RootPb,'(I5)') IPROB
              RootPb = Trim(AdjustL(RootPb))
              RootPb = Trim(Root) // '-p' // Trim(RootPb) // '- '

          End If !NumProbs .GT. 1


C-------------------------Si es transitorio se empieza a contar en 2
C-------------------------porque el el segundo tiempo es el primero que
C-------------------------se escribe en el Mxx (si IOMxx = 1).

          !If (ISOLEQ(1,IVar).EQ.1 .AND. IRegimen.NE.0) Then
          If (IRegimen.NE.1) Then

             INI = 1

          Else

             INI = 2

          End If !ISOLEQ(1,IVar).EQ.1 .AND. IRegimen.NE.0

          Do IT=INI,NINT

              JT = MAX(1,IT-1)  ! Should be equal to IT-1 except for
                                ! IT=0 to avoid accessing zero element of ISOLEQ

              If( (ISOLEQ(JT,IVar).EQ.0 .AND. IRegimen.GT.0 .AND.    !TRANSIENT
     ;                ( (MOD(IT,IOMxx).EQ.0 .AND. IT.NE.1) .OR.     !TRANSIENT
     ;                                       IT.EQ.NINT) ) .OR.    !TRANSIENT
     ;          (IT.EQ.1 .AND. (IRegimen.EQ.0 .OR. IRegimen.EQ.2) ) .OR. ! STEADY
     ;          (MOD(IT,IOMxx).EQ.0 .AND. ISOLEQ(JT,IVar).EQ.1) ) THEN


                  Read (MxxUnit,*) !Se salta la línea que dice estacionario

                  Do I=1,NumVals

                      Read (MxxUnit,strFmt1) IDummy,Values(I)
C   10                Format(I<4+IFmt>,F15.8)

                  End Do !I=1,NumVals

                  Read (MxxUnit,*) !Se salta el '-1'

                  Call Write_Var_Real
     &                (COORD    ,2        ,1        ,IOWri1D  ,IT
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NumVals  ,RootPb   ,Values   ,VarName
     &                ,Times(IT),VarType  ,VTKUnit)


              End If !(ISOLEQ(JT,IVar).EQ.0 .AND. IRegimen.GT.0

          End Do !IT=INI,NINT

      End Do !IProb = 1, NumProbs

      Close (MxxUnit)

      End Subroutine LeeMxx

      Subroutine LeePLT
     &          (FilePLT  ,NumDevs  ,PLTUnit  ,OutUnit  ,Root
     &          ,VTKUnit)

C###############################################################################
C
C     Lee los valores calculados y medidos de todos los devices del
C     archivo PLT y los escribe en archivos separados (uno por device)
C     par que se puedan representar fácilmente.
C
C###############################################################################

      Implicit None

C------------------------- External

      Integer*4::NumDevs  ,OutUnit  ,PLTUnit  ,VTKUnit

      Character::FilePLT*50    ,Root*20


C------------------------- Internal

      Integer*4::I        ,IError   ,NComp    ,NCompOld ,NDevs    ,NMeas
     &          ,NMeasOld ,NVals

      Character::DataType*5 ,DevName*10 ,FileName*40,strComp*27
     &          ,strDummy*30,strMeas*27

      Real*8,Allocatable::ValsComp(:,:),ValsMeas(:,:)

C-------------------------
C-------------------------

      Open (PLTUnit, File=FilePLT, Status='OLD',IOSTAT=IError)

      Write (OutUnit,12)
      Write (*,12)
   12 Format (/,'Leyendo el archivo PLT...')

      NCompOld = 0
      NMeasOld = 0
      NDevs = 0

      Do While (NDevs .LT. NumDevs)

C-------------------------Valores calculados.

          Read(PLTUnit,*)
          Read(PLTUnit,10) NComp
   10     Format(I5)

          Read(PLTUnit,20) strDummy,DevName
   20     Format(A30,A10)

          Read(PLTUnit,*)

          Read(PLTUnit,30) DataType
   30     Format(A5)

          If(NCompOld .LT. 1) Then

              Allocate(ValsComp(2,NComp))

          Else

              If(NComp .GT. NCompOld) Then

                  Deallocate(ValsComp)
                  Allocate(ValsComp(2,NComp))

              End If !NComp.NE.NCompOld

          End If !NCompOld.EQ.0

          Do I=1,NComp

              Read(PLTUnit,40) ValsComp(1,I),ValsComp(2,I)
   40         Format(2G13.6)
CPLT PARCHE
          End Do !I=1,NComp

C-------------------------Valores medidos.

          Read (PLTUnit,10) NMeas

          Read (PLTUnit,*)
          Read (PLTUnit,*)
          Read (PLTUnit,*)

          If(NMeasOld .LT. 1) Then

              Allocate(ValsMeas(2,NMeas))

          Else

              If(NMeas .GT. NMeasOld) Then

                  Deallocate(ValsMeas)
                  Allocate(ValsMeas(2,NMeas))

              End If !NMeas.NE.NMeasOld

          End If !NMeasOld.EQ.0


          Do I=1,NMeas

              Read(PLTUnit,40) ValsMeas(1,I),ValsMeas(2,I)

          End Do !I=1,NComp

C-------------------------Escribe el archivo con los valores calculados
C-------------------------y medidos.

C-------------------------El archivo se nombra según el tipo de medida.

          Filename = Trim(AdjustL(Root)) // "-" // DataType(2:2) // "-"
     &             // Trim(AdjustL(DevName)) // ".DAT"

          Open (VTKUnit, File=Filename, Status='UNKNOWN')

C-------------------------Cabecera

          Write(VTKUnit,50)
   50     Format(
     &          '#  Time Meas.       Measure   Time Comp.     Computed')

C-------------------------El número de datos medidos y calculados no
C-------------------------tiene por qué ser igual.
          NVals = Max(NComp,NMeas)

          Do I=1,NVals

              strMeas = ''
              strComp = ''

              If (I .LE. NMeas) Then

                  Write(strMeas,60) ValsMeas(1,I),ValsMeas(2,I)
   60             Format(G13.6,' ',G13.6)

              Else

                  Write(strMeas,70)
   70             Format('     ?             ?')

              End If !I .LE. NMeas

              If (I .LE. NComp) Then

                  Write(strComp,60) ValsComp(1,I),ValsComp(2,I)

              Else

                  Write(strMeas,70)

              End If !I .LE. NMeas

              Write(VTKUnit,80) strMeas,strComp
   80         Format(2A27)

          End Do !I=1,NVals

          Close(VTKUnit)
C------------------------- Se guarda los valores y actualiza contador.

          NCompOld = NComp
          NMeasOld = NMeas
          NDevs = NDevs + 1

      End Do !While NDevs .LT. NumDevs

      If (Allocated(ValsComp) .EQV. .TRUE.) Deallocate(ValsComp)
      If (Allocated(ValsMeas) .EQV. .TRUE.) Deallocate(ValsMeas)

      Close(PLTUnit)

      End Subroutine LeePLT

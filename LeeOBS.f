      Subroutine LeeOBS
     &          (FileOBS  ,IOWri1D  ,NumDevs  ,ObsUnit  ,OutUnit  ,Root)

C###############################################################################
C
C     Lee del archivo OBS las coordenadas de los puntos de observación.
C
C     Se aprovechan las subritinas de Transin4.
C
C
C     ObsType --> Tipo de observación.
C                 1 Nivel
C                 2 Concentración
C                 3 Water Content
C                 4 Mass flow
C
C###############################################################################

      Implicit None

C------------------------- External

      Integer*4::IOWri1D,NumDevs,ObsUnit,OutUnit

      Character::FileOBS*50,Root*20


C------------------------- Internal

      Integer*4::IDev,IDummy,NOOBS,NRow,Num

      Character*100::LEAUX,LEEL

      Integer*4,Allocatable::ObsType(:)
      Real*8,Allocatable::DevCoor(:,:)

      Character,Allocatable::DevName(:)*10

C------------------------- 
C------------------------- 

      Write (OutUnit,12)
      Write (*,12)
   12 Format (/,'Leyendo el archivo OBS...')

      Allocate(DevName(NumDevs))
      Allocate(ObsType(NumDevs))
      Allocate(DevCoor(NumDevs,3))

      NRow = 0

      Open (UNIT=OBSUnit, FILE=FileOBS, STATUS='OLD')


      DO IDev=1,NumDevs

C------------------------- Lee el nombre y las coordenadas

          LEAUX = LEEL(FileOBS,OBSUnit,OutUnit,NRow,0)
          Read(LEAUX,10) Num,DevName(IDev),ObsType(IDev)
   10     Format(I5,A10,I5)


          LEAUX = LEEL(FileOBS,OBSUnit,OutUnit,NRow,0)
          Read(LEAUX,20) IDummy,DevCoor(IDev,1:3)
   20     Format(I5,3F10.0)

          NOOBS = 0
C------------------------- Se salta las observaciones

          Do While (NOOBS.GE.0)

              LEAUX=LEEL(FileOBS,OBSUnit,OutUnit,NRow,0)
              Read(LEAUX,30) NOOBS
   30         Format(I5)

          End Do !While (NOOBS.GE.0)

      End Do !IDev=1,NumDevs

C------------------------- Escribe los puntos de observacion

      Call Write_Var_Int
     &    (DevCoor  ,1        ,1        ,-1       ,IOWri1D
     &    ,IDummy   ,1        ,IDummy   ,IDummy   ,NumDevs
     &    ,NumDevs  ,NumDevs  ,Root     ,ObsType  ,'OBS   '
     &    ,0        ,1        ,OBSUnit)


      DeAllocate(DevCoor)
      DeAllocate(ObsType)
      DeAllocate(DevName)

      End Subroutine LeeOBS

      PROGRAM TD2VTK

C###############################################################################
C
C     Programa que lee los archivos de entrada y salida de Trandens y los
C     convierte en el formato VTK para su visualización.
C
C     Autor: JHG
C     Fecha: Julio, 2007
C            Marzo, 2008 (v0.8)
C            Junio, 2008 (v0.91)
C            Junio, 2009 (v0.94)
C            Enero, 2010 (v0.95)
C            Marzo, 2010 (v0.96)
C            Abril, 2010 (v0.97)
C
C###############################################################################

      Implicit None

      Integer*4::IFormat,IOWri1D,IODIM  ,IOEQT  ,IOFLLI ,IOFLSAT,IOMCC
     &          ,IOMHH  ,IOINV  ,IORTS  ,IOTRLI ,IOTRS  ,ISOT   ,LMXNDL
     &          ,LenRoot,NBAND  ,NErrs  ,NFNL   ,NFNT   ,NINT   ,NPAR
     &          ,NPARF  ,NPBFL  ,NPBTP  ,NPBMX  ,NTDMT  ,NumDevs,NumGNL
     &          ,NumGNN ,NUMEL  ,NUMNP  ,NWarns ,NZPAR

      Integer*4::IGen

      Integer*4,Parameter::
     &                     InUnit   = 1
     &                    ,OutUnit  = 2
     &                    ,DIMUnit  = 3
     &                    ,GRIUnit  = 4
     &                    ,PARUnit  = 7
     &                    ,INIUnit  = 8
     &                    ,TIMUnit  = 9
     &                    ,MCCUnit  = 10
     &                    ,MHHUnit  = 11
     &                    ,MSVUnit  = 12
     &                    ,PSCUnit  = 14
     &                    ,PSHUnit  = 15
     &                    ,VTKUnit  = 16
     &                    ,MeshUnit = 17
     &                    ,CoefUnit = 18
     &                    ,OBSUnit  = 19
     &                    ,GenUnit  = 20
     &                    ,PLTUnit  = 21
     &                    ,NPAREL   = 11
     &                    ,NPARNP   = 10
     &                    ,NTYPAR   = 30


      Logical::IOExistBLH,IOExistBLC,IOExistDAT,IOExistDIM,IOExistGRI
     &        ,IOExistINI,IOExistMCC,IOExistMHH,IOExistMSV,IOExistOBS
     &        ,IOExistPAR,IOExistPLT,IOExistPSC,IOExistPSH,IOExistTIM

      Logical::IOWriMesh,IOWriGNL,IOWriGNN,IOWriGRI,IOWriINI,IOWriPAR
     &        ,IOWriMCC,IOWriMHH ,IOWriMSV,IOWriOBS,IOWriPLT,IOWriPSC
     &        ,IOWriPSH,IOWriTIM

      Character*50::FileBLC,FileDAT,FileBLH,FileDIM,FileGRI,FileINI
     &             ,FileMCC,FileMHH,FileMSV,FileOBS,FilePAR,FilePLT
     &             ,FilePSC,FIlePSH,FileTIM

      Character::Root*20

      Integer*4,Allocatable::IBCOD(:,:)   ,IBTCO(:,:)   ,INORPAR(:)
     &                      ,ISOLEQ(:,:)  ,ISOZ(:)      ,IXPARNP(:,:,:)
     &                      ,KXX(:,:)     ,LDIM(:)      ,LNNDEL(:)
     &                      ,LTYPE(:)     ,LXPAREL(:,:,:)

      Real*8,Allocatable::CBASE(:)      ,CFPAREL(:,:)  ,CFPARNP(:,:)
     &                   ,CINI(:,:)     ,Conc(:)       ,COORD(:,:)
     &                   ,HBASE(:)      ,Head(:)       ,HINI(:,:)
     &                   ,PARC(:)       ,PARM(:)       ,PARZ(:)
     &                   ,THICKL(:)     ,THICKN(:)     ,TIMES(:)
     &                   ,Vels(:,:)   ,VelTimes(:)

      Character,Allocatable::FileGNL(:)*50,FileGNN(:)*50,NameGNL(:)*3
     &                      ,NameGNN(:)*3

      Integer*4::NZONES(20)

C------------------------- Primera orden ejecutable

      Write(*,1)
    1 Format(/,'TD2VTK v0.981 - Copyright JHG (2007 - 2015)'
     &      ,/,'------------------------------------------'
     &      ,/,'Use el archivo TD2VTK.DAT para elegir el tipo de salida'
     &      ,/)


C------------------------- Comprueba si hay archivo de entrada

      NErrs = 0
      NWarns = 0
      NumGNL = 0
      NumGNN = 0

      Open (OutUnit, File="TD2VTK.OUT", Status='UNKNOWN')
      Write(OutUnit,1)

      FileDAT = 'TD2VTK.DAT'
      Inquire (FILE=FileDat, EXIST=IOExistDAT)


C------------------------- Entrada de datos.

      If (IOExistDAT) Then

C------------------------- Opciones desde archivo de entrada

        Open (InUnit, File="TD2VTK.DAT", Status='UNKNOWN')
        Read(InUnit,10) Root
        Read(InUnit,20) IOWriMesh,IOWriGRI,IOWriINI,IOWriPAR,IOWriMCC
     &                 ,IOWriMHH ,IOWriMSV,IOWriTIM,IOWriPSH,IOWriPSC
     &                 ,IOWriOBS ,IOWriGNN,IOWriGNL,IOWriPLT

        Read(InUnit,50) IFormat,IOWri1D
        Read(InUnit,*)
        
        If (IOWriGNN) Then

            Read(InUnit,30) NumGNN

            If (NumGNN.GT.0) Then

                Allocate(NameGNN(NumGNN))
                NameGNN(:) =''
                Allocate(FileGNN(NumGNN))
                FileGNN(:) =''

                Do IGen=1,NumGNN

                    Read(InUnit,40) NameGNN(IGen)

                End Do !IGen=1,NumGNN

            End If !NumGNN.GT.0

          End If !IOWriGNN

          If (IOWriGNL) Then

            Read(InUnit,30) NumGNL

                If (NumGNL.GT.0) Then

                    Allocate(NameGNL(NumGNL))
                    NameGNL(:) =''
                    Allocate(FileGNL(NumGNL))
                    FileGNL(:) =''

                Do IGen=1,NumGNL

                    Read(InUnit,40) NameGNL(IGen)

                End Do !IGen=1,NumGNL

            End If !NumGNN.GT.0

        End If !IOWriGNL

   20   Format(14L5)
   30   Format(I5)
   40   Format(A3)
   50   Format(2I5)       

      Else

C------------------------- Entrada manual.
C------------------------- Se activan todas las salidas.

        Write(*,2,ADVANCE='NO')
    2   Format(/,'Dame la raiz de los archivos: ')
        Read(*,10) Root
   10   Format (A20)

        Write(*,3,ADVANCE='NO')
    3   Format(/,'Dame el formato de lectura (1:I5; 2:I6)): ')
        Read(*,30) IFormat

        Write(*,4,ADVANCE='NO')
    4   Format(/,'Escribir elementos 1D (0:No; 1:Si)): ')
        Read(*,30) IOWri1D


        IOWriMesh = .TRUE.
        IOWriGRI  = .TRUE.
        IOWriINI  = .TRUE.
        IOWriPAR  = .TRUE.
        IOWriMCC  = .TRUE.
        IOWriMHH  = .TRUE.
        IOWriMSV  = .TRUE.
        IOWriTIM  = .TRUE.
        IOWriPSC  = .TRUE.
        IOWriPSH  = .TRUE.
        IOWriGNN  = .FALSE.
        IOWriGNL  = .FALSE.
        IOWriPLT  = .FALSE.

      End If !IOExistDAT


      Write(*,5)
    5 Format(/)

      LenRoot = Index(Root,' ')
      FileBLC = Root(1:LenRoot-1)//'BLC.OUT'
      FileBLH = Root(1:LenRoot-1)//'BLH.OUT'
      FileDIM = Root(1:LenRoot-1)//'DIM.DAT'
      FileGRI = Root(1:LenRoot-1)//'GRI.DAT'
      FileTIM = Root(1:LenRoot-1)//'TIM.DAT'
      FileINI = Root(1:LenRoot-1)//'INI.DAT'
      FilePAR = Root(1:LenRoot-1)//'PAR.DAT'
      FileMCC = Root(1:LenRoot-1)//'MCC.OUT'
      FileMHH = Root(1:LenRoot-1)//'MHH.OUT'
      FileOBS = Root(1:LenRoot-1)//'OBS.DAT'
      FilePLT = Root(1:LenRoot-1)//'PLT.OUT'
      FilePSC = Root(1:LenRoot-1)//'PSC.OUT'
      FilePSH = Root(1:LenRoot-1)//'PSH.OUT'
      FileMSV = Root(1:LenRoot-1)//'VMSHV.INP'

      If (NumGNN.GT.0) Then

        FileGNN(:) = Root(1:LenRoot-1)//NameGNN(:)//'.OUT'

      End If !NumGNN.GT.0

      If (NumGNL.GT.0) Then

        FileGNL(:) = Root(1:LenRoot-1)//NameGNL(:)//'.OUT'

      End If !NumGNL.GT.0


      Write(OutUnit,11) Root
   11 Format('Raiz: ',A20)

      Write(OutUnit,21) IOWriMesh,IOWriGRI,IOWriINI,IOWriPAR,IOWriMCC
     &                 ,IOWriMHH ,IOWriMSV,IOWriTIM,IOWriPSH,IOWriPSC
     &                 ,IOWriGNN ,IOWriGNL,IOWriPLT
   21 Format(/,'Opciones de salida:'
     &      ,/,'-------------------'
     &      ,/,'Escribir malla: ',L7
     &      ,/,'Escribir GRI: ',L7
     &      ,/,'Escribir INI: ',L7
     &      ,/,'Escribir PAR: ',L7
     &      ,/,'Escribir MCC: ',L7
     &      ,/,'Escribir MHH: ',L7
     &      ,/,'Escribir MSV: ',L7
     &      ,/,'Escribir TIM: ',L7
     &      ,/,'Escribir PSH: ',L7
     &      ,/,'Escribir PSC: ',L7
     &      ,/,'Escribir GNN: ',L7
     &      ,/,'Escribir GNL: ',L7
     &      ,/,'Escribir PLT: ',L7)

      Write(OutUnit,31) FileDIM ,FileGRI ,FilePAR ,FileTIM ,FileINI
     &                 ,FileBLC ,FileBLH ,FileMCC ,FileMHH ,FileMSV
     &                 ,FilePSC ,FilePSH ,FileOBS ,FilePLT
   31 Format(/,'DIM File: ',A50,/,'GRI File: ',A50,/,'PAR File: ',A50,/
     &        ,'TIM File: ',A50,/,'INI File: ',A50,/,'BLC File: ',A50,/
     &        ,'BLH File: ',A50,/,'MCC File: ',A50,/,'MHH File: ',A50,/
     &        ,'MSV File: ',A50,/,'PSC File: ',A50,/,'PSH File: ',A50,/
     &        ,'OBS File: ',A50,/,'PLT File: ',A50)


      If (NumGNN.GT.0) Then

        Write (OutUnit,311)

        Do IGen=1,NumGNN

            Write (OutUnit,313) FileGNN(IGen)

        End Do !IGen = 1,NumGNN

      End If !NumGNN.GT.0


      If (NumGNL.GT.0) Then

        Write (OutUnit,312)

        Do IGen=1,NumGNL

            Write (OutUnit,313) FileGNL(IGen)

        End Do !IGen = 1,NumGNL

      End If !NumGNN.GT.0

  311 Format('GNN Files: ')
  312 Format('GNL Files: ')
  313 Format(A50)

      Write(OutUnit,32) IFormat,IOWri1D
   32 Format(/,'Formato de lectura (1:I5; 2:I6): ',I5,
     &       /,'Escribir elementos 1D (0:No; 1:Si): ',I5)

      If (IFormat.NE.1 .AND. IFormat.NE.2) Then

              IFormat = 1
              Write(OutUnit,33)
   33            Format(/,'AVISO: Formato de lectura incorrecto.'
     &              ,/,'Se pone a I5.')

            NWarns = NWarns + 1

      End If !IFormat.NE.1 .OR. IFormat.NE.2

      If (IOWri1D.GT.0) Then
          IOWri1D = 1
      Else
          IOWri1D = 0
      End If

      Inquire (FILE=FileBLH, EXIST=IOExistBLH)
      Inquire (FILE=FileBLC, EXIST=IOExistBLC)
      Inquire (FILE=FileDIM, EXIST=IOExistDIM)
      Inquire (FILE=FileGRI, EXIST=IOExistGRI)
      Inquire (FILE=FileINI, EXIST=IOExistINI)
      Inquire (FILE=FileMCC, EXIST=IOExistMCC)
      Inquire (FILE=FileMHH, EXIST=IOExistMHH)
      Inquire (FILE=FileMSV, EXIST=IOExistMSV)
      Inquire (FILE=FilePAR, EXIST=IOExistPAR)
      Inquire (FILE=FileTIM, EXIST=IOExistTIM)
      Inquire (FILE=FilePSC, EXIST=IOExistPSC)
      Inquire (FILE=FilePSC, EXIST=IOExistPSH)
      Inquire (FILE=FileOBS, EXIST=IOExistOBS)
      Inquire (FILE=FilePLT, EXIST=IOExistPLT)


      If (IOExistDIM .AND. IOExistGRI .AND. IOExistTIM) Then

          NZONES(:)=0
          Allocate(INORPAR(NTYPAR))
          INORPAR = 0

C------------------------- Lee DIM

          Call LeeDIM
     &        (DIMUnit  ,FileDIM  ,IFormat  ,INORPAR  ,IODIM
     &        ,IOEQT    ,IOFLLI   ,IOFLSAT  ,IOINV    ,IOMCC
     &        ,IOMHH    ,IORTS    ,IOTRLI   ,IOTRS    ,ISOT
     &        ,LMXNDL   ,NBAND    ,NFNL     ,NFNT     ,NINT
     &        ,NPAR     ,NPARF    ,NPBFL    ,NPBTP    ,NTDMT
     &        ,NTYPAR   ,NumDevs  ,NUMEL    ,NUMNP    ,NZPAR
     &        ,NZONES   ,OutUnit)


          NPBMX = MAX(NPBFL,NPBTP)

          IF (NINT .EQ. 0) THEN !For Steady state problems
              NINT = 1
          END IF
C------------------------- Se reserva memoria para los vectores que se leen del GRI

         Allocate(CBASE(NUMNP)
     &           ,COORD(NUMNP,3)
     &           ,HBASE(NUMNP)
     &           ,IBCOD(NUMNP,NPBMX)
     &           ,IBTCO(NUMNP,NPBMX)
     &           ,IXPARNP(NUMNP,NPARNP,NPBMX)
     &           ,KXX(LMXNDL,NUMEL)
     &           ,LDIM(NUMEL)
     &           ,LNNDEL(NUMEL)
     &           ,LTYPE(NUMEL)
     &           ,LXPAREL(NUMEL,NPAREL,NPBMX)
     &           ,THICKL(NUMEL)
     &           ,THICKN(NUMNP))

          CBASE = 0D0
          COORD = 0D0
          HBASE = 0D0
          IBCOD = 0
          IBTCO = 0
          IXPARNP = 0
          KXX = 0
          LDIM = 0
          LNNDEL = 0
          LTYPE = 0
          LXPAREL = 0
          THICKL = 0D0
          THICKN = 0D0
C------------------------- Lee GRI

          Call LeeGRI
     &        (CBASE    ,COORD    ,FileGRI  ,GRIUnit  ,HBASE
     &        ,IBCOD    ,IBTCO    ,IFormat  ,IOEQT    ,IOFLLI
     &        ,IOFLSAT  ,IORTS    ,IOTRLI   ,IOTRS    ,IOWri1D
     &        ,IOWriGRI ,IOWriMesh,IXPARNP  ,KXX      ,LDIM
     &        ,LMXNDL   ,LNNDEL   ,LTYPE    ,LXPAREL  ,MeshUnit
     &        ,NBAND    ,NPAREL   ,NPARNP   ,NPBFL   ,NPBMX
     &        ,NPBTP    ,NTDMT    ,NUMEL    ,NUMNP    ,NZONES
     &        ,THICKL   ,THICKN   ,OutUnit  ,Root)

C------------------------- Se reserva memoria para los vectores que se leen del TIM

          Allocate(ISOLEQ(NINT,4)
     &            ,Times(NINT))

          ISOLEQ = 0
          Times = 0d0

C------------------------- Lee TIM.

          Call LeeTIM
     &        (FileTIM  ,IOEQT    ,IOFLLI   ,IORTS    ,IOTRLI
     &        ,IOTRS    ,IOWriTIM ,ISOLEQ   ,NFNT     ,NINT
     &        ,OutUnit  ,Root     ,Times    ,TIMUnit)


          If (IOExistPAR .AND. IOWriPAR) Then

C------------------------- Se reserva memoria para los vectores que se leen del PAR

              Allocate(ISOZ(NZONES(1))
     &                ,CFPAREL(NUMEL,NPAREL)
     &                ,CFPARNP(NUMNP,NPARNP)
     &                ,PARC(NPAR)
     &                ,PARM(NPAR)
     &                ,PARZ(NZPAR))

              ISOZ = 0
              CFPAREL = 0D0
              CFPARNP = 0D0
              PARC = 0D0
              PARM = 0D0
              PARZ = 0D0

C------------------------- Lee PAR

              Call LeePAR
     &          (CFPAREL  ,CFPARNP  ,CoefUnit ,COORD    ,FilePAR
     &          ,IBCOD    ,IBTCO    ,IFormat  ,INORPAR  ,IODIM
     &          ,IOINV    ,IOEQT    ,IOFLLI   ,IOFLSAT  ,IORTS
     &          ,IOTRLI   ,IOTRS    ,IOWri1D  ,ISOT     ,ISOZ
     &          ,KXX      ,LDIM     ,LMXNDL   ,LNNDEL   ,LTYPE
     &          ,NFNL     ,NPAR     ,NPARF    ,NPAREL   ,NPARNP
     &          ,NTYPAR   ,NUMEL    ,NUMNP    ,NZPAR    ,NZONES
     &          ,PARUnit  ,PARC     ,PARM     ,PARZ     ,OutUnit
     &          ,Root)


          Else

              If (.NOT. IOExistPAR) Then

                  Write (OutUnit,92)
                  Write (*,92)
   92             Format (/,'AVISO: El archivo PAR no existe.',
     &                    /,'No se escriben los coeficientes del '
     &                     ,'modelo.')

                  NWarns = NWarns + 1

            Else If (.NOT. IOWriPAR) Then

                Write (OutUnit,82)
                  Write (*,82)
   82             Format (/,'Ignoro el archivo PAR.'
     &                    /,'No se escriben los coeficientes del '
     &                     ,'modelo.')


            End If !.NOT. IOExistPAR

          End If !IOExistPAR .AND. IOWriPAR

          If (IOExistINI .AND. (IOTRS.EQ.1 .OR. IORTS.EQ.1)
     &       .AND. IOWriINI) Then

C------------------------- Lee INI

             Allocate(CINI(NUMNP,NPBTP)
     &               ,HINI(NUMNP,NPBFL))

            CINI = 0D0
            HINI = 0D0

              Call LeeINI
     &           (CINI     ,COORD    ,FileINI  ,HINI     ,Iformat
     &           ,INIUnit  ,IORTS    ,IOTRS    ,IOWri1D  ,KXX
     &           ,LMXNDL   ,LNNDEL   ,LTYPE    ,NPBFL    ,NPBTP
     &           ,NUMNP    ,NUMEL    ,OutUnit  ,Root     ,VTKUnit)
          Else

            If (.NOT. IOExistINI) Then

                  Write (OutUnit,93)
                  Write (*,93)
   93             Format (/,'AVISO: El archivo INI no existe.',
     &                    /,'No se escriben las condiciones iniciales '
     &                     ,'del modelo.')

                  NWarns = NWarns + 1

            Else If (.NOT. IOWriINI) Then

                Write (OutUnit,83)
                Write (*,83)
   83           Format (/,'Ignoro el archivo INI.'
     &                  /,'No se escriben las condiciones iniciales '
     &                   ,'del modelo.')

            End If !.NOT. IOExistINI

          End If !IOExistINI .AND. IOWritINI


          If (IOExistMHH .AND. IOWriMHH) Then

C------------------------- Lee MHH

              Allocate(Head(NUMNP))
              Head = 0D0

              Call LeeMxx
     &            (COORD    ,FileMHH  ,IFormat  ,IOMHH    ,IOWri1D
     &            ,IOTRS    ,ISOLEQ   ,1        ,KXX      ,LMXNDL
     &            ,LNNDEL   ,LTYPE    ,NINT     ,MHHUnit  ,NUMEL
     &            ,NUMNP    ,NPBFL    ,NUMNP    ,OutUnit  ,Root
     &            ,Times    ,Head     ,'HH    ' ,1        ,VTKUnit)

          Else

              If (.NOT. IOExistMHH) Then

                  Write (OutUnit,95)
                  Write (*,95)
   95             Format (/,'AVISO: El archivo MHH file no existe.',
     &                    /,'No se escriben los resultado de flujo.')

                  NWarns = NWarns + 1

              Else If (.NOT. IOWriMHH) Then

                  Write (OutUnit,85)
                  Write (*,85)
   85             Format (/,'Ignoro el archivo MHH.'
     &                    /,'No se escriben los resultados de flujo.')

            End If !.NOT. IOExistMHH

          End If !IOExistMHH .AND. IOWriMHH


          If (IOExistMCC .AND. IOWriMCC) Then

C------------------------- Lee MCC

              Allocate(Conc(NUMNP))
              Conc = 0D0

              Call LeeMxx
     &            (COORD    ,FileMCC  ,IFormat  ,IOMCC    ,IOWri1D
     &            ,IORTS    ,ISOLEQ   ,2        ,KXX      ,LMXNDL
     &            ,LNNDEL   ,LTYPE    ,NINT     ,MCCUnit  ,NUMEL
     &            ,NUMNP    ,NPBTP    ,NUMNP    ,OutUnit  ,Root
     &            ,Times    ,Conc     ,'CC    ' ,1        ,VTKUnit)

          Else

              If (.NOT. IOExistMCC) Then

                  Write (OutUnit,96)
                  Write (*,96)
   96             Format (/,'AVISO: El archivo MCC no existe.',
     &                    /,'No se escriben los resultados de '
     &                     ,'transporte.')

                  NWarns = NWarns + 1

            Else If (.NOT. IOWriMCC) Then

                  Write (OutUnit,86)
                  Write (*,86)
   86             Format (/,'Ignoro el archivo MCC.'
     &                    /,'No se escriben los resultados de '
     &                     ,'transporte.')

            End If !.NOT. IOExistMCC

          End If !IOExistMCC .AND. IOWriMCC


          If (IOExistMSV .AND. IOWriMSV) Then

C------------------------- Lee MSV

              Allocate(Vels(3,NUMEL))
              Vels = 0D0

              Call LeeMSV
     &            (COORD    ,FileMSV  ,IFormat  ,IOWri1D  ,KXX
     &            ,LMXNDL   ,LNNDEL   ,LTYPE    ,MSVUnit  ,NUMEL
     &            ,NUMNP    ,OutUnit  ,Root     ,Vels     ,VTKUnit)


          Else

            If (.NOT. IOExistMSV) Then

                  Write (OutUnit,97)
                  Write (*,97)
   97             Format (/,'AVISO: El archivo MSV no existe.'
     &                   ,/,'No se escriben las velocidades.')

                  NWarns = NWarns + 1

            Else If (.NOT. IOWriMSV) Then

                  Write (OutUnit,87)
                  Write (*,87)
   87             Format (/,'Ignoro el archivo MSV'
     &                   ,/,'No se escriben las velocidades.')

            End If !.NOT. IOExistMSV

          End If !IOExistMSV .AND. IOExistMSV


          If (IOExistPSH .AND. IOWriPSH) Then

C------------------------- Lee PSH

              Call LeePSx
     &            (COORD    ,FilePSH  ,IOWri1D  ,1        ,KXX
     &            ,LMXNDL   ,LNNDEL   ,LTYPE    ,NINT     ,PSHUnit
     &            ,NPAR     ,NUMEL    ,NUMNP    ,OutUnit  ,Root
     &            ,Times    ,1        ,VTKUnit)

          Else

              If (.NOT. IOExistPSH) Then

                  Write (OutUnit,98)
                  Write (*,98)
   98             Format (/,'AVISO: El archivo PSH no existe.',
     &                    /,'No se escriben las sensibilidades del '
     &                     ,'nivel.')

                  NWarns = NWarns + 1

              Else If (.NOT. IOWriPSH) Then

                  Write (OutUnit,88)
                  Write (*,88)
   88             Format (/,'Ignoro el archivo PSH.'
     &                    /,'No se escriben las sensibilidades del '
     &                     ,'nivel.')


            End If !.NOT. IOExistPSH

          End If !IOExistPSH .AND. IOWriPSH


          If (IOExistPSC .AND. IOWriPSC) Then

C------------------------- Lee PSC

             Call LeePSx
     &            (COORD    ,FilePSC  ,IOWri1D  ,2        ,KXX
     &            ,LMXNDL   ,LNNDEL   ,LTYPE    ,NINT     ,PSHUnit
     &            ,NPAR     ,NUMEL    ,NUMNP    ,OutUnit  ,Root
     &            ,Times    ,1        ,VTKUnit)

          Else

              If (.NOT. IOExistPSC) Then

                  Write (OutUnit,99)
                  Write (*,99)
   99             Format (/,'AVISO: El archivo PSC no existe.',
     &                    /,'No se escriben las sensibilidades de la '
     &                     ,'concentracion.')

                  NWarns = NWarns + 1

              Else If (.NOT. IOWriPSH) Then

                  Write (OutUnit,89)
                  Write (*,89)
   89             Format (/,'Ignoro el archivo PSC.'
     &                    /,'No se escriben las sensibilidades de la '
     &                     ,'concentracion.')


            End If !.NOT. IOExistPSC

          End If !IOExistPSC .AND. IOWriPSC


          If (IOExistOBS .AND. IOWriOBS) Then

C------------------------- Lee OBS

                 Call LeeOBS
     &            (FileOBS  ,IOWri1D  ,NumDevs  ,ObsUnit  ,OutUnit
     &            ,Root)

          Else

              If (.NOT. IOExistOBS) Then

                  Write (OutUnit,119)
                  Write (*,119)
  119             Format (/,'AVISO: El archivo OBS no existe.',
     &                    /,'No se escriben los puntos de observacion.')

                  NWarns = NWarns + 1

              Else If (.NOT. IOWriOBS) Then

                  Write (OutUnit,109)
                  Write (*,109)
  109             Format (/,'Ignoro el archivo OBS.'
     &                    /,'No se escriben los puntos de observacion.')


            End If !.NOT. IOExistOBS

          End If !IOExistOBS .AND. IOWriOBS

          If (IOWriGNN) Then

C------------------------- Lee GNN

                 Call LeeGEN
     &            (COORD    ,FileGNN  ,IFormat  ,IOWri1D  ,IORTS
     &            ,ISOLEQ   ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE
     &            ,NINT     ,GenUnit  ,NameGNN  ,NUMEL    ,NumGNN
     &            ,NUMNP    ,NUMNP    ,NWarns   ,OutUnit  ,Root
     &            ,Times    ,1        ,VTKUnit)

          Else

                  Write (OutUnit,209)
                  Write (*,209)
  209             Format (/,'Ignoro los archivos GNN.'
     &                 /,'No se escriben los datos genericos por nudo.')

          End If !IOWriGNN


          If (IOWriGNL) Then

C------------------------- Lee GNL

                 Call LeeGEN
     &            (COORD    ,FileGNL  ,IFormat  ,IOWri1D  ,1
     &            ,ISOLEQ   ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE
     &            ,NINT     ,GenUnit  ,NameGNL  ,NUMEL    ,NumGNL
     &            ,NUMNP    ,NUMEL    ,NWarns   ,OutUnit  ,Root
     &            ,Times    ,2        ,VTKUnit)


          Else

                  Write (OutUnit,309)
                  Write (*,309)
  309             Format (/,'Ignoro los archivos GNL.'
     &             /,'No se escriben los datos genericos por elemento.')

          End If !IOWriGNL

          If (IOExistPLT .AND. IOWriPLT) Then

C------------------------- Lee GNN

                 Call LeePLT
     &               (FilePLT  ,NumDevs  ,PLTUnit  ,OutUnit  ,Root
     &               ,VTKUnit)

          Else

                  Write (OutUnit,409)
                  Write (*,409)
  409             Format (/,'Ignoro el archivo PLT.'
     &                 /,'No se escriben los valores calculados y '
     &                  ,'medidos en los puntos de observación.')

          End If !IOWriPLT

      Else


          If (.NOT.IOExistDIM) Then

              Write (OutUnit,901)
              Write (*,901)
  901         Format (/,'ERROR: El archivo DIM no existe.')

              NErrs = NErrs + 1

          End If !IOExistDIM

          If (.NOT.IOExistGRI) Then

              Write (OutUnit,902)
              Write (*,902)
  902         Format (/,'ERROR: El archivo GRI no existe.')

              NErrs = NErrs + 1

          End If !IOExistGRI

          If (.NOT. IOExistTIM) Then

              Write (OutUnit,903)
              Write (*,903)
  903         Format (/,'AVISO: El archivo TIM no existe.'
     &               /)

                  NErrs = NErrs + 1

          End If !IOExistTIM
      End If !IOExistDIM .AND. IOExistGRI


      Write (OutUnit,998) NWarns,NErrs
      Write (*,999) NWarns,NErrs

  998 Format (/,'Numero total de AVISOS: ',I5
     &       ,/,'Numero total de ERRORES: ',I5
     &       ,/,/,'Fin de la ejecucion.'
     &       ,/,'--------------------',/)

  999 Format (/,'Numero total de AVISOS: ',I5
     &       ,/,'Numero total de ERRORES: ',I5
     &       ,/,/,'Consulte los detalles en el archivo TD2VTK.OUT.'
     &       ,/,/,'Fin de la ejecucion.'
     &       ,/,'--------------------',/)


      If (Allocated(IBCOD))    Deallocate(IBCOD)
      If (Allocated(IBTCO))    Deallocate(IBTCO)
      If (Allocated(INORPAR))  Deallocate(INORPAR)
      If (Allocated(ISOLEQ))   Deallocate(ISOLEQ)
      If (Allocated(ISOZ))     Deallocate(ISOZ)
      If (Allocated(IXPARNP))  Deallocate(IXPARNP)
      If (Allocated(KXX))      Deallocate(KXX)
      If (Allocated(LDIM))     Deallocate(LDIM)
      If (Allocated(LNNDEL))   Deallocate(LNNDEL)
      If (Allocated(LTYPE))    Deallocate(LTYPE)
      If (Allocated(LXPAREL))  Deallocate(LXPAREL)
      If (Allocated(CBASE))    Deallocate(CBASE)
      If (Allocated(CFPAREL))  Deallocate(CFPAREL)
      If (Allocated(CFPARNP))  Deallocate(CFPARNP)
      If (Allocated(CINI))     Deallocate(CINI)
      If (Allocated(Conc))     Deallocate(Conc)
      If (Allocated(COORD))    Deallocate(COORD)
      If (Allocated(HBASE))    Deallocate(HBASE)
      If (Allocated(Head))     Deallocate(Head)
      If (Allocated(HINI))     Deallocate(HINI)
      If (Allocated(PARC))     Deallocate(PARC)
      If (Allocated(PARM))     Deallocate(PARM)
      If (Allocated(PARZ))     Deallocate(PARZ)
      If (Allocated(THICKL))   Deallocate(THICKL)
      If (Allocated(THICKN))   Deallocate(THICKN)
      If (Allocated(TIMES))    Deallocate(TIMES)
      If (Allocated(Vels))     Deallocate(Vels)
      If (Allocated(VelTimes)) Deallocate(VelTimes)
      If (Allocated(NameGNL)) Deallocate(NameGNL)
      If (Allocated(NameGNN)) Deallocate(NameGNN)
      If (Allocated(FileGNL)) Deallocate(FileGNL)
      If (Allocated(FileGNN)) Deallocate(FileGNN)


      Close(OutUnit)

      End Program TD2VTK

      Subroutine LeeGRI
     &          (CBASE    ,COORD    ,FileGRI  ,GRIUnit  ,HBASE
     &          ,IBCOD    ,IBTCO    ,IFmt     ,IOEQT    ,IOFLLI
     &          ,IOFLSAT  ,IORTS    ,IOTRLI   ,IOTRS    ,IOWri1D
     &          ,IOWriGRI ,IOWriMesh,IXPARNP  ,KXX      ,LDIM
     &          ,LMXNDL   ,LNNDEL   ,LTYPE    ,LXPAREL  ,MeshUnit
     &          ,NBAND    ,NPAREL   ,NPARNP   ,NPBFL    ,NPBMX
     &          ,NPBTP    ,NTDMT    ,NUMEL    ,NUMNP    ,NZONES
     &          ,THICKL   ,THICKN   ,OutUnit  ,Root)

C###############################################################################
C
C     Lee del archivo GRI las coordenadas de los nudos, condiciones de
C     contorno, conectividades y zonas por elemento.
C
C     Se aprovechan las subritinas de Transin4.
C
C     Localización en NZONES
C
C        1  Transmissivity
C        2  Storage
C        3  Recharge
C        4  Prescribed head
C        5  Prescribed flow
C        6  Leakage
C        7  Long. dispersivity
C        8  Transv. dispersivity
C        9  Difussion
C       10  Porosity
C       11  First order decay
C       12  Retardation
C       13  External concentration
C       14  Generic param.
C       15  Age coefficient
C       16  Matrix diffusion
C
C       Localización en LXPAREL
C
C        1  Transmissivity             INTRA
C        2  Storage                    INSTG
C        3  Recharge (steady state)    INARR
C        4  Recharge (transient)       INARRT
C        5  Dispersivity               INDSP
C        6  Diffusion                  INDFM
C        7  Porosity                   INPOR
C        8  First order decay          INFOD
C        9  Retardation                INCRD
C       10  External concentration     INCOE
C       11  Age coefficient
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################


      Implicit None

C------------------------- External

      Integer*4::IFmt   ,IOEQT  ,IOFLLI ,IOFLSAT,IORTS  ,IOTRLI
     &          ,IOTRS  ,LMXNDL ,NBAND  ,NPAREL ,NPARNP ,NPBFL
     &          ,NPBMX, NPBTP   ,NTDMT  ,NUMEL  ,NUMNP
      Integer*4::GRIUnit,IOWri1D,MeshUnit,OutUnit

      Logical::IOWriGRI,IOWriMesh

      Character::FileGRI*50,Root*20

      Integer*4::IBCOD(NUMNP,NPBMX)         ,IBTCO(NUMNP,NPBMX)
     &          ,IXPARNP(NUMNP,NPARNP,NPBMX),KXX(LMXNDL,NUMEL)
     &          ,LDIM(NUMEL)                ,LNNDEL(NUMEL)
     &          ,LTYPE(NUMEL)               ,LXPAREL(NUMEL,NPAREL,NPBMX)
     &          ,NZONES(20)


      Real*8::CBASE(NUMNP),COORD(NUMNP,3),HBASE(NUMNP)
     &       ,THICKL(NUMEL),THICKN(NUMNP)

C------------------------- Internal

      Integer*4::IDALF  ,IDALFT ,IDCHP  ,IDCHPT ,IDCLK  ,IDCON  ,IDCONT
     &          ,IDDMT  ,IDQQP  ,IDQQPT ,IERROR ,IPBFL  ,IPBTP  ,IPROB
     &          ,IOEQTOR,L      ,LDARR  ,LDARRT ,LDCOE  ,LDCRD  ,LDDFM
     &          ,LDDSP  ,LDFOD  ,LDPOR  ,LDSTG  ,LDTRA  ,NROW


      Integer*4,Parameter:: ! Location in array IXPARNP and LXPAREL
     &                     INCHP  = 1   ,INTRA  = 1
     &                    ,INCHPT = 2   ,INSTG  = 2
     &                    ,INQQP  = 3   ,INARR  = 3
     &                    ,INQQPT = 4   ,INARRT = 4
     &                    ,INALF  = 5   ,INDSP  = 5
     &                    ,INALFT = 6   ,INDFM  = 6
     &                    ,INCON  = 7   ,INPOR  = 7
     &                    ,INCONT = 8   ,INFOD  = 8
     &                    ,INDMT  = 9   ,INCRD  = 9
     &                    ,INCLK  = 10  ,INCOE  = 10

      Character::Filename*20,RootPb*20,Title*80

C-------------------------
C-------------------------

      Open (GRIUnit, File=FileGRI, Status='OLD',IOSTAT=IError)

      Write (OutUnit,12)
      Write (*,12)
   12 Format (/,'Procesando el archivo GRI...')

      NROW = 0

C------------------------- Guarda  IOEQT (se modifica temporalmente
C------------------------- durante la lectura de los datos de cada
C------------------------- problema).

      IOEQTOR = IOEQT


C------------------------- Lee coordenadas

      CALL ENTDATIX_COOR
     &    (IERROR   ,IDALF    ,IDALFT   ,IDCHP   ,IDCHPT   ,IDCON
     &    ,IDCONT   ,IDDMT    ,1        ,IDQQP   ,IDQQPT   ,IFmt
     &    ,IOEQT    ,IOFLLI   ,IOTRLI   ,1        ,GRIUnit ,OutUnit
     &    ,NROW     ,NTDMT    ,NUMNP    ,THICKN  ,CBASE    ,FileGRI
     &    ,HBASE    ,COORD(1,1),COORD(1,2),COORD(1,3), IDCLK)


C------------------------- Lee condiciones de contorno

      DO IPROB=1,NPBMX

C------------------------- Next assignations are to avoid reading of flow or
C------------------------- transport data when the number of problems of this
C------------------------- type is surpassed

          IF (IPROB .GT. NPBFL) THEN

             IOEQT = 2
             IPBFL = 1

          ELSE

             IPBFL = IPROB

          END IF !IPROB .GT. NPBFL

          IF (IPROB .GT. NPBTP) THEN

             IOEQT = 1
             IPBTP = 1

          ELSE

             IPBTP = IPROB

          END IF !IPROB .GT. NPBTP

          CALL ENTDATIX_ZON
     &        (IDALF    ,IDALFT   ,IDCHP    ,IDCHPT   ,IDCON    ,IDCONT
     &        ,IDDMT    ,IDQQP    ,IDQQPT   ,IERROR   ,IFmt     ,INALF
     &        ,INALFT   ,INCHP    ,INCHPT   ,INCON    ,INCONT   ,INDMT
     &        ,1        ,INQQP    ,INQQPT   ,IOEQT    ,IORTS    ,IOTRS
     &        ,1        ,IPROB    ,GRIUnit  ,OutUnit  ,NPARNP   ,NROW
     &        ,NTDMT    ,NUMNP    ,NZONES(6),NZONES(4),NZONES(13)
     &        ,NZONES(16)         ,NZONES(5),FileGRI  ,IBCOD(1,IPBFL)
     &        ,IBTCO(1,IPBTP)     ,IXPARNP(1,1,IPROB) ,IDCLK
     &        ,NZONES(18)         ,INCLK)

      END DO !IPROB=1,NPBMX


C------------------------- Reads elements number KXX,LX

      CALL ENTDATLX_ELEM
     &    (IERROR   ,IFmt     ,1        ,IOEQT    ,1        ,GRIUnit
     &    ,LDARR    ,LDARRT   ,LDCOE    ,LDCRD    ,LDDFM    ,LDDSP
     &    ,LDFOD    ,LDPOR    ,LDSTG    ,LDTRA    ,LMXNDL   ,OutUnit
     &    ,NBAND    ,NROW     ,NUMEL    ,NUMNP    ,THICKL   ,FileGRI
     &    ,KXX      ,LNNDEL   ,LTYPE    ,COORD(1,1)
     &    ,COORD(1,2))


      DO IPROB=1,NPBMX
C------------------------- Next assignations are to avoid reading of flow or
C------------------------- transport data when the number of problems of this
C------------------------- type is surpassed

          IF (IPROB .GT. NPBFL) IOEQT = 2
          IF (IPROB .GT. NPBTP) IOEQT = 1


          CALL ENTDATLX_ZON
     &        (IERROR   ,IFmt     ,1        ,IOEQT    ,IOFLSAT  ,IOTRS
     &        ,1        ,IPROB    ,GRIUnit  ,LDARR    ,LDARRT   ,LDCOE
     &        ,LDCRD    ,LDDFM    ,LDDSP    ,LDFOD    ,LDPOR    ,LDSTG
     &        ,LDTRA    ,OutUnit  ,NROW     ,NUMEL    ,NZONES(3)
     &        ,NZONES(13)         ,NZONES(12)         ,NZONES(9)
     &        ,NZONES(7)          ,NZONES(11)         ,NZONES(10)
     &        ,NZONES(2)          ,NZONES(1)
     &        ,FileGRI  ,LDIM     ,LTYPE    ,LXPAREL(1,3,IPROB)
     &        ,LXPAREL(1,4,IPROB) ,LXPAREL(1,10,IPROB)
     &        ,LXPAREL(1,9,IPROB) ,LXPAREL(1,6,IPROB)
     &        ,LXPAREL(1,5,IPROB) ,LXPAREL(1,8,IPROB)
     &        ,LXPAREL(1,7,IPROB) ,LXPAREL(1,2,IPROB)
     &        ,LXPAREL(1,1,IPROB))

      END DO !IPROB-1,NPBMX

C------------------------- Recupera el valor de IOEQT.

      IOEQT = IOEQTOR


C------------------------- Cambia el tipo de elemento

      Do L=1,NUMEL

          If (LTYPE(L).EQ.3) Then            !TRIANGULAR ELEMENT (2-D)

             LTYPE(L)=2

          Else If (LTYPE(L).EQ.5) Then       !CUADRANGULAR ELEMENT (2-D)

             LTYPE(L)=3

          Else If (LTYPE(L).EQ.9) Then       !TETRAHEDRON ELEMENT (3-D)

             LTYPE(L)=4

          Else If (LTYPE(L).EQ.10) Then      !TRIANGULAR ELEMENT (3-D)

             LTYPE(L)=5

          Else If (LTYPE(L).EQ.11)Then       !TOBLERONE (3-D)

             LTYPE(L)=6

          End If

      End Do !L=1,NUMEL

C------------------------- Escribe la malla en un archivo aparte.

      If(IOWriMesh) Then

C------------------------- Construye el nombre del archivo

          Filename = Trim(Root) // 'MESH.vtk'

          Open (MeshUnit, File=Filename, Status='UNKNOWN')
          Write (Title, 10) Trim(Root),'MESH'
   10     Format(A20,1X,A4)

          Call Write_VTK_Unstructured_Grid
     &        (COORD   ,IOWri1D,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE
     &        ,NUMEL   ,NUMNP   ,Title  ,MeshUnit)

          Close(MeshUnit)

      End If !IOWriMesh

      If (IOWriGRI) Then

C------------------------- Escribe las condiciones de contorno y
C-------------------------  zonas de nudo y elemento de flujo

          RootPb = Root

          Do IPROB=1,NPBFL

C------------------------- Si hay más de un problema modifica la ráiz
C------------------------- para incluir el número de problema.

              If (NPBMX .GT. 1) Then

                  Write(RootPb,'(I5)') IPROB
                  RootPb = Trim(AdjustL(RootPb))
                  RootPb = Trim(Root) // '-p' // Trim(RootPb) // '- '

              End If !NPBMX .GT. 1

C------------------------- Escribe las condiciones de contorno de flujo

              Call Write_Var_Int
     &            (COORD    ,1        ,1        ,IOWRi1D  ,-1   
     &            ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &            ,NUMNP    ,NUMNP    ,RootPb   ,IBCOD(1,IPROB)
     &            ,'BCF   ' ,0        ,1        ,MeshUnit)

C------------------------- Escribe las zonas para cada tipo de condición
C------------------------- de contorno de flujo

C------------------------- Nivel fijo

              If (NZONES(4).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP    ,RootPb,IXPARNP(1,INCHP,IPROB)
     &                ,'CHP   ' ,0        ,1        ,MeshUnit)

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP   ,RootPb,IXPARNP(1,INCHPT,IPROB)
     &                ,'CHPT  ' ,0        ,1        ,MeshUnit)

              End If !NZONES(4).GT.0

C------------------------- Caudal

              If (NZONES(5).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP    ,RootPb,IXPARNP(1,INQQP,IPROB)
     &                ,'QQP   ' ,0        ,1        ,MeshUnit)

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP   ,RootPb,IXPARNP(1,INQQPT,IPROB)
     &                ,'QQPT  ' ,0        ,1        ,MeshUnit)

              End If !NZONES(5).GT.0

C------------------------- Goteo

              If (NZONES(6).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP    ,RootPb,IXPARNP(1,INALF,IPROB)
     &                ,'ALF   ' ,0        ,1        ,MeshUnit)

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP   ,RootPb,IXPARNP(1,INALFT,IPROB)
     &                ,'ALFT  ' ,0        ,1        ,MeshUnit)

              End If !NZONES(6).GT.0

C------------------------- Escribe las zonas de elementos de flujo.

C------------------------- Transmisividad

              If (NZONES(1).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INTRA,IPROB)
     &                ,'TRA   ' ,0        ,2        ,MeshUnit)

              End If !NZONES(1).GT.0

C------------------------- Almacenamiento

              If (NZONES(2).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INSTG,IPROB)
     &                ,'STG   ' ,0        ,2        ,MeshUnit)

              End If !NZONES(2).GT.0

C------------------------- Recarga

              If (NZONES(3).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INARR,IPROB)
     &                ,'ARR   ' ,0        ,2        ,MeshUnit)

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL   ,RootPb,LXPAREL(1,INARRT,IPROB)
     &                ,'ARRT  ' ,0        ,2        ,MeshUnit)

              End If !NZONES(3).GT.0

          End Do !IPROB=1,NPBFL

C------------------------- Escribe las condiciones de contorno y
C------------------------- zonas de nudo y elemento de transporte

          RootPb = Root

          Do IPROB=1,NPBTP

C------------------------- Si hay más de un problema modifica la ráiz
C------------------------- para incluir el número de problema.

              If (NPBMX .GT. 1) Then

                  Write(RootPb,'(I5)') IPROB
                  RootPb = Trim(AdjustL(RootPb))
                  RootPb = Trim(Root) // '-p' // Trim(RootPb) // '- '

              End If !NPBMX .GT. 1

C------------------------- Escribe las condiciones de contorno de tpt.

              Call Write_Var_Int
     &            (COORD    ,1        ,1        ,IOWri1D  ,-1
     &            ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &            ,NUMNP    ,NUMNP    ,RootPb   ,IBTCO(1,IPROB)
     &            ,'BCT   ',0        ,1        ,MeshUnit)

C------------------------- Escribe las zonas para cada tipo de condición
C------------------------- de contorno de transporte.

C------------------------- Concentración externa (nudo).

              If (NZONES(13).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP    ,RootPb,IXPARNP(1,INCON,IPROB)
     &                ,'CON   ' ,0        ,1        ,MeshUnit)

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP   ,RootPb,IXPARNP(1,INCONT,IPROB)
     &                ,'CONT  ' ,0        ,1        ,MeshUnit)

              End If !NZONES(13).GT.0

C------------------------- Difusión en la matriz.

              If (NZONES(16).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP    ,RootPb,IXPARNP(1,INDMT,IPROB)
     &                ,'DMT   ' ,0        ,1        ,MeshUnit)

              End If !NZONES(16).GT.0

C------------------------- 'Goteo' de concentración.

              If (NZONES(18).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,1        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMNP    ,RootPb,IXPARNP(1,INCLK,IPROB)
     &                ,'CLK   ' ,0        ,1        ,MeshUnit)

              End If !NZONES(18).GT.0

C------------------------- Escribe las zonas de elementos de tranporte.

C------------------------- Dispersión

              If (NZONES(7).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INDSP,IPROB)
     &                ,'DSP   ' ,0        ,2        ,MeshUnit)

              End If !NZONES(7).GT.0

C------------------------- Difusión molecular

              If (NZONES(9).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INDFM,IPROB)
     &                ,'DFM   ' ,0        ,2        ,MeshUnit)

              End If !NZONES(9).GT.0

C------------------------- Porosidad

              If (NZONES(10).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INPOR,IPROB)
     &                ,'POR   ' ,0        ,2        ,MeshUnit)

              End If !NZONES(10).GT.0

C------------------------- Retardo

              If (NZONES(12).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INCRD,IPROB)
     &                ,'CRD   ' ,0        ,2        ,MeshUnit)

              End If !NZONES(12).GT.0

C------------------------- Concentración externa (elemento)

              If (NZONES(13).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INCOE,IPROB)
     &                ,'COE   ' ,0        ,2        ,MeshUnit)

              End If !NZONES(13).GT.0

C------------------------- Desintegración de primer orden

              If (NZONES(11).GT.0) Then

                  Call Write_Var_Int
     &                (COORD    ,2        ,1        ,IOWri1D  ,-1
     &                ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &                ,NUMNP    ,NUMEL    ,RootPb,LXPAREL(1,INFOD,IPROB)
     &                ,'FOD   ' ,0        ,2        ,MeshUnit)

              End If !NZONES(11).GT.0

      End Do !IPROB=1,NPBTP

      End If !IOWriGRI

      End Subroutine LeeGRI

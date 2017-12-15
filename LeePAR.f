      Subroutine LeePAR
     &          (CFPAREL  ,CFPARNP  ,CoefUnit ,COORD    ,FilePAR
     &          ,IBCOD    ,IBTCO    ,IFmt     ,INORPAR  ,IODIM
     &          ,IOINV    ,IOEQT    ,IOFLLI   ,IOFLSAT  ,IORTS
     &          ,IOTRLI   ,IOTRS    ,IOwri1D  ,ISOT     ,ISOZ
     &          ,KXX      ,LDIM     ,LMXNDL   ,LNNDEL   ,LTYPE
     &          ,NFNL     ,NPAR     ,NPARF    ,NPAREL   ,NPARNP
     &          ,NTYPAR   ,NUMEL    ,NUMNP    ,NZPAR    ,NZONES
     &          ,PARUnit  ,PARC     ,PARM     ,PARZ     ,OutUnit
     &          ,Root)


C###############################################################################
C
C     Lee del archovo GRI las coordenadas de los nudos, condiciones de
C     contorno, conectividades y zonas por elemento.
C
C     Se aprovechan las subritinas de Transin4
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
C       Localización en CFPAREL
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

      Integer*4::IFmt   ,IODIM  ,IOINV  ,IOEQT  ,IOFLLI ,IOFLSAT,IORTS
     &          ,IOTRLI ,IOTRS  ,ISOT   ,LMXNDL ,NFNL   ,NPAR   ,NPARF
     &          ,NPAREL ,NPARNP ,NTYPAR ,NUMEL  ,NUMNP  ,NZPAR

      Integer*4::IOWri1D  ,CoefUnit ,PARUnit  ,OutUnit

      Character::FilePAR*50,Root*20

      Integer*4::NZONES(20)
      Integer*4::IBCOD(NUMNP)      ,IBTCO(NUMNP)
     &          ,INORPAR(NTYPAR)   ,ISOZ(NZONES(1))
     &          ,KXX(LMXNDL,NUMEL) ,LDIM(NUMEL)
     &          ,LNNDEL(NUMEL)     ,LTYPE(NUMEL)


      Real*8::CFPAREL(NUMEL,NPAREL)  ,CFPARNP(NUMNP,NPARNP)
     &       ,COORD(NUMNP,3)         ,PARC(NPAR)
     &       ,PARM(NPAR)             ,PARZ(NZPAR)

C------------------------- Internal

      Integer*4::IERROR   ,IDIMWGT  ,IOINV_GS ,MXGRPZN  ,NGROUP_ZN
     &          ,NPARDET  ,NPARFPRG ,NROW

      Integer*4::IOLG_PAR(NTYPAR,2)

      Integer*4,Allocatable::INDPAR(:)     ,IOPT_GS(:,:)  ,IPNT_PAR(:)
     &                      ,IVPAR(:,:)    ,NFNLPAR(:)    ,NFTPAR(:)
     &
      Real*8,Allocatable::PAR_WGT(:)    ,STPAR(:)      ,WGT_PAR(:)
     &                   ,WGT_UNK(:)


      Integer*4,Parameter::
     &                     INCHP  = 1 ! Location in array CFPARNP
     &                    ,INCHPT = 2
     &                    ,INQQP  = 3
     &                    ,INQQPT = 4
     &                    ,INALF  = 5
     &                    ,INALFT = 6
     &                    ,INCON  = 7
     &                    ,INCONT = 8
     &                    ,INDMT  = 9
     &                    ,INCLK  = 10
     &
     &                    ,INTRA  = 1 ! Location in array CFPAREL
     &                    ,INSTG  = 2
     &                    ,INARR  = 3
     &                    ,INARRT = 4
     &                    ,INDSP  = 5
     &                    ,INDFM  = 6
     &                    ,INPOR  = 7
     &                    ,INFOD  = 8
     &                    ,INCRD  = 9
     &                    ,INCOE  = 10

C-------------------------
C-------------------------

      Open (PARUnit, File=FilePAR, Status='OLD',IOSTAT=IError)

      Write (OutUnit,12)
      Write (*,12)
   12 Format (/,'Procesando el archivo PAR...')

      NROW = 0

      Call LEC_CFN
     &    (IERROR   ,IFmt     ,INALF    ,INALFT   ,INCHP    ,INCHPT
     &    ,INCON    ,INCONT   ,1        ,INQQP    ,INQQPT   ,IOEQT
     &    ,IORTS    ,IOTRS    ,1        ,PARUnit  ,OutUnit  ,NPARNP
     &    ,NROW     ,NUMNP    ,NZONES(6)          ,NZONES(4),NZONES(13)
     &    ,NZONES(5),CFPARNP  ,FilePAR  ,IBCOD              ,IBTCO
     &    ,INCLK    ,NZONES(18))

C------------------------- Reads elements coeficients

      Call LEC_CFE
     &    (IERROR   ,IFmt     ,INARR    ,INARRT   ,INCOE    ,INCRD
     &    ,INDFM    ,INDSP    ,INFOD    ,INPOR    ,1        ,INSTG
     &    ,INTRA    ,IOEQT    ,IOFLSAT  ,IOTRS    ,1        ,PARUnit
     &    ,OutUnit  ,NPAREL   ,NROW     ,NUMEL    ,NZONES(3),NZONES(13)
     &    ,NZONES(12)         ,NZONES(9),NZONES(7),NZONES(11)
     &    ,NZONES(10)         ,NZONES(2),NZONES(1),CFPAREL  ,FilePAR)

C------------------------- Reads zone parameters

      IDIMWGT = 1
      MXGRPZN = 20
      NPARFPRG = 1
      NGROUP_ZN = 1
      IOINV_GS = 0

      Allocate(INDPAR(NPAR)
     &        ,IOPT_GS(MXGRPZN,20)
     &        ,IPNT_PAR(NZPAR*IDIMWGT)
     &        ,IVPAR(NZPAR,4)
     &        ,NFNLPAR(NZPAR)
     &        ,NFTPAR(NZPAR)
     &        ,PAR_WGT(NTYPAR)
     &        ,STPAR(NZPAR)
     &        ,WGT_PAR(NZPAR*IDIMWGT)
     &        ,WGT_UNK(NPAR))

      INDPAR   = 0
      IOPT_GS  = 0
      IPNT_PAR = 0
      IVPAR    = 0
      NFNLPAR  = 0
      NFTPAR   = 0
      PAR_WGT  = 0
      STPAR    = 0
      WGT_PAR  = 0
      WGT_UNK  = 0

      Call ENTDATNZ
     &    (IDIMWGT  ,IERROR   ,1        ,IODIM    ,IOEQT    ,IOFLLI
     &    ,IOFLSAT  ,IOINV    ,IOTRLI   ,IORTS    ,IOTRS    ,1
     &    ,ISOT     ,PARUnit  ,OutUnit  ,MXGRPZN  ,NFNL     ,NPAR
     &    ,NPARF    ,NPARFPRG ,NROW     ,NTYPAR   ,NUMEL    ,NZPAR
     &    ,FilePAR  ,INDPAR   ,INORPAR  ,IOLG_PAR ,IOPT_GS  ,IPNT_PAR
     &    ,ISOZ     ,IVPAR    ,LDIM     ,NFNLPAR  ,NFTPAR   ,NZONES
     &    ,PAR_WGT  ,PARC     ,PARM     ,PARZ     ,STPAR    ,WGT_PAR
     &    ,NGROUP_ZN,NPARDET  ,IOINV_GS ,WGT_UNK)

C------------------------- Libera memoria

      Deallocate(INDPAR
     &          ,IOPT_GS
     &          ,IPNT_PAR
     &          ,IVPAR
     &          ,NFNLPAR
     &          ,NFTPAR
     &          ,PAR_WGT
     &          ,STPAR
     &          ,WGT_PAR
     &          ,WGT_UNK)

C------------------------- Reads matrix diffusion zones

c      If (IOEQT.NE.1. AND .NTDMT.NE.0) Then

c         Call ENTDAT_DMT
c     &        (NZDMT    ,NTDMT    ,IVPAR    ,PARZ     ,INORPAR  ,NZONES
c     &        ,NTYPAR   ,NZPAR    ,IXPARNP  ,NUMNP    ,NPAR     ,1
c     &        ,MAINF    ,IOINV    ,1        ,NPARNP)


c      End If !IOEQT.NE.1. AND .NTDMT.NE.0


C------------------------- Escribe los coeficientes de nudo

C------------------------- Nivel fijo

      If (NZONES(4).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INCHP)
     &        ,'CFCHP ' ,0        ,1        ,CoefUnit)

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INCHPT)
     &        ,'CFCHPT' ,0        ,1        ,CoefUnit)

      End If !NZONES(4).GT.0

C------------------------- Caudal

      If (NZONES(5).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INQQP)
     &        ,'CFQQP ' ,0        ,1        ,CoefUnit)

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INQQPT)
     &        ,'CFQQPT' ,0        ,1        ,CoefUnit)

      End If !NZONES(5).GT.0

C------------------------- Goteo

      If (NZONES(6).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INALF)
     &        ,'CFALF ' ,0        ,1        ,CoefUnit)

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INALFT)
     &        ,'CFALFT' ,0        ,1        ,CoefUnit)

      End If !NZONES(6).GT.0

C------------------------- Concentraci�n externa (nudo).

      If (NZONES(13).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INCON)
     &        ,'CFCON ' ,0        ,1        ,CoefUnit)

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INCONT)
     &        ,'CFCONT' ,0        ,1        ,CoefUnit)

      End If !NZONES(13).GT.0

C------------------------- Difusión en la matriz.
C
C      If (NZONES(16).GT.0) Then
C
C         Call Write_Var_Real
C     &        (COORD    ,1        ,-1       ,KXX      ,LMXNDL
C     &        ,LNNDEL   ,LTYPE    ,NUMEL    ,NUMNP    ,NUMNP
C     &        ,Root     ,CFPARNP(1,INDMT)   ,'CFDMT ' ,0
C     &        ,1        ,CoefUnit)
C
C     End If !NZONES(16).GT.0

C------------------------- 'Goteo' de concentración.

      If (NZONES(18).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMNP    ,Root     ,CFPARNP(1,INCLK)
     &        ,'CFCLK ' ,0        ,1        ,CoefUnit)

      End If !NZONES(18).GT.0

C------------------------- Escribe los coeficientes de elementos.

C------------------------- Transmisividad

      If (NZONES(1).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INTRA)
     &        ,'CFTRA ' ,0        ,2        ,CoefUnit)
      End If !NZONES(1).GT.0

C------------------------- Almacenamiento

      If (NZONES(2).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INSTG)
     &        ,'CFSTG ' ,0        ,2        ,CoefUnit)

      End If !NZONES(2).GT.0

C------------------------- Recarga

      If (NZONES(3).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INARR)
     &        ,'CFARR ' ,0        ,2        ,CoefUnit)

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INARRT)
     &        ,'CFARRT' ,0        ,2        ,CoefUnit)

      End If !NZONES(3).GT.0

C------------------------- Dispersi�n

      If (NZONES(7).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INDSP)
     &        ,'CFDSP '  ,0        ,2        ,CoefUnit)

      End If !NZONES(7).GT.0

C------------------------- Difusi�n molecular

      If (NZONES(9).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INDFM)
     &        ,'CFDFM ' ,0        ,2        ,CoefUnit)

      End If !NZONES(9).GT.0

C------------------------- Porosidad

      If (NZONES(10).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INPOR)
     &        ,'CFPOR ' ,0        ,2        ,CoefUnit)

      End If !NZONES().GT.0

C------------------------- Retardo

      If (NZONES(12).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INCRD)
     &        ,'CFCRD ' ,0        ,2        ,CoefUnit)

      End If !NZONES(12).GT.0

C------------------------- Concentración externa (elemento)

      If (NZONES(13).GT.0) Then

          Call Write_Var_Real
     &        (COORD    ,2        ,1        ,IOWri1D  ,-1
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,CFPAREL(1,INCOE)
     &        ,'CFCOE ' ,0        ,2        ,CoefUnit)

      End If !NZONES(13).GT.0

C------------------------- Desintegración de primer orden
C
C      If (NZONES(11).GT.0) Then
C
C         Call Write_Var_Real
C     &        (COORD    ,1        ,-1       ,KXX      ,LMXNDL
C     &        ,LNNDEL   ,LTYPE    ,NUMEL    ,NUMNP    ,NUMEL
C     &        ,Root     ,CFPAREL(1,INFOD)   ,'CFFOD ' ,0
C     &        ,2        ,CoefUnit)
C
C     End If !NZONES(11).GT.0


      End Subroutine LeePAR

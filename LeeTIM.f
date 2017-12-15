      Subroutine LeeTIM
     &          (FileTIM  ,IOEQT    ,IOFLLI   ,IORTS    ,IOTRLI
     &          ,IOTRS    ,IOWriTIM ,ISOLEQ   ,NFNT     ,NINT
     &          ,OutUnit  ,Root     ,TIMES    ,TIMUnit)

C###############################################################################
C
C     Lee del archivo TIM los tiempos de cálculo y escribe las funciones
C     a un archivo si se le pide.
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################

      Implicit None

C------------------------- External

      Integer*4::IOEQT  ,IOFLLI ,IORTS  ,IOTRLI ,IOTRS  ,NFNT   ,NINT


      Integer*4::OutUnit,TIMUnit

      Logical:: IOWriTIM

      Integer*4::ISOLEQ(NINT,4)

      Real*8::TIMES(NINT)

      Character::FileTIM*50,Root*20

C------------------------- Internal

      Integer*4::IDIMFNT,IERROR,ITim,NROW

      Real*8::EPSFLU,EPSTRA,THETAF,THETAT

      Character::strBlank*3, strFmt*20,Ext*3, Suffix*7

      Integer*4,Allocatable::KINT(:)

      Real*8,Allocatable::FNT(:,:) ,DTMXDS(:)

C------------------------- 
C------------------------- 

      Open (TIMUnit, File=FileTIM, Status='OLD',IOSTAT=IError)
          
      Write (OutUnit,12)
      Write (*,12)
   12 Format (/,'Leyendo el archivo TIM...')


      NROW = 0

      IDIMFNT = MAX(1,NFNT)

      Allocate(FNT(IDIMFNT,NINT+1)
     &        ,KINT(NINT)
     &        ,DTMXDS(NINT))

      Call LEC_FT
     &    (EPSFLU   ,EPSTRA   ,IDIMFNT  ,IERROR   ,1         ,1
     &    ,1        ,IOEQT    ,IOFLLI   ,1        ,1         ,IORTS
     &    ,IOTRLI   ,IOTRS    ,1        ,TIMUnit  ,OutUnit   ,NFNT
     &    ,NINT     ,NROW     ,THETAF   ,THETAT   ,DTMXDS    ,FileTIM
     &    ,FNT      ,ISOLEQ   ,KINT     ,TIMES)


      If (IOWriTIM .AND. NFNT .GT. 0) Then

          Ext = 'DAT'
          Suffix ='FT_    '
          Call Make_File_Name(Ext,FileTIM,-1,Root,Suffix)
          print*,FileTIM
          Open (TIMUnit, File=FileTIM, Status='UNKNOWN')

          strFmt=''
          strBlank = "' '"
          Write(strFmt,*) NFNT+1
          strFmt = '(I5,' // Trim(AdjustL(strFMt))
          strFmt = Trim(strFmt) // '(' // strBlank // ',G15.10))'


          Do ITim = 1,NINT

             Write(TIMUnit,strFmt) ITim,Times(ITim),FNT(1:NFNT,ITim)

          End Do !IFT = 1,NINT

          Close(TIMUnit)

      Else If (.Not. IOWriTIM) Then

          Write (OutUnit,84)
          Write (*,84)
   84     Format (/,'Ignoro el archivo TIM.')

      Else If (NFNT .LT. 1) Then

          Write (OutUnit,85)
          Write (*,85)
   85     Format (/,'No hay funciones de tiempo que escribir.')

      End If ! IOWriTIM .AND. NFT.GT.0

      DeAllocate(FNT
     &          ,KINT
     &          ,DTMXDS)

      End Subroutine LeeTIM

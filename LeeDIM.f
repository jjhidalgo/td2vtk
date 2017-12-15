      Subroutine LeeDIM
     &          (DIMUnit  ,FileDIM  ,IFmt     ,INORPAR  ,IODIM
     &          ,IOEQT    ,IOFLLI   ,IOFLSAT  ,IOINV    ,IOMCC
     &          ,IOMHH    ,IORTS    ,IOTRLI   ,IOTRS    ,ISOT
     &          ,LMXNDL   ,NBAND    ,NFNL     ,NFNT     ,NINT
     &          ,NPAR     ,NPARF    ,NPBFL    ,NPBTP    ,NTDMT
     &          ,NTYPAR   ,NumDevs  ,NUMEL    ,NUMNP    ,NZPAR
     &          ,NZONES   ,OutUnit)

C###############################################################################
C
C     Lee del archivo DIM las dimensiones del modelo
C
C     Posicion en NZONES
C       1 -> NZTRA
C       2 -> NZSTG
C       3 -> NZARR
C       4 -> NZCHP
C       5 -> NZQQP
C       6 -> NZALF
C       7 -> NZDSP
C       8 -> NZDFM
C       9 -> NZPOR
C      10 -> NZFOD
C      11 -> NZCRD
C      12 -> NZCOE
C      13 -> NZDMT
C      14 -> NZPRG,
C      15 -> NPARFPRG
C      16 -> NPARPRG
C      17 -> NZCLK
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################

      Implicit None

C------------------------- External

      Integer*4::IFmt   ,IODIM  ,IOEQT  ,IOFLLI ,IOFLSAT,IOINV  ,IOMCC
     &          ,IOMHH  ,IORTS  ,IOTRLI ,IOTRS  ,ISOT   ,LMXNDL ,NBAND
     &          ,NFNL   ,NFNT   ,NINT   ,NPAR   ,NPARF  ,NPBFL ,NPBTP
     &          ,NTDMT  ,NumDevs,NUMEL  ,NUMNP  ,NTYPAR ,NZPAR

      Integer*4::DIMUnit,OutUnit

      Character::FileDIM*50
      Character::LEEL*100,strFmt*20

      Integer*4::INORPAR(NTYPAR),NZONES(20)

C------------------------- Internal

      Integer*4::I,INALFC,INARRC,INCHPC,INCLK,INCOEC,INCRDC,INDFMC
     &          ,INDSLC,INDSTC,INFODC,INPRGC,INPORC,INQQPC,INSTGC,IZSUM
     &          ,IOError,NRow  ,NZTRA  ,NZSTG      ,NZARR  ,NZCHP ,NZQQP
     &          ,NZALF  ,NZDSP  ,NZDFM  ,NZPOR  ,NZFOD  ,NZCRD  ,NZCOE
     &          ,NZDMT  ,NZPRG  ,NPARFPRG       ,NPARPRG,NZCLK

      Character::LeAux*100,N_CARD*80

      Integer*4::IDummy(20),INTRAC(6)

C------------------------- 
C------------------------- 

      Open (DIMUnit, File=FileDIM, Status='OLD',IOSTAT=IOError)
          
      Write (OutUnit,12)
      Write (*,12)
   12 Format (/,'Leyendo el archivo DIM...')

C------------------------- Opciones del problema

      N_CARD='DEF_OPTIONS'

      Call SRC_NCARD
     &    (IOError  ,DIMUnit  ,NRow   ,N_CARD)

       LeAux = LEEL(FileDIM,DIMUnit,OutUnit,NRow,1)

      Read(LeAux,1300) IOEQT,IOINV,IOTRS,IDummy(2),IORTS,IDummy(3:5)
     &                ,IODIM,IOFLLI,IOTRLI,IDummy(7)


 1300 Format(17I5)

C------------------------- Dimensiones

      N_CARD='DIMENSIONS'

      Call SRC_NCARD
     &    (IOError  ,DIMUnit  ,NRow   ,N_CARD)

      LeAux = LEEL(FileDIM,DIMUnit,OutUnit,NRow,1)

      strFmt = ''
      Write(strFmt,*) 4+IFmt
      strFmt = '(2I'//Trim(AdjustL(strFmt))//',15I5)'

      Read(LeAux,strFmt) NUMEL  ,NUMNP  ,LMXNDL ,ISOT   ,NBAND
     &                  ,NPAR   ,NINT   ,NFNT   ,NPARF  ,NTDMT
     &                  ,NFNL   ,IDummy(1)      ,IOFLSAT,IDummy(2:3)


          LeAux = LEEL(FileDIM,DIMUnit,OutUnit,NRow,1)
          Read(LeAux,strFmt) NumDevs

C 1310 Format(2I<4+IFmt>,15I5)
     ;  

      Write(OutUnit,3600) NINT
 3600 Format(/,'Number of time intervals: ',I5)


C------------------------- Número de problemas

      NPBFL=0
      NPBTP=0

      LeAux=LEEL(FileDIM,DIMUnit,OutUnit,NRow,1)
      Read(LeAux,20) IDummy(1:3) ,NPBFL ,NPBTP ,IDummy(4)
   20 Format(6I5)

C------------------------- If zero, the number of flow and transport problems
C------------------------- are set to one

      If (NPBFL.LE.0 .AND. IOEQT.NE.2) NPBFL = 1
      If (NPBTP.LE.0 .AND. IOEQT.NE.1) NPBTP = 1

C------------------------- Zone numbers.

      N_CARD='ZONE_NUMBERS'

      Call SRC_NCARD
     &    (IOError  ,DIMUnit  ,NRow   ,N_CARD)

      LeAux = LEEL(FileDIM,DIMUnit,OutUnit,NRow,1)

      Read(LeAux,1300) NZTRA  ,NZSTG  ,NZARR  ,NZCHP  ,NZQQP  ,NZALF
     &                ,NZDSP  ,NZDFM  ,NZPOR  ,NZFOD  ,NZCRD  ,NZCOE
     &                ,NZDMT  ,NZPRG  ,NPARFPRG       ,NPARPRG,NZCLK
          


      Write(OutUnit,3700) NZTRA  ,NZSTG  ,NZARR  ,NZCHP  ,NZQQP  ,NZALF
     &                   ,NZDSP  ,NZDFM  ,NZPOR  ,NZFOD  ,NZCRD  ,NZCOE
     &                   ,NZDMT  ,NZPRG  ,NPARFPRG       ,NPARPRG,NZCLK
      
      NZONES(1)  = NZTRA
      NZONES(2)  = NZSTG
      NZONES(3)  = NZARR
      NZONES(4)  = NZCHP
      NZONES(5)  = NZQQP
      NZONES(6)  = NZALF
      NZONES(7)  = NZDSP
      NZONES(9)  = NZDFM
      NZONES(10) = NZPOR
      NZONES(11) = NZFOD
      NZONES(12) = NZCRD
      NZONES(13) = NZCOE
      NZONES(14) = NZPRG
      NZONES(15) = 0 !NZZOR
      NZONES(16) = NZDMT
      NZONES(17) = 0 !NZZOD
      NZONES(18) = NZCLK

      !NZONES(15) = NPARFPRG
      !NZONES(16) = NPARPRG
      

 3700 Format(//,10X,'NUMBERS OF ZONES',/
     &         10X,'================',//
     & 5X,'TRANSMISIVITY ...... =',I5,/
     & 5X,'STORAGE ............ =',I5,/
     & 5X,'RECH. COEFF. ....... =',I5,/
     & 5X,'PRESC. HEAD ........ =',I5,/
     & 5X,'PRESC. FLOW ........ =',I5,/
     & 5X,'LEAKAGE ............ =',I5,/
     & 5X,'DISPERSIVITY ....... =',I5,/
     & 5X,'MOLEC. DIFFUSION ... =',I5,/
     & 5X,'POROSITY ........... =',I5,/
     & 5X,'FIRST ORDER DECAY .. =',I5,/
     & 5X,'RETARD. COEFF. ..... =',I5,/
     & 5X,'EXTERNAL CONCENT.... =',I5,/
     & 5X,'MATRIX DIFFUSION.... =',I5,/
     & 5X,'GROUP PARAMETERS.... =',I5,/
     & 5X,'FLOW GENERIC ESTIMATED PARAMETERS=',I5,/
     & 5X,'TOTAL GENERIC ESTIMATED PARAMETERS=',I5,/
     & 5X,'CONC. LEAKAGE....... =',I5)


      IF (NPAR.EQ.0 .AND. NPARPRG.EQ.0) NPAR=1
      NPAR=NPAR+NPARPRG
      NPARF=NPARF+NPARFPRG

      If (IOEQT.NE.2) THEN        ! Only flow or flow plus transport
          NZPAR = NZTRA*MAX(ISOT,IODIM)+NZSTG+NZARR+NZCHP+NZQQP+NZALF+
     ;          NZPRG+NZCLK
          IF (IOEQT.EQ.3) THEN
              NZPAR= NZPAR+2*NZDSP+NZPOR+NZDFM+NZCRD+NZFOD+NZCOE
          ELSE

C------------------------- Porosity has to be explicitly added when no 
C------------------------- transport is solved and unsaturated flow is required

              IF (IOFLSAT.NE.0) NZPAR=NZPAR+NZPOR      
          END IF !IOEQT.EQ.3
      ELSE

          NZPAR=2*NZDSP+NZPOR+NZDFM+NZCRD+NZFOD+NZCOE+NZPRG

      END IF !IOEQT.NE.2


C------------------------- Se calculan algunas dimensiones derivadas de las leidas
      INTRAC(1)=0

      DO I=2,MAX(ISOT,IODIM)
          INTRAC(I)= INTRAC(I-1)+NZTRA
      END DO

      INSTGC = INTRAC( MAX (ISOT,IODIM))
      IZSUM = NZTRA
      IF (IOTRS.NE.0) INSTGC=INSTGC+IZSUM
      IF (NZSTG.NE.0) IZSUM=NZSTG
      INARRC=INSTGC
      IF (NZARR.NE.0) THEN
          INARRC=INARRC+IZSUM
          IZSUM=NZARR
      END IF
      INCHPC=INARRC
      IF (NZCHP.NE.0) THEN
          INCHPC=INCHPC+IZSUM
          IZSUM=NZCHP
      END IF
      INQQPC=INCHPC
      IF (NZQQP.NE.0) THEN
          INQQPC=INQQPC+IZSUM
          IZSUM=NZQQP
      END IF
      INALFC=INQQPC
      IF (NZALF.NE.0)  THEN
          INALFC=INALFC+IZSUM
          IZSUM=NZALF
      END IF
      INDSLC=INALFC
      IF (NZDSP.NE.0)  THEN
          INDSLC=INDSLC+IZSUM
          IZSUM=NZDSP
      END IF
      INDSTC=INDSLC
      IF (NZDSP.NE.0)  THEN
          INDSTC=INDSTC+IZSUM
          IZSUM=NZDSP
      END IF
      INDFMC=INDSTC
      IF (NZDFM.NE.0)  THEN
          INDFMC=INDFMC+IZSUM
          IZSUM=NZDFM
      END IF
      INPORC=INDFMC
      IF (NZPOR.NE.0)  THEN
          INPORC=INPORC+IZSUM
          IZSUM=NZPOR
      END IF
      INFODC=INPORC
      IF (NZFOD.NE.0)  THEN
          INFODC=INFODC+IZSUM
          IZSUM=NZFOD
      END IF
      INCRDC=INFODC
      IF (NZCRD.NE.0)  THEN
          INCRDC=INCRDC+IZSUM
          IZSUM=NZCRD
      END IF
      INCOEC=INCRDC
      IF (NZCOE.NE.0)  THEN
          INCOEC=INCOEC+IZSUM
          IZSUM=NZCOE
      END IF

      INPRGC=INCOEC
      IF (NZPRG.NE.0)  THEN
          INPRGC=INPRGC+IZSUM
          IZSUM = NZPRG
      END IF

      INCLK=INPRGC
      IF (NZCLK.NE.0)  THEN
          INCLK=INPRGC+IZSUM
      END IF

      INORPAR(1) =INTRAC(1)
      INORPAR(2) =INTRAC(2)
      INORPAR(3) =INTRAC(3)
      INORPAR(4) =INTRAC(4)
      INORPAR(5) =INTRAC(5)
      INORPAR(6) =INTRAC(6)
      INORPAR(7) =INSTGC
      INORPAR(8) =INARRC
      INORPAR(9) =INCHPC
      INORPAR(10)=INQQPC
      INORPAR(11)=INALFC
      INORPAR(12)=INDSLC
      INORPAR(13)=INDSTC
      INORPAR(14)=INDFMC
      INORPAR(15)=INPORC
      INORPAR(16)=INFODC
      INORPAR(17)=INCRDC
      INORPAR(18)=INCOEC
      INORPAR(19)=INPRGC
C     INORPAR(20)=INAGE
      INORPAR(21)=INCLK

C------------------------- Opciones de salida
C------------------------- Card A6.1

      N_CARD='OUTPUT_OPTIONS'

      Call SRC_NCARD
     &    (IOError  ,DIMUnit  ,NRow   ,N_CARD)

      Call SRC_NCARD
     &    (IOError   ,DIMUnit  ,NRow     ,N_CARD)

      LeAux = LEEL(FileDIM,DIMUnit,OutUnit,NRow,1)

      Read(LeAux,1300) IDummy(1:4),IOMHH,IOMCC,IDummy(5:11)


C------------------------- If flow equation is not solved, flow output options
C------------------------- are set to zero

       If (IOEQT.EQ.2) IOMHH=0

C------------------------- If transport equation is not solved, transport
C------------------------- output options are set to zero

      If (IOEQT.EQ.1) IOMCC=0

      Write (OutUnit,3900) IDummy(1:4),IOMHH,IOMCC,IDummy(5:11)


 3900  FORMAT(////,10X,'OUTPUT OPTIONS',/,
     .             10X,'=============='//,
     .           5X,'HEAD RESIDUALS ...........................=',I5,/,
     .           5X,'CONCENTRATION RESIDUALS ..................=',I5,/,
     .           5X,'COMP. MEASURED HEAD VS TIME ..............=',I5,/,
     .           5X,'COMP. MEASURED CONCENTRATION VS TIME .....=',I5,/,
     .           5X,'HEAD CONTOUR MAPS ........................=',I5,/,
     .           5X,'CONCENTRATION CONTOUR MAPS ...............=',I5,/,
     .           5X,'NUM. HEAD CROSS SECTIONS .................=',I5,/,
     .           5X,'NUM OF CONCENTR. CROSS SECTIONS ..........=',I5,/,
     .           5X,'COMPUTED VS MEASURED HEAD ................=',I5,/,
     .           5X,'COMPUTED VS MEASURED CONCENTR.  ..........=',I5,/,
     .           5X,'INVERSE PROBLEM INFORMATION ..............=',I5,/,
     .           5X,'EVOLUTION OF PARAMETER VALUES ............=',I5,/,
     .           5X,'DARCY FLOW OUTPUT ........................=',I5)

      Close(DIMUnit)

      End Subroutine LeeDIM

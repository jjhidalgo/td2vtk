      Subroutine LeeINI
     &          (CINI     ,COORD    ,FileINI  ,HINI     ,IFmt
     &          ,INIUnit  ,IORTS    ,IOTRS    ,IOwri1D  ,KXX
     &          ,LMXNDL   ,LNNDEL   ,LTYPE    ,NPBFL    ,NPBTP
     &          ,NUMNP    ,NUMEL    ,OutUnit  ,Root     ,VTKUnit)


C###############################################################################
C
C     Lee del archivo INI las condiciones inciales, si es necesario.
C
C     Se aprovechan las subritinas de Transin4.
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################


      Implicit None

C------------------------- External

      Integer*4::IFmt   ,IORTS  ,IOTRS  ,LMXNDL   ,NPBFL    ,NPBTP
     &          ,NUMEL    ,NUMNP

      Integer*4::INIUnit  ,IOWri1D  ,OutUnit  ,VTKUnit

      Character::FileINI*50,Root*20

      Integer*4::KXX(LMXNDL,NUMEL),LNNDEL(NUMEL),LTYPE(NUMEL)

      Real*8::COORD(NUMNP,3)  ,CINI(NUMNP,NPBTP)  ,HINI(NUMNP,NPBFL)

C------------------------- Internal

      Integer*4::IERROR   ,IPROB

      Character::RootPb*20

C-------------------------
C-------------------------

      Open (INIUnit, File=FileINI, Status='OLD',IOSTAT=IError)

      Write (OutUnit,12)
      Write (*,12)
   12 Format (/,'Reading INI file...')


      Call ENDATINICOND
     &    (IERROR   ,IFmt     ,1        ,IORTS    ,IOTRS    ,1
     &    ,INIUnit  ,OutUnit  ,NPBFL    ,NPBTP    ,NUMNP    ,CINI
     &    ,FileINI  ,HINI)


      If (IOTRS.EQ.1) Then

          RootPb = Root

          Do IPROB=1,NPBFL

C------------------------- Si hay más de un problema modifica la ráiz
C------------------------- para incluir el número de problema.

              If (NPBFL .GT. 1) Then

                  Write(RootPb,'(I5)') IPROB
                  RootPb = Trim(AdjustL(RootPb))
                  RootPb = Trim(Root) // '-p' // Trim(RootPb) // '- '

              End If !NPBFL .GT. 1

              Call Write_Var_Real
     &            (COORD    ,2        ,1        ,IOWri1D  ,0
     &            ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &            ,NUMNP    ,NUMNP    ,RootPb   ,HINI(1,IPROB)
     &            ,'HH    ' ,0D0      ,1        ,VTKUnit)

          End Do !IPROB=1,NPBFL

      End If !IOTRS.EQ.1


      If (IORTS.EQ.1) Then

          RootPb = Root

          Do IPROB=1,NPBTP

C------------------------- Si hay más de un problema modifica la ráiz
C------------------------- para incluir el número de problema.

              If (NPBTP .GT. 1) Then

                  Write(RootPb,'(I5)') IPROB
                  RootPb = Trim(AdjustL(RootPb))
                  RootPb = Trim(Root) // '-p' // Trim(RootPb) // '- '

              End If !NPBTP .GT. 1


              Call Write_Var_Real
     &            (COORD    ,2        ,1        ,IOWri1D  ,0
     &            ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &            ,NUMNP    ,NUMNP    ,RootPb   ,CINI(1,IPROB)
     &            ,'CC    ' ,0D0      ,1        ,VTKUnit)

          End Do !IPROB=1,NPBTP

      End If !IORTS.EQ.1

      End Subroutine LeeINI

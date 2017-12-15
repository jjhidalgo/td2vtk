      Subroutine LeeMSV
     &          (COORD    ,FileMSV  ,IFmt     ,IOWri1D  ,KXX
     &          ,LMXNDL   ,LNNDEL   ,LTYPE    ,MSVUnit  ,NUMEL
     &          ,NUMNP    ,OutUnit  ,Root     ,Vels     ,VTKUnit)

C###############################################################################
C
C     Lee del archivo VMSHV.INP las velocidades.
C
C
C     Autor: JHG
C     Fecha: Julio, 2007
C
C###############################################################################

      Implicit None

C------------------------- External

      Integer*4::IFmt     ,LMXNDL   ,NUMEL  ,NUMNP
      
      Integer*4::IOWri1D,MSVUnit,OutUnit,VTKUnit

      Character::FileMSV*50,Root*20

      Integer*4::KXX(LMXNDL,NUMEL),LNNDEL(NUMEL),LTYPE(NUMEL)

      Real*8::COORD(NUMNP,3),Vels(3,NUMEL)

C------------------------- Internal

      Integer*4::IError   ,IndBlank ,ITime    ,L        ,LDummy
     &          ,NLines

      Real*8::VarTime

      Logical::EOF

      Character::Line*100 ,StrDummy1*1,StrDummy2*1,StrDummy3*1
     &          ,strFmt1*20

C------------------------- 

      Open (MSVUnit, File=FileMSV, Status='OLD',IOSTAT=IError)
          
      Write (OutUnit,12)
      Write (*,12)
   12 Format (/,'Procesando el archivo MSV...')


C------------------------- Se salta la cabecera del archivo INP

      Call SkipLines(3,MSVUnit)


C------------------------- Lee el valor del primer paso de tiempo


      EOF = .FALSE.
      ITime = 0

      Do While (.NOT.EOF)

          Read (MSVUnit,20,ERR=999,END=999) Line
   20     Format(A100)
          Line = TRIM(ADJUSTL(Line))
          IndBlank = Index(Line,' ')
          Line = Line(IndBlank+1:)
          ITime = ITime + 1

          Read(Line,*) VarTime

C------------------------- Se salta las líneas que hagan falta
C------------------------- La primera vez hay que saltarse la malla
C------------------------- Las demás sálo la estructura de los datos.

          If (ITime.EQ.1) Then

              NLines = 1 + NUMNP + NUMEL + 3

          Else

              NLines = 3

          End If !ITimes.EQ.1

          Call SkipLines(NLines,MSVUnit)


C------------------------- Lee los valores

          strFmt1 = ''
          Write(strFmt1,*) 4+IFmt
          strFmt1 = '(I'//Trim(AdjustL(strFmt1))//',3(A1,G15.5))'

          Do L=1,NUMEL

              Read (MSVUnit,strFmt1) LDummy,StrDummy1,Vels(1,L)
     &                         ,StrDummy2,Vels(2,L),StrDummy3
     &                         ,Vels(3,L)

C   30         Format(I<4+IFmt>,3(A1,G15.5))

          End Do !I=1,NLines

          Call Write_Var_Real
     &        (COORD    ,2        ,3        ,IOwri1D  ,ITime
     &        ,KXX      ,LMXNDL   ,LNNDEL   ,LTYPE    ,NUMEL
     &        ,NUMNP    ,NUMEL    ,Root     ,Vels     ,'VV    '
     &        ,VarTime  ,4        ,VTKUnit)

          Cycle
  999     EOF = .TRUE.

      End Do !While (Not(EOF))

      Close(MSVUnit)

      End Subroutine LeeMSV

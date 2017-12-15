       SUBROUTINE WRITE_ARRAYN (IFmt,MAINF,NDIM,TITULO,WW)

*****************************************************************************
* PURPOSE
*     Writes the content of array WW
*
* DESCRIPTION
*     Writes the content of array WW in columns of 10
*
* EXTERNAL VARIABLES: ARRAYS
*
*  WW                     Array to be written in file MAINF
*
* EXTERNAL VARIABLES: SCALARS
*
*  TITULO                 Title to be written
*  MAINF                  Unit number of the main output file (RES.OUT)         
*  NDIM                   Dimension of array WW
*
* HISTORY
*
*     AMS        1988     First coding
*     AMS      1-1998     Revison, common elimination and addition of header
*****************************************************************************

       IMPLICIT REAL*8 (A-H,O-Z)
       CHARACTER*(*) TITULO
       Character::strFmt*20
       DIMENSION WW(NDIM)

C------------------------- FIRST EXECUTABLE STATEMENT.
C------------------------- Writes title

       WRITE(MAINF,2000) TITULO
 2000  FORMAT(/,A80)

C------------------------- Writes array WW in columns (10)

       strFmt = ''
       Write(strFmt,*) 4+IFmt
       strFmt = '(/,60X,"/",I'//Trim(AdjustL(strFmt))//',"/")'

       DO I=1,NDIM,10

C------------------------- Every 100 values, a title is written

          IF (MOD(I-1,100).EQ.0) THEN
             WRITE(MAINF,strFmt) I-1
          ENDIF
          WRITE(MAINF,2200) (WW(J),J=I,MIN(I+9,NDIM))
       ENDDO

C 2100  FORMAT(/,60X,'/',I<4+IFmt>,'/')
 2200  FORMAT(1P,10E13.6)

       RETURN
       END

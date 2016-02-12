C--------------------------------------------------------------------
C The user of the OPEN-ADAS system and data (hereinafter "the User")
C accepts the following terms and conditions:
C
C   1. The University of Strathclyde uses all reasonable endeavours
C      to ensure the accuracy of the data provided and any
C      information given, but the University makes no warranty,
C      expressed or implied as to its accuracy, integrity, fitness
C      for purpose and shall not be responsible for the use to
C      which the data is put by the User and/or any consequences
C      arising out of any inaccuracies or omissions.
C
C   2. Any downloaded OPEN-ADAS file is made available here for the
C      personal use of the User only.  It must not be used for any
C      commercial application, incorporated into any web site or in
C      any form re-distributed without express prior written
C      permission from the ADAS Project. In particular, but not by
C      way of limitation, it must not be:
C
C        * inserted into a managed database structure
C        * redistributed along with a modelling or analysis code
C        * made available on a public website.
C
C Permission for OPEN-ADAS data incorporation in non-commercial
C code/system development for scientific advance will not be
C unreasonably withheld, but will be subject to written agreement
C on a case by case basis.
C
C The User will indemnify the University of Strathclyde and keep
C it fully and effectively indemnified against each and every
C claim made against the University as a result of the User's
C use of the data.
C--------------------------------------------------------------------
       PROGRAM TEST

       IMPLICIT NONE
C------------------------------------------------------------------------
C  Purpose:
C          Test the XXDATA_04 routine and also provide a brief
C          example of its use.
C
C  Explanaton:
C          This program is distributed as part of the ADAS
C          subroutine library either via direct CVS to ADAS Project
C          members or via download by OPEN-ADAS users. It reads a
C          file called "test.dat" (distributed along with this
C          program) and prints out some of the contents. The
C          program is intended to serve as an example of using the
C          XXDATA_04 subroutine as well as testing it. A short
C          shell script called "test.sh" is used to compile and
C          test the code.
C
C  Variables:
C          All variables are documented by XXDATA_04. See either the
C          Fortran source or the PDF documentation for the routine.
C          Variables not passed into XXDATA_04 are trivial in nature.
C
C  Routines:
C          Routine    Source    Brief description
C          -------------------------------------------------------------
C          XXDATA_04  ADAS      Read complete data from an ADF04 file
C
C  Author : Allan Whiteford
C
C  Date   : 11/01/08
C
c-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Parameters required by XXDATA_04
C     --------------------------------
      INTEGER   NVMAX      , NDLEV
      INTEGER   NDTRN      , NDQDN
      INTEGER   NDMET
      PARAMETER( NVMAX = 14   , NDLEV= 200,
     &           NDTRN = 5000 , NDQDN = 6   , NDMET=5)
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Input variables (i.e. non-parameter) required by XXDATA_04
C     ----------------------------------------------------------
      INTEGER   IUNIT           , ITIEACTN
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Output variables required by XXDATA_04
C     --------------------------------------
      INTEGER   IORB          ,
     &          IL            ,
     &          IZ            , IZ0         , IZ1     ,
     &          NV            , ITRAN       ,
     &          MAXLEV        , NPL         , IADFTYP
      INTEGER   I1A(NDTRN)  , I2A(NDTRN)
      INTEGER   IA(NDLEV)     , ISA(NDLEV)   , ILA(NDLEV)
      INTEGER   IPLA(NDMET,NDLEV)            , NPLA(NDLEV)
      REAL*8    BWNO   , BWNOA(NDMET)   , PRTWTA(NDMET)
      REAL*8    ZPLA(NDMET,NDLEV)
      REAL*8    SCEF(NVMAX)
      REAL*8    XJA(NDLEV)  , WA(NDLEV)
      REAL*8    AVAL(NDTRN) , SCOM(NVMAX,NDTRN) , BETH(NDTRN)
      REAL*8    QDORB((NDQDN*(NDQDN+1))/2) , QDN(NDQDN)
      CHARACTER CPLA(NDLEV)*1 , CPRTA(NDMET)*9   , TCODE(NDTRN)*1
      CHARACTER TITLED*3      , CSTRGA(NDLEV)*18
      LOGICAL   LPRN        , LCPL           , LORB       , LBETH
      LOGICAL   LETYP       , LPTYP          , LRTYP      , LHTYP   ,
     &          LITYP       , LSTYP          , LLTYP
      LOGICAL   LTIED(NDLEV),LBSETA(NDMET)
      LOGICAL   LQDORB((NDQDN*(NDQDN+1))/2)
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Variables only used by this test program
C     ----------------------------------------
      CHARACTER*80 DSNIN
      INTEGER   I             , IPL
C----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Open input file for reading and set option to not stop when
C     encountering untied levels.
C     ---------------------------
      IUNIT = 10
      DSNIN= 'test.dat'
      OPEN(UNIT = IUNIT , FILE=DSNIN , STATUS = 'OLD')
      ITIEACTN=1
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Pass unit number and dimension parameters to XXDATA_04 and
C     get back contents of file.
C     --------------------------

      CALL XXDATA_04( IUNIT  ,
     &                NDLEV  , NDTRN  , NDMET , NDQDN , NVMAX ,
     &                TITLED , IZ     , IZ0   , IZ1   , BWNO  ,
     &                NPL    , BWNOA  , LBSETA, PRTWTA, CPRTA ,
     &                IL     , QDORB  , LQDORB, QDN   , IORB  ,
     &                IA     , CSTRGA , ISA   , ILA   , XJA   ,
     &                WA     ,
     &                CPLA   , NPLA   , IPLA  , ZPLA  ,
     &                NV     , SCEF   ,
     &                ITRAN  , MAXLEV ,
     &                TCODE  , I1A    , I2A   , AVAL  , SCOM  ,
     &                BETH   ,
     &                IADFTYP, LPRN   , LCPL   , LORB , LBETH ,
     &                LETYP  , LPTYP  , LRTYP  , LHTYP, LITYP ,
     &                LSTYP  , LLTYP  , ITIEACTN, LTIED
     &              )
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Print out some of the contents to screen
C     ----------------------------------------

      PRINT *,'XXDATA_04 Test Program'
      PRINT *,'----------------------'
      PRINT *,' '
      PRINT *,'The following is some information about the'
      PRINT *,'input file: '//DSNIN
      PRINT *,' '
      PRINT *,'NDLEV  , NDTRN  , NDMET , NDQDN , NVMAX=',
     &                    NDLEV  , NDTRN  , NDMET , NDQDN , NVMAX
      PRINT *,' '
      PRINT *,'TITLED,IZ,IZ0,IZ1,BWNO=',
     &                    TITLED,IZ,IZ0,IZ1,BWNO
      PRINT *,' '
      PRINT *, 'NPL=',NPL
      IF(NPL.GT.0) THEN
          PRINT *,'IPL        BWNOA     LBSETA  PRTWTA   CPRTA'
          DO IPL=1,NPL
            PRINT '(I4,2X,F15.1,3X,L1,2X,F8.3,4X,1A9)',
     &        IPL, BWNOA(IPL), LBSETA(IPL), PRTWTA(IPL), CPRTA(IPL)
          ENDDO
      ENDIF
      PRINT *,' '
      PRINT *,'IL,NV,ITRAN=',IL,NV,ITRAN
      PRINT *,' '
      PRINT *, 'IORB=',IORB
      IF(IORB.GT.0) THEN
          PRINT *,'IORB         QDORB   LQDORB    QDN'
          DO I=1,IORB
            PRINT '(I4,2X,F15.8,3X,L1,2X,F10.5)',
     &        I, QDORB(I), LQDORB(I), QDN(I)
          ENDDO
      ENDIF

      PRINT *,' '
      PRINT *,'Check print for levels 1 and IL :-'
      PRINT *,IA(1),' ',CSTRGA(1),ISA(1),ILA(1),XJA(1),
     &                   WA(1)
      PRINT *,IA(IL),' ',CSTRGA(IL),ISA(IL),ILA(IL),XJA(IL),
     &                   WA(IL)

      PRINT *,' '

      PRINT *,'Check parent links for levels 1 and IL :-'
      PRINT *,'CPLA=',(CPLA(I),I=1,IL)
      PRINT *,'NPLA=',(NPLA(I),I=1,IL)
      PRINT *,'IA(1),NPLA(1),CPLA(1)=',IA(1),NPLA(1),'  ',
     &                    CPLA(1)
      PRINT *,'  IPLA(1,1),ZPLA(1,1)=',
     &                      IPLA(1,1),ZPLA(1,1)
      PRINT *,'IA(IL),NPLA(IL),CPLA(IL)=',IA(IL),NPLA(IL),
     &                    '  ',CPLA(IL)
      PRINT *,'  IPLA(NPLA(IL),IL),ZPLA(NPLA(IL),IL)=',
     &                      IPLA(NPLA(IL),IL),ZPLA(NPLA(IL),IL)
      PRINT *,' '
      PRINT *,'Check print for temperatures 1 and NV :-'
      PRINT '(1P,2D10.2)',SCEF(1),SCEF(NV)
      PRINT *,' '
      PRINT *,'CHECK PRINT FOR TRNS. 1,1 AND NV,ITRAN,:-'
      PRINT '(A1,2I5,1P3D10.2)',
     &                   TCODE(1),I2A(1),I1A(1),AVAL(1),SCOM(1,1),
     &                   SCOM(NV,1)
      PRINT '(A1,2I5,1P3D10.2)',
     &                   TCODE(ITRAN),I2A(ITRAN),I1A(ITRAN),
     &                   AVAL(ITRAN),SCOM(1,ITRAN),SCOM(NV,ITRAN)
      PRINT *,' '
      PRINT *,'IADFTYP=',IADFTYP
      PRINT *,' '
      PRINT *,'LPRN, LCPL, LORB,LBETH=',
     &                    LPRN, LCPL, LORB, LBETH
      PRINT *,'LETYP, LPTYP, LRTYP, LHTYP, LITYP=',
     &                    LETYP, LPTYP, LRTYP, LHTYP, LITYP
      PRINT *,'LSTYP, LLTYP=',
     &                    LSTYP, LLTYP
C-----------------------------------------------------------------------
       END


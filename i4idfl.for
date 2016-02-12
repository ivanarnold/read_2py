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
CX UNIX PORT - SCCS info: Module @(#)$Header: /work/dummy/xxdata_04/i4idfl.for,v 1.3 2007/04/11 13:01:47 allan Exp $ Data $Date: 2007/04/11 13:01:47 $
CX
      FUNCTION I4IDFL( N , L )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C
C  *************** FORTRAN77 INTEGER*4 FUNCTION: I4INDL ****************
C
C  PURPOSE:  RETURNS A UNIQUE INDEX NUMBER BASED ON THE VALUE OF THE
C            N AND L QUANTUM NUMBERS PASSED TO IT. THE INDEX IS USED TO
C            REFERENCE ARRAYS CONTAINING DATA DEPENDENT ON THE N AND L
C            QUANTUM NUMBERS.
C
C  CALLING PROGRAM: GENERAL USE
C
C  FUNCTION:
C
C  FUNC:   (I*4)   I4IDFL  = INDEX
C
C  INPUT:  (I*4)   N       = N QUANTUM NUMBER.
C  INPUT:  (I*4)   L       = L QUANTUM NUMBER.
C
C AUTHOR:   JONATHAN NASH (TESSELLA SUPPORT SERVICES PLC)
C           K1/0/81
C           JET EXT. 5183
C
C DATE:     10/09/93
C
C VERSION  : 1.2                          
C DATE     : 20-12-2001
C MODIFIED : Martin O'Mullane
C               - Removed mainframe listing information beyond column 72.
C
C VERSION  : 1.3                          
C DATE     : 10-04-2007
C MODIFIED : Allan Whiteford
C               - Modified documentation as part of automated
C		  subroutine documentation preparation.
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER    I4IDFL
C-----------------------------------------------------------------------
      INTEGER    N  , L
C-----------------------------------------------------------------------
C
C***********************************************************************
C CALCULATE INDEX.
C***********************************************************************
C
      I4IDFL = ((N * (N - 1)) / 2) + L + 1
C
C-----------------------------------------------------------------------
C
      RETURN
      END

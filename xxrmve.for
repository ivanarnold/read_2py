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
      subroutine xxrmve( cstrg1 , cstrg2 , crmve )
      implicit none
C-----------------------------------------------------------------------
C
C  ****************** fortran77 subroutine: xxrmve *********************
C
C  purpose: to remove all occurrences of a selected character from a
C           string and concatenate.  Output string tail is blank filled
C
C  calling program: general use
C
C  subroutine:
C
C  input : (c*(*)) cstrg1   = input string for conversion
C  input : (c*1)   crmve    = character to be removed
C
C  output: (c*(*)) cstrg2   = output string after conversion
C
C          (i*4)   i        = general use
C          (i*4)   ilen     = length of 'cstrng' string in bytes
C
C routines:
C          routine    source    brief description
C          -------------------------------------------------------------
C          i4unit     adas      fetch unit number for output of messages
C
C
C
C author:  H. P. Summers, university of strathclyde
C          ja7.08
C          tel. 0141-548-4196
C
C date:    06/09/01
C
C version : 1.1
C date    : 06/09/2001
C modified: Hugh Summers
C               - first edition.
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      integer     ilen1       , ilen2      , i       , ic
      integer     i4unit
C-----------------------------------------------------------------------
      character   cstrg1*(*)  , cstrg2*(*)  , crmve*1
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      ilen1   = len(cstrg1)
      ilen2   = len(cstrg2)

      ic = 1
      cstrg2 = ' '

         do i=1,ilen1
            if (cstrg1(i:i).ne.crmve) then
                if(ic.le.ilen2) then
                   cstrg2(ic:ic)=cstrg1(i:i)
                   ic=ic+1
                else
                   write(i4unit(-1),1001)
                   write(i4unit(-1),1002)
                endif
            endif

          enddo

       return

C-----------------------------------------------------------------------
 1001 format(1x,32('*'),' xxrmve warning ',32('*')//
     &       1x,'output string too short and returned incomplete: ')
 1002 format(/1x,29('*'),' subroutine terminated ',29('*'))
C-----------------------------------------------------------------------
       end

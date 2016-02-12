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
CX  SCCS Info : Module @(#)$Header: /work/dummy/xxdata_04/xxprs1.for,v 1.1 2004/07/06 15:38:18 whitefor Exp $ Date $Date: 2004/07/06 15:38:18 $
CX
       subroutine xxprs1(ndmet,string,wno,cpl,npt,ipla,zpla,ifail)

       implicit none
C-----------------------------------------------------------------------
C
C  ****************** fortran77 subroutine: xxprs1 *********************
C
C  purpose:  to analyse the tail character string of an level data line
C            of an adf04 specific ion file into wave-number and sets of
C            (parent identifier, effective zeta for the parent) pairs.
C
C            unified version of baprs1, b9prs1, bbprs1, g5prs1 which is 
C            a replacement for these subroutines
C
C  calling program: various
C
C  notes: detect  -  level wave number which preceeds first '{'
C                 -  sets of   parent index contained in '{.}'
C                              followed by effective zeta
C         nb. 'x' as first parent assignment means exclude ionisation
C             from this level.
C             no parent assignment means take lowest parent with
C             zeta =1.
C             lowest parent but no zeta means take zeta =1.
C             if there is more than one parent then zeta's must be in.
C
C
C  subroutine:
C
C  input : (i*4)  ndmet    =  maximum number of parents
C  input : (c*(*))string   =  string to be parsed
C
C  output: (r*8)  wno      =  excitation wave number of level relative
C                             to lowest parent
C  output: (c*1)  cpl      =  lead parent for ionisation  or 'x'
C  output: (i*4)  npt      =  number of parents detected
C  output: (i*4)  ipla()   =  parent indices.
C  output: (r*8)  zpla()   =  effective zeta for parent ipla()
C  output: (i*4)  ifail    =  0 - subroutine concludes correctly
C                             1 - fault detected in subroutine
C                             2 - single ionisation potential detected
C
C          (i*4)  maxwrd   =  maximum number of words sought initially
C                             initially, finally number actually found
C          (i*4)  nfirst   =  first word to be extracted from string
C          (i*4)  ifirst() =  index of first char. of word () in string
C          (i*4)  ilast()  =  index of last  char. of word () in string
C          (i*4)  iwords   =  number of words found in string
C
C          (l*4)  lset     =  .true.  -  wave number part set
C                             .false. -  wave number part not set
C          (l*4)  lwno     =  .true.  -  in the wave number part
C                             .false. -  not in the wave number part
C          (l*4)  lprnt    =  .true.  -  in a parent specifier
C                             .false. -  not in a parent specifier
C          (l*4)  lzeta    =  .true.  -  in a zeta specifier
C                             .false. -  not in a zeta specifier
C          (i*4)  ic       =  general use
C          (i*4)  iabt     =  failure number from r8fctn
C          (i*4)  nchar    =  number of characters in substring
C          (c*15) sstrng   =  isolated substring
C
C routines:
C          routine    source    brief description
C          -------------------------------------------------------------
C          i4unit     adas      fetch unit number for output of messages
C          r8fctn     adas      converts from character to real variable
C          i4fctn     adas      converts from char. to integer  variable
C          xxword     adas      parses a string into separate words
C                               for ' ()<>{}' delimiters
C
C AUTHOR:  HP Summers
C          JA7.08, University of Strathclyde
C          Tel: 0141-548-4196
C
C DATE:    04/12/02
C
C UPDATE:   :
C
C-----------------------------------------------------------------------
       integer*4 ndmet
       integer*4 npt          , iabt       , ic        , nchar     , i
       integer*4 ifail
       integer*4 nfirst       , maxwrd     , iwords
       integer*4 i4fctn       , i4unit
C-----------------------------------------------------------------------
       real*8    wno
       real*8    r8fctn
C-----------------------------------------------------------------------
       character string*(*)   , sstrng*15  , cpl*1
       character cdelim*7
C-----------------------------------------------------------------------
       logical   lset         , lwno         , lprnt        , lzeta
C-----------------------------------------------------------------------
       integer*4 ipla(ndmet)
       integer*4 ifirst(12)   , ilast(12)
C-----------------------------------------------------------------------
       real*8    zpla(ndmet)
C-----------------------------------------------------------------------
       data cdelim/' ()<>{}'/
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
       lset   = .false.
       lprnt  = .false.
       lzeta  = .false.
       npt    = 0
       nchar  = 0
       ifail  = 0
       wno    = 0.0d0

       do 10 ic=1,ndmet
        ipla(ic)=0
        zpla(ic)=0.0d0
   10  continue

       nfirst=1
       maxwrd=2*ndmet+1
       call xxword(string,cdelim,nfirst,maxwrd,ifirst,ilast,iwords)

       if(iwords.eq.0)then
           write(i4unit(-1),1001)'no excitation energy'
           write(i4unit(-1),1002)
           ifail = 1
           return
       elseif (iwords.eq.1) then
           wno=r8fctn(string(ifirst(1):ilast(1)),iabt)
           if(iabt.gt.0)then
               write(i4unit(-1),1001)'fault in excit. energy'
               write(i4unit(-1),1002)
               ifail = 1
               return
           endif
                npt=0
                cpl='x'
                ipla(1)=0
                zpla(1)=0.0
               ifail = 2
           return
       elseif (iwords.eq.2) then
           if((string(ifirst(2):ilast(2)).eq.'x').or. 
     &        (string(ifirst(2):ilast(2)).eq.'X'))  then
               wno=r8fctn(string(ifirst(1):ilast(1)),iabt)
               if(iabt.gt.0)then
                   write(i4unit(-1),1001)'fault in excit. energy'
                   write(i4unit(-1),1002)
                   ifail = 1
                   return
               endif
               npt=1
               cpl='x'
               ipla(1)=1
               zpla(1)=0.0
               ifail = 2
               return
           else
               wno=r8fctn(string(ifirst(1):ilast(1)),iabt)
               if(iabt.gt.0)then
                   write(i4unit(-1),1001)'fault in excit. energy'
                   write(i4unit(-1),1002)
                   ifail = 1
                   return
               endif
               npt=1
               ipla(1)=i4fctn(string(ifirst(2):ilast(2)),iabt)
               if(iabt.gt.0.or.ipla(1).gt.ndmet)then
                   write(i4unit(-1),1001)'fault in parent index '
                   write(i4unit(-1),1002)
                   ifail = 1
                   return
               endif
               cpl=string(ifirst(2):ilast(2))
               zpla(1)=1.0
               ifail = 2
               return
           endif
       endif
       wno=r8fctn(string(ifirst(1):ilast(1)),iabt)
       if(iabt.gt.0)then
           write(i4unit(-1),1001)'fault in excit. energy'
           write(i4unit(-1),1002)
           ifail = 1
           return
       endif

       if (mod(iwords-1,2).ne.0)then
           write(i4unit(-1),1001)'mismatch of parents'
           write(i4unit(-1),1002)
           ifail = 1
           return
       endif

       npt=(iwords-1)/2

       do 100 i=1,npt
        ipla(i)=i4fctn(string(ifirst(2*i):ilast(2*i)),iabt)
            if(iabt.gt.0)then
                write(i4unit(-1),1001)'fault in parent index '
                write(i4unit(-1),1002)
                ifail = 1
                return
            endif
        zpla(i)=r8fctn(string(ifirst(2*i+1):ilast(2*i+1)),iabt)
            if(iabt.gt.0)then
                write(i4unit(-1),1001)'fault in zeta value   '
                write(i4unit(-1),1002)
                ifail = 1
                return
            endif
  100  continue
       cpl=string(ifirst(2):ilast(2))
       return
C-----------------------------------------------------------------------
 1001 format(1x,32('*'),' xxprs1 error ',32('*')//
     &       1x,'fault in input data file: ',a,i3,a)
 1002 format(/1x,27('*'),' subroutine terminated ',28('*'))
C-----------------------------------------------------------------------
      end

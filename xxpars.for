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
       subroutine xxpars( ndmet  , strng1 , npt    , bwnoa   , lseta   ,
     &                    prtwta , cprta  , ifail  , itype)
c
       implicit none
c-----------------------------------------------------------------------
c
c  ****************** fortran77 subroutine: xxpars *********************
c
c  purpose:  to analyse the tail character string of the first line of
c            a specific ion file into binding wave numbers for different
c            parents and statistical weights for the parents.
c
c            unified version of b9pars, bapars & g5pars and a   
c            replacement for these subroutines.
c
c  calling program: various
c
c
c  subroutine:
c
c  input : (i*4)  ndmet    =  maximum number of metastables allowed
c  input : (c*(*))strng1   =  string to be parsed
c
c  output: (i*4)  npt      =  number of binding wave numbers detected
c  output: (l*4)  lseta()  =  .true.  - parent term set for this w.no.
c                             .false. - parent term not set for w.no.
c  output: (l*4)  lfnd     =  .true.  - l quantum number present in 
c                                       string
c                             .false. - no l quantum number detected
c  output: (r*8)  bwnoa()  =  binding wave numbers
c  output: (r*8)  prtwta() =  parent statistical weights
c  output: (c*(*))cprta()  =  parent name in brackets
c  output: (i*4)  ifail    =  0 - subroutine concludes correctly
c                             1 - fault detected in subroutine
c                             2 - single ionisation potential detected
c  output: (i*4)  itype    =  resolution of parent metastables
c                             1 - ls resolved
c                             2 - lsj resolved
c                             3 - arbitrary resolution
c
c          (i*4)  maxwrd   =  maximum number of words sought initially
c                             initially, finally number actually found
c          (i*4)  nfirst   =  first word to be extracted from string
c          (i*4)  ifirst() =  index of first char. of word () in string
c          (i*4)  ilast()  =  index of last  char. of word () in string
c          (i*4)  iwords   =  number of words found in string
c          (i*4)  iabt     =  failure number from r8fctn
c          (i*4)  nchar    =  number of characters in substring
c          (i*4)  i        =  general use
c          (i*4)  j        =  general use
c          (i*4)  k        =  general use
c          (i*4)  ic       =  general use
c          (i*4)  itp      =  flag for incompatible types
c          (i*4)  ityp     =  copy of current itype
c          (i*4)  kmrk     =  position marker in the string for parent
c                             l quantum number
c          (i*4)  itypea() =  resolution of each parent
c          (r*8)  twta()   =  (2L+1) value for parent L quantum number
c          (c*1)  ctrma()  =  parent L quantum number letter set
c                             (inclusive convention for 'l'=j in set of 
c                              character values for 'l' and extended
c                              ctrma, twta vectors)
c routines:
c          routine    source    brief description
c          -------------------------------------------------------------
c          i4unit     adas      fetch unit number for output of messages
c          r8fctn     adas      converts from character to real variable
c          i4fctn     adas      converts from char. to integer  variable
c          xxword     adas      parses a string into separate words
c                               for ' ()<>{}' delimiters
c          xxslen     adas      finds the length of a string excluding
c                               leading and trailing blanks 
c          xxrmve     adas      removes a character from a string
c          xxcase     adas      change string to upper or lower case
C
c
c author:  hp summers
c          JA7.08, University of Strathclyde
c          Tel: 0141-548-4196
c
c date:    30/01/03
c
c update:  22/11/04 - hp summers - corrected error in write back of 
c                                  cprta strings for the unified itype  
c
c update:  17/05/07 - ad whiteford - Updated comments as part of
C                                    subroutine documentation
C                                    procedure.
c
c-----------------------------------------------------------------------
       integer*4 idmet
c-----------------------------------------------------------------------
       parameter ( idmet = 10 )       
c-----------------------------------------------------------------------
       integer*4 ndmet
c-----------------------------------------------------------------------
       integer*4 npt          , iabt       , ic        , i     , ifail
       integer*4 nfirst       , maxwrd     , iwords    , itype , itp  
       integer*4 kmrk      , j     , k
       integer*4 i4fctn       , i4unit     , ityp
       integer*4 len_cprt     , len_c12a   , lenstr
c-----------------------------------------------------------------------
       real*8    r8fctn
c-----------------------------------------------------------------------
       character strng1*(*)   , sstrng*15  , ctrma(22)*1
c-----------------------------------------------------------------------
       logical   lseta(ndmet) , lfnd       , linside
c-----------------------------------------------------------------------
       integer*4 ifirst(12)   , ilast(12)  , itypea(idmet) 
c-----------------------------------------------------------------------
       real*8    bwnoa(ndmet) , prtwta(ndmet)          , twta(22)
c-----------------------------------------------------------------------
       character string*100   , strng2*100 , c12*12
       character cdelim*7     , cprta(ndmet)*(*)
       character c12a(idmet)*12    
c-----------------------------------------------------------------------
       data  ctrma/ 'S' , 'P' , 'D' , 'F' , 'G' , 'H' , 'I' , 'J' , 'K',
     &              'L' , 'M' , 'N' , 'O' , 'Q' , 'R' , 'T' , 'U' , 'V',
     &              'W' , 'X' , 'Y' , 'Z' /
       data  twta / 1.0 , 3.0 , 5.0 , 7.0 , 9.0 , 11.0, 13.0, 15.0,17.0,
     &             19.0 ,21.0 ,23.0 ,25.0 ,27.0 , 29.0, 31.0, 33.0,35.0,
     &             37.0 ,39.0 ,41.0 ,43.0 /
       data cdelim/' ()<>{}'/
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
       if (idmet.lt.ndmet) then
           write(i4unit(-1),1003)idmet, ndmet
           write(i4unit(-1),1004)
           stop
       endif
           
       npt    = 0
       ifail  = 0
       len_cprt = len(cprta(1))
c
       do 10 i =1,ndmet
        bwnoa(i)=0.0d0
        lseta(i)=.false.
        prtwta(i)=0.0d0
   10  continue
   
c-----------------------------------------------------------------------
c  check for blanks within the bracketted parent descriptor and remove
c-----------------------------------------------------------------------
c       string = strng1
c       strng2=' '
c       call xxslen(string,ifirst(1),ilast(1))
c       
c       ic=1
c       linside = .false.
c       do i=ifirst(1),ilast(1)
c         if(.not.linside.and.(string(i:i).eq.'(')) then
c             linside=.true.
c             strng2(ic:ic)=string(i:i)
c             ic=ic+1
c         elseif(linside.and.(string(i:i).eq.')')) then
c             linside = .false.
c             strng2(ic:ic)=string(i:i)
c             ic=ic+1
c         elseif(linside.and.(string(i:i).eq.' ')) then
c             continue
c         else
c             strng2(ic:ic)=string(i:i)
c             ic=ic+1
c         endif
c       enddo

             call xxrmve(strng1,strng2,' ')       
             call xxcase(strng2,string,'uc')
c             string=strng2
c	     write(i4unit(-1),*)'string=',string
         
c-----------------------------------------------------------------------
c  isolate ionisation potentials and parent descriptor strings
c-----------------------------------------------------------------------

       nfirst=1
       maxwrd=2*ndmet
       call xxword(string,cdelim,nfirst,maxwrd,ifirst,ilast,iwords)
c
       if(iwords.eq.0)then
           write(i4unit(-1),1001)'no ionisation potential'
           write(i4unit(-1),1002)
           ifail = 1
           return
       elseif (iwords.eq.1) then
           bwnoa(1)=r8fctn(string(ifirst(1):ilast(1)),iabt)
           if(iabt.gt.0)then
               write(i4unit(-1),1001)'fault in ionis. potential'
               write(i4unit(-1),1002)
               ifail = 1
               return
           endif
	   
c------------------------------
c           npt=0 trapped
c------------------------------
           cprta(1)='(1s)'
           prtwta(1)=1.0
           lseta(1)=.true.
           write(i4unit(-1),1005)'no parent specified - 1s forced'
           write(i4unit(-1),1006)
	   npt=1
	   itypea(1)=1
	   itype=1
           ifail = 2
           return
       endif
c------------------------------------
c           unmatched parents trapped
c------------------------------------
       if (mod(iwords,2).ne.0)then
           write(i4unit(-1),1001)'mismatch of parents'
           write(i4unit(-1),1002)
           ifail = 1
           return
       endif
c
c--------------------------------------------------------------
c          analyse parent ionisation potential/assignment pairs
c--------------------------------------------------------------
       npt=iwords/2
       do 100 i = 1,npt
        itypea(i)=0
        c12=' '
        bwnoa(i)=r8fctn(string(ifirst(2*i-1):ilast(2*i-1)),iabt)
        if(iabt.gt.0)then
            write(i4unit(-1),1001)'fault in ionis. potential',i
            write(i4unit(-1),1002)
            ifail = 1
            return
        endif
c-----------------------------------------------------------------------
c detect whether l quantum number is present in the string 
c-----------------------------------------------------------------------
       lfnd = .false.
         do 12 k = ifirst(2*i),ilast(2*i)
           do 13 j = 1,22
             if(string(k:k).eq.ctrma(j))then
               lfnd = .true.
               kmrk = k
             endif
 13        continue
 12      continue
c-----------------------------------------------------------------------
c decide on resolution type 
c-----------------------------------------------------------------------
       if(.not.lfnd)then
         itypea(i) = 3
       else
         if(kmrk.eq.ilast(2*i))then
           itypea(i) = 1
         else
           itypea(i) = 2 
         endif  
       endif     
c-----------------------------------------------------------------------
c specific parent weight handling for different types
c-----------------------------------------------------------------------
        if(itypea(i).eq.1)then
          do 20 ic=1,22
           if(string(ilast(2*i):ilast(2*i)).eq.ctrma(ic))then
               prtwta(i)=i4fctn(string(ifirst(2*i):
     &                     ilast(2*i)-1),iabt)*twta(ic)
               if(iabt.gt.0)then
                   write(i4unit(-1),1001)'fault in parent term',i
                   write(i4unit(-1),1002)
                   ifail = 1
                   return
               else
                   lseta(i)=.true.
               endif
               c12 = string(ifirst(2*i)-1:ilast(2*i))
               c12(12:12) = ')'
c               write(c12(5:11),'(f7.1)')prtwta(i)
               call xxrmve(c12,c12a(i),' ')
           endif
   20     continue
c
        else if(itypea(i).eq.2)then
          do 21 ic = 1,22
            if(string(kmrk:kmrk).eq.ctrma(ic))then
              prtwta(i)=r8fctn(string(kmrk+1:ilast(2*i)),iabt)
              if(iabt.gt.0)then
                  write(i4unit(-1),1001)'fault in parent term',i
                  write(i4unit(-1),1002)
                  ifail = 1
                  return
              else
                  lseta(i)=.true.
              endif
              c12 = string(ifirst(2*i)-1:ilast(2*i)+1)
              call xxrmve(c12,c12a(i),' ')
            endif
   21     continue
c
        else if(itypea(i).eq.3)then
              prtwta(i) = r8fctn(string(ifirst(2*i):ilast(2*i)),iabt)
              if(iabt.gt.0)then
                  write(i4unit(-1),1001)'fault in parent term',i
                  write(i4unit(-1),1002)
                  ifail = 1
                  return
              else
                  lseta(i)=.true.
              endif
              c12 = string(ifirst(2*i)-1:ilast(2*i)+1)
              call xxrmve(c12,c12a(i),' ')
        endif
  100 continue
c-----------------------------------------------------------------------
c   check consistency of resolution and copy back to cprta()
c-----------------------------------------------------------------------
        itype=itypea(1)
       
        do i = 1,npt
          if(itypea(i).ne.itype) then
	      itype = 3
	      go to 25
	  endif
        enddo
       
   25   len_c12a = 0
        do i=1,npt
           if(itypea(i).ne.itype) then
	       itypea(i) = 3
	       write(c12,'(''('',f10.1,'')'')')prtwta(i)
	       call xxrmve(c12,c12a(i),' ')
	       call xxslen(c12a(i),ifirst(1),ilast(1))
	       lenstr=ilast(1)-ifirst(1)+1
	       len_c12a=max0(len_c12a,lenstr)
	   endif
	enddo
	
	if((len_c12a.gt.len_cprt).or.(itype.eq.3)) then
	       itype = 3
	       do i = 1,npt
	         write(c12,'(''('',f10.1,'')'')')prtwta(i)
	         call xxrmve(c12,cprta(i),' ')
	       enddo
	elseif (itype.eq.2) then
	       do i=1,npt
	         cprta(i)= c12a(i)
	       enddo
	else
	       do i=1,npt
	         call xxslen(c12a(i),ifirst(1),ilast(1))
		 kmrk=0
                 do k = ifirst(1)+1,ilast(1)-1
                   do j = 1,22
                     if(c12a(i)(k:k).eq.ctrma(j)) kmrk = k
                   enddo
                 enddo
		 cprta(i)='('//c12a(i)(ifirst(1)+1:kmrk)//')'
	       enddo
	endif       
	      
       return
c-----------------------------------------------------------------------
 1001 format(1x,32('*'),' xxpars error ',32('*')//
     &       1x,'fault in input data file: ',a,i3,a)
 1002 format(/1x,27('*'),' subroutine terminated ',28('*'))
 1003 format(1x,32('*'),' xxpars error ',32('*')//
     &       1x,'internal parameter idmet = ',i3,' < ndmet = ',i3)
 1004 format(/1x,27('*'),' program terminated   ',28('*'))
 1005 format(1x,32('*'),'xxpars warning',32('*')//
     &       1x,'fault in input data file: ',a,i3,a)
 1006 format(/1x,27('*'),' subroutine continues ',28('*'))
c-----------------------------------------------------------------------
      end

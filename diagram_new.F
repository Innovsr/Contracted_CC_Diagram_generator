#define maximum  26
#define ind   900000
#define id   100
#define MX    900000

      program main
      implicit real*8(a-h,o-z)
      character operator*6,operator1(10)*6
      dimension row(100,maximum),row_new(ind,maximum)
     :,checkk(MX,maximum),check1_new(MX),row_final(MX,maximum),
     :check_new(MX,maximum),check_new2(MX,maximum),checkk2(MX,maximum)
     :,row_norm(MX,maximum),row_miror(MX,maximum)
     :,row_1(MX,maximum),row_end(MX,maximum),row_final1(MX,maximum)
     :,row_final2(MX,maximum)
    
cccc   T2dag = [3,4,5,6], T2 = [25,26,27,28], T1dag = [1,2], T1 = [23,24]
cccc   S2dag = [9,10,11,31], S2 = [12,32,33,34], S1dag = [7,29], S1 = [8,30]
cccc   Oph = [13,14], Ohp = [35,36], Opp = [16,38], Ohh = [15,37]
       print*,'how many operators u want to put here'
       read(*,*)n
       l=2
       ll=1
       do i=1,n
       print*,'put the the operator name',i
       read(*,'(a)'),operator
        operator1(i)=operator
       if (operator.eq.'T1dag') then
       
         row(ll,l+1)=1
         row(ll,l+2)=2
         l=l+2
        endif
       if (operator.eq.'T2dag') then
      
         row(ll,l+1)=3
         row(ll,l+2)=4
         row(ll,l+3)=5
         row(ll,l+4)=6
         l=l+4
        endif
       if (operator.eq.'T1') then
        
         row(ll,l+1)=23
         row(ll,l+2)=24
        l=l+2
        endif
       if (operator.eq.'T2') then
      
         row(ll,l+1)=25
         row(ll,l+2)=26
         row(ll,l+3)=27
         row(ll,l+4)=28
         l=l+4
        endif
       if (operator.eq.'S2') then
      
         row(ll,l+1)=12
         row(ll,l+2)=32
         row(ll,l+3)=33
         row(ll,l+4)=34
         l=l+4
        endif
       if (operator.eq.'S2dag') then
      
         row(ll,l+1)=9
         row(ll,l+2)=10
         row(ll,l+3)=11
         row(ll,l+4)=31
         l=l+4
        endif
       if (operator.eq.'S1') then
        
         row(ll,l+1)=8
         row(ll,l+2)=30
        l=l+2
        endif
       if (operator.eq.'S1dag') then
       
         row(ll,l+1)=7
         row(ll,l+2)=29
         l=l+2
        endif
       enddo
        do ll=1,4
         do lll=3,l
         row(ll,lll)=row(1,lll)
         enddo
         if(ll.eq.1)then
         row(ll,1)=13
         row(ll,2)=14
         endif
         if(ll.eq.2)then
         row(ll,1)=35
         row(ll,2)=36
         endif
         if(ll.eq.3)then
         row(ll,1)=15
         row(ll,2)=37
         endif
         if(ll.eq.4)then
         row(ll,1)=16
         row(ll,2)=38
         endif
      print*,(row(ll,nn),nn=1,l)
      
        enddo 
         max=l
          print*,'max',max

cccc   "row" is total set coming from the combination ccccc
cccc   "row_new" is the new permissible set of diagrams ccccc



       do 600 mal=1,4
c      do 600 mal=2,2
        kkk=1
       mmm=0
      do i=1,ind
       do j=1,maximum
      row_new(i,j)=0.0
       enddo
      enddo

      do i=1,ind
       do j=1,maximum
      row_final(i,j)=0.0
       enddo
      enddo
      do i=1,ind
       do j=1,maximum
      row_final1(i,j)=0.0
       enddo
      enddo
      do i=1,ind
       do j=1,maximum
      row_final2(i,j)=0.0
       enddo
      enddo
       j=0
       do i=1,max
      if((row(mal,i)-21).le.0) goto 255
       j=j+1
       row_new(j,1)=row(mal,i)
c     print*,(row_new(j,nn),nn=1,max),j
 255      enddo
c      print*,'jjjjjj',j 
   
cccc  for the first element ccccccc
      do 900  mm=1,j
c     do 900 mm=1,1
      iiiii=0
      it =2
      i = j
      iii= 0
      mmm=0
      j=0
    
c     print*,'1st',(row_new(mm,nn),nn=1,max),mm
c     print*,'*****************'
     

cccc  "it" is the column identity ccccccc
cccc  start of the loop of second element ccccc    



      do 100 j=1,max
c     print*,'row(mal,j)',row(mal,j),mal,j
      if((row(mal,j)-21)*(row_new(mm,it-1)-21).gt.0) goto 100
c     print*,'mm',mm
      if(mod(row(mal,j),2).ne.mod(row_new(mm,it-1),2)) goto 100      
c      if(row(mal,j).eq.row_new(mm,it-1))goto 100
      if(row_new(mm,it-1).eq.15.and.row(mal,j).eq.37)goto 100
      if(row_new(mm,it-1).eq.37.and.row(mal,j).eq.15)goto 100
      if(row_new(mm,it-1).eq.16.and.row(mal,j).eq.38)goto 100
      if(row_new(mm,it-1).eq.38.and.row(mal,j).eq.16)goto 100
      if(row_new(mm,it-1).eq.7.and.row(mal,j).eq.29)goto 100
      if(row_new(mm,it-1).eq.29.and.row(mal,j).eq.7)goto 100
      if(row_new(mm,it-1).eq.8.and.row(mal,j).eq.30)goto 100
      if(row_new(mm,it-1).eq.30.and.row(mal,j).eq.8)goto 100
      
      row_new(i+1,it)=row(mal,j)
      row_new(i+1,1)=row_new(mm,1)
      do ii=it+1,max
      row_new(i+1,ii)=0
      enddo
c     print*,'mal',mal,mm
c     print*,(row_new(i+1,nn),nn=1,max),mal,i+1
c     print*,(row(mal,nn),nn=1,max)
      i=i+1
      mmm=mmm+1
 100  continue      

cccc end of the loop for 2nd element ccccccccccc

      iiiii=i
      kk=0
      llll=i
      mmmm=i

c     print*,(row_new(mm,nn),nn=1,max),mm


         lll=0
 149     set=0
c      print*,'2222222222222',it
       m=0
      do 200 i=mmmm+1-mmm,llll
       set=0
 150  do ll=1,it
c     print*,'row_new,row,it,ll',row_new(i,ll),row(mal,set+1),it,ll
c     print*,'set+1',set+1
      if(set+1.eq.max)then
      if((row(mal,set+1)-21).le.0) goto 250
      endif
      if(row(mal,set+1).eq.row_new(i,ll))goto 250
       
      enddo


cccc  end of cheking cccccccccc
     
 400  row_new(iiiii+1,it+1)=row(mal,set+1)
      do j=1,it
      row_new(iiiii+1,j)=row_new(i,j)
      enddo
      do ii=it+2,max
      row_new(iiiii+1,ii)=0
      enddo
c     print*,(row_new(iiiii+1,nn),nn=1,max),mal,iiiii+1
c     print*,'iiiii+1',iiiii+1,it,m
      iiiii=iiiii+1
      m=m+1
 250  set=set+1
      if(set+1.eq.max+1)goto 200
      goto 150
 200  enddo
       mmmm=iiiii
       iii=m
       mmm=0
       set=0
c      print*,'it,max-2',it,max-2
      if(it.eq.max-2)then
      goto 850
      endif
      do 700 i=iiiii+1-m,iiiii
      do 700 jj=1,max
c      print*,'row_new',(row_new(i,ll),ll=1,max),i,iiiii,m
      do l=1,it
      if(row(mal,jj).eq.row_new(i,l)) goto 700
      enddo
      if((row(mal,jj)-21)*(row_new(i,it+1)-21).gt.0) goto 700
      if(mod(row(mal,jj),2).ne.mod(row_new(i,it+1),2)) goto 700      
      if(row_new(i,it+1).eq.15.and.row(mal,jj).eq.37)goto 700
      if(row_new(i,it+1).eq.37.and.row(mal,jj).eq.15)goto 700
      if(row_new(i,it+1).eq.16.and.row(mal,jj).eq.38)goto 700
      if(row_new(i,it+1).eq.38.and.row(mal,jj).eq.16)goto 700
      if(row_new(i,it+1).eq.7.and.row(mal,jj).eq.29)goto 700
      if(row_new(i,it+1).eq.29.and.row(mal,jj).eq.7)goto 700
      if(row_new(i,it+1).eq.8.and.row(mal,jj).eq.30)goto 700
      if(row_new(i,it+1).eq.30.and.row(mal,jj).eq.8)goto 700

      row_new(mmmm+1,it+2)=row(mal,jj)
      do j=1,it+1
      row_new(mmmm+1,j)=row_new(i,j)
      enddo

c     print*,(row_new(mmmm+1,nn),nn=1,max),mmmm+1
c     print*,'mmmm+1',mmmm+1
      
      mmmm=mmmm+1
      mmm=mmm+1
 700  continue

c     print*,'*****mmmm',mmmm
      it=it+2
     
      llll=mmmm
      iiiii=mmmm
      goto 149


 850     do 900 iiii=mmmm+1-m,mmmm
      do 900 jjj=1,max
      do l=1,it+1
      if(row(mal,jjj).eq.row_new(iiii,l)) goto 900
      enddo
      if(row_new(iiii,max-1).eq.1.and.row(mal,jjj).eq.2) goto 900
      if(row_new(iiii,max-1).eq.2.and.row(mal,jjj).eq.1) goto 900
      if(row_new(iiii,max-1).eq.24.and.row(mal,jjj).eq.23) goto 900
      if(row_new(iiii,max-1).eq.23.and.row(mal,jjj).eq.24) goto 900
      if(row_new(iiii,max-1).eq.13.and.row(mal,jjj).eq.14) goto 900
      if(row_new(iiii,max-1).eq.14.and.row(mal,jjj).eq.13) goto 900
      if(row_new(iiii,max-1).eq.35.and.row(mal,jjj).eq.36) goto 900
      if(row_new(iiii,max-1).eq.36.and.row(mal,jjj).eq.35) goto 900
      if(row_new(iiii,max-1).eq.16.and.row(mal,jjj).eq.38) goto 900
      if(row_new(iiii,max-1).eq.38.and.row(mal,jjj).eq.16) goto 900
      if(row_new(iiii,max-1).eq.15.and.row(mal,jjj).eq.37) goto 900
      if(row_new(iiii,max-1).eq.37.and.row(mal,jjj).eq.15) goto 900
      if(row_new(iiii,max-1).eq.7.and.row(mal,jjj).eq.29) goto 900
      if(row_new(iiii,max-1).eq.29.and.row(mal,jjj).eq.7) goto 900
      if(row_new(iiii,max-1).eq.8.and.row(mal,jjj).eq.30) goto 900
      if(row_new(iiii,max-1).eq.30.and.row(mal,jjj).eq.8) goto 900

c     print*,jjj 
      t=0
      do i=1,n
      if(operator1(i).eq.'S2'.or.operator1(i).eq.'S2dag'.or.
     : operator1(i).eq.'S1'.or.operator1(i).eq.'S1dag')goto 516
       t=t+1
 516  enddo
      if(t.eq.n)then
c     print*,t
      if((row(mal,jjj)-21)*(row_new(iiii,max-1)-21).gt.0.and
     :.mod(row(mal,jjj),2).eq.mod(row_new(iiii,max-1),2)) goto 900      
      if((row(mal,jjj)-21)*(row_new(iiii,max-1)-21).le.0.and
     :.mod(row(mal,jjj),2).ne.mod(row_new(iiii,max-1),2)) goto 900      
      goto 901
      endif
      if((row(mal,jjj)-21)*(row_new(iiii,max-1)-21).gt.0)goto 900
      if(mod(row(mal,jjj),2).ne.0.and.mod(row_new(iiii,max-1),2).ne.0)
     : goto 900      

 901   row_new(iiiii+1,max)=row(mal,jjj)
      do j=1,max-1
      row_new(iiiii+1,j)=row_new(iiii,j)
      enddo
c     print*,'*******',(row_new(iiiii+1,nn),nn=1,max),iiiii+1
c     print*,'iiiii+1,kkk',iiiii+1,kkk
c     print*,'sourav'
      
      do nn=1,max
      row_final(kkk,nn)=row_new(iiiii+1,nn)
      enddo
c     print*,(row_final(kkk,nn),nn=1,max),kkk
      
      kkk=kkk+1
 910  iiiii=iiiii+1
 900  continue
      print*,'********************',kkk
c600  continue
      print*,'kk',kkk
      
      do i=1,ind
       do j=1,maximum
      row_miror(i,j)=0.0
       enddo
      enddo


      do ii=2,kkk-1
      do nn=1,max
      if(row_final(ii,nn).ne.3.or.row_final(ii,nn).ne.5.or.
     : row_final(ii,nn).ne.4.or.row_final(ii,nn).ne.6.or.
     : row_final(ii,nn).ne.25.or.row_final(ii,nn).ne.26.or.
     : row_final(ii,nn).ne.27.or.row_final(ii,nn).ne.28)then
       row_miror(ii,nn)=row_final(ii,nn)
       endif
      if(row_final(ii,nn).eq.3)then
       row_miror(ii,nn)=5
      endif
      if(row_final(ii,nn).eq.5)then
       row_miror(ii,nn)=3
      endif
      if(row_final(ii,nn).eq.4)then
       row_miror(ii,nn)=6
      endif
      if(row_final(ii,nn).eq.6)then
       row_miror(ii,nn)=4
      endif
      if(row_final(ii,nn).eq.25)then
       row_miror(ii,nn)=27
      endif
      if(row_final(ii,nn).eq.26)then
       row_miror(ii,nn)=28
      endif
      if(row_final(ii,nn).eq.27)then
       row_miror(ii,nn)=25
      endif
      if(row_final(ii,nn).eq.28)then
       row_miror(ii,nn)=26
      endif
      enddo 
c       print*,'row_final ',ii,(row_final(ii,nn),nn=1,max)
c       print*,'row_miror',ii,(row_miror(ii,nn),nn=1,max)
       enddo
       do i=1,kkk-1
        do j=1,max-2,2
c      do j=1,max,2
       check1=row_miror(i,j)*row_miror(i,j)+row_miror(i,j+1)
     :*row_miror(i,j+1)
       checkk2(i,j)=check1
      check1=0
       enddo
       enddo
        print*,'row_miror',kk+1,(row_miror(kk+1,nn),nn=1,max),
      : (check1_new(j),j=1,max-2,2)
     

cccc  fix up the set identity cccccccccccccccc

c       check=0
c      check1=0
       do i=1,kkk-1
       do j=1,max-2,2
c      do j=1,max,2
       check=row_final(i,j)*row_final(i,j)+row_final(i,j+1)
     :*row_final(i,j+1)
       checkk(i,j)=check
       check=0
       enddo
       enddo
cccccccccccccccccccccccccccccccccccccccccccccccccc
ccccc  checking of douplicate set starts ccccccccccccccc

      do i=1,max
      row_1(1,i)=row_final(1,i)
      enddo
      print*,'the final set ****','(1)',(row_final(1,nn),nn=1,max),
     : (checkk(1,j),j=1,max-2,2)
      do nn=1,max
      row_final1(1,nn)=row_final(1,nn)
      enddo
      do nn=1,max
      row_final2(1,nn)=row_final(1,nn)
      enddo
       do nn=1,max
       row_norm(1,nn)=row_final(1,nn)
       enddo
        do i=1,max-2,2
c       do i=1,max,2
       check_new(1,i)=checkk(1,i)
       enddo
       kk=1
       flg=0
      flg1=0
      do ii=2,kkk-1
      do j=1,kk
      do i=1,max-2,2
      do k=1,max-2,2
c     do i=1,max,2
c     do k=1,max,2
      if(check_new(j,i).eq.checkk(ii,k))then
      flg=flg+1
      endif
      enddo
      enddo
c     print*,'*********',flg
      if(flg.eq.(max-2)/2) goto 333
c     if(flg.eq.(max)/2) goto 333
       flg=0
      enddo
      t=0
      do i=1,n

cccccc  as T2 and T2dag can generate mirror duplicate set so need farther
cccccc  varification 

      if(operator1(i).eq.'T2'.or.operator1(i).eq.'T2dag')goto 515
c    : operator1(i).eq.'S2'.or.operator1(i).eq.'S2dag')goto 515
       t=t+1
 515  enddo
      if(t.eq.n)then
      print*,'the final set no','(',kk+1,')',(row_final(ii,nn),
     : nn=1,max),(checkk(ii,i),i=1,max-2,2),ii
      endif
      do j=1,kk
      do i=1,max-2,2
      do k=1,max-2,2
c     dO i=1,max,2
c     do k=1,max,2
      if(check_new(j,i).eq.checkk2(ii,k))then
      flg1=flg1+1
      endif
      enddo
      enddo
c     print*,'*********',flg
      if(flg1.eq.(max-2)/2) goto 333
c     if(flg1.eq.(max)/2) goto 333
       flg1=0
      enddo
cccc checking of mirror set start ccccccccccccc

      do nn=1,max
      row_final1(kk+1,nn)=row_final(ii,nn)
      enddo

c       print*,'the final set',kk,(row_final(ii,nn),
c    : nn=1,max),(checkk(ii,j),j=1,max-2,2)
       kk=kk+1
      do j=1,max-2,2
c     do j=1,max,2
      check_new(kk,j)=checkk(ii,j)
      enddo
 333  flg=0
      flg1=0
      enddo
       if(t.eq.n) goto 600

      print*,'kkmiddle',kk
      ll=kk
      kk=1

      do i=1,ind
       do j=1,maximum
      row_miror(i,j)=0.0
       enddo
      enddo


      do i=1,ind
       do j=1,maximum
      checkk(i,j)=0.0
       enddo
      enddo


      do i=1,ind
       do j=1,maximum
      checkk2(i,j)=0.0
       enddo
      enddo


      do i=1,ind
       do j=1,maximum
      check_new(i,j)=0.0
       enddo
      enddo

      do ii=2,ll
      do nn=1,max
      if(row_final1(ii,nn).ne.3.or.row_final1(ii,nn).ne.5.or.
     : row_final1(ii,nn).ne.4.or.row_final1(ii,nn).ne.6)then
       row_miror(ii,nn)=row_final1(ii,nn)
       endif
      if(row_final1(ii,nn).eq.3)then
       row_miror(ii,nn)=5
      endif
      if(row_final1(ii,nn).eq.5)then
       row_miror(ii,nn)=3
      endif
      if(row_final1(ii,nn).eq.4)then
       row_miror(ii,nn)=6
      endif
      if(row_final1(ii,nn).eq.6)then
       row_miror(ii,nn)=4
      endif
      enddo 
       enddo
       do i=1,ll
        do j=1,max-2,2
c      do j=1,max,2
       check1=row_miror(i,j)*row_miror(i,j)+row_miror(i,j+1)
     :*row_miror(i,j+1)
       checkk2(i,j)=check1
      check1=0
       enddo
       enddo
c       print*,'row_miror',kk+1,(row_miror(kk+1,nn),nn=1,max),
c     : (check1_new(j),j=1,max-2,2)
     

cccc  fix up the set identity cccccccccccccccc

c       check=0
c      check1=0
       do i=1,ll
       do j=1,max-2,2
c      do j=1,max,2
       check=row_final1(i,j)*row_final1(i,j)+row_final1(i,j+1)
     :*row_final1(i,j+1)
       checkk(i,j)=check
       check=0
       enddo
       enddo
cccccccccccccccccccccccccccccccccccccccccccccccccc
ccccc  checking of douplicate set starts ccccccccccccccc

      do i=1,max
      row_1(1,i)=row_final1(1,i)
      enddo
c     print*,'the final set','(1)',(row_final1(1,nn),nn=1,max),
c    : (checkk(1,j),j=1,max-2,2)
       do nn=1,max
       row_norm(1,nn)=row_final1(1,nn)
       enddo
        do i=1,max-2,2
c       do i=1,max,2
       check_new(1,i)=checkk(1,i)
       enddo
       kk=1
       flg=0
      flg1=0
      do ii=2,ll
      do j=1,kk
      do i=1,max-2,2
      do k=1,max-2,2
c     do i=1,max,2
c     do k=1,max,2
      if(check_new(j,i).eq.checkk(ii,k))then
      flg=flg+1
      endif
      enddo
      enddo
c     print*,'*********',flg
c     if(flg.eq.(max-2)/2) goto 334
      if(flg.eq.(max)/2) goto 334
       flg=0
      enddo
      do j=1,kk
      do i=1,max-2,2
      do k=1,max-2,2
c     dO i=1,max,2
c     do k=1,max,2
      if(check_new(j,i).eq.checkk2(ii,k))then
      flg1=flg1+1
      endif
      enddo
      enddo
c     print*,'*********',flg
      if(flg1.eq.(max-2)/2) goto 334
c     if(flg1.eq.(max)/2) goto 334
       flg1=0
      enddo
cccc checking of mirror set start ccccccccccccc
c     print*,'the final1 set','(',kk+1,')',(row_final1(ii,nn),
c    : nn=1,max),(checkk(ii,i),i=1,max-2,2),ii

      do nn=1,max
      row_final2(kk+1,nn)=row_final1(ii,nn)
      enddo

c       print*,'the final set',kk,(row_final1(ii,nn),
c    : nn=1,max),(checkk(ii,j),j=1,max-2,2)
       kk=kk+1
      do j=1,max-2,2
c     do j=1,max,2
      check_new(kk,j)=checkk(ii,j)
      enddo
 334  flg=0
      flg1=0
      enddo

      print*,'kklast',kk
      
      mm=kk

      do i=1,ind
       do j=1,maximum
      row_miror(i,j)=0.0
       enddo
      enddo


      do i=1,ind
       do j=1,maximum
      checkk(i,j)=0.0
       enddo
      enddo


      do i=1,ind
       do j=1,maximum
      checkk2(i,j)=0.0
       enddo
      enddo


      do i=1,ind
       do j=1,maximum
      check_new(i,j)=0.0
       enddo
      enddo

      do ii=2,mm
      do nn=1,max
      if(row_final2(ii,nn).ne.25.or.row_final2(ii,nn).ne.26.or.
     : row_final2(ii,nn).ne.27.or.row_final2(ii,nn).ne.28)then
       row_miror(ii,nn)=row_final2(ii,nn)
       endif
      if(row_final2(ii,nn).eq.25)then
       row_miror(ii,nn)=27
      endif
      if(row_final2(ii,nn).eq.26)then
       row_miror(ii,nn)=28
      endif
      if(row_final2(ii,nn).eq.27)then
       row_miror(ii,nn)=25
      endif
      if(row_final2(ii,nn).eq.28)then
       row_miror(ii,nn)=26
      endif
      enddo 
c       print*,'row_final2',ii,(row_final2(ii,nn),nn=1,max)
c       print*,'row_miror',ii,(row_miror(ii,nn),nn=1,max)
       enddo
       do i=1,mm
        do j=1,max-2,2
c      do j=1,max,2
       check1=row_miror(i,j)*row_miror(i,j)+row_miror(i,j+1)
     :*row_miror(i,j+1)
       checkk2(i,j)=check1
      check1=0
       enddo
       enddo
     

cccc  fix up the set identity cccccccccccccccc

c       check=0
c      check1=0
       do i=1,mm
       do j=1,max-2,2
c      do j=1,max,2
       check=row_final2(i,j)*row_final2(i,j)+row_final2(i,j+1)
     :*row_final2(i,j+1)
       checkk(i,j)=check
       check=0
       enddo
       enddo
cccccccccccccccccccccccccccccccccccccccccccccccccc
ccccc  checking of douplicate set starts ccccccccccccccc

      do i=1,max
      row_1(1,i)=row_final2(1,i)
      enddo
c     print*,'the final set','(1)',(row_final2(1,nn),nn=1,max),
c    : (checkk(1,j),j=1,max-2,2)
       do nn=1,max
       row_norm(1,nn)=row_final2(1,nn)
       enddo
        do i=1,max-2,2
c       do i=1,max,2
       check_new(1,i)=checkk(1,i)
       enddo
       kk=1
       flg=0
      flg1=0
      do ii=2,mm
      do j=1,kk
      do i=1,max-2,2
      do k=1,max-2,2
c     do i=1,max,2
c     do k=1,max,2
      if(check_new(j,i).eq.checkk(ii,k))then
      flg=flg+1
      endif
      enddo
      enddo
c     print*,'*********',flg
      if(flg.eq.(max-2)/2) goto 335
c     if(flg.eq.(max)/2) goto 335
       flg=0
      enddo
      do j=1,kk
      do i=1,max-2,2
      do k=1,max-2,2
c     dO i=1,max,2
c     do k=1,max,2
      if(check_new(j,i).eq.checkk2(ii,k))then
      flg1=flg1+1
      endif
      enddo
      enddo
c     print*,'*********',flg
      if(flg1.eq.(max-2)/2) goto 335
c     if(flg1.eq.(max)/2) goto 335
       flg1=0
      enddo
cccc checking of mirror set start ccccccccccccc
      print*,'the final set','(',kk+1,')',(row_final2(ii,nn),
     : nn=1,max),(checkk(ii,i),i=1,max-2,2),ii


c       print*,'the final set',kk,(row_final(ii,nn),
c    : nn=1,max),(checkk(ii,j),j=1,max-2,2)
       kk=kk+1
      do j=1,max-2,2
c     do j=1,max,2
      check_new(kk,j)=checkk(ii,j)
      enddo
 335  flg=0
      flg1=0
      enddo
      print*,'kk',kk
 600  continue
     

      stop
      end

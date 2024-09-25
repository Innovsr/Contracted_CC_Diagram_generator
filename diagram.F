#define maximum  26
#define ind   6000
#define MX    6000

      program main
      implicit real*8(a-h,o-z)
      character operator*6
      dimension row(100,maximum),row_new(ind,ind,maximum)
     :,checkk(MX,maximum),check1_new(MX),row_final(MX,maximum),
     :check_new(MX,maximum),check_new2(MX,maximum),checkk2(MX,maximum)
     :,row_norm(MX,maximum),row_miror(MX,maximum)
     :,row_1(MX,maximum),row_end(MX,maximum)
    
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

        kkk=1

c      val=0
c      do i=1,max
c      row_new(i,i,1)=row(1,i)
c     print*,(row_new(i,i,nn),nn=1,max)
c      enddo

       do mal=1,4
      do k=1,ind
      do i=1,ind
       do j=1,maximum
      row_new(k,i,j)=0.0
       enddo
      enddo
      enddo

       do i=1,max
       row_new(i,i,1)=row(mal,i)
c     print*,(row_new(i,i,nn),nn=1,max)
       enddo
   
    

cccc  for the first element ccccccc
      do mm=1,max
c     do mm=32,32
      iiiii=0
      it =2
      ii= 0
      iii= 0


    
c      do i=1,max
c       row_new(mm,mm,1)=row(mm,i)
c      enddo
c     print*,(row_new(mm,mm,nn),nn=1,max),mm
c     print*,'*****************'
c     print*,row(2,1)
     

cccc  "it" is the column identity ccccccc
cccc  start of the loop of second element ccccc    

c      if (it.gt.2) goto 500



      i = 0
      if(mal.eq.1)kmm=1
      if(mal.eq.2)kmm=2
      if(mal.eq.3)kmm=3
      if(mal.eq.4)kmm=4
      do j=1,max
      if((row(kmm,j)-21)*(row_new(mm,mm,it-1)-21).gt.0) goto 100
      if(mod(row(kmm,j),2).ne.mod(row_new(mm,mm,it-1),2)) goto 100      
c      if(row(mm,j).eq.row_new(mm,mm,it-1))goto 100
      if(row_new(mm,mm,it-1).eq.15.and.row(kmm,j).eq.37)goto 100
      if(row_new(mm,mm,it-1).eq.37.and.row(kmm,j).eq.15)goto 100
      if(row_new(mm,mm,it-1).eq.16.and.row(kmm,j).eq.38)goto 100
      if(row_new(mm,mm,it-1).eq.38.and.row(kmm,j).eq.16)goto 100
      if(row_new(mm,mm,it-1).eq.7.and.row(kmm,j).eq.29)goto 100
      if(row_new(mm,mm,it-1).eq.29.and.row(kmm,j).eq.7)goto 100
      if(row_new(mm,mm,it-1).eq.8.and.row(kmm,j).eq.30)goto 100
      if(row_new(mm,mm,it-1).eq.30.and.row(kmm,j).eq.8)goto 100
      
      row_new(mm,i+1,it)=row(kmm,j)
      row_new(mm,i+1,1)=row_new(mm,mm,1)
      print*,(row_new(mm,i+1,nn),nn=1,max),mal
c     print*,(row(kmm,nn),nn=1,max)
      i=i+1
 100  enddo      

cccc end of the loop for 2nd element ccccccccccc

      ii=i
      iiiii=i
      kk=0
      llll=i
      mmmm=0

c     print*,(row_new(mm,mm,nn),nn=1,max),mm


         lll=0
 149     set=1
       m=0
      do i=mmmm+1-mmm,llll
       set=1
 150  do ll=1,it
c     print*,'row_new,row,it,ll',row_new(mm,i,ll),row(kmm,set+1),it,ll
c     print*,'set+1',set+1
      if(row(kmm,set+1).eq.row_new(mm,i,ll))goto 250
       
      enddo

c      goto 250

cccc  end of cheking cccccccccc
     
 400  row_new(mm,iiiii+1,it+1)=row(kmm,set+1)
      do j=1,it
      row_new(mm,iiiii+1,j)=row_new(mm,i,j)
      enddo
c     print*,(row_new(mm,iiiii+1,nn),nn=1,max),mal
      iiiii=iiiii+1
c     print*,'iiiii+1',iiiii+1
      m=m+1
 250  set=set+1
      if(set+1.eq.max+1)goto 200
      goto 150
 200  enddo
       mmmm=iiiii
       iii=m
       mmm=0
       set=0
      if (it.eq.max-2) goto 850
      do i=iiiii+1-m,iiiii
      do jj=1,max
      do l=1,it
      if(row(kmm,jj).eq.row_new(mm,i,l)) goto 700
      enddo
      if((row(kmm,jj)-21)*(row_new(mm,i,it+1)-21).gt.0) goto 700
      if(mod(row(kmm,jj),2).ne.mod(row_new(mm,i,it+1),2)) goto 700      
      if(row_new(mm,i,it+1).eq.15.and.row(kmm,jj).eq.37)goto 700
      if(row_new(mm,i,it+1).eq.37.and.row(kmm,jj).eq.15)goto 700
      if(row_new(mm,i,it+1).eq.16.and.row(kmm,jj).eq.38)goto 700
      if(row_new(mm,i,it+1).eq.38.and.row(kmm,jj).eq.16)goto 700
      if(row_new(mm,i,it+1).eq.7.and.row(kmm,jj).eq.29)goto 700
      if(row_new(mm,i,it+1).eq.29.and.row(kmm,jj).eq.7)goto 700
      if(row_new(mm,i,it+1).eq.8.and.row(kmm,jj).eq.30)goto 700
      if(row_new(mm,i,it+1).eq.30.and.row(kmm,jj).eq.8)goto 700
      row_new(mm,mmmm+1,it+2)=row(kmm,jj)
      do j=1,it+1
      row_new(mm,mmmm+1,j)=row_new(mm,i,j)
      enddo

c     print*,'iiiii+1',mmmm+1
c     print*,(row_new(mm,mmmm+1,nn),nn=1,max)
      
      mmmm=mmmm+1
      mmm=mmm+1
 700  enddo
      enddo

      it=it+2
     
      ii=mmm
      llll=mmmm
      iiiii=mmmm
      goto 149


 850  do iiii=mmmm+1-m,mmmm
      do jjj=1,max
      do l=1,it
      if(row(kmm,jjj).eq.row_new(mm,iiii,l)) goto 900
      enddo
      if(row_new(mm,iiii,max-1).eq.1.and.row(kmm,jjj).eq.2) goto 900
      if(row_new(mm,iiii,max-1).eq.2.and.row(kmm,jjj).eq.1) goto 900
      if(row_new(mm,iiii,max-1).eq.24.and.row(kmm,jjj).eq.23) goto 900
      if(row_new(mm,iiii,max-1).eq.23.and.row(kmm,jjj).eq.24) goto 900
      if(row_new(mm,iiii,max-1).eq.13.and.row(kmm,jjj).eq.14) goto 900
      if(row_new(mm,iiii,max-1).eq.14.and.row(kmm,jjj).eq.13) goto 900
      if(row_new(mm,iiii,max-1).eq.35.and.row(kmm,jjj).eq.36) goto 900
      if(row_new(mm,iiii,max-1).eq.36.and.row(kmm,jjj).eq.35) goto 900
      if(row_new(mm,iiii,max-1).eq.16.and.row(kmm,jjj).eq.38) goto 900
      if(row_new(mm,iiii,max-1).eq.38.and.row(kmm,jjj).eq.16) goto 900
      if(row_new(mm,iiii,max-1).eq.15.and.row(kmm,jjj).eq.37) goto 900
      if(row_new(mm,iiii,max-1).eq.37.and.row(kmm,jjj).eq.15) goto 900
      if(row_new(mm,iiii,max-1).eq.7.and.row(kmm,jjj).eq.29) goto 900
      if(row_new(mm,iiii,max-1).eq.29.and.row(kmm,jjj).eq.7) goto 900
      if(row_new(mm,iiii,max-1).eq.8.and.row(kmm,jjj).eq.30) goto 900
      if(row_new(mm,iiii,max-1).eq.30.and.row(kmm,jjj).eq.8) goto 900

      if((row(kmm,jjj)-21)*(row_new(mm,iiii,max-1)-21).gt.0.and
     :.mod(row(kmm,jjj),2).eq.mod(row_new(mm,iiii,max-1),2)) goto 900      
      if((row(kmm,jjj)-21)*(row_new(mm,iiii,max-1)-21).le.0.and
     :.mod(row(kmm,jjj),2).ne.mod(row_new(mm,iiii,max-1),2)) goto 900      

      row_new(mm,iiiii+1,max)=row(kmm,jjj)
      do j=1,max-1
      row_new(mm,iiiii+1,j)=row_new(mm,iiii,j)
      enddo
c     print*,(row_new(mm,iiiii+1,nn),nn=1,max),iiiii+1
c     print*,'iiiii+1,kkk',iiiii+1,kkk
c     print*,'sourav'
      
      do nn=1,max
      row_final(kkk,nn)=row_new(mm,iiiii+1,nn)
      enddo
c     print*,(row_final(kkk,nn),nn=1,max),kkk
      
      kkk=kkk+1
 910  iiiii=iiiii+1
 900  enddo
      enddo
      
 600  enddo
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
c       print*,'row_miror',kk+1,(row_miror(kk+1,nn),nn=1,max),
c     : (check1_new(j),j=1,max-2,2)
     

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
      print*,'the final set','1',(row_final(1,nn),nn=1,max),
     : (checkk(1,j),j=1,max-2,2)
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
      print*,'the final set',kk+1,(row_final(ii,nn),
     : nn=1,max),(checkk(ii,i),i=1,max-2,2),ii


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


      stop
      end

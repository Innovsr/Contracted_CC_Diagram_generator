#define maximum  26
#define ind   900000
#define id   100
#define MX    900000

      program main
      implicit real*8(a-h,o-z)
      character operator(10)*6,operator1(10)*6,tyop*8,yn*3,yeno*3,
     : datype*6,doptype*2,tyop1*8,tyop2*8
      dimension row(100,maximum),row_new(ind,maximum)
     :,checkk(MX,maximum),check1_new(MX),row_final(MX,maximum),
     :check_new(MX,maximum),check_new2(MX,maximum),checkk2(MX,maximum)
     :,row_norm(MX,maximum),row_miror(MX,maximum),mrow(MX,maximum)
     :,row_1(MX,maximum),row_end(MX,maximum),row_final1(MX,maximum)
     :,row_final2(MX,maximum),iop(10,10),ivert(5),identity(10)
    
cccc   T2dag = [3,4,5,6], T2 = [25,26,27,28], T1dag = [1,2], T1 = [23,24]
cccc   S2dag = [9,10,11,31], S2 = [12,32,33,34], S1dag = [7,29], S1 = [8,30]
cccc   Oph = [13,14], Ohp = [35,36], Opp = [16,38], Ohh = [15,37]
 
         
       print*,'Put the type of the operation open or closed'
       read(*,'(a)'),datype

       if(datype.eq.'open')then
       print*,'do you want get all pp, hh, ph,hp type diagram 
     :  together?'
       read(*,'(a)'),yn
       if(yn.eq.'no') then
       print*,'put the type of open diagram you want to get hh,
     :   pp, hp or ph'
       read(*,'(a)'),doptype
       endif
       print*,'do you want to put all prop operatos together'
       
       read(*,'(a)'),yeno
       if(yeno.eq.'yes') then
       print*,'Put the total no. of annihilation operators 
     : except property operators'
       read(*,*)n
       num=n+3+4
       num1=n+3+4
       print*,'Put the number of operator other than property 
     :  operators'
       read(*,*)number
       goto 516
       endif
       endif

       print*,'Put the total no. of annihilation operators'
       read(*,*)n
       num=n+3
       num1=n+3
       print*,'Put the number of operator'
       read(*,*)number

516       l=2
       kk=1
       ll=1
       irep=1
       iidd=0
       print*,'give the operator specification one by one'
       do i=1,number
       k=1
       print*,'put the name of the operator',i
       read(*,'(a)'),operator(i)
       operator1(i)=operator(i)
       print*,'put the number of vertex of operator',i
       read(*,*)ivert(i)

        do j=1,ivert(i)
        print*,'put the no. of annihilation operator in the vertex',j
        read(*,*)iann 

        if (iann.eq.0) then
        iop(i,k)=1+num
        iop(i,k+1)=2+num
        if (ivert(i).eq.2)then
        mrow(ll,kk)=1+num
        mrow(ll,kk+1)=2+num
        endif
        num=num+2
        goto 940
        endif

      
        if (iann.eq.1)then
        print*,'type of annihilation operator particle or hole'
        read(*,'(a)'),tyop
        if(ivert(i).eq.2.and.j.eq.1) tyop1=tyop
        if(ivert(i).eq.2.and.j.eq.2) tyop2=tyop
        if(tyop.eq.'particle')then
         do jj=irep,num-1
        if(mod(jj,2).eq.0) then
        iop(i,k)=jj
        if (ivert(i).eq.2.and.'tyop1'.eq.'type2')then
        mrow(ll,kk)=jj
        endif
        goto 939
        endif
        enddo
 939    do jj=num+1,3*(num+1)
        if(mod(jj,2).eq.0) then
        iop(i,k+1)=jj
        if (ivert(i).eq.2.and.'tyop1'.eq.'type2')then
        mrow(ll,kk+1)=jj
        endif
        goto 940
        endif
        enddo

        else
       
         do jj=irep,num-1
        if(mod(jj,2).ne.0) then
         iop(i,k)=jj
        if (ivert(i).eq.2.and.'tyop1'.eq.'type2')then
        mrow(ll,kk)=jj
        endif
        irep=jj+1
        goto 941
        endif
        enddo
 941       do jj=num+1,3*(num+1)
        if(mod(jj,2).ne.0) then
        iop(i,k+1)=jj
        if (ivert(i).eq.2.and.'tyop1'.eq.'type2')then
        mrow(ll,kk+1)=jj
        endif
        num=jj+1
         goto 940
         endif
        enddo
        endif
        
        endif      
        if (iann.eq.2)then
        iop(i,k)=irep
        iop(i,k+1)=irep+1
        if (ivert(i).eq.2)then
        mrow(ll,kk)=irep
        mrow(ll,kk+1)=irep+1
        endif
        irep=irep+2
        endif
 
 940       row(1,kk)=iop(i,k)
        row(1,kk+1)=iop(i,k+1)
        row(2,kk)=iop(i,k)
        row(2,kk+1)=iop(i,k+1)
        row(3,kk)=iop(i,k)
        row(3,kk+1)=iop(i,k+1)
        row(4,kk)=iop(i,k)
        row(4,kk+1)=iop(i,k+1)

        if(ivert(i).eq.1) then
        iidd=iidd+1
        identity(iidd)=iop(i,k)*iop(i,k)+iop(i,k+1)*iop(i,k+1)
         endif
        k=k+2
        kk=kk+2
        ll=ll+1
        enddo
        
        enddo

        last=ll-1
       if(yeno.eq.'yes') then
        row(1,kk)=1+num
        row(1,kk+1)=2+num
        print*,'O_ph',1+num,2+num
        iidd=iidd+1
        identity(iidd)=(1+num)*(1+num)+(2+num)*(2+num)
        num=num+2
       

        print*,'irep',irep 
        row(2,kk)=irep
        row(2,kk+1)=irep+1
        print*,'O_hp',irep,1+irep
        iidd=iidd+1
        identity(iidd)=(irep)*(irep)+(1+irep)*(1+irep)
        irep=irep+2

        do j1=num+1,2*(num+1)
        if(mod(j1,2).eq.0) then
        row(3,kk)=j1
        goto 577
        endif
        enddo
 577       do j2=irep+1,num-1
        if(mod(j2,2).eq.0) then
        row(3,kk+1)=j2
         goto 578
        endif
        enddo
         
 578       print*,'O_pp',j1,j2
        irep=irep+1
        num=num+1
        iidd=iidd+1
        identity(iidd)=j1*j1+j2*j2


        do j3=num+1,2*(num+1)
        if(mod(j3,2).ne.0) then
        row(4,kk)=j3
         goto 579
        endif
        enddo
 579       do j4=irep+1,num-1
        if(mod(j4,2).ne.0) then
        row(4,kk+1)=j4
         goto 580
        endif
        enddo
 580       print*,'O_hh',j3,j4
        iidd=iidd+1
        identity(iidd)=j3*j3+j4*j4
       endif
        do l=1,iidd
        print*,'identity',identity(l)
        enddo

        print*,'N',num1
        print*,'The operators are'
        do i=1,number
        print*,operator(i),(iop(i,j),j=1,2*ivert(i))
        enddo

       if(yeno.eq.'yes') then
        do ll=1,4
      print*,(row(ll,nn),nn=1,kk+1)
      
        enddo 
        loop=4
         max=kk+1
       endif

       if(yeno.eq.'no') then
      print*,(row(1,nn),nn=1,kk-1)
      
        loop=1
         max=kk-1
       endif
          print*,'max',max

cccc   "row" is total set coming from the combination ccccc
cccc   "row_new" is the new permissible set of diagrams ccccc



       do 600 mal=1,loop
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
      if((row(mal,i)-num1).le.0) goto 255
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
      if((row(mal,j)-num1)*(row_new(mm,it-1)-num1).gt.0) goto 100
c     print*,'mm',mm
      if(mod(row(mal,j),2).ne.mod(row_new(mm,it-1),2)) goto 100      
c      if(row(mal,j).eq.row_new(mm,it-1))goto 100
      do idd=1,iidd
      if(row_new(mm,it-1)*row_new(mm,it-1)+
     : row(mal,j)*row(mal,j).eq.identity(idd)) goto 100
      enddo
      
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

ccccccccccccccccccccccccccccccccccccccccccccccccc
cccc end of the loop for 2nd element cccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccc

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
      if((row(mal,set+1)-num1).le.0) goto 250
      endif
      if(row(mal,set+1).eq.row_new(i,ll))goto 250
       
      enddo

cccccccccccccccccccccccccccccccccccccccccccccccccc
cccc  end of cheking ccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccc
     
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
      if(it.eq.max-2.and.datype.eq.'open')then
      goto 850
      endif
      do 700 i=iiiii+1-m,iiiii
      do 700 jj=1,max
c      print*,'row_new',(row_new(i,ll),ll=1,max),i,iiiii,m
      do l=1,it
      if(row(mal,jj).eq.row_new(i,l)) goto 700
      enddo
      if((row(mal,jj)-num1)*(row_new(i,it+1)-num1).gt.0) goto 700
      if(mod(row(mal,jj),2).ne.mod(row_new(i,it+1),2)) goto 700      

      do idd=1,iidd
      if(row_new(i,it+1)*row_new(i,it+1)+
     : row(mal,jj)*row(mal,jj).eq.identity(idd)) goto 700
      enddo

      row_new(mmmm+1,it+2)=row(mal,jj)
      do j=1,it+1
      row_new(mmmm+1,j)=row_new(i,j)
      enddo
      do nn=1,max
      row_final(mmmm+1,nn)=row_new(mmmm+1,nn)
      enddo
      if(datype.eq.'closed')goto 535

      print*,(row_new(mmmm+1,nn),nn=1,max),mmmm+1
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
      do idd=1,iidd
      if(row_new(iiii,max-1)*row_new(iiii,max-1)+
     : row(mal,jjj)*row(mal,jjj).eq.identity(idd)) goto 900
      enddo
      
       print*,'************Sourav'

       if(datype.eq.'open'.and.yn.eq.'no')then
       print*,'************'
      
       if (doptype.eq.'hh') then
ccccccccc p-p ccccccccccccccccccccccccc
      if((row(mal,jjj)-num1)*(row_new(iiii,max-1)-num1).le.0.and
     :.mod(row(mal,jjj),2).eq.0.and.mod(row_new(iiii,max-1),2).eq.0)
     :  goto 900      
ccccccccc p-h cccccccccccccccccccccccccc
      if((row(mal,jjj)-num1).gt.0.and.(row_new(iiii,max-1)-num1)
     :.gt.0.and.mod(row(mal,jjj),2).ne.mod(row_new(iiii,max-1),2))
     : goto 900 
ccccccc h-p ccccccccccccccccccccccccccccc
      if((row(mal,jjj)-num1).le.0.and.(row_new(iiii,max-1)-num1)
     : .le.0.and.mod(row(mal,jjj),2).ne.mod(row_new(iiii,max-1),2)) 
     :goto 900      

       endif

       if (doptype.eq.'pp') then
ccccccccc h-h ccccccccccccccccccccccccc
      if((row(mal,jjj)-num1)*(row_new(iiii,max-1)-num1).le.0.and
     :.mod(row(mal,jjj),2).ne.0.and.mod(row_new(iiii,max-1),2).ne.0)
     :  goto 900      
ccccccccc p-h cccccccccccccccccccccccccc
      if((row(mal,jjj)-num1).gt.0.and.(row_new(iiii,max-1)-num1)
     : .gt.0.and.mod(row(mal,jjj),2).ne.mod(row_new(iiii,max-1),2)) 
     : goto 900 
ccccccc h-p ccccccccccccccccccccccccccccc
      if((row(mal,jjj)-num1).le.0.and.(row_new(iiii,max-1)-num1)
     : .le.0.and.mod(row(mal,jjj),2).ne.mod(row_new(iiii,max-1),2)) 
     : goto 900      

       endif

       if (doptype.eq.'ph') then
ccccccccc h-h ccccccccccccccccccccccccc
      if((row(mal,jjj)-num1)*(row_new(iiii,max-1)-num1).le.0.and
     :.mod(row(mal,jjj),2).ne.0.and.mod(row_new(iiii,max-1),2).ne.0)
     :  goto 900      
ccccccccc p-p ccccccccccccccccccccccccc
      if((row(mal,jjj)-num1)*(row_new(iiii,max-1)-num1).le.0.and
     :.mod(row(mal,jjj),2).eq.0.and.mod(row_new(iiii,max-1),2).eq.0)
     :  goto 900      
ccccccc h-p ccccccccccccccccccccccccccccc
      if((row(mal,jjj)-num1).le.0.and.(row_new(iiii,max-1)-num1)
     :.le.0.and.mod(row(mal,jjj),2).ne.mod(row_new(iiii,max-1),2))
     : goto 900      

       endif

       if (doptype.eq.'hp') then
ccccccccc h-h ccccccccccccccccccccccccc
      if((row(mal,jjj)-num1)*(row_new(iiii,max-1)-num1).le.0.and
     :.mod(row(mal,jjj),2).ne.0.and.mod(row_new(iiii,max-1),2).ne.0)
     :  goto 900      
ccccccccc p-p ccccccccccccccccccccccccc
      if((row(mal,jjj)-num1)*(row_new(iiii,max-1)-num1).le.0.and
     :.mod(row(mal,jjj),2).eq.0.and.mod(row_new(iiii,max-1),2).eq.0)
     :  goto 900      
ccccccccc p-h cccccccccccccccccccccccccc
      if((row(mal,jjj)-num1).gt.0.and.(row_new(iiii,max-1)-num1)
     :.gt.0.and.mod(row(mal,jjj),2).ne.mod(row_new(iiii,max-1),2))
     : goto 900 

       endif
       endif

c     if((row(mal,jjj)-num1)*(row_new(iiii,max-1)-num1).gt.0.and
c    :.mod(row(mal,jjj),2).eq.mod(row_new(iiii,max-1),2)) goto 900      
c     if((row(mal,jjj)-num1)*(row_new(iiii,max-1)-num1).le.0.and
c    :.mod(row(mal,jjj),2).ne.mod(row_new(iiii,max-1),2)) goto 900      

       row_new(iiiii+1,max)=row(mal,jjj)
      do j=1,max-1
      row_new(iiiii+1,j)=row_new(iiii,j)
      enddo
      print*,'*******',(row_new(iiiii+1,nn),nn=1,max),iiiii+1
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
      
 535     do i=1,ind
       do j=1,maximum
      row_miror(i,j)=0.0
       enddo
      enddo


      do ii=2,kkk-1
      do nn=1,max
      do ll=1,last
      do nnn=1,2
      if(row_final(ii,nn).eq.mrow(ll,nnn))then
       row_miror(ii,nn)=mrow(ll,nnn+2)
      endif
      enddo
      enddo
      enddo 
      enddo
      
      do ii=2,kkk-1
      do nn=max,1
      do ll=1,last
      do nnn=1,2
      if(row_final(ii,nn).eq.mrow(ll,nnn+2))then
       row_miror(ii,nn)=mrow(ll,nnn)
      endif
      enddo
      enddo
      enddo 
      enddo
      
c       print*,'row_final ',ii,(row_final(ii,nn),nn=1,max)
c       print*,'row_miror',ii,(row_miror(ii,nn),nn=1,max)
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
     

 999     stop
      end

      colm()=column()
      collll1=0
      collll2=0
      iter=1
      L=1
      K=1
      jjj=(a-1)+(b-1)+c
      d0 it=3,jjj-3
 400  d0 j=it,jjj
         K=K+1
c      colm(iter,it-1)
       colll1=colm(iter,it-1)/2
       if (colm(iter,it-1).gt.10) collll1=1
       do ii=2,ll
       if(colm(iter,it-1).eq.a(ii)) index1=1
       enddo
       do ii=2,ll
       if(colm(iter,it-1).eq.b(ii)) index1=1
       enddo
       do ii=2,ll
       if(colm(iter,it-1).eq.c(ii)) index1=1
       enddo
 500   do i=it,jjj      
       L=L+1
       if (K/2.eq.zero) goto 300
       colll2=column(L,it)/2
       if(column(L,it).gt.10) collll2=1
        do ii=2,ll
       if(colm(iter,it-1).eq.a(ii)) index2=1
       enddo
       do ii=2,ll
       if(colm(iter,it-1).eq.b(ii)) index2=1
       enddo
       do ii=2,ll
       if(colm(iter,it-1).eq.c(ii)) index2=1
       enddo
       if(colll1.eq.coll2)
       if(colll1.ne.coll2)
       if(colll1.ne.coll2)
 300   if(i.eq.j)goto 600
       colm(L,i)=colm(iter,j)
       goto 700
 600   colm(L,i)=colm(iter,i)
 700   enddo
       L=L
       enddo
       iter=iter+1
       ijk=ijk*(ijk-1)
       if (iter.le.ijk) goto 400
       enddo

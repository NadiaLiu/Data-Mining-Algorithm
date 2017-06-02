D<-list(T100=c("I1","I2","I5"),
        T200=c("I2","I4"),
        T300=c("I2","I3"),
        T400=c("I1","I2","I4"),
        T500=c("I1","I3"),
        T600=c("I2","I3"),
        T700=c("I1","I3"),
        T800=c("I1","I2","I3","I5"),
        T900=c("I1","I2","I3"))
min_sup=2

apriori<-function(D,min_sup){
  
  items<-list("I1","I2","I3","I4","I5")
  L<-list()
  
  #求子集
  subset<-function(sub)
  { 
    s=1
    res<-list()
    for(a in 1:length(sub[1,]))
    { n1=length(sub[,a])
    res[[s]]=lapply(n1-1, function(i) combn(sub[,a],i))
    s=s+1
    }
    return(res)
  }
  
  #判断子集是否在k-1频繁项集
  sub_fre<-function(sub3,b){
    del<-vector()
    cou<-0
    for(i in 1:length(sub3)){
      for(j in 1:ncol(sub3[[i]][[1]])){
        
        for(m in 1:nrow(b)){
          if(all(sub3[[i]][[1]][,j] %in% b[m,]))
          {cou<-cou+1
          next}
        }
      }
      if(cou<ncol(sub3[[i]][[1]])){
        del<-c(del,i)
      }
      cou<-0
    }
    return(del)
  }
  
  #遍历所有item找到1-候选集
  support<-rep(0,length(items))
  for(i in 1:length(items))
  {
    for(j in 1:length(D))
    {
      if(items[i] %in% D[[j]])
      {
        support[i]<-support[i]+1
      }
    }
  }
  C1<-cbind(items,support)
  
  #支持度判断找出1-频繁项集
  L1<-C1[which(support>=min_sup),]
  L<-list(L,L1)
  
  #2-候选项集
  b<-L1[,1]
  C2<-combn(b,2)
  subset(C2)
  
  support<-rep(0,ncol(C2))
  for(i in 1:ncol(C2))
  {
    for(j in 1:length(D))
    {
      if(all(C2[,i] %in% D[[j]]))
      {
        support[i]<-support[i]+1
      }
    }
  }
  C2<-rbind(C2,support)
  L2<-C2[,which(support>=min_sup)]
  L2<-t(L2)
  L<-list(L,L2)
  
  
  Lk<-L2
  K=3
  #找k频繁项集
  pd=TRUE
  while(pd==TRUE){
    b<-Lk[,1:(ncol(Lk)-1)]
    u<-unique(as.vector(unlist(b)))
    Ck<-combn(u,K)
    sub<-subset(Ck)
    del<-sub_fre(sub,b)
    Ck<-Ck[,-del]
    if(ncol(Ck)==0)
    {pd==FALSE
      break}
    #计算支持度
    else{
      support<-rep(0,ncol(Ck))
    for(i in 1:ncol(Ck))
    {
      for(j in 1:length(D))
      {
        if(all(Ck[,i] %in% D[[j]]))
        {
          support[i]<-support[i]+1
        }
      }
    }
    
    Ck<-rbind(Ck,support)
    Lk<-Ck[,which(support>=min_sup)]
    Lk<-t(Lk)
    L<-list(L,Lk)
    K<-K+1
    }
  }
  return(L)
}

apriori(D,min_sup)

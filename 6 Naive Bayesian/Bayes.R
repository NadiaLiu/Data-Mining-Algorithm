D<-read.csv("E:\\Rlanguage\\6 Naive Bayesian\\tree.csv")
colnames(D)<-c("age","income","student","credit_rating","buys_computer")

Bayes<-function(p){
  class<-unique(D[,ncol(D)])
  count<-rep(0,length(class))
  for(i in 1:length(class)){
    for(j in 1:nrow(D)){
      if(D[j,ncol(D)]==class[i]) count[i]<-count[i]+1
  }
  }
  pc<-count/nrow(D)
  
  x<-rep(0,2) 
  px<-rep(1,2)
  for(i in 1:(length(D)-1)){
    for(j in 1:nrow(D)){
      if(D[j,i]==p[[i]] && D[j,ncol(D)]==class[1]) x[1]=x[1]+1
      if(D[j,i]==p[[i]] && D[j,ncol(D)]==class[2]) x[2]=x[2]+1
    }
    px[1]<-(x[1]/count[1])*px[1]
    px[2]<-(x[2]/count[2])*px[2]
  }
  
  pr<-rep(1,2)
  pr[1]<-px[1]*pc[1]
  pr[2]<-px[2]*pc[2]
  
  ifelse(pr[1]>=pr[2],return(class[1]),return(class[2]))
}

p<-list(age="youth",income="low",student="yes",credit_rating="exllent")

Bayes(p)
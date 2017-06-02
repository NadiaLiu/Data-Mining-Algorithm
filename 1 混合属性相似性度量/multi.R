data<-read.csv("E:\\Rlanguage\\1 混合属性相似性度量\\stat.csv")

mul<-function(data){
  
#normal
  q<-data[,2]
  D=matrix(0,length(q),length(q))
    for(i in 1:length(q))
      {
       for(j in 1:length(q))
             {
              if(q[i]==q[j]) D[i,j] <-0
              else D[i,j] <- 1
       }
      }
     D[1:length(q),1:length(q)]


#ordinary
  x<-factor(data[,3],levels=c("Normal","Good","Great"))
  q<-unclass(x)
  dif<-max(q)-min(q)
  M=matrix(0,length(q),length(q))
  for(i in 1:length(q))
  {
    for(j in 1:length(q))
    {
      M[i,j]<-abs(q[c(i)]-q[c(j)])/dif
    }
  }
  M[1:length(q),1:length(q)]

#numerical
  q<-data[,4]
  dif<-max(q)-min(q)
  P=matrix(0,length(q),length(q))
  for(i in 1:length(q))
  {
    for(j in 1:length(q))
    {
      P[i,j]<-abs(q[c(i)]-q[c(j)])/dif
    }
  }
  P[1:length(q),1:length(q)]

#multiple
final<-(D+M+P)/3
return(final)
}  
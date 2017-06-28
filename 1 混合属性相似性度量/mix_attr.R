mix<-function(data,class){
  #将所有属性都变成数值型
  data[,2]<-factor(data[,2],levels=c("Normal","Good","Great"))
  for(i in 1:ncol(data)){
    data[,i]<-as.numeric(data[,i])
  }
  #生成距离矩阵
  D<-matrix(numeric(0),nrow(data),nrow(data))
  s=0
  d=0
  for(i in 1:nrow(D)){
    for(j in 1:ncol(D)){
      for(p in 1:ncol(data)){
        
        if(class[p]==1){
          if(data[i,p]==0||data[j,p]==0||is.na(data[i,p])||is.na(data[j,p])){
            s=s+0
            next
          }
          ifelse(data[i,p]==data[j,p],d<-d+0,d<-d+1)
          s=s+1
        }
        
        if(class[p]==2||class[p]==3){
          if(data[i,p]==0||data[j,p]==0||is.na(data[i,p])||is.na(data[j,p])){
            s=s+0
            next
          }
          diff<-max(data[,p])-min(data[,p])
          d<-d+(abs(data[i,p]-data[j,p])/diff)
          s=s+1
        }
      }
      D[i,j]=d/s
      d=0
      s=0
    }
  }
  return(D)
}


#main function
data<-read.csv("stat.csv")
class<-c(1,2,3) #1标称 2序数 3数值
mix(data,class)

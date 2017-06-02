  #数据来源网络
  x1 <- seq(0,pi,length.out=100)
  y1 <- sin(x1) + 0.1*rnorm(100)
  x2 <- 1.5+ seq(0,pi,length.out=100)
  y2 <- cos(x2) + 0.1*rnorm(100)
  data <- data.frame(c(x1,x2),c(y1,y2))
  names(data) <- c('x','y')
  
  #直接输出矩阵的话不好判断循环几次，DB函数输出类别class
  DB<-function(D,e,MinPts){
    n<-nrow(D)
    data<-cbind(1:n,D)
    class<-rep(0,time=n)
    cc<-0
    
    #标记所有对象为unvisited
    vi<-rep(FALSE,time=n)
    
    #建立距离矩阵
    dist<-matrix(numeric(0),nrow=n,ncol=0);
    for(i in 1:n){
      t<-sqrt((data[,2]-data[i,2])^2+(data[,3]-data[i,3])^2)
      t<-as.matrix(t)
      dist<-cbind(dist,t)
    }
    
    
    while(TRUE){
    #随机选择一个unvisited对象p标记为visited
    p<-sample(which(vi==FALSE),size=1)
    vi[p]<-TRUE
    cc<-cc+1
    class[p]=cc
    
    #判断随机点p是不是核心点
    ps<-which(dist[p,]<=e)
    
    if(length(ps)>=MinPts){
      N<-ps
      
      #判断p的邻域中每个点是不是核心点
      j<-1
      while(j<=length(N)){
        pp<-N[j]
        if(vi[pp]==FALSE){
          vi[pp]<-TRUE
          pps<-which(dist[pp,]<=e)
          
          if(length(pps)>=MinPts){
            N<-c(N,pps)
            if(class[pp]==0){
              class[pp]=cc
            }
          }
        }
        j<-j+1
      }
    }
    else {class[p]=-1} #噪声
    if(all(vi)==TRUE){
      return (class)
    }
    }
  
    return(class)
    }
  
  #画图
  pic<-function(D,e,MinPts){
    class<-DB(D,e,MinPts)
    result<-cbind(D,class)
    result<-as.data.frame(result)
    names(result)<-c("x","y","class")
    result$class<-as.factor(result$class)
    p<-ggplot(data = result, mapping = aes(x = x, y = y,colour=factor(class)))
    p + geom_point()
    
  }
  
  pic(data,0.42,5)
  
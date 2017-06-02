D<-read.csv("E:\\Rlanguage\\3 决策树\\tree.csv")
colnames(D)<-c("age","income","student","credit_rating","buys_computer")

  #求最大增益的属性那一列
  attribute<-function(D){
  #计算Info(D)
  InfoD<-0
  n<-nrow(D)
  class<-unlist(unique(D["buys_computer"]))
  for(i in 1:length(class)){
    a<-length(which(D[,"buys_computer"]==class[i]))/n
    InfoD<-InfoD-a*log2(a)
  }
  
  #循环找出Gain(atrr)=Info(D)-Info(attr^D)最大的attr作为分裂属性
  max_gain<-0
  attr<-0
  for(j in 1:(ncol(D)-1)){
    cla<-unlist(unique(D[,j]))
    info<-0
    for(c in 1:length(cla)){
      a<-which(D[,j]==cla[c])
      t<-length(a)/n
      db<-D[a,]
      total<-0
      for(p in 1:length(class)){
        b<-length(which(db[,"buys_computer"]==class[p]))/(length(a))
        ifelse(b==0,total<-total-0,total<-total-b*log2(b))
      }
      
      info<-info+t*total
    }
    gain<-InfoD-info
    if(max_gain<=gain){
      max_gain<-gain
      attr<-j
    }
  }
  return(attr)
  }

  #创建决策树
  tree<-data.frame(age=0,income=0,student=0, credit_rating=0, buys_computer=0)
  tree<-tree[-1,]
  t<-1
  a=attribute(D)
  
  create_tree<-function(a,D)
  {
    class<-as.character(unique(D[,a]))
    for(z in 1:length(class))
    {
      data=subset(D,D[,a]==class[z])
      name=colnames(data)[a]
      attr=as.character(data[1,a])
      data=data[-a]
      
      if(length(unique(data[,length(data)]))==1)
      { 
        tree[t,name]<<-attr
        tree[t,"buys_computer"]<<-as.character(data[1,length(data)])
        t<<-t+1
        
        
        for(i in 1:nrow(tree)){
          if(is.na(tree[i,"age"])){
            tree[i,"age"]<<-tree[i-1,"age"]
          }
        }
      }
      else{
        tree[t,colnames(D)[a]]<<-attr
        a_new=attribute(data)
        create_tree(a_new,data)
      }
    }
  }
  
  create_tree(a,D)
  
  #预测
    predict<-function(p){
      count=0
      for(i in 1:nrow(tree)){
        if(p[[1]]==tree[i,1]){
          name<-colnames(tree)[!(is.na(tree[i,]))]
          for(j in 1:(length(name)-1)){
            if(p[[name[j]]]==tree[i,name[j]])
              count=count+1}
          if(count==length(name)-1)
          {
            return(tree[i,length(tree)])}
          count=0
        }
      }
    }
    
  p<-list(age="youth",income="low",student="yes",credit_rating="exllent")
  predict(p)
    
    
    
  

  

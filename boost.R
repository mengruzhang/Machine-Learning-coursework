##Mengru Zhang 
#set path
setwd("~/Desktop/CSE512/HW2")

#read data
bupa<-as.matrix(read.table("bupa.txt", sep = ",",header=F))
##map y=1 to -1, y=2 to 1
bupa[,7][bupa[,7]==1]<--1
bupa[,7][bupa[,7]==2]<-1

#split train set and test set
##set train and test set size
m<-round(dim(bupa)[1]*0.9)  #m=310
n<-dim(bupa)[1]-m  #n=35

#initialize overall
err.test<-NULL
err.train<-NULL

##############


for(i in 1:50){
  ##random split
  
  sub.tr<-sample(nrow(bupa),m,replace = FALSE)
  x.tr<-bupa[sub.tr,]
  x.te<-bupa[-sub.tr,]
  
  
  #model
  #initialize
  T=100
  alpha<-NULL
  err<-NULL
  err.te<-NULL
  err.tr<-NULL
  p<-NULL
  k<-NULL
  index<-NULL
  D<-rep(1/m,m)
  z<-NULL
  
  #define a function helping select h: calculate the weighted classify error
  compare<-function(x){
    y.te<-matrix(rep(x.tr[,7],dim(x)[2]),nrow=m)
    s<-D%*%(x!=y.te) #weighted classify
  }
  #define a constant to be used when selecting h
  title<-c("V1","V2","V3","V4","V5","V6")
  
  
  #begin the iteration
  for (t in 1:T){
    #h is from D, also use error to be selected
    h1<-function(x){
      k<-t(matrix(rbind(rep(min(x):max(x),m)),ncol=m))
      temp<-matrix(rep(x,max(x)-min(x)+1),nrow=m)
      y1<-1*(temp>=k)-1*(temp<k)
      return(y1)
    }
    h1.v6<-function(x){
      k<-t(matrix(rbind(rep(seq(min(x),max(x),by=0.1),m)),ncol=m))
      temp<-matrix(rep(x,(max(x)-min(x))/0.1+1),nrow=m)
      y1<-1*(temp>=k)-1*(temp<k)
      return(y1)
    }
    h2<-function(x){
      k<-t(matrix(rbind(rep(min(x):max(x),m)),ncol=m))
      temp<-matrix(rep(x,max(x)-min(x)+1),nrow=m)
      y2<-1*(temp<k)-1*(temp>=k)
      return(y2)
    }
    h2.v6<-function(x){
      k<-t(matrix(rbind(rep(seq(min(x),max(x),by=0.1),m)),ncol=m))
      temp<-matrix(rep(x,(max(x)-min(x))/0.1+1),nrow=m)
      y2<-1*(temp<k)-1*(temp>=k)
      return(y2)
    }
    
    y.tr1<-apply(x.tr[,-(6:7)],2,h1) 
    y.tr1.6<-h1.v6(x.tr[,6])
    y.tr2<-apply(x.tr[,-(6:7)],2,h2)
    y.tr2.6<-h2.v6(x.tr[,6])
    
    error1<-lapply(y.tr1,compare) 
    error1.6<-compare(y.tr1.6)
    error2<-lapply(y.tr2,compare)
    error2.6<-compare(y.tr2.6)
    
    error1$V6<-error1.6 #error1 is a list of 6 elements.
    error2$V6<-error2.6 #error2 is a list of 6 elements.
    
      
    min1<-min(unlist(lapply(error1,min))) 
    min2<-min(unlist(lapply(error2,min)))
    
    
    if(min1<=min2) {
      temp<-which.min(unlist(lapply(error1,min)))
      ind<-as.numeric(which(names(temp)==title))
      inindex<-as.numeric(which.min(unlist(error1[ind])))
      
      ##store C1,C2 as p(parameter)
      p<-c(p,1,-1)
      ##store x_j
      index<-c(index,ind)
      ##store stump parameter as k    
    }else {
      temp<-which.min(unlist(lapply(error2,min)))
      ind<-as.numeric(which(names(temp)==title))
      inindex<-as.numeric(which.min(unlist(error2[ind])))
      
      ##store C1,C2 as p(parameter)
      p<-c(p,-1,1)
      ##store x_j
      index<-c(index,ind)    
    }
    ##store stump parameter as k
    if(ind!=6) k<-c(k,min(x.tr[,index])+inindex-1) else k<-c(k,min(x.tr[,index])+(inindex-1)*0.1) ## 
 
    #decision stump
    h<-function(x){
      y<-p[2*t-1]*(x[index[t]]>=k[t])+p[2*t]*(x[index[t]]<k[t])
      return(y)
    } 
    
    #error comes from h
    y.hat<-apply(x.tr[,-7],1,h)
    temp<-as.numeric(y.hat!=as.vector(x.tr[,7]))
    err<-c(err,D%*%temp)
    
    #alpha, Z, D
    alpha<-c(alpha,0.5*log((1-err[t])/err[t]))
    
    #tempz<-(1-err[t])*exp(-alpha[t])+err[t]*exp(alpha[t])
    tempz<-2*sqrt(err[t]*(1-err[t]))
    z<-c(z,tempz)
    
    s<-as.numeric(x.tr[,7]==y.hat)
    s[s==0]<--1
    D<-(D*exp(-alpha[t]*s))/z[t]   #forget the z!!!
  }#one roll of 100 iterations
  
  ##after one roll of 100 iterations:
  
  err.train<-c(err.train,err) #every 100 of error.train belong to one train set. 
                              #when iteration is over, should be total 100*50 elements.
  
  ##final classifier
  y<-NULL
  for(t in 1:T){
    H<-function(x){
      temp<-alpha[t]*(p[2*t-1]*(x[index[t]]>=k[t])+p[2*t]*(x[index[t]]<k[t]))
      y<-sum(c(y,temp))
    }
    #train
    y.pre.tr<-apply(x.tr[,-7],1,H)
    err.tr<-c(err.tr,sum(as.numeric(y.pre.tr!=x.tr[,7]))/m)
    #test
    y.pre<-apply(x.te[,-7],1,H)
    err.te<-c(err.te,sum(as.numeric(y.pre!=x.te[,7]))/n)
  }  
  err.test<-c(err.test,err.te) #every 100 of error.test belong to one test set. 
                               #when iteration is over, should be total 100*50 elements.
    
}

#calculate average error rate
error.train<-matrix(err.train, nrow=50, ncol=100, byrow=T)
avrg.err.tr<-apply(error.train,2,mean) #vector length = 100 

error.test<-matrix(err.test, nrow=50, ncol=100, byrow=T)
avrg.err.te<-apply(error.test,2,mean) #vector length = 100












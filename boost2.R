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
#define a function helping select h: calculate the weighted classify error
compare<-function(x){ #x is a matrix
  y.te<-matrix(rep(x.tr[,7],dim(x)[2]),nrow=m)
  s<-D%*%(x!=y.te) #weighted classify
  return(s)
}

h1<-function(x){ #x is a column vector
  k<-t(matrix(rep(min(x):max(x),m),ncol=m))
  temp<-matrix(rep(x,max(x)-min(x)+1),nrow=m)
  y1<-1*(temp>=k)-1*(temp<k)
  return(y1)
}
h1.v6<-function(x){ #x is a column vector
  k<-t(matrix(rep(seq(min(x),max(x),by=0.1),m),ncol=m))
  temp<-matrix(rep(x,(max(x)-min(x))/0.1+1),nrow=m)
  y1<-1*(temp>=k)-1*(temp<k)
  return(y1)
}
#decision stump
h<-function(x){ #x is a row vector
  y<-p[2*t-1]*(x[index[t]]>=k[t])+p[2*t]*(x[index[t]]<k[t])
  return(y)
}
#weighted classifier
H<-function(x){ #x is a row vector
  temp<-alpha[t]*h(x)
  stump<-c(stump,temp)
  y<-sign(sum(stump))
  return(y)      
}  
#define a constant to be used when selecting h
title<-c("V1","V2","V3","V4","V5","V6")

#begin iteration
for(i in 1:50){
  
  #random split
  
  sub.tr<-sample(nrow(bupa),m,replace = FALSE)
  x.tr<-bupa[sub.tr,]
  x.te<-bupa[-sub.tr,]
  
  #initialize
  T=100
  alpha<-NULL
  err<-NULL
  p<-NULL
  k<-NULL
  index<-NULL
  D<-rep(1/m,m)
  z<-NULL
  
  
  #begin the iteration
  for (t in 1:T){
    y.tr1<-apply(x.tr[,-(6:7)],2,h1) 
    y.tr1.6<-h1.v6(x.tr[,6])
   
    error1<-lapply(y.tr1,compare) 
    error1.6<-compare(y.tr1.6)
      
    error1$V6<-error1.6 #error1 is a list of 6 elements. each element is a vector.
  
    min1<-min(unlist(lapply(error1,min))) 
   
    if(min1<0.5) {
      temp<-which.min(unlist(lapply(error1,min)))
      ind<-as.numeric(which(names(temp)==title)) #which x_j to be used
      inindex<-as.numeric(which.min(unlist(error1[ind]))) #which position's value of x_j to be used
      
      ##store C1,C2 as p(parameter)
      p<-c(p,1,-1)
      ##store x_j
      index<-c(index,ind) 
    }else {
      temp<-which.max(unlist(lapply(error1,max)))
      ind<-as.numeric(which(names(temp)==title)) #which x_j to be used
      inindex<-as.numeric(which.min(unlist(error1[ind]))) #which value of x_j to be used
      
      ##store C1,C2 as p(parameter)
      p<-c(p,-1,1)
      ##store x_j
      index<-c(index,ind)    
    }
    ##store stump parameter as k
    if(ind!=6) k<-c(k,min(x.tr[,index])+inindex-1) else k<-c(k,min(x.tr[,index])+(inindex-1)*0.1) ## 
    
  
    #error comes from h
    y.hat<-apply(x.tr[,-7],1,h)
    temp<-as.numeric(y.hat!=as.vector(x.tr[,7]))
    err<-c(err,D%*%temp)
    
    #alpha, Z, D
    alpha<-c(alpha,0.5*log((1-err[t])/err[t]))
    
    #tempz<-(1-err[t])*exp(-alpha[t])+err[t]*exp(alpha[t])
    tempz<-2*sqrt(err[t]*(1-err[t]))
    z<-c(z,tempz)
    
    #s<-as.numeric(x.tr[,7]==y.hat)
    #s[s==0]<--1
    D<-(D*exp(-alpha[t]*(x.tr[,7]*y.hat)))/z[t]   #forget the z!!!
  }
  
  
  stump<-NULL
  y<-NULL
  err.tr<-NULL
  err.te<-NULL
  
  #final classifier
  for(t in 1:T){
 
   
    #train
    y.pre.tr<-apply(x.tr[,-7],1,H)
    err.tr<-c(err.tr,sum(as.numeric(y.pre.tr!=x.tr[,7]))/m) #is a vector of length 100
   
    
    #test
    y.pre.te<-apply(x.te[,-7],1,H)
    err.te<-c(err.te,sum(as.numeric(y.pre.te!=x.te[,7]))/n)
   
  }
  err.tr[100] 
  err.te[100]
  
  
  ##one roll of 100 iterations is over
  
  err.train<-c(err.train,err.tr)
  err.test<-c(err.test,err.te) 
  #every 100 of error.test belong to one test set. 
  #when iteration is over, should be total 100*50 elements.
  
}#50 sampling over


#calculate average error rate
error.train<-matrix(err.train, nrow=50, ncol=100, byrow=T)
avrg.err.tr<-apply(error.train,2,mean) #vector length = 100 

error.test<-matrix(err.test, nrow=50, ncol=100, byrow=T)
avrg.err.te<-apply(error.test,2,mean) #vector length = 100












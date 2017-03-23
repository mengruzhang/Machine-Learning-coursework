##Menrgu Zhang 
#set path
setwd("~/Desktop/CSE512_HW1")

#read data
x.tr<-read.table("train.txt", header=F)
y.tr<-read.table("train.label.txt", header=F)

x.tr<-as.matrix(x.tr)
m<-ncol(x.tr)

#read data
x.te<-read.table("test.txt", header=F)
x.te<-as.matrix(x.te)

#read test data
y.test<-read.table("test.label.txt", header=F)
y.test<-as.vector(y.test)


#model
w<-rep(0,m+1)
w<-as.matrix(w)

prob<-function(x,w){
  y = (exp(w[1]+x%*%w[2:(m+1)]))/(exp(w[1]+x%*%w[2:(m+1)])+1)
  return(y)
}

for(t in 1:2000){
  temp = (y.tr-prob(x.tr,w))
  w[1] = w[1]+0.0001*sum(temp);
  temp<-as.matrix(temp)
  w[2:(m+1)] = w[2:(m+1)]+0.0001*t(x.tr)%*%temp; 
}




#trainning error
p.tr<-exp(w[1]+x.tr%*%(w[2:(m+1)]))
pre.tr= p.tr>1
error_tr=1- sum(pre.tr==y.tr)/200
error_tr #0

#test error
p<-exp(w[1]+x.te%*%(w[2:(m+1)]))
pre= p>1
error_te=1- sum(pre==y.test)/200
error_te #0.27

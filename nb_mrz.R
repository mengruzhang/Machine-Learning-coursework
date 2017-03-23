#Mengru Zhang 
#set path
setwd("~/Desktop/CSE512_HW1")

#read data
x.tr<-read.table("train.txt", header=F)
y.tr<-read.table("train.label.txt", header=F)

x.tr<-as.matrix(x.tr)
m<-ncol(x.tr)


#D and m in the add-one smoothing method
D0<-sum(x.tr[1:100,])
D1<-sum(x.tr[101:200,])
D<-D0+D1


estimate0<-function(x) {
  y<-(1+sum(x))/(D0+m)
  return(y)
}
#estimate theta0
theta0<-apply(x.tr[1:100,],2,estimate0)

estimate1<-function(x) {
  y<-(1+sum(x))/(D1+m)
  return(y)
}
#estimate theta0
theta1<-apply(x.tr[101:200,],2,estimate1)

################
#read data
x.te<-read.table("test.txt", header=F)
x.te<-as.matrix(x.te)

label<-function(x){
  p<-c(log(D0/D)+sum(x*log(theta0)),log(D1/D)+sum(x*log(theta1)))
  if (p[1]>p[2]) return(0) else return(1)
}
label.pre<-apply(x.te,1,label)
label.train<-apply(x.tr,1,label)

############
#read test data
y.test<-read.table("test.label.txt", header=F)
y.test<-as.vector(y.test)
#test error
(200-sum((label.pre==y.test)))/200 #0.25


#train error
(200-sum((label.train==y.tr)))/200 #0.01








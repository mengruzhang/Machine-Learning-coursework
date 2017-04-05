# SVM using QP
install.packages("R.matlab") #used to load .mat data into R
install.packages("quadprog")
library(quadprog)
library(R.matlab)
# data
data <- readMat('faces.mat')
train <- data$traindata
dim(train)
test <- data$testdata
dim(test)
y.train <- as.vector(data$trainlabels)
y.test <- as.vector(data$testlabels)


n<-dim(train)[1]
y.train[which(y.train==2)]=-1

# build the system matrices
Q<-sapply(1:n, function(i) y.train[i]*t(train)[,i])
D<-t(Q)%*%Q
d<-matrix(1, nrow=n)
b0<-rbind( matrix(0, nrow=1, ncol=1) , matrix(0, nrow=n, ncol=1) )
A<-t(rbind(matrix(y.train, nrow=1, ncol=n), diag(nrow=n)))

# call the QP solver:
sol<-solve.QP(D, d, A, b0, meq=1, factorized=FALSE)
qpsol<-matrix(sol$solution, nrow=n)


# build the classifier
ind<-which(abs(qpsol)>1e-7)
alpha<-qpsol[ind]
x.tr<-train[ind,]
dim(x.tr)
y.tr<-y.train[ind]

w<-alpha%*%diag(y.tr)%*%x.tr
b<-mean(y.tr-t(w%*%t(x.tr)))

# prediction
y.pre.tr<-sign(w%*%t(train)-b)
print("train error of SVM")
sum(y.pre.tr!=y.train)/280

y.pre.te<-sign(w%*%t(test)-b)
y.test[which(y.test==2)]=-1
print("test error of SVM")
sum(y.pre.te!=y.test)/120






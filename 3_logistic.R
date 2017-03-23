library(R.matlab)
#data preparation
data <- readMat('faces.mat')
train <- data$traindata
dim(train)
test <- data$testdata
dim(test)
y.train <- as.vector(data$trainlabels)
y.test <- as.vector(data$testlabels)

y.train[which(y.train==2)]=0
y.test[which(y.test==2)]=0


m<-ncol(train)

#model
w<-rep(0,m+1)
w<-as.matrix(w)

prob<-function(x,w){
  y = (exp(w[1]+x%*%w[2:(m+1)]))/(exp(w[1]+x%*%w[2:(m+1)])+1)
  return(y)
}

for(t in 1:2000){
  temp = (y.train-prob(train,w))
  w[1] = w[1]+0.0001*sum(temp);
  temp<-as.matrix(temp)
  w[2:(m+1)] = w[2:(m+1)]+0.0001*t(train)%*%temp; 
}




#trainning error
p.tr<-exp(w[1]+train%*%(w[2:(m+1)]))
pre.tr= p.tr>1
error_tr=1- sum(pre.tr==y.train)/280
print("train error of logistic regression")
error_tr 

#test error
p<-exp(w[1]+test%*%(w[2:(m+1)]))
pre= p>1
error_te=1- sum(pre==y.test)/120
print("test error of logistic regression")
error_te 




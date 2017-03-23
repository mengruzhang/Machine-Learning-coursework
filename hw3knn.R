#1.1.1
#setwd("~/Documents/CSE512/HW3")
install.packages("R.matlab") #used to load .mat data into R
install.packages("ggplot2") #used to draw nice pictures

library(R.matlab)
#data preparation
data <- readMat('faces.mat')
train <- data$traindata
dim(train)
test <- data$testdata
dim(test)
y.train <- as.vector(data$trainlabels)
y.test <- as.vector(data$testlabels)

#define distance function
cosdist <- function(x,y) {
  denom <- sqrt(sum(x^2)*sum(y^2))
  dist <- 1-(sum(x*y))/denom
  return(dist)
}
#calculate the distance matrix between test and train data points
dist <- NULL
for(j in 1:dim(test)[1]){
  for(i in 1:dim(train)[1]){
    dist <- c(dist,cosdist(test[j,],train[i,]))    
  }
}
dist.data <- matrix(dist, nrow=120, ncol=280, byrow=T)

#calculate the distance matrix between train and train data points
dist.tr <- NULL
for(j in 1:dim(train)[1]){
  for(i in 1:dim(train)[1]){
    dist.tr <- c(dist.tr,cosdist(train[j,],train[i,]))    
  }
}
dist.tr.data <- matrix(dist.tr, nrow=280, ncol=280, byrow=T)

#k-NN
major.response <- function(distance,label,k){
  label[sort.int(distance, index.return=T)$ix[1:k]] 
}
y.hat <- NULL
kNN <- function(distance,label,k){
  if(sum(as.numeric(major.response(distance,label,k)==1)) > k/2) y.hat <- c(y.hat, 1) else y.hat <- c(y.hat, 2)
  return(y.hat)
}
test.predict <- apply(dist.data, 1, kNN, k=20, label = y.train)
train.predict <- apply(dist.tr.data, 1, kNN, k=20, label = y.train)

print("test error")
1-sum(test.predict == y.test)/120 #0.3833333
print("train error")
1-sum(train.predict == y.train)/280 #0.3821429


#1.1.2
#n-fold cross validation for kNN to choose k, n=10
#randomly split the train data set into 10 parts of equal size of 28 data points
random.index <- matrix(sample(1:dim(train)[1]), nrow=10, ncol=28, T)

n = 10
train.cv.error <- NULL

j<-20
for(i in 1:n){
  train.predict <- apply(dist.tr.data[random.index[i,],random.index[-i,]], 1, kNN, k=j, label = y.train[random.index[-i,]])
  train.cv.error <- c(train.cv.error, 1-sum(train.predict == y.train[random.index[i,]])/28)   
}
print("train cv error")
sum(train.cv.error)/n #0.4678571


#1.2.3
#n-fold cross validation for kNN to choose k, n=10
#randomly split the train data set into 10 parts of equal size of 28 data points
random.index <- matrix(sample(1:dim(train)[1]), nrow=10, ncol=28, T)

n = 10
error <- NULL
train.error.cv <- NULL

for(j in 1:100){
  for(i in 1:n){
    train.predict <- apply(dist.tr.data[random.index[i,],random.index[-i,]], 1, kNN, k=j, label = y.train[random.index[-i,]])
    error <- c(error, 1-sum(train.predict == y.train[random.index[i,]])/28)   
    
  }
  train.error.cv <- c(train.error.cv, sum(error)/n)
  error <- NULL
}
plot(x=1:100, y=train.error.cv, type="l")

#calculate the train and test error
test.error <- NULL
train.error <- NULL
for(j in 1:100){
  test.predict <- apply(dist.data, 1, kNN, k=j, label = y.train)
  train.predict <- apply(dist.tr.data, 1, kNN, k=j, label = y.train)
  
  test.error<-c(test.error,1-sum(test.predict == y.test)/120)
  train.error<-c(train.error,1-sum(train.predict == y.train)/280)
}
plot(x=1:100, y=train.error, type="l")
plot(x=1:100, y=test.error, type="l")
graphics.off()

library(ggplot2)
err<-data.frame(x=1:100,y=c(train.error.cv,train.error,test.error),
                z=c(rep("train.cv.error",100),rep("train.error",100),rep("test.error",100)))
p <- ggplot(data=err, mapping=aes(x=x, y=y, shape=z,colour=z))
p+geom_point()+geom_line()
ggsave("113.png")



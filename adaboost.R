# Mengru Zhang 
# please set the path using setwd()
# problem 1
# read data
bupa <- unname(as.matrix(read.table("bupa.data", sep = ",",header=F)))
# map y=1 to -1, y=2 to 1
bupa[,7][bupa[,7]==1] <- (-1)
bupa[,7][bupa[,7]==2] <-  1

# define functions
# h1 used to determin the "stump" at each iteration step
h1 <- function(selCol){
  # random the step
  step <- sample(seq(1.0,6.0,by=0.1),1)
  possibleThres <- seq(min(selCol),max(selCol),by=step)
  weightedErr1 <- NULL
  weightedErr2 <- NULL
  for (each in possibleThres)
  {
    guessResult1 <- 1*(selCol >= each) + (-1)*(selCol < each)
    guessResult2 <- (-1)*(selCol >= each) + 1*(selCol < each)
    # compare with trainTruth
    weightedErr1 <- c(weightedErr1, sum(D*(guessResult1 != trainTruth)) )
    weightedErr2 <- c(weightedErr2, sum(D*(guessResult2 != trainTruth)) )
  }
  # return selCol, acc, threshold(k)
  if (min(weightedErr1) <= min(weightedErr2)) {
    k <- possibleThres[which.min(weightedErr1)] #threshold
    c(min(weightedErr1),k,1,-1)
  }
  else {
    k <- possibleThres[which.min(weightedErr2)] #threshold
    c(min(weightedErr2),k,-1,1)
  }
}

h1v6 <- function(selCol){
  # step = 0.5 is enough
  possibleThres <- seq(min(selCol),max(selCol),by = 0.5)
  weightedErr1  <- NULL
  weightedErr2  <- NULL
  for (each in possibleThres)
  {
    guessResult1 <- 1*(selCol >= each) + (-1)*(selCol < each)
    guessResult2 <- (-1)*(selCol >= each) + 1*(selCol < each)
    # compare with yisou
    weightedErr1 <- c(weightedErr1, sum(D*(guessResult1 != trainTruth)) )
    weightedErr2 <- c(weightedErr2, sum(D*(guessResult2 != trainTruth)) )
  }
  # return selCol, acc, threshold(k)
  if (min(weightedErr1) <= min(weightedErr2)) {
    k <- possibleThres[which.min(weightedErr1)] #threshold
    as.matrix( c(min(weightedErr1),k,1,-1), ncol=1)
  }
  else {
    k <- possibleThres[which.min(weightedErr2)] #threshold
    as.matrix( c(min(weightedErr2),k,-1,1), ncol=1)
  }
}


# H combines all the "weak h" to predict a value
H <- function(x){
  # count means the number of weak_h we have for now.(equals to the time of the loops have passed)
  count <- t
  # here x is a single element: x=[v1,v2,...,v7]
  weakPredict <- NULL
  for (j in 1:count) {
    alpha          <- weak_h[j,1]
    compareFeature <- weak_h[j,2]
    thresholdK     <- weak_h[j,3]
    C1             <- weak_h[j,4]
    C2             <- weak_h[j,5]
    # resugar to if (x[compareFeature] >= thresholdK) alpha*C1 else alpha*C2
    weakPredict <- c(weakPredict, alpha*(if (x[compareFeature] >= thresholdK) C1 else C2) )
  }
  sign2(sum(weakPredict))
}

sign2 <- function (x){if (x>=0) 1 else -1}
# end define functions

# split train set and test set
# set train and test set size
m <- round(dim(bupa)[1]*0.9)  # m=310, training
n <- dim(bupa)[1] - m         # n=35, test

testCount <- 50    # test "testCount" times
T         <- 100   # loop T times
trainAcc  <- NULL  # record accuracy of train set in each test.
testAcc   <- NULL  # record accuracy of test set in each test
weak_h    <- matrix(0,T,5) # weak classifier h
# each small classifier "h" need feature "selCol", threshold "c" in that column, coefficient "alpha", "C1", and "C2"

# record the training error for each weak_h[t] in the 50 random splits
# The default value is 1. so if classifier do nothing, it remains 1
weak_h.trainErr <- matrix(1,testCount,T)
# the same for test error 
weak_h.testErr <- matrix(1,testCount,T)

for(i in 1:testCount){
  # random split
  x.sample   <- sample(nrow(bupa),m,replace = FALSE)
  x.train    <- bupa[x.sample,]  #training
  x.test     <- bupa[-x.sample,]  #test
  trainTruth <- x.train[,7]
  testTruth  <- x.test[,7]
  # initialize
  alpha        <- NULL
  err          <- NULL
  D            <- rep(1/m,m)   #initialized Weight
  z            <- NULL
  thresholdCol <- NULL
  # begin the iteration
  for (t in 1:T){
    # ks means a number of k(six here)
    ks <- apply(x.train[,-(6:7)],2,h1)
    ks <- cbind(ks,h1v6(x.train[,6]))
    # (minWeightedErr, threshold k, C1, C2) == one col of ks
    thresholdCol <- which.min(ks[1,])
    minErr_t     <- ks[1,thresholdCol]
    k            <- ks[2,thresholdCol]
    C1           <- ks[3,thresholdCol]
    C2           <- ks[4,thresholdCol]
    
    err    <- c(err,minErr_t)
    # alpha, Z, D
    alpha <- c(alpha,0.5*log((1-err[t])/err[t]))
    tempz <- (1-err[t])*exp(-alpha[t]) + err[t]*exp(alpha[t])
    z     <- c(z,tempz)

    # each weak classifier h need selCol, threshold in that column, coefficient alpha, C1, and C2
    weak_h[t,]  <- c(alpha[t],thresholdCol,k,C1,C2)

    guessResult <- C1*(x.train[,thresholdCol] >= k) + C2*(x.train[,thresholdCol] < k)
    s           <- guessResult * trainTruth
    D           <- (D*exp(-alpha[t]*s))/z[t]

    # weak_h.trainErr[i,t] <- 
    tempPredict <- apply(x.train,1,H)
    weak_h.trainErr[i,t] <- mean(tempPredict != trainTruth)
    tempPredict <- apply(x.test,1,H)
    weak_h.testErr[i,t] <- mean(tempPredict != testTruth)
  }# end for 1:T
  # testIt
  trainPredict <- apply(x.train,1,H)
  trainAcc     <- c(trainAcc, mean(trainPredict == trainTruth))
  testPredict  <- apply(x.test,1,H)
  testAcc      <- c(testAcc, mean(testPredict == testTruth))
} # endfor 1:testCount

# print (weak_h)

# a vector consits 100 elemtns, represents the mean of error of (each weak_h stump)
meanWeakhTrainErr <- apply(weak_h.trainErr,2,mean)
meanWeakhTestErr <- apply(weak_h.testErr,2,mean)

print("meanWeakhTrainErr:")
print(meanWeakhTrainErr)
print("meanWeakhTestErr:")
print(meanWeakhTestErr)



# problem 2

x.train    <- bupa  #training
trainTruth <- x.train[,7]
# initialize
alpha        <- NULL
err          <- NULL
D            <- rep(1/(dim(bupa)[1]),dim(bupa)[1])   #initialized Weight
z            <- NULL
thresholdCol <- NULL

feature_j    <- NULL
threshold_c  <- NULL
label_C1     <- NULL

for (t in 1:10) {
  # ks means a number of k(six here)
  ks <- apply(x.train[,-(6:7)],2,h1)
  ks <- cbind(ks,h1v6(x.train[,6]))
  # (minWeightedErr, threshold k, C1, C2) == one col of ks
  thresholdCol <- which.min(ks[1,])
  minErr_t     <- ks[1,thresholdCol]
  k            <- ks[2,thresholdCol]
  C1           <- ks[3,thresholdCol]
  C2           <- ks[4,thresholdCol]
  
  feature_j   <- c(feature_j,thresholdCol)
  threshold_c <- c(threshold_c,k)
  label_C1    <- c(label_C1,C1)

  err         <- c(err,minErr_t)
  # alpha, Z, D
  alpha <- c(alpha,0.5*log((1-err[t])/err[t]))
  tempz <- (1-err[t])*exp(-alpha[t]) + err[t]*exp(alpha[t])
  z     <- c(z,tempz)

  # each weak classifier h need selCol, threshold in that column, coefficient alpha, C1, and C2
  weak_h[t,]  <- c(alpha[t],thresholdCol,k,C1,C2)

  guessResult <- C1*(x.train[,thresholdCol] >= k) + C2*(x.train[,thresholdCol] < k)
  s           <- guessResult * trainTruth
  D           <- (D*exp(-alpha[t]*s))/z[t]

  tempPredict <- apply(x.train,1,H)
  weak_h.trainErr[i,t] <- mean(tempPredict != trainTruth)

  print (paste("in loop",t))
  print (paste("feature_j:",  feature_j[t]))
  print (paste("threshold_c:",threshold_c[t]))
  print (paste("label_C1:",   label_C1[t]))

}# end for 1:T

print ("aggregated display:")
print ("feature_j")
print (feature_j)
print ("threshold_c")
print (threshold_c)
print ("label_C1")
print (label_C1)


# problem 3
# H_ only return the sum of weighted weak classifiers
H_ <- function(x, count){
  # count means the number of weak_h we have for now.(equals to the time of the loops have passed)
  count <- t
  # here x is a single element: x=[v1,v2,...,v7]
  weakPredict <- NULL
  for (j in 1:count) {
    alpha          <- weak_h[j,1]
    compareFeature <- weak_h[j,2]
    thresholdK     <- weak_h[j,3]
    C1             <- weak_h[j,4]
    C2             <- weak_h[j,5]
    # resugar to if (x[compareFeature] >= thresholdK) alpha*C1 else alpha*C2
    weakPredict <- c(weakPredict, (alpha/alphaSum) *(if (x[compareFeature] >= thresholdK) C1 else C2) )
  }
  sum(weakPredict)
}



# iteration
# initialize
alpha        <- NULL
err          <- NULL
D            <- rep(1/(dim(bupa)[1]),dim(bupa)[1])   #initialized Weight
z            <- NULL
thresholdCol <- NULL
fT           <- NULL
margin       <- NULL
for (t in 1:100) {
  # ks means a number of k(six here)
  ks <- apply(x.train[,-(6:7)],2,h1)
  ks <- cbind(ks,h1v6(x.train[,6]))
  # (minWeightedErr, threshold k, C1, C2) == one col of ks
  thresholdCol <- which.min(ks[1,])
  minErr_t     <- ks[1,thresholdCol]
  k            <- ks[2,thresholdCol]
  C1           <- ks[3,thresholdCol]
  C2           <- ks[4,thresholdCol]

  err   <- c(err,minErr_t)
  # alpha, Z, D
  alpha <- c(alpha,0.5*log((1-err[t])/err[t]))
  tempz <- (1-err[t])*exp(-alpha[t]) + err[t]*exp(alpha[t])
  z     <- c(z,tempz)

  # each weak classifier h need selCol, threshold in that column, coefficient alpha, C1, and C2
  weak_h[t,]  <- c(alpha[t],thresholdCol,k,C1,C2)

  guessResult <- C1*(x.train[,thresholdCol] >= k) + C2*(x.train[,thresholdCol] < k)
  s           <- guessResult * trainTruth
  D           <- (D*exp(-alpha[t]*s))/z[t]
  
  if (t==10){
    alphaSum <- sum(head(alpha,t))
    fT       <- apply(x.train,1,H_)
    margin1  <- trainTruth * fT    
  }
  if (t==50) {
    alphaSum <- sum(head(alpha,t))
    fT       <- apply(x.train,1,H_)
    margin2  <- trainTruth * fT
  }
  if (t==100) {
    alphaSum  <- sum(head(alpha,t))
    fT        <- apply(x.train,1,H_)
    margin3   <- trainTruth * fT
  }
}# end for 1:T

y<-NULL
ecdf1 <- function(x){
  for(i in 1:dim(bupa)[1]){
    y<-c(y,sum(margin1<=x[i])/dim(bupa)[1])
  }
  y
}
ecdf2 <- function(x){
  for(i in 1:dim(bupa)[1]){
    y<-c(y,sum(margin2<=x[i])/dim(bupa)[1])
  }
  y
}
ecdf3 <- function(x){
  for(i in 1:dim(bupa)[1]){
    y<-c(y,sum(margin3<=x[i])/dim(bupa)[1])
  }
  y
}

# plot
min_ <- min(cbind(margin1,margin2, margin3))
max_ <- max(cbind(margin1,margin2, margin3))
x.value <- seq(min_,max_, by =(max_-min_)/(345-1))
color   <-rainbow(3)

### plot
graphics.off()
library(graphics)
par(mfrow=c(1,2)) 
matplot(1:100,cbind(meanWeakhTrainErr,meanWeakhTestErr),xlab="t",ylab="error rate",type="l",col=c("blue","red"))
legend("topright", c("meanTrainErr", "meanTestErr"),cex=0.5, pt.cex = 0.5, lty = 1, col = c("blue","red"))
matplot(x.value,cbind(ecdf1(x.value),ecdf2(x.value),ecdf3(x.value)),xlab="x",ylab="empirical cdf",type="l",col=color)
legend("topleft", c("T=10", "T=50", "T=100"), cex=0.5, pt.cex = 0.5,lty=1, col=color)


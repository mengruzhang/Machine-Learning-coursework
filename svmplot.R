install.packages("ggplot2")
library(ggplot2)
#plot the graph in svm
cv.error.svm<-c(0.6071,0.4929,0.4357,0.3321,0.3500,0.3286,0.3500,0.3464)
test.error.svm<-c(0.4333,0.4333,0.3500,0.2583,0.2333,0.2333,0.2333,0.2333)
train.error.svm<-c(0.3964,0.3964,0.2571,0.0679,0,0,0,0)
err.svm<-data.frame(x=log10(c(10, 10^2, 10^3, 10^4, 5*10^4, 10^5, 5*10^5, 10^6)),y=c(cv.error.svm,train.error.svm,test.error.svm),
                    z=c(rep("cv.error.svm",8),rep("train.error.svm",8),rep("test.error",8)))
p.svm <- ggplot(data=err.svm, mapping=aes(x=x, y=y, shape=z,colour=z))
p.svm+geom_point()+geom_line()
ggsave("123.png")

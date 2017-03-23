#Mengru Zhang 
data<-read.table("em.txt")
x<-as.numeric(unlist(data))
data<-as.data.frame(x=x)
library(ggplot2)

density<-function(x,mean){
  exp(-(x-mean)^2/2)
}

s<-c(0.33,0.67,1,2)
em <- function(x,s) {
  p0=s[1]*density(x,s[3])/(s[1]*density(x,s[3])+s[2]*density(x,s[4]))
  p1=s[2]*density(x,s[4])/(s[1]*density(x,s[3])+s[2]*density(x,s[4]))
  s[1]=sum(p0)/(sum(p0)+sum(p1))
  s[2]=sum(p1)/(sum(p0)+sum(p1))
  s[3]=sum(p0*x)/sum(p0)
  s[4]=sum(p1*x)/sum(p1)
  s
}

s1=em(x,s)
while(abs(s-s1)[1]>0.0001 & abs(s-s1)[2]>0.0001 & 
        abs(s-s1)[3]>0.0001 & abs(s-s1)[4]>0.0001){
  s=s1
  s1=em(x,s)
}

p<-s1
print(c("theta0","theta1","mu0","mu1"))
p

#plot
est1<-p[1]*dnorm(x, mean=p[3], sd=1)
est2<-p[2]*dnorm(x, mean=p[4], sd=1)
gm<-est1+est2

p1<-ggplot()+geom_line(data=data.frame(x,gm),aes(x=x,y=gm))

m <- ggplot(data, aes(x = x))
p2<-m + geom_histogram(binwidth=(range(x)[2]-range(x)[1])/30,colour="black",
                       fill="skyblue",aes(y = ..density..))
#share x-axis in one plot
p2+geom_line(data=data.frame(x,gm),aes(x=x,y=gm))
ggsave("em1.png")

#contour plot
l<-function(m){
  sum(log(0.5*dnorm(x,m[1],1)+0.5*dnorm(x,m[2],1)))
}
model<-function(a,b){
  temp<-cbind(a,b)
  apply(temp,1,l)  
}

miu<-as.numeric(seq(-1, 4, by=0.25))
mat <- outer(miu,miu,model)
image(miu, miu, mat)
contour(miu, miu, mat, col = "blue",  method = "edge",
        vfont = c("sans serif", "plain"), add=T)

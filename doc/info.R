## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4,fig.align = "center") 
knitr::opts_chunk$set(collapse = TRUE,comment = "#>")

## ----setup--------------------------------------------------------------------
library(rpackage)

## -----------------------------------------------------------------------------
mychisim(n=10,sigma=4,mean=10,iter=1000,ymax=0.1)

## -----------------------------------------------------------------------------
c<-mychisim(n=10,sigma=4,mean=10,iter=1000,ymax=0.1)

## -----------------------------------------------------------------------------
set.seed(40)
sam<-rnorm(25,mean=25,sd=10)
myboot(x=sam,fun="mean",iter=10000, xlab="mean",col="Purple")

## -----------------------------------------------------------------------------
set.seed(40)
sam<-rgamma(30,shape=2,scale=3)
myboot(x=sam,fun="var",iter=10000,xlab="variance", col="Blue",alpha=0.20)

## -----------------------------------------------------------------------------
set.seed(40)
sam<-rbeta(30,shape1=2,shape2=3)
b<-myboot(x=sam,fun="median",iter=10000,xlab="median", col="Yellow",alpha=0.20)

## -----------------------------------------------------------------------------
sam<-rbeta(30,shape1=3,shape2=4)
nsam<-length(sam)
mymlbeta(x=sample(sam,nsam,replace=TRUE),alpha=seq(0.1,20,length=100),beta=seq(0.1,20,length=100),lwd=2,labcex=1,col="steelblue")

## -----------------------------------------------------------------------------
sam<-rbeta(50,shape1=3,shape2=4)
nsam<-length(sam)
m<-mymlbeta(x=sample(sam,nsam,replace=TRUE),alpha=seq(0.1,20,length=100),
beta=seq(0.1,20,length=100),lwd=2,labcex=1,col="red")

## -----------------------------------------------------------------------------
mycltp(n=20,iter=10000,lambda=10)

## -----------------------------------------------------------------------------
mycltp(n=2,iter=10000,lambda=4)

## -----------------------------------------------------------------------------
p<-mycltp(n=5,iter=10000,lambda=10)

## -----------------------------------------------------------------------------
p<-mycltp(n=5,iter=10000,lambda=4)

## -----------------------------------------------------------------------------
head(POWERLOADS)


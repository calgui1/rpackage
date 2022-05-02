#' Maximum Likelihood Function for Beta Distribution
#'
#' @importFrom graphics axis contour abline points par
#' @importFrom stats dbeta
#'
#' @description A function that takes in a sample from the beta distribution along
#' with alpha and beta of the sample and produces a graph showing the maximum
#' likelihood function for the sample.
#'
#' @param x sample vector
#' @param alpha vector of alpha values
#' @param beta vector of beta values
#' @param ... additional plot labels/settings
#'
#' @return Graph showing the maximum likelihood for Beta
#' @export
#'
#' @examples
#' sam<-rbeta(30,shape1=3,shape2=4)
#' nsam<-length(sam)
#' mymlbeta(x=sample(sam,nsam,replace=TRUE),alpha=seq(0.1,20,length=100),
#' beta=seq(0.1,20,length=100),lwd=2,labcex=1,col="steelblue")
#'
mymlbeta=function(x,alpha,beta,...){  #x sample vector
  na=length(alpha) # number of values in alpha
  nb=length(beta)
  n=length(x) # sample size
  zz=c()    ## initialize a new vector
  lfun=function(x,a,b) log(dbeta(x,shape1=a,shape2=b))   # log lik for beta
  for(j in 1:nb){
    z=outer(x,alpha,lfun,b=beta[j]) # z a matrix
    # col 1 of z contains lfun evaluated at each x with first value of alpha,
    # col2 each x with 2nd value of a
    # all with b=beta[j]
    y=apply(z,2,sum)
    # y is a vector filled with log lik values,
    # each with a difft alpha and all with the same sig[j]
    zz=cbind(zz,y)
    ## zz is the matrix with each column containing log L values, rows difft alpha, cols difft betas
  }
  par(mar = c(2, 2, 2, 2), oma = c(2,2,2,2))
  maxl=max(exp(zz))    # max lik
  coord=which(exp(zz)==maxl,arr.ind=TRUE)  # find the co-ords of the max
  aest=alpha[coord[1]] # mxlik estimate of alpha
  best=beta[coord[2]]
  contour(alpha,beta,exp(zz),las=3,xlab=expression(alpha),ylab=expression(beta),axes=TRUE,
          main=expression(paste("L(",alpha,",",beta,")",sep="")),...)

  abline(v=aest, h=best)
  points(aest,best,pch=19)
  axis(4,best,round(best,2),col="Red")
  axis(3,aest,round(aest,2),col="Red")
  return(list(x=x,coord=coord,maxl=maxl,maxalpha=aest,maxbeta=best))
}


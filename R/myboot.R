#' Bootstrap Function for Re-sampling
#'
#' @description Create bootstrapped samples of any distribution by supplying the sample vector, number of iterations
#' for the bootstrapping technique, the function to be applied to the sample, and the alpha value for the probability statistic.
#'
#' @importFrom stats quantile
#' @importFrom graphics abline segments
#'
#' @param iter number of trials to be performed
#' @param x data used to create samples
#' @param fun function to apply to sample matrix
#' @param alpha 1-alpha is the percent of the confidence interval
#' @param cx the amount by which text and symbols on graph should be magnified
#' @param ... additional labels and settings for plot
#'
#' @return Histogram of the sample statistic function, confidence interval for
#' the data, function used for the statistic, and the original sample vector
#' @export
#'
#' @examples
#' set.seed(39)
#' sam<-rnorm(25,mean=25,sd=10)
#' myboot(x=sam,fun="mean",iter=10000, xlab="mean",col="Purple")
#'
#' set.seed(40)
#' sam<-rgamma(30,shape=2,scale=3)
#' myboot(x=sam,fun="var",iter=10000,xlab="variance", col="Blue",alpha=0.20)
#'
myboot<-function(x,iter,fun,alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  return(list(ci=ci,fun=fun,x=x,para=para))# Some output to use if necessary
}

#' Chi Squared Function Simulator
#'
#' @description Takes in the size of sample, standard deviation of the sample,
#' mean of the sample, number of iterations of samples, and the maximum y value
#' for the sample and returns a histogram of the Chi Squared statistic for the
#' sample as well as a density plot containing a theoretical normal curve.
#'
#' @param n the size of each sample
#' @param sigma standard deviation of the sample
#' @param mean mean for the sample
#' @param iter number of iterations
#' @param ymax maximum y value for sample
#' @param ... additional labels for plot
#'
#' @importFrom stats dchisq density sd var rnorm
#' @importFrom graphics lines curve legend hist text
#'
#' @return Chisq Plot of input sample, Chi-Squared Statistic w, summary for the
#' statistic, and the standard deviations of the statistic.
#' @export
#'
#' @examples
#' mychisim(n=10,sigma=4,mean=10,iter=1000,ymax=0.1)
#'
mychisim<-function(n,sigma,mean,iter,ymax,...){
  y=rnorm(n*iter,mean=mean,sd=sigma)# generate iter samples of size n1

  data.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # Each column is a sample size n1
  ssq=apply(data.mat,2,var) # ssq1 is s squared

  w=(n-1)*ssq/sigma^2      #chi-sq stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation

       main=substitute(paste("Sample size = ",n[1]," = ",n," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)

  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dchisq(x,df=n-1),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2)) #mathematical annotation -see ?plotmath
  legend(x="right",c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #

  return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # some output to use if needed
}

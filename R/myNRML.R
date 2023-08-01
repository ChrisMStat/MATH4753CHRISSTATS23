#' Title
#'
#' @param x0 x0 value; such as 1
#' @param delta delta; value such as 0.000001
#' @param llik used to pass a function; see example for more information (each dpois has '*' between them but it broke this documentation so they were removed. Please include those is using the example :) )
#' @param xrange the numerical range of x
#' @param parameter parameter; such as 'lambda'
#'
#' @return a plot and all the dat regarding the plot
#' @export
#'
#' @examples
#' myNRML(x0=1,delta=0.000001,llik=function(x) log(dpois(4,x)dpois(6,x)dpois(7,x)dpois(6,x)dpois(5,x)),xrange=c(0,20),parameter="lambda" )
#'
myNRML=function(x0,delta=0.001,llik,xrange,parameter="param"){
  f=function(x) (llik(x+delta)-llik(x))/delta
  fdash=function(x) (f(x+delta)-f(x))/delta
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<100){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i+1])
  }
  layout(matrix(1:2,nr=1,nc=2,byrow=TRUE),width=c(1,2))
  curve(llik(x), xlim=xrange,xlab=parameter,ylab="log Lik",main="Log Lik")
  curve(f(x),xlim=xrange,xaxt="n", xlab=parameter,ylab="derivative",main=  "Newton-Raphson Algorithm \n on the derivative")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")

  list(x=x,y=y)
}

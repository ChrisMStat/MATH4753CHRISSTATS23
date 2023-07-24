#' Normal Curve Plotting and Calculation
#'
#' @param mu is the mean
#' @param sigma is the standard deviation
#' @param a is our supplied x value
#'
#' @return a plot of the curve, shaded area, and area probability to the console
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5, a=6)
#'
myncurve = function(mu, sigma, a){

  # This is to avoid this NOTE when running Check:
  # " myncurve: no visible binding for global variable ‘x’
  # Undefined global functions or variables: x "
  x <- y <- NULL

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-(3*sigma), mu + (3*sigma)))

  # Find the area x<=a
  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(mu-(3*sigma),a,length=1000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(mu-(3*sigma),xcurve,a),c(0,ycurve,0),col="Red")

  # Put in the text with the appropriate area

  # Area
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)

  # text placement
  text(x=((mu-(3*sigma))+a)/2, y=ycurve[500], paste("Area = ", prob, sep=""))

  l=list(prob)
  l

}

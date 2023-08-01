#'Piecewise Function
#'
#' @param datadf the data from the correct file
#' @param xk the variable used in piecewise linear model
#'
#' @return a plot and various values of data
#' @export
#'
#' @examples
#' piecewise_lm(spruce.df, 18)
#'
piecewise_lm = function(datadf, xk) {
  BHDiameter=NULL # to get rid of package warning
  ## piecewise linear model in R
  ## Model y = b0 + b1x + b2(x-xk)*(x>xk)
  ## You will need to change the code appropriately
  sp2.df=within(datadf, X<-(BHDiameter-xk)*(BHDiameter>xk)) # this makes a new variable and places it within the same df
  sp2.df

  lmp=lm(Height~BHDiameter + X,data=sp2.df)
  tmp=summary(lmp)
  names(tmp)
  x=NULL # to get rid of package warning
  myf = function(x,coef){
    coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
  }
  plot(datadf,main="Piecewise regression")
  myf(0, coef=tmp$coefficients[,"Estimate"])
  curve(myf(x,coef=tmp$coefficients[,"Estimate"] ),add=TRUE, lwd=2,col="Blue")
  abline(v=xk)
  text(xk,16,paste("R sq.=",round(tmp$r.squared,4) ))
}

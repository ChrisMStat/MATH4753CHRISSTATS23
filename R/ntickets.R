#' Airline Ticket Overbooking Problem
#'
#' @param N number of seats available on plane
#' @param gamma gamma value
#' @param p probability value
#'
#' @return a list including values: gamma, N, p, nd, nc & two plots (one for nd, one for nc)
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve layout polygon text
#' @importFrom stats dnorm optimize pbinom pnorm
#'
#' @examples
#' ntickets(N=200, gamma=0.02, p=0.95)
#'
ntickets = function(N, gamma, p){

  ### Testing Purposes

  # gamma = 0.02
  # N=200
  # p=0.95

  ### Testing Purposes

  # range for possible n values
  n = N:(N*1.10)

  # used for continuous formula
  q=1-p

  # discrete distribution
  discrete = (1-gamma)-pbinom(N, n, p)

  # finding the index of the correct value for n
  index1 = which.min(abs(discrete))
  nd = n[index1]
  # nd # testing purposes

  # normal approximation

  # function since optimize() requires a function as an argument
  continuous_fun = function(n) {
    abs( (1-gamma) - pnorm(N + 0.5, mean = n*p, sd = sqrt(n*p*q) ) )
  }

  # continuous formula (norm)
  continuous = (1-gamma) - pnorm(N + 0.5, mean = n*p, sd = sqrt(n*p*q) )

  # finally setting nc to the correct value
  nc = optimize(continuous_fun, interval = n)$minimum
  # nc # testing purposes

  # prints list of required values
  print(list(nd=nd, nc=nc, N = N, p = p, gamma = gamma))

  # setting layout for plots
  layout(matrix(1:2, nrow=2,ncol=2))

  # plot the discrete distribution
  plot(n, discrete, type='b', ylab = "Objective", col="black", main=paste("Objective Vs n to find optimal tickets sold\n", "(", nd, ")", ", gamma=", gamma, ", N=", N, ", p=", p, ", discrete") )

  # places a different (blue) colored "crosshairs" centered exactly on the correct
  # value of n found through discrete distribution
  abline(h = 0, v = nd, lwd = 2, col = "blue")

  # plot the continuous distribution (normal)
  plot(n, continuous, type='l', ylab = "Objective", col="black", main=paste("Objective Vs n to find optimal tickets sold\n", "(", nc, ")", ", gamma=", gamma, ", N=", N, ", p=", p, ", continuous") )

  # places a different colored (red) "crosshairs" centered exactly on the correct
  # value of n found through continuous distribution (normal)
  abline(h = 0, v = nd, lwd = 2, col = "red")

}

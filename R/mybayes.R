#' Bayes Theorem Function
#'
#' @param x corresponds to P(A) of theorem
#' @param y corresponds to P(B) of theorem
#' @param z corresponds to P(B|A) of theorem
#'
#' @return the probability value of an event using Bayes Theorem (i.e. P(A|B))
#' @export
#'
#' @examples
#' mybayes(0.90, 0.10, 0.5)
#'
mybayes = function(x, y, z) {
  result <- x*z/y
  return(result)
}

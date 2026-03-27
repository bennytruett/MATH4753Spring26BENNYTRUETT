#' @title Normal Curve
#'
#' @param mu the mean of the curve
#' @param sigma the sigma of the curve
#' @param a the upper bound of Y
#'
#' @returns displays the curve with a shaded area between the curve and x-axis from negative infinity to x = a
#'          and calculate the area which is released to the command-line in a list
#' @importFrom stats dnorm pnorm
#' @importFrom graphics curve polygon
#' @export
#'
#' @examples
#' myncurve(10, 4, 10)
#'
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma, a,length=100)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  prob=pnorm(a,mean=mu,sd=sigma)
  list(mu = mu, sigma = sigma, a = a, probability = prob)
}

#' @title Selling Plane Tickets
#'
#' @param N the number of seats in the flight
#' @param gamma the probability a plane will be truly overbooked
#' @param p the probability of a "show"
#'
#' @returns the number of tickets to be sold, displays a graph of two plots showing
#' the discrete and continuous distributions of the problem, as well as a named list of each variable
#' @importFrom stats qbinom qnorm pbinom pnorm
#' @importFrom graphics abline par
#' @export
#'
#' @examples
#' ntickets(N = 200, gamma = 0.02, p = 0.95)
#'
ntickets <- function(N, gamma, p){
  N_values <- N:(N+(N/10))
  nd <- N_values[which(qbinom(1 - gamma, N_values, p) >= N)[1]]
  nc <- N_values[which(qnorm(1 - gamma, mean = N_values * p, sd = sqrt(N_values * p * (1 - p))) >= N)[1]]

  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

  par(mfrow = c(2,1))

  plot(N_values, 1 - gamma - pbinom(N, N_values, p), type = "b", pch = 19, col = "blue",
       main = paste("Objective vs n to find optimal tickets sold \n (", nd, ") gamma = ", gamma, "N = ", N, "discrete"),
       xlab = "n (tickets sold)", ylab = "Objective")

  abline(h = 0, col = "red")
  abline(v = nd, col = "red")

  plot(N_values, 1 - gamma - pnorm(N, mean = N_values*p, sd = sqrt(N_values*p*(1-p))), type = "l", col = "black", pch = 19, main = paste("Objective vs n to find optimal tickets sold \n (", nc, ") gamma = ", gamma, "N = ", N, "coninuous"), xlab = "n (Tickets Sold)", ylab = "Objective")
  abline(h = 0, col = "blue")
  abline(v = nc, col = "blue")

  return(nd)

}

#' ntickets
#'
#' @param N number of seats
#' @param gamma probability of overbooking
#' @param p probability of a show from a passenger
#'
#' @return list containing provided arguments, number of tickets to sell using discrete distribution,
#' and number to sell using normal approximation
#' @export
#' @importFrom stats qbinom qnorm pbinom pnorm optimise
#' @importFrom graphics curve abline
#'
#' @examples
#' ntickets(N=400,gamma=0.02,p=0.95)
ntickets <- function(N=400, gamma=0.02, p=0.95) {
  # Discrete nd
  discFn <- function(n) { abs(N - qbinom(1-gamma,n,p)) }
  discOptResult <- optimise(discFn, lower=1, upper=N+N*gamma*5)
  nd <- round(discOptResult$minimum)

  # Normal Approximation nc
  normApproxFn <- function(n) { abs(qnorm(1-gamma, n * p, sqrt(n * p * (1 - p))) - N - 0.5) }
  normApproxOptResult <- optimise(normApproxFn, lower=1, upper=N+N*gamma*5)
  nc <- normApproxOptResult$minimum

  nVals <- c(N:(N+N*gamma*5))

  # Discrete plot
  discObjVals <- sapply(nVals, function(n) { 1 - gamma - pbinom(N,n,p)})
  plot(nVals, discObjVals, main=paste0("Discrete Objective vs n optimal \ntickets sold (n=",nd," N=",N," gamma=",gamma, ")"), xlab="n", ylab="Objective")
  abline(v=nd, col="Red")
  abline(h = 0)
  abline(v = 0)

  # Normal Approximation plot
  napf <- function(n) { 1 - gamma - pnorm(N + 0.5,n*p, sqrt(n*p*(1-p))) }
  n <- NULL
  curve(napf(n), xlim=c(N,N+N*gamma*5), main=paste0("Continuous Objective vs n optimal \ntickets sold (n=",round(nc,4)," N=",N," gamma=",gamma, ")"), xlab="n",xname="n", ylab="Objective")
  abline(v=nd, col="Blue")
  abline(h = 0)
  abline(v = 0)

  list(N=N, p=p, gamma=gamma, nc=nc, nd=nd)
}

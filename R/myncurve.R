#' Title
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a max x-value of shaded region
#'
#' @return curve and list of mean, standard deviation, and area
#' @export
#'
#' @examples
#' myncurve(5,1, 4)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  area=pnorm(a,mean=mu,sd=sigma)
  area=round(area,4)
  text(x=a+0.5, y=0.07, paste("Area = ", area, sep=""))
  list(mu = mu, sigma = sigma, area=area)
}

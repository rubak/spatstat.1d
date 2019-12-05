#' Point pattern on single line segment
#'
#' @param x numeric vector of points on the line segment
#' @param domain numeric of length 2 with start and end of line segment
#' @param marks vector or `data.frame` with marks for the points given in `x`.
#'
#' @return object of class `lpp1` and `lpp`.
#' @export
#' @import spatstat
#'
#' @examples
#' L <- c(0, 5)
#' x <- runif(10, L[1], L[2])
#' m <- data.frame(a = sample(letters, length(x)), b = seq_along(x))
#' X <- lpp1(x, domain = L, marks = m)
lpp1 <- function(x, domain, marks=NULL){
  if(!is.numeric(domain) || length(domain)!=2 || domain[1]>=domain[2]){
    stop('domain needs to be a length two vector with second element larger than first')}
  n <- length(x)
  v <- ppp(x = domain, y = c(0,0), window = owin(domain, c(-0.5,0.5)))
  m <- matrix(c(FALSE,TRUE,TRUE,FALSE), ncol=2)
  L <- linnet(vertices=v, m=m)
  X <- lpp(X=cbind(x, rep(0,n)), L=L, marks=marks)
  class(X) <- c("lpp1", class(X))
  return(X)
}

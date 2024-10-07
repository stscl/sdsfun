#' @title standardization
#' @description
#' To calculate the Z-score using variance normalization, the formula is as follows:
#'
#' \eqn{Z = \frac{(x - mean(x))}{sd(x)}}
#'
#' @param x A numeric vector
#'
#' @return A standardized numeric vector
#' @export
#'
#' @examples
#' standardize_vector(1:10)
#'
standardize_vector = \(x){
  return((x - mean(x,na.rm = TRUE)) / stats::sd(x,na.rm = TRUE))
}

#' @title normalization
#'
#' @param x A continuous numeric vector.
#' @param to_left (optional) Specified minimum. Default is `0`.
#' @param to_right (optional) Specified maximum. Default is `1`.
#'
#' @return A continuous vector which has normalized.
#' @export
#'
#' @examples
#' normalize_vector(c(-5,1,5,0.01,0.99))
#'
normalize_vector = \(x,to_left = 0,to_right = 1){
  xmin = range(x,na.rm = TRUE)[1]
  xmax = range(x,na.rm = TRUE)[2]
  xnew = (x - xmin) / (xmax - xmin) * (to_right - to_left) + to_left
  return(xnew)
}

#' @title discretization
#'
#' @param x A continuous numeric vector.
#' @param n (optional) The number of discretized classes.
#' @param method (optional) The method of discretization, default is `natural`.
#' @param breakpoint (optional) Break points for manually splitting data. When
#' `method` is `manual`, `breakpoint` is required.
#' @param sampleprob (optional) When the data size exceeds `3000`, perform sampling
#' for discretization, applicable only to natural breaks. Default is `0.15`.
#' @param seed (optional) Random seed number, default is `123456789`.
#'
#' @return A discretized integer vector
#' @export
#'
#' @examples
#' xvar = c(22361, 9573, 4836, 5309, 10384, 4359, 11016, 4414, 3327, 3408,
#'          17816, 6909, 6936, 7990, 3758, 3569, 21965, 3605, 2181, 1892,
#'          2459, 2934, 6399, 8578, 8537, 4840, 12132, 3734, 4372, 9073,
#'          7508, 5203)
#' discretize_vector(xvar, n = 5, method = 'natural')
#'
discretize_vector = \(x, n, method = 'natural',
                      breakpoint = NULL,
                      sampleprob = 0.15,
                      seed = 123456789){
  if (method %in% c("sd","equal","geometric","quantile")){
    res = eval(parse(text = paste0(method,"Disc(x,n)")))
  } else if (method == "manual") {
    if (is.null(breakpoint)) {
      stop("When method is manual, breakpoint is required.")
    } else {
      res = manualDisc(x,breakpoint)
    }
  } else if (method == "natural") {
    base::set.seed(seed)
    res = naturalDisc(x,n,sampleprob)
  } else {
    stop("Only support those methods: sd,equal,quantile,geometric,natural and manual.")
  }
  return(res)
}

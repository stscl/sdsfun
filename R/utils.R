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
#' @param n (optional) The number of discretized, default is `6`
#' @param method (optional) The method of discretization, default is `sd`
#'
#' @return A discretized integer vector
#' @export
#'
#' @examples
#' xvar = c(22361, 9573, 4836, 5309, 10384, 4359, 11016, 4414, 3327, 3408,
#'          17816, 6909, 6936, 7990, 3758, 3569, 21965, 3605, 2181, 1892,
#'          2459, 2934, 6399, 8578, 8537, 4840, 12132, 3734, 4372, 9073,
#'          7508, 5203)
#' discretize_vector(xvar, n = 5, method = 'sd')
#'
discretize_vector = \(x, n = 6, method = 'sd'){
  if (method %in% c("sd","equal","geometric","quantile","natural")){
    res = eval(parse(text = paste0(method,"Disc(x,n)")))
    return(res)
  } else {
    stop("Only support those methods: equal, natural, quantile, geometric and sd")
  }
}

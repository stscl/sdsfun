#' @title determine optimal spatial data discretization for individual variables
#' @description
#' Function for determining optimal spatial data discretization for individual variables
#' based on locally estimated scatterplot smoothing (LOESS) model.
#' @note
#' When `increase_rate` is not satisfied by the calculation, the discrete number corresponding
#' to the highest `q statistic` is selected as a return.
#'
#' Note that `sdsfun` sorts `discnumvec` from smallest to largest and keeps `qvec` in
#' one-to-one correspondence with `discnumvec`.
#'
#' @param qvec A numeric vector of q statistics.
#' @param discnumvec A numeric vector of break numbers corresponding to `qvec`.
#' @param increase_rate (optional) The critical increase rate of the number of discretization.
#' Default is `0.05`.
#'
#' @return A two element numeric vector.
#' \describe{
#' \item{\code{discnum}}{optimal number of spatial data discretization}
#' \item{\code{increase_rate}}{the critical increase rate of the number of discretization}
#' }
#' @export
#'
#' @examples
#' qv = c(0.26045642,0.64120405,0.43938704,0.95165535,0.46347836,
#'        0.25385338,0.78778726,0.95938330,0.83247885,0.09285196)
#' loess_optnum(qv,3:12)
#'
loess_optnum = \(qvec, discnumvec, increase_rate = 0.05){
  qvec = qvec[which(!is.na(qvec))] # debug: remove NA value in qvec and discnumver.
  discnumvec = discnumvec[which(!is.na(qvec))]
  discnumrank = order(discnumvec) # debug: sort discnumvec from smallest to largest
  qvec = qvec[discnumrank]
  discnumvec = discnumvec[discnumrank]
  loessf = stats::loess(qvec ~ discnumvec)
  lr = (loessf$fitted - dplyr::lag(loessf$fitted)) / dplyr::lag(loessf$fitted)
  lr[which(is.na(lr))] = 0
  lr_before = dplyr::lag(lr,default = 0)
  lr_indice = which(lr <= increase_rate & lr_before > increase_rate)[1]

  # debug: when no increase_rate is satisfied, the highest Q-statistic is selected
  if (is.na(lr_indice)){
    res = c('discnum' = discnumvec[which.max(qvec)],
            'increase_rate' = 0)
  } else {
    res = c('discnum' = discnumvec[lr_indice],
            'increase_rate' = increase_rate)
  }
  return(res)
}

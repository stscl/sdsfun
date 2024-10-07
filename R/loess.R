#' @title determine optimal spatial data discretization for individual variables
#' @description
#' Function for determining optimal spatial data discretization for individual variables
#' based on locally estimated scatterplot smoothing (LOESS) model.
#'
#' @note
#' When `increase_rate` is not satisfied by the calculation, `increase_rate*0.5` is used first.
#' At this time, if `increase_rate*0.5` is not satisfied again, the discrete number corresponding
#' to the highest `q statistic` is selected as a return.
#' @note
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
#' loess_optnum(stats::runif(10),3:12)
#'
loess_optnum = \(qvec, discnumvec, increase_rate = 0.05){
  qvec = qvec[which(!is.na(qvec))] # debug: remove NA value in qvec and discnumver.
  discnumvec = discnumvec[which(!is.na(qvec))]
  discnumrank = order(discnumvec) # debug: sort discnumvec from smallest to largest
  qvec = qvec[discnumrank]
  discnumvec = discnumvec[discnumrank]
  loessf = stats::loess(qvec ~ discnumvec)
  loessrate = (loessf$fitted - dplyr::lag(loessf$fitted)) / dplyr::lag(loessf$fitted)
  increase_rate = ifelse(max(loessrate,na.rm = TRUE) < increase_rate,
                         increase_rate * 0.5, increase_rate)
  lrtbf = tibble::tibble(discnum = discnumvec,
                         qstatistic = qvec,
                         lr = loessrate,
                         lr_before = dplyr::lag(lr)) %>%
    dplyr::filter(lr <= increase_rate & lr_before > increase_rate)

  # debug: when no increase_rate is satisfied, the highest Q-statistic is selected
  if (is.na(lrtbf[1,1,drop = TRUE])){
    res = c('discnum' = discnumvec[which.max(qvec)],
            'increase_rate' = increase_rate)
  } else {
    res = c('discnum' = lrtbf[1,1,drop = TRUE],
            'increase_rate' = increase_rate)
  }
  return(res)
}

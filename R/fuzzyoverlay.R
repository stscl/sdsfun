#' @title spatial fuzzy overlay
#' @note
#' Independent variables in the `data` provided to `fuzzyoverlay()` must be discretized
#' variables, and dependent variable are continuous variable.
#'
#' @param formula A formula of spatial fuzzy overlay.
#' @param data A data.frame or tibble of discretized data.
#' @param method (optional) Overlay methods. When `method` is `and`, use `min` to do
#' fuzzy overlay; and when `method` is `or`,use `max` to do fuzzy overlay. Default is `and`.
#'
#' @return A numeric vector.
#' @export
#' @examples
#' sim = tibble::tibble(y = stats::runif(7,0,10),
#'                      x1 = c(1,rep(2,3),rep(3,3)),
#'                      x2 = c(rep(1,2),rep(2,2),rep(3,3)))
#' fo1 = fuzzyoverlay(y~x1+x2,data = sim, method = 'and')
#' fo1
#' fo2 = fuzzyoverlay(y~x1+x2,data = sim, method = 'or')
#' fo2
#'
fuzzyoverlay = \(formula, data, method = "and"){
  if (!(method %in% c("and","or"))){
    stop("`method` must `and` or `or`!")
  }

  fuzzyf = ifelse(method == "and","which.min","which.max")

  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  y = data[, formula.vars[1], drop = TRUE]
  if (formula.vars[2] == "."){
    xs = dplyr::select(data,-dplyr::any_of(formula.vars[1]))
  } else {
    xs = dplyr::select(data,dplyr::all_of(formula.vars[-1]))
  }
  xs = xs %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.factor),
                                as.character)) %>%
    purrr::map2_dfc(colnames(xs),
                    \(.x,.y) paste(.y,.x,sep = "_"))
  meanrisk = purrr::map(xs, \(.x) tapply(y,.x,mean))
  fuzzynum = normalize_vector(unlist(meanrisk,use.names = FALSE))
  names(fuzzynum) = unlist(lapply(meanrisk, names),use.names = FALSE)
  xsfn = dplyr::mutate(xs, dplyr::across(dplyr::everything(),
                                   \(.x) return(fuzzynum[.x])))
  fuzzyindice = apply(xsfn, 1, fuzzyf)
  fuzzyzone = xs %>%
    split(seq(nrow(xs))) %>%
    purrr::map2_chr(fuzzyindice,
                   \(.tdf,.indice) .tdf[1,.indice,drop = TRUE])
  return(fuzzyzone)
}

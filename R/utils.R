#' get variable names in a formula and data
#'
#' @param formula A formula.
#' @param data A `data.frame`, `tibble` or `sf` object of observation data.
#'
#' @return A list.
#' \describe{
#' \item{\code{yname}}{Independent variable name}
#' \item{\code{xname}}{Dependent variable names}
#' }
#' @export
#'
#' @examples
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' formula_varname(PS_Score ~ EL_Score + OH_Score, gzma)
#' formula_varname(PS_Score ~ ., gzma)
#'
formula_varname = \(formula,data){
  formula = stats::as.formula(formula)
  formula.vars = all.vars(formula)
  if (inherits(data,'sf')){
    data = sf::st_drop_geometry(data)
  }
  if (formula.vars[2] != "."){
    data = dplyr::select(data,dplyr::all_of(formula.vars))
  }
  yname = formula.vars[1]
  xname = names(data)[-which(names(data) == yname)]
  res = list("yname" = yname, "xname" = xname)
  return(res)
}

#' convert discrete variables in a tibble to integers
#'
#' @param tbl A `tibble`,`data.frame` or `sf` object.
#'
#' @return A converted `tibble`,`data.frame` or `sf` object.
#' @export
#'
#' @examples
#' demotbl = tibble::tibble(x = c(1,2,3,3,1),
#'                          y = letters[1:5],
#'                          z = c(1L,1L,2L,2L,3L),
#'                          m = factor(letters[1:5],levels = letters[5:1]))
#' tbl_all2int(demotbl)
#'
tbl_all2int = \(tbl){
  res = dplyr::mutate(tbl,
                      dplyr::across(dplyr::everything(),
                                     \(.x) {
                                       if (inherits(.x, "numeric")) {
                                         .res = as.integer(.x)
                                       } else if (inherits(.x, "factor")){
                                         .res = as.integer(.x)
                                       } else if (inherits(.x, "character")){
                                         .res = as.integer(as.factor(.x))
                                       } else {
                                         .res = .x
                                       }
                                       return(.res)
                                     }))
  return(res)
}

#' check for NA values in a tibble
#'
#' @param tbl A `tibble`
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' demotbl = tibble::tibble(x = c(1,2,3,NA,1),
#'                          y = c(NA,NA,1:3),
#'                          z = 1:5)
#' demotbl
#' check_tbl_na(demotbl)
#'
check_tbl_na = \(tbl){
  return(any(is.na(tbl)))
}

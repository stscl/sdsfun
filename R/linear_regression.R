#' remove variable linear trend based on covariate
#'
#' @param formula A formula.
#' @param data The observation data.
#' @param method (optional) The method for using, which can be chosen as either `cpp`
#' or `r`. Default is `cpp`.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' gzma = sf::read_sf(system.file('extdata/gzma.gpkg',package = 'sdsfun'))
#' rm_lineartrend(PS_Score ~ ., gzma)
#' rm_lineartrend(PS_Score ~ ., gzma, method = "r")
#'
rm_lineartrend = \(formula,data,method = c("cpp","r")){
  method = match.arg(method)

  if (inherits(data,"sf")){
    data = sf::st_drop_geometry(data)
  }

  formulaname = formula_varname(formula,data)
  xymat = as.matrix(data[,c(formulaname[[1]],
                            formulaname[[2]])
                         ])
  notNAIndice = apply(xymat, 1, \(.x) all(!is.na(.x)))
  yvec = data[,formulaname[[1]],drop = TRUE]
  ypredRM = rep(NA,times = length(yvec))

  if (method == "r"){
    lm_model = stats::lm(formula,data[notNAIndice,])
    ypredRM[notNAIndice] = yvec[notNAIndice] - stats::predict(lm_model)
  } else {
    xmat = as.matrix(data[,formulaname[[2]]])
    ypredRM[notNAIndice] = as.double(LinearTrendRM(yvec[notNAIndice],
                                                   xmat[notNAIndice,]))
  }

  return(ypredRM)
}

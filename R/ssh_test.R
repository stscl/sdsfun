ssh_test = \(y,hs){
  if (inherits(hs,"data.frame")) {
    qvs = purrr::map(hs,\(.h) GDFactorQ(y,.h))
    qv = purrr::map_dbl(qvs,\(.qv) .qv[[1]])
    pv = purrr::map_dbl(qvs,\(.qv) .qv[[2]])
    return(tibble::tibble(Variable = names(hs),
                          Qvalue = qv,
                          Pvalue = pv))
  } else {
    return(tibble::as_tibble(GDFactorQ(y,hs)))
  }
}

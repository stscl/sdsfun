cor_test = \(x,y,z = NULL,level = 0.05){
  if (is.null(z)) {
    res = RcppPearsonCor(x,y,level)
  } else {
    if (is.atomic(z)) z = matrix(z,ncol = 1)
    if (!inherits(z,"matrix")) z = as.matrix(z)
    res = RcppPartialCor(x,y,z,level)
  }
  return(res)
}

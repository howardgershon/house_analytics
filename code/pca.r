pca = function(e){
  m = nrow(e)
  n = ncol(e)
  mean = apply(e, 2, mean)
  e.new = e - matrix(rep(mean, m), nrow=m, byrow=T)
  c = cov(e)
  ei = eigen(c)
  return(e.new%*%ei$vectors)
}

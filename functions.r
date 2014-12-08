pca = function(e){
  m = nrow(e)
  n = ncol(e)
  mean = apply(e, 2, mean)
  e.new = e - matrix(rep(mean, m), nrow=m, byrow=T)
  c = cov(e)
  ei = eigen(c)
  return(e.new%*%ei$vectors)
}

pagerank = function(e){
  n = matrix(rep(1, nrow(e)), ncol=1)
  x=e
  repeat
  {
    x.1 = n*x
    x = x.1
    if (norm(x-x.1) < .000001){
      break
    }
  }
  return(x)
}

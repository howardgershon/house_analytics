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
  n = matrix(rep(1, nrow(e)), ncol=1)/nrow(e)
  x=0
  x.1=0
  repeat
  {
    x.1 = x%*%e
    if (norm(x-x.1) < .000001){
      break
    }
    x = x.1
  }
  return(x)
}

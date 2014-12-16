pagerank = function(e){
  x = matrix(rep(1, nrow(e)), ncol=nrow(e))/nrow(e)
  i = 0
  repeat
  {
    x.1 = x%*%e
    i = i + 1
    if (norm(x-x.1) < .000001){
      break
    }
    x = x.1
  }
  return(list('x' = x, 'iter' = i))
}

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

makeChart = function(e){
  i = e$ideology
  l = e$leadership
  p = e$party
  stats.df = data.frame(i, l, p)
  ggplot(stats.df,aes(x=i,y=l, fill=p))+geom_point(size=4)+scale_fill_manual(values=c("blue", "red"))
}

reg = function(e){
  i = e$ideology
  l = e$leadership
  p = e$party
  stats.df = data.frame(i, l, p)

}

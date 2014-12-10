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
  Ideology = e$ideology
  Leadership = e$leadership
  Party = e$party
  stats.df = data.frame(Ideology, Leadership, Party)
  ggplot(stats.df, aes(x=Ideology, y=Leadership, colour=Party))+geom_point(size=4)+scale_colour_manual(values=c('blue', 'red'))
}

reg = function(e){
  i = e$ideology
  l = e$leadership
  p = e$party
  stats.df = data.frame(i, l, p)

  rep = stats.df[which(stats.df$Party == ' Republican')]
  dem = stats.df[which(stats.df$Party == ' Democrat')]

  r = glm(rep$l~rep$i)
  d = glm(dem$l~dem$i)

  return(list('rep'=r$coefficients[rep$i], 'dem'=d$coefficents[dem$i]))

}

reg.gam = function(e){

}

get.people = function(num) {
    people = xmlParse('http://kansascitystandard.com/house_analytics/house_analytics/data/people/people93.xml')
    data = xmlToList(people)
    district = c()
    name = c()
    for (i in 1:553) {
        if (data[[i]]$.attr['title'][[1]] == 'Rep.'){
            district[i] = paste0(data[[i]]$.attr['state'][[1]], '-', data[[i]]$.attr['district'][[1]])
            name[i] = paste(data[[i]]$.attr['firstname'][[1]], data[[i]]$.attr['lastname'][[1]])
        } else {
            i = i + 1
        }
    }
}

get.latlon = function(cd) {
    API.key = 'AIzaSyBLPCwn4eQz6AYuSnlHqkUsxuDxvBWbPaE'
    doc.id = '20174hYH1Ncitje-U2nxykmwFn6nilfpo_PjJRMcOk'
    query = paste0('SELECT * FROM ', doc.id,' WHERE CD = ', cd)
    url = paste0('https://www.googleapis.com/fusiontables/v1/query?', query, '&key=', API.key)
    info = getURL(url)
    lon = fromJSON(info)$rows[[1]]$geometry$coordinates[[1]][1]
    lat = fromJSON(info)$rows[[1]]$geometry$coordinates[[1]][2]
}


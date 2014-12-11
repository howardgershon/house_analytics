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

build.data = function(num, st) {
    people = xmlParse('http://kansascitystandard.com/house_analytics/house_analytics/data/people/people93.xml')
    data = xmlToList(people)
    district = c(); name = c(); lat = c(); lon = c(); lead = c(); ideo = c(); blm = c(); sess = c();

    for (i in 1:553) {
        if (data[[i]]$.attr['title'][[1]] == 'Rep.'){
            district = append( district, paste0(data[[i]]$.attr['state'][[1]], '-', data[[i]]$.attr['district'][[1]]))
            name = append(name, paste(data[[i]]$.attr['firstname'][[1]], data[[i]]$.attr['lastname'][[1]]))
            #print(name)
            lat = append(lat, get.latlon(paste0(data[[i]]$.attr['state'][[1]], '-', data[[i]]$.attr['district'][[1]])[1]))
            lon = append(lon, get.latlon(paste0(data[[i]]$.attr['state'][[1]], '-', data[[i]]$.attr['district'][[1]])[2]))
            lead = append(lead, st[which(st['name'][1] == paste0(' ', data[[i]]$.attr['lastname'][[1]])),]$leadership)
            ideo = append(ideo, st[which(st['name'][1] == paste0(' ', data[[i]]$.attr['lastname'][[1]])),]$ideology)
            sess = append(sess, num)
        } else {
            i = i + 1
        }
        blm = acos(ideo/(ideo^2+lead^2))
    }

        df = data.frame('name'=name, 'district'=district, 'lat'=lat, 'lon'=lon, 'lead'=lead, 'ideo'=ideo, 'blm'=blm)
        return(df)

}

get.latlon = function(cd) {
    API.key = 'AIzaSyCJdbgfXHHJ0eGAoWblLsEvAZ0cVHCDw58'
    doc.id = '174hYH1Ncitje-U2nxykmwFn6nilfpo_PjJRMcOk'
    query = paste0('sql=SELECT%20geometry_pos%20FROM%20', doc.id,'%20WHERE%20CD%20=%20%27', cd, '%27')
    url = paste0('https://www.googleapis.com/fusiontables/v1/query?', query, '&key=', API.key)
    info = getURL(url)
    lon = fromJSON(info)$rows[[1]]$geometry$coordinates[[1]][1]
    lat = fromJSON(info)$rows[[1]]$geometry$coordinates[[1]][2]
    return(list(lat, lon))
}

make.tables = function(){
    congress = list()
    for (i in 93:113){
        print(i)
        stat =  read.csv(paste0('data/', i, '/stats/sponsorshipanalysis_h.txt'))
        print(i)
        congress[[i]]= build.data(i, stat)
    }
    return(congress)
}

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

makeChart = function(d){
  e = read.csv(paste0('data/', d, '/stats/sponsorshipanalysis_h.txt'))
  e = rbind(e[which(e$party == ' Republican'),], e[which(e$party == ' Democrat'),])
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

build.data = function(num) {
    people = xmlParse(paste0('http://kansascitystandard.com/house_analytics/house_analytics/data/people/people', num, '.xml'))
    data = xmlToList(people)
    district = c(); name = c(); lat = c(); lon = c(); lead = c(); ideo = c(); blm = c(); sess = c();

    st = read.csv(paste0('data/',num,'/stats/sponsorshipanalysis_h.txt'))
    for (i in 1:546){

        if (data[[i]]$role['type'][[1]] == 'rep' && i!=142 && i!=441 && i!=108){
            print(i)
            if(as.numeric(data[[i]][[1]]['district'][[1]]) < 10){ data[[i]][[1]]['district'][[1]] = paste0('0', data[[i]][[1]]['district'][[1]])}
            district = append( district, paste0(data[[i]][[1]]['state'][[1]], '-', data[[i]][[1]]['district'][[1]]))
            name = append(name, paste(data[[i]][[2]]['firstname'][[1]], data[[i]][[2]]['lastname'][[1]]))
            print(paste0(tolower(data[[i]][[1]]['state'][[1]]), '-', data[[i]][[1]]['district'][[1]]))
            lat = append(lat, get.latlon(paste0(tolower(data[[i]][[1]]['state'][[1]]), '-', data[[i]][[1]]['district'][[1]]))[[1]])
            lon = append(lon, get.latlon(paste0(tolower(data[[i]][[1]]['state'][[1]]), '-', data[[i]][[1]]['district'][[1]]))[[2]])
            lead = append(lead, st[which(st['ID'][1] == paste0(data[[i]][[2]]['id'][[1]])),]$leadership)
            ideo = append(ideo, st[which(st['ID'][1] == paste0(data[[i]][[2]]['id'][[1]])),]$ideology)
            sess = append(sess, num)
        } else {
            i = i + 1
        }
        ##blm = acos(ideo/(ideo^2+lead^2))
    }
        print(paste(length(district), length(name), length(lat), length(lon), length(lead), length(ideo), length(sess)))
        df = data.frame(name, district, lat, lon, lead, ideo, sess)
        return(df)

        ##return(list(district, name, lat, lon, lead, ideo, sess))

}

get.latlon = function(cd) {
    url = paste0('http://gis.govtrack.us/boundaries/cd-2012/', cd, '/centroid')
    if(getURL(url)=='Nothing here!\n\n'){return(list('na', 'na'))}
    info = fromJSON(getURL(url))
    lon = info$coordinates[1]
    lat = info$coordinates[2]
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

parties = function(num, e){
    people = xmlParse(paste0('http://kansascitystandard.com/house_analytics/house_analytics/data/people/people', num, '.xml'))
    data = xmlToList(people)
    party=c()
    for(i in 1:(length(data)-1)){
        name = paste(data[[i]][[2]]['firstname'][[1]], data[[i]][[2]]['lastname'][[1]])
        ind = which(e$name == name)
        if(any(ind)){
            party[ind]=data[[i]][[1]]['party'][1]
        }
    }
    return(party)
}

full.page = function(){
    par(mfrow=c(7,3))
    for(i in 93:113){
        makeChart(i)
    }
}

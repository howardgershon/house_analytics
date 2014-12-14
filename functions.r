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
  ggplot(stats.df, aes(x=Ideology, y=Leadership, colour=Party))+geom_point(size=4)+ggtitle(paste0(d))+scale_colour_manual(values=c('blue', 'red'))
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


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

get.mean = function(party) {
    means.x = c()
    means.y = c()
    for (i in 93:113){
        stat = read.csv(paste0('data/',i,'/stats/sponsorshipanalysis_h.txt'))
        stat = stat[which(stat$party == party),]
        means.x[i-92]=mean(stat$ideology, na.rm=T)
        means.y[i-92]=mean(stat$leadership, na.rm=T)
    }
    return(list(means.x, means.y))
}

get.quad = function(e, m.x, m.y, party){
    quad = c()
    if (party == 'R'){
       for (i in 1:dim(e)[1]){
            if(e$ideo[i] > m.x && e$lead[i] > m.y){
                quad[i] = 1
            } else if (e$ideo[i] > m.x && e$lead[i] < m.y){
                quad[i] = 2
            } else if (e$ideo[i] < m.x && e$lead[i] < m.y){
                quad[i] = 3
            } else {
                quad[i] = 4
            }
       }
    } else if (party == 'D'){
       for (i in 1:dim(e)[1]){
             if(e$ideo[i] > m.x && e$lead[i] > m.y){
                 quad[i] = 4
             } else if (e$ideo[i] > m.x && e$lead[i] < m.y){
                 quad[i] = 3
             } else if (e$ideo[i] < m.x && e$lead[i] < m.y){
                 quad[i] = 2
             } else {
                 quad[i] = 1
             }
        }
    }
    return(quad)
}

## map
map.quad = function(e, mean.d.x, mean.d.y, mean.r.x, mean.r.y, title){
    u = get_googlemap('united states', style='feature:all|element:labels|visibility:off', zoom=4, color='bw', maptype='road')
    e.d = e[which(e$party == 'Democrat'),]
    e.r = e[which(e$party == 'Republican'),]
    e.d$quad = get.quad(e.d, mean.d.x, mean.d.y, 'D')
    e.r$quad = get.quad(e.r, mean.r.x, mean.r.y, 'R')
    e = rbind(e.d, e.r)
    e.df = data.frame(lat=as.numeric(as.matrix(e$lat)), lon=as.numeric(as.matrix(e$lon)), quad=as.numeric(as.matrix(e$quad)), stringsAsFactors = FALSE)
    g = ggmap(u) + geom_point(data=e.df, aes(x=lon, y=lat, colour=factor(quad)), size=3)+scale_color_brewer(type="qual", palette="Set1", guide=F)+xlab('')+ylab('')+theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank()) + ggtitle(title) + theme(plot.title=element_text(family="Century", size = 70, face="bold"))
    return(g)
}

make.maps = function(e){
  maps = list()
  for(i in 1:length(e)){
    maps = c(maps, map.quad(e[[i]], mean.d.x, mean.d.y, mean.r.x, mean.r.y))
  }
  # png('fullMap.png', height=3700, width=2950)
  # for(i in 1:length(maps)){
  #   maps[[i]]
  # }
  # dev.off()
  return(maps)
}


# multiplot(map.quad(sessions[[1]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '93rd Congress'),
# map.quad(sessions[[2]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '94th Congress'),
# map.quad(sessions[[3]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '95th Congress'),
# map.quad(sessions[[4]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '96th Congress'),
# map.quad(sessions[[5]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '97th Congress'),
# map.quad(sessions[[6]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '98th Congress'),
# map.quad(sessions[[7]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '99th Congress'),
# map.quad(sessions[[8]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '100th Congress'),
# map.quad(sessions[[9]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '101st Congress'),
# map.quad(sessions[[10]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '102nd Congress'),
# map.quad(sessions[[11]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '103rd Congress'),
# map.quad(sessions[[12]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '104th Congress'),
# map.quad(sessions[[13]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '105th Congress'),
# map.quad(sessions[[14]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '106th Congress'),
# map.quad(sessions[[15]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '107th Congress'),
# map.quad(sessions[[16]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '108th Congress'),
# map.quad(sessions[[17]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '109th Congress'),
# map.quad(sessions[[18]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '110th Congress'),
# map.quad(sessions[[19]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '111th Congress'),
# map.quad(sessions[[20]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '112th Congress'),
# map.quad(sessions[[21]], mean.d.x, mean.d.y, mean.r.x, mean.r.y, '113th Congress'))

# png('IdeoMean.png', width=1400, height=600)
# plot(seq(93,113),(1-means.d[[1]]), type='l', col='blue', ylim=c(0.5,0.8), xlab='Sessions of Congress', ylab='Ideological Score')
# lines(seq(93,113),means.r[[1]], col='red')
# points(seq(93,113),(1-means.d[[1]]),cex=1,col='black')
# points(seq(93,113),means.r[[1]],cex=1,col='black')
# grid(lwd=2, col='gray')
# legend('topleft', c('Dem', 'Rep'), fill=c('blue', 'red'))
# dev.off()
#
# png('LeadMean.png', width=1400, height=600)
# plot(seq(93,113),means.d[[2]], type='l', col='blue', ylim=c(0.4,0.7), xlab='Sessions of Congress', ylab='Ideological Score')
# lines(seq(93,113),means.r[[2]], col='red')
# points(seq(93,113),means.d[[2]],cex=1,col='black')
# points(seq(93,113),means.r[[2]],cex=1,col='black')
# grid(lwd=2, col='gray')
# legend('topleft', c('Dem', 'Rep'), fill=c('blue', 'red'))
# dev.off()

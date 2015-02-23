require(reshape2)
require(XML)
require(plyr)
require(ggplot2)
require(dplyr)

# Generic Least Squared solver
genericsolver <- function(x,y) {
  # Ensure matrices
  xm <- as.matrix(x)
  ym <- as.matrix(y)
  xmt <- t(xm) # Transpose
  
  # Setup lambda matrix to ensure invertability
  l <- ncol(x)
  lambda <- diag(1,l,l)
  
  # Do actual solve
  theta <- solve((xmt %*% xm) + lambda) %*% xmt %*% ym
  theta
}

# Pretty schedule listing
sched <- function(ratings,games,team) {
  s <- games[which(games$Team==team),c("Date","Team","Team.Score","Opponent.Score", "Opponent","Team.Location","gof")]
  l <- nrow(s)
  r <- sapply(1:l,function(x) {
      w <- ratings$team==as.character(s$Opponent[x])
      ifelse(all(w),NA,which(w))
    })
  pwr <- ratings$team.power[r]
  res <- ifelse(s$Team.Score > s$Opponent.Score,"W","L")
  res <- ifelse(s$Team.Score == s$Opponent.Score,"D",res)
  data.frame(Date=s$Date,Team=s$Team,Location=s$Team.Location,Result=res,Score=s$Team.Score,OppScore=s$Opponent.Score,Opponent=s$Opponent,OppPower=pwr,gof=s$gof)
}

# Make pretty graph
teamanalysis <- function(ratings,games,team) {
  require(ggplot2)
  require(ggthemes)
  res <- sched(ratings,games,team)
  team_power <- ratings[ratings$team==team,]$team.power
  p <- ggplot(res,aes(x=OppPower,y=gof,label=Opponent,color=Location))
  p <- p+geom_point(aes(size=as.numeric(Date)),alpha=0.8)+scale_size_continuous(guide=FALSE,range=c(3,7))
  p <- p+geom_text(hjust=1.2,angle=30,fontface=3,show_guide=FALSE)
  p <- p+theme_bw()+xlim(0,1000)+ylim(0,1)
  p <- p+ylab("Game Outcome Function")+xlab("Opponent Power")
  p <- p+ggtitle(team)
  p <- p+scale_color_wsj(palette = "colors6")
  p+theme_wsj()
}

# Compute game output function from two scores
gof <- function(s1,s2,c1=200,c2=50) {
  top <- s1-s2
  bot <- sqrt(c1 *sqrt((s1+s2)/c2))
  a <- pnorm(top/bot)
  a
}

# Compute ratings given games and a target stat
# Sets up giant matrix, solves with genericsolver(), then returns results in data.frame
# Output is ugly and mostly unnecessary.  Use extractcbbrankings() to prettify
computeratings <- function(games,stat,hf=0,da=0) {
  games$id <- seq(1:nrow(games))
  games$TeamOff <- paste0(games$Team,'$$offense')
  games$TeamDef <- paste0(games$Opponent,'$$defense')
  games$v <- .5
  if(da != 0) {
    dateadj <- (relpower(as.numeric(games$Date))/1000)*da
    games$v <- games$v + (dateadj - median(dateadj))
  }
  o <- dcast(games,id~TeamOff,value.var='v')
  d <- dcast(games,id~TeamDef,value.var='v')
  a <- cbind(o,d)
  if(hf != 0) {
    games$TeamHome <- ifelse(games$Team.Location=="Home" | games$Team.Location=="H",
                             paste0(games$Team,'$$home'),
                             paste0(games$Opponent,'$$home'))
    games$h <- ifelse(games$Team.Location=="Home" | games$Team.Location=="H",hf,0)
    games$h <- ifelse(games$Team.Location=="Away" | games$Team.Location=="V",-hf,games$h)
    if( da != 0 ) {
      da2 <- dateadj*(hf/.5)
      da2 <- da2-median(da2)
      games$h <- ifelse((games$h > 0), games$h+da2, games$h-da2)
    }
    h <- dcast(games,id~TeamHome,value.var='h')
    a <- cbind(a,h)
  }
  a[is.na(a)] <- 0
  a <- a[,!names(a) %in% c("id")]
  theta <- genericsolver(a,games[stat])
  a <- strsplit(rownames(theta),"$$",fixed=TRUE)
  r <- data.frame(team=sapply(a,"[[",1),Type=sapply(a,"[[",2),Score=theta)
  results <- dcast(r,team~Type,value.var=stat)
  results$diff <- results$offense - results$defense
  results$team.power <- relpower(results$diff)
  results$off.power <- relpower(results$offense)
  results$def.power <- relpower(-results$defense)
  if(hf != 0) {
    results$home.power <- relpower(results$home)
  }
  data.frame(row.names=NULL,results[with(results,order(-diff)),])
}

# Gets CBB Data from KenPom Site
getcbbdata <- function() {
  a <- read.fwf('http://kenpom.com/cbbga15.txt',
                widths=c(11,23,4,23,4,30),
                col.names=c("Date","Opponent","Opponent.Score","Team","Team.Score","Site"),
                strip.white=TRUE,
                header=FALSE)
  a$Site <- gsub("^ *","",a$Site)
  a$Site <- ifelse(a$Site == "",NA,a$Site)
  a$Team.Location <- ifelse(nchar(a$Site)<3,"Home","Neutral")
  a2 <- data.frame(Date=a$Date,
                   Team=a$Opponent,
                   Team.Score=a$Opponent.Score,
                   Opponent=a$Team,
                   Opponent.Score=a$Team.Score,
                   Site=a$Site)
  a2$Site <- as.character(a2$Site)
  a2$Team.Location <- ifelse(nchar(a2$Site)<3,"Away","Neutral")
  cbb <- rbind(a,a2)
  cbb$Site <- NULL
  cbb$gof <- gof(cbb$Team.Score, cbb$Opponent.Score,c1=200,c2=50)
  cbb$win <- ifelse(cbb$Team.Score > cbb$Opponent.Score,1,0)
  cbb$Date <- as.Date(cbb$Date, "%m/%d/%Y")
  cbb <- cbb[order(cbb$Date),]
  cbb
}

# Predictions?
# Note, this model isn't necessarily great for predictions
matchpredict <- function(results, home, away, hf=0, da=0) {
  ht <- results[which(results$team==home),]
  at <- results[which(results$team==away),]
  hs <- (ht$offense + at$defense)*(.5+da/2)
  as <- (at$offense + ht$defense)*(.5+da/2)
  if(hf != 0) {
    hs <- hs + (ht$home*hf)
    as <- as + (ht$home*hf)
  }
  h <- (hs / (hs+as))
  a <- (as / (hs+as))
  data.frame(Team=c(home,away),Probability=c(h,a))
}

# Computes relative power
# @param x vector of scores to normalize (0-1000)
relpower <- function(x) {
  round(1000*(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE)))
}

# Makes ratings prettier
extractcbbrankings <- function(ratings,games,teams=NULL) {
  if(is.null(teams)) {
    teams = as.character(unique(games$Team))
  }
  l <- ratings[ratings$team %in% teams,]
  l$team <- as.character(l$team)
  record <- unlist(lapply(l$team,computerecord,games=games))
  gp <- sapply(record,FUN=function(x) {sum(as.numeric(unlist(strsplit(x,"-"))))})
  hr <- unlist(lapply(l$team,computerecord,games=games,location="Home"))
  ar <- unlist(lapply(l$team,computerecord,games=games,location="Away"))
  sos <- unlist(lapply(l$team,computesos,games=games,ratings=ratings))
  df <- data.frame(Team=l$team,GP=gp,Overall=record,Home=hr,Away=ar,Rating=l$diff,SOS=sos)
  df <- filter(df,GP>5)
  df <- df[with(df,order(-Rating)),]
  rownames(df) <- NULL
  df$SOSRank <- as.integer(rank(-df$SOS))
  df
}

# Record for printing
computerecord <- function(team,games,location=NULL) {
  if(!is.null(location)) {
    a <- plyr::count(games[(games$Team==team) & (games$Team.Location==location),'win'])
  } else {
    a <- plyr::count(games[games$Team==team,'win'])
  }
  w <- a[which(a$x==1),]$freq
  l <- a[which(a$x==0),]$freq
  w <- max(length(w),w)
  l <- max(length(l),l)
  paste0(w,'-',l)
}

computesos <- function(team,games,ratings) {
  opps <- as.character(games[games$Team==team,"Opponent"])
  opppow <- sapply(opps,FUN=function(x) {ratings[ratings$team==x,"team.power"]})
  mean(opppow)/1000
}

# Master function to just get rankings
docbb <- function(stat='gof',hf=.2,da=.2,n=0) {
  cbb <- getcbbdata()
  cbbrank <- computeratings(cbb,stat,hf,da)
  r <- extractcbbrankings(cbbrank,cbb)
  if(n > 0) {
    r <- r[0:n,]
  }
  r
}

# Make lots of result plots
make_plots <- function(ratings,games,teams,dirname) {
  lapply(teams,function(team) {ggsave(paste0(dirname,"/",team,".png"),teamanalysis(ratings,games,team))})
}

# Filter 'games' to only those involving both teams from 'teams'
filtergames <- function(games,teams) {
  games[which(games$Opponent %in% teams & games$Team %in% teams),]
}

# Do rankings for a subset of teams
cbbjustconference <- function(games,teams,stat='gof') {
  games <- filtergames(games,teams)
  ratings <- computeratings(games,stati,.2,.2)
  r <- extractcbbrankings(ratings,games,teams)
  r
}

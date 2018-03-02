library(XML)
library(dplyr)
library(stringr)
library(lubridate)
library(rvest)

# function that converts team names to abbreviations
name_abbrev <- function(name) {
  names <- c("Orioles", "RSox", "Sox", "Indians", "Tigers", "Astros", "Royals", "Angels", "Twins",
             "Yankees", "Athletics", "Mariners", "Rays", "Rangers", "Jays", "Diamondbacks", "Braves",
             "Cubs", "Reds", "Rockies", "Dodgers", "Marlins", "Brewers", "Mets", "Phillies", "Pirates",
             "Padres", "Giants", "Cardinals", "Nationals")
  abbreviations <- c("BAL", "BOS", "CHW", "CLE", "DET", "HOU", "KCR", "LAA", "MIN", "NYY", "OAK",
                     "SEA", "TBR", "TEX", "TOR", "ARI", "ATL", "CHC", "CIN", "COL", "LAD", "MIA",
                     "MIL", "NYM", "PHI", "PIT", "SDP", "SFG", "STL", "WSN")
  for (i in 1:length(names)) {
    if (name==names[i]) {
      abbrev <- abbreviations[i]
      break
    }
  }
  return(abbrev)
}


name_abbrev2 <- function(name) {
  names <- c("Arizona", "Atlanta", "Baltimore", "Boston", "Chicago Cubs", "Chicago Sox", "Cincinnati", "Cleveland", 
             "Colorado", "Detroit", "Houston", "Kansas City", "LA Angels", "LA Dodgers", "Miami", "Milwaukee", 
             "Minnesota", "NY Mets", "NY Yankees", "Oakland", "Philadelphia", "Pittsburgh", "San Diego", 
             "San Francisco", "Seattle", "St. Louis", "Tampa Bay", "Texas", "Toronto", "Washington")
  abbreviations <- c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", 
                     "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", 
                     "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WSN")
  for (i in 1:length(names)) {
    if (name==names[i]) {
      abbrev <- abbreviations[i]
      break
    }
  }
  return(abbrev)
}


convertName <- function(origName) {
  newNames <- c("ANA", "CHN", "KCA", "SFN", "LAN", "NYA", "CHA", "NYN", "TBA", "SLN", "SDN", "WAS")
  oldNames <- c("LAA", "CHC", "KCR", "SFG", "LAD", "NYY", "CHW", "NYM", "TBR", "STL", "SDP", "WSN")
  for (i in 1:length(oldNames)) {
    if (origName==oldNames[i]) {
      abbrev <- newNames[i]
      break
    } else {
      abbrev <- origName
    }
  }
  return(abbrev)
}




##############    TEAM DATA   ##############

url <- "https://www.baseball-reference.com/leagues/MLB-standings.shtml#expanded_standings_overall::none"
code <- readLines(url)
#code <- code[787:856]   # start at line with <div class="table_outer_container"> and end with </tbody></table>
code <- code[grep("<div class=\"table_outer_container\">", code)[7]:grep("</tbody></table>", code)[7]]
data <- readHTMLTable(code, stringsAsFactors = FALSE, as.data.frame = TRUE, which=1)
data <- data[-31,]
names(data) <- gsub("≥.", "abv", gsub("<.", "bel", names(data)))


urlE <- "http://proxy.espn.com/mlb/standings?type=expanded&group=9"
code <- readLines(urlE)
code <- code[grep("<table class=\"tablehead\"", code)]
espn <- readHTMLTable(code, stringsAsFactors = FALSE, as.data.frame = TRUE, which=1)
names(espn) <- espn[1,]
names(espn)[1] <- "Team"
espn <- espn[-1, -c(2:4,10:11)]
espn$GB[which(espn$GB=="-")] <- 0
espn <- espn[order(espn$Team),]

for (i in 1:30) {
  espn$Team[i] <- gsub("\\*-","",gsub("x-","",gsub("y-","",gsub("z-","",espn$Team[i]))))
  espn$Team[i] <- name_abbrev2(espn$Team[i])
}

data <- merge(x=data, y=espn, by.x="Tm", by.y="Team")



# luck / games played
data$G <- as.numeric(data$G)
data$Luck <- as.numeric(data$Luck)
data$LuckperG <- round(data$Luck/data$G, digits=3)

# change streak from W1, L1 to +1, -1
for (i in 1:nrow(data)) {
  if (length(grep("W", data$Strk[i]))==1) {
    data$Strk[i] <- as.numeric(gsub(".* ", "", data$Strk[i]))
  } else {
    data$Strk[i] <- as.numeric(gsub(".* ", "", data$Strk[i]))*-1
  }
  data$OneRunPct[i] <- round((as.numeric(gsub(".*-","",data$`1Run`[i]))+as.numeric(gsub("-.*","",data$`1Run`[i])))/data$G[i], digits=3)
}

# add columns for win pct and diff for each variable below
pctdiff <- c("DAY","NIGHT","Inter","Home","Road","ExInn","1Run","vRHP","vLHP","abv500","bel500","last10","last20","last30","GRASS","TURF")
for (i in pctdiff) {
  data[,paste(i,"W",sep="")] <- as.numeric(gsub("-.*","\\1",data[,i]))
  data[,paste(i,"L",sep="")] <- as.numeric(gsub(".*-","\\1",data[,i]))
  data[,paste(i,"pct",sep="")] <- round(data[,paste(i,"W",sep="")]/(data[,paste(i,"W",sep="")]+data[,paste(i,"L",sep="")]), digits=3)
  data <- data[,-c(ncol(data)-2,ncol(data)-1)]
  data[,paste(i,"diff",sep="")] <- data[,paste(i,"pct",sep="")]-as.numeric(data$`W-L%`)
}

data$TURFpct[which(data$TURFpct=="NaN")] <- NA
data$TURFdiff[which(data$TURFdiff=="NaN")] <- NA




##############    PITCHING DATA   ##############

urlP <- "https://www.baseball-reference.com/leagues/MLB/2017-standard-pitching.shtml"
code <- readLines(urlP)
code <- code[grep("<div class=\"table_outer_container\">", code)[2]:grep("</tbody></table>", code)[2]]   # start at line with <div class=\"table_outer_container\"> and end with </tbody></table>
pitching <- readHTMLTable(code, stringsAsFactors = FALSE, as.data.frame = TRUE, which=1)
pitching <- pitching[-nrow(pitching),]
pitching <- pitching[-which(pitching$Rk=="Rk"),-1]

pitching$lefty <- gsub(".*\\*","",pitching$Name)==""
pitching$Name <- gsub("\\*", "", pitching$Name)


pitchingteams <- pitching$Tm[!duplicated(pitching[,c("Name","Age")], fromLast = TRUE)]
pitching <- pitching[!duplicated(pitching[,c("Name","Age")]),]
pitching$Tm <- pitchingteams

pitching[,c(2,5:34)] <- sapply(pitching[,c(2,5:34)], as.numeric)




urlB <- "https://www.espn.com/mlb/stats/team/_/stat/pitching/split/128"
code <- readLines(urlB)
code <- code[grep("<table class=\"tablehead\"", code):grep("</table>", code)]
bullpen <- readHTMLTable(code, stringsAsFactors = FALSE, as.data.frame = TRUE, which=1)
names(bullpen) <- bullpen[1,]
bullpen <- bullpen[2:31,c(2:6,10,12:16)]
bullpen <- bullpen[order(bullpen$TEAM),]

for (i in 1:30) {
  bullpen$TEAM[i] <- name_abbrev2(bullpen$TEAM[i])
}

# get number of complete games pitched by each time, and add to number of games played by bullpen
ptch <- pitching
ptch$CG <- as.numeric(ptch$CG)
ptch <- ptch %>% group_by(Tm) %>% mutate(sumcg = sum(CG))
ptch <- ptch[,c(3,36)]
ptch <- unique(ptch)
ptch <- ptch[order(ptch$Tm),]
ptch <- ptch[c(1:23,25,24,26:30),]
bullpen$IperG <- as.numeric(bullpen$IP)/(as.numeric(bullpen$GP)+ptch$sumcg)  # innings pitched per game


pitching$IPG <- 0
for (j in 1:nrow(pitching)) {
  if (pitching$G[j]!=0) {
    pitching$IPG[j] <- pitching$IP[j]/pitching$G[j]
  }
  team <- pitching$Tm[j]
  coef <- pitching$GS[j]/(pitching$GS[j]+5)
  IPGweight <- (coef*pitching$IPG[j]) + ((1-coef)*(9-bullpen$IperG[team==bullpen$TEAM]))
  pitching$WtERA[j] <- ((9-IPGweight)*as.numeric(bullpen$ERA[team==bullpen$TEAM]) + pitching$ERA[j]*IPGweight)/9
}

pitching <- pitching[-which(is.na(pitching$WtERA) | pitching$WtERA=="Inf"),]



########## UPDATE PITCHER TEAMS #########

url <- "http://www.espn.com/mlb/players?position=sp&league=mlb"
code <- readLines(url)
line <- code[grep("PLAYER</td><td>TEAM</td><td>POSITION", code)]
npitchers <- str_count(line, "player-10")
pitcherTeams <- data.frame(matrix(vector(), npitchers, 2, dimnames=list(c(), c("Name", "Team"))))

i <- 1
while (i <= npitchers) {
  pitcher <- gsub("Starting Pitcher.*","",gsub("(.*?)(player-10.*)", "\\2", line))
  ptchrNm <- gsub(".*\">","",gsub("</a>.*","",pitcher))
  ptchrNm <- gsub("  ", " ", paste(gsub(".*, ","",ptchrNm), gsub(", .*","",ptchrNm)))
  
  ptchrTm <- gsub("</a>.*","",gsub(".*\">","",pitcher))
  if (ptchrTm=="Boston Red Sox") {
    ptchrTm <- "Boston RSox"
  }
  ptchrTm <- name_abbrev(gsub(".* ","", ptchrTm))
  
  pitcherTeams[i,] <- c(ptchrNm, ptchrTm)
  
  line <- gsub("(.*?)(Starting Pitcher.*)", "\\2", gsub("(.*?)(player-10.*)", "\\2", line))
  i <- i+1
}


url <- "http://www.espn.com/mlb/players?position=rp&league=mlb"
code <- readLines(url)
line <- code[grep("PLAYER</td><td>TEAM</td><td>POSITION", code)]
pitcherTeams <- rbind(pitcherTeams, data.frame(matrix(vector(), str_count(line, "player-10"), 2, dimnames=list(c(), c("Name", "Team")))))

i <- npitchers+1
while (i <= nrow(pitcherTeams)) {
  pitcher <- gsub("Relief Pitcher.*","",gsub("(.*?)(player-10.*)", "\\2", line))
  ptchrNm <- gsub(".*\">","",gsub("</a>.*","",pitcher))
  ptchrNm <- gsub("  ", " ", paste(gsub(".*, ","",ptchrNm), gsub(", .*","",ptchrNm)))
  
  ptchrTm <- gsub("</a>.*","",gsub(".*\">","",pitcher))
  if (ptchrTm=="Boston Red Sox") {
    ptchrTm <- "Boston RSox"
  }
  ptchrTm <- name_abbrev(gsub(".* ","", ptchrTm))
  
  pitcherTeams[i,] <- c(ptchrNm, ptchrTm)
  
  line <- gsub("(.*?)(Relief Pitcher.*)", "\\2", gsub("(.*?)(player-10.*)", "\\2", line))
  i <- i+1
}


pitching$Name <- gsub(intToUtf8(160), " ", pitching$Name)
for (i in 1:nrow(pitching)) {
  if (pitching$Name[i] %in% pitcherTeams$Name) {
    if (pitching$Tm[i] != pitcherTeams$Team[which(pitching$Name[i]==pitcherTeams$Name)]) {
      pitching$Tm[i] <- pitcherTeams$Team[which(pitching$Name[i]==pitcherTeams$Name)]
    }
  }
}






##############    UPCOMING GAMES   ##############

### Record games and pitching matchups
urlS <- "https://www.baseball-reference.com/previews/"
code <- readLines(urlS)
gamestart <- grep("<table class=\"teams\">", code)
games <- data.frame(matrix(vector(), length(gamestart), 11, dimnames=list(c(), c("AwayTeam", "HomeTeam", "AwayP", "HomeP", "Time", "Surface", "AwayOdds", "HomeOdds", "Winner", "Rundiff", "HomeWin"))))
notplayed <- c()
for (i in 1:nrow(games)) {
  games$AwayTeam[i] <- gsub("</a>.*", "", gsub(".*.shtml\">", "", code[gamestart[i]+3]))
  if (games$AwayTeam[i]=="Red Sox") {
    games$AwayTeam[i] <- "RSox"
  } else if (games$AwayTeam[i]=="D'backs") {
    games$AwayTeam[i] <- "Diamondbacks"
  }
  games$AwayTeam[i] <- gsub(".* ", "", games$AwayTeam[i])
  games$AwayTeam[i] <- name_abbrev(games$AwayTeam[i])
  games$HomeTeam[i] <- gsub("</a>.*", "", gsub(".*.shtml\">", "", code[gamestart[i]+11]))
  if (games$HomeTeam[i]=="Red Sox") {
    games$HomeTeam[i] <- "RSox"
  } else if (games$HomeTeam[i]=="D'backs") {
    games$HomeTeam[i] <- "Diamondbacks"
  }
  games$HomeTeam[i] <- gsub(".* ", "", games$HomeTeam[i])
  games$HomeTeam[i] <- name_abbrev(games$HomeTeam[i])
  games$AwayP[i] <- gsub("</a>.*", "", gsub(".*\">", "", code[gamestart[i]+23]))
  games$HomeP[i] <- gsub("</a>.*", "", gsub(".*\">", "", code[gamestart[i]+27]))
  if (gsub(".* ", "", games$AwayP[i])==games$AwayP[i]) {
    games$AwayP[i] <- NA
    notplayed <- c(notplayed, i)
  }
  if (gsub(".* ", "", games$HomeP[i])==games$HomeP[i]) {
    games$HomeP[i] <- NA
  }
  
  time <- code[gamestart[i]+13]
  time <- gsub(".*right\">", "", time)
  hour <- as.numeric(gsub(":.*", "", time))
  if (hour<6 | hour==12) {
    games$Time[i] <- "Day"
  } else {
    games$Time[i] <- "Night"
  }
  
  if (games$HomeTeam[i] %in% c("TBR", "TOR")) {
    games$Surface[i] <- "Turf"
  } else {
    games$Surface[i] <- "Grass"
  }
}

if (length(notplayed)>0) {
  games <- games[-notplayed,]  # should get rid of games that are postponed
}

today <- Sys.Date()

### Get odds for each game
urlO <- "http://www.vegasinsider.com/mlb/odds/las-vegas/"
code <- readLines(urlO)
oddsend <- grep(gsub("-", "/", gsub("[0-9][0-9][0-9][0-9]-","",today+1)), code)[1]
oddstart <- grep("class=\"cellTextHot\">", code)
if (!is.na(oddsend)) {
  code <- code[1:oddsend]
  oddstart <- grep("class=\"cellTextHot\">", code)
  looplen <- length(oddstart)-1
} else {
  looplen <- length(oddstart)
}


for (i in 1:looplen) {
  Ateam <- gsub(".* title=\"", "", code[oddstart[i]+1])
  Ateam <- gsub(".*\">","", gsub("</a>.*", "", Ateam))
  Hteam <- gsub(".* title=\"", "", code[oddstart[i]+2])
  Hteam <- gsub(".*\">","", gsub("</a>.*", "", Hteam))
  teams <- c(Ateam, Hteam)
  
  for (j in 1:2) {
    if (gsub(" .*", "", teams[j]) %in% c("N.Y.","L.A.")) {
      teams[j] <- gsub("\\.", "", teams[j])
    } else if (gsub("Chi\\..*", "", teams[j])=="") {
      teams[j] <- gsub("\\.", "cago", gsub("White ", "", teams[j]))
    }
  }
  
  Ateam <- teams[1]
  Hteam <- teams[2]
  
  Hpitcher <- code[oddstart[i]+48]
  Hpitcher <- gsub("  "," ", gsub("</b>.*","", gsub(".*<b> ","", gsub(".*Pitchers:","",Hpitcher))))
  
  Apitcher <- code[oddstart[i]+48]
  Apitcher <- gsub("  "," ", gsub(".* <b> ","", gsub("</b>.*","", gsub(".*</b>&nbsp;&nbsp;","", gsub(".*Pitchers:","",Apitcher)))))

  odds <- code[oddstart[i]+11]
  Hodds <- gsub(".*<br>", "", odds)
  Aodds <- gsub(".*<br>(.*)<br>.*", "\\1", odds)
  if (Aodds=="&nbsp;" | Hodds=="&nbsp;") {
    Aodds <- NA
    Hodds <- NA
  }
  
  if (length(games$AwayOdds[games$AwayTeam==name_abbrev2(Ateam) & games$HomeP==Hpitcher])==0) {
    games$AwayOdds[games$AwayTeam==name_abbrev2(Ateam) & games$AwayP==Apitcher] <- Aodds
    games$HomeOdds[games$HomeTeam==name_abbrev2(Hteam) & games$AwayP==Apitcher] <- Hodds
  } else {
    games$AwayOdds[games$AwayTeam==name_abbrev2(Ateam) & games$HomeP==Hpitcher] <- Aodds
    games$HomeOdds[games$HomeTeam==name_abbrev2(Hteam) & games$HomeP==Hpitcher] <- Hodds
  }
}




########## CREATE BIG TABLE ##########

bigtable <- games

prefix <- c("A", "H")  # H = home
for (i in 1:2) {
  cols <- ncol(bigtable)
  bigtable <- cbind(bigtable, data[1:nrow(bigtable),2:ncol(data)])
  colnames(bigtable)[(cols+1):ncol(bigtable)] <- paste(prefix[i], colnames(bigtable)[(cols+1):ncol(bigtable)], sep="")
  for (j in 1:nrow(games)) {
    bigtable[j,(cols+1):ncol(bigtable)] <- data[data$Tm==bigtable[j,i],2:ncol(data)]
  }
}

prefix <- c("AB.", "HB.")  # HB = home bullpen
for (i in 1:2) {
  cols <- ncol(bigtable)
  bigtable <- cbind(bigtable, bullpen[1:nrow(bigtable),2:ncol(bullpen)])
  colnames(bigtable)[(cols+1):ncol(bigtable)] <- paste(prefix[i], colnames(bigtable)[(cols+1):ncol(bigtable)], sep="")
  for (j in 1:nrow(games)) {
    bigtable[j,(cols+1):ncol(bigtable)] <- bullpen[bullpen$TEAM==bigtable[j,i],2:ncol(bullpen)]
  }
}

prefix <- c("AP.", "HP.")
for (i in 1:2) {
  cols <- ncol(bigtable)
  bigtable <- cbind(bigtable, pitching[1:nrow(bigtable),c(2,5:ncol(pitching))])
  bigtable[,(cols+1):ncol(bigtable)] <- NA
  colnames(bigtable)[(cols+1):ncol(bigtable)] <- paste(prefix[i], colnames(bigtable)[(cols+1):ncol(bigtable)], sep="")
  for (j in 1:nrow(games)) {
    if (sum(gsub(intToUtf8(160)," ",gsub("\302\240"," ",pitching$Name))==bigtable[j,i+2])==0) {  # first pitching appearance
      bigtable[j,(cols+1):ncol(bigtable)] <- NA
    } else {
      if (nrow(pitching[pitching$Tm==bigtable[j,i] & gsub(intToUtf8(160)," ",gsub("\302\240"," ",pitching$Name))==bigtable[j,i+2],c(2,5:ncol(pitching))])==0) {
        url <- paste("https://www.baseball-reference.com/players/", substr(tolower(gsub(".* ","",bigtable[j,i+2])),1,1), "/", substr(tolower(gsub(".* ","",bigtable[j,i+2])),1,5), substr(tolower(gsub(" .*","",bigtable[j,i+2])),1,2), "01.shtml", sep="")
        code <- readLines(url)
        newTm <- code[grep("Team:", code)+1]
        newTm <- gsub(".*/teams/","",gsub(paste("/",year(Sys.Date()),".shtml.*",sep=""),"",newTm))
        pitching$Tm[pitching$Name==bigtable[j,i+2]] <- newTm
      }
      bigtable[j,(cols+1):ncol(bigtable)] <- pitching[pitching$Tm==bigtable[j,i] & gsub(intToUtf8(160)," ",gsub("\302\240"," ",pitching$Name))==bigtable[j,i+2],c(2,5:ncol(pitching))]
    }
  }
}





########## Save FiveThirtyEight's Predictions ##########

urlF <- "https://projects.fivethirtyeight.com/2017-mlb-predictions/games/"
selector_yearly <- 'p , p a'
fivethirtyeight <- read_html(urlF) %>% html_nodes(selector_yearly) %>% html_text() %>% as.data.frame()
names(fivethirtyeight) <- c("pcts")
fivethirtyeight$pcts <- as.character(fivethirtyeight$pcts)
pctstart <- grep("Chance of winning", fivethirtyeight$pcts)[1:nrow(games)]
fivethirtyeight <- cbind(fivethirtyeight, NA, NA, NA, NA, NA, NA)
names(fivethirtyeight)[2:7] <- c("AwayTeam", "HomeTeam", "AwayP", "HomeP", "Away538Pct", "Home538Pct")
for (i in 1:length(pctstart)) {
  Ateam <- fivethirtyeight$pcts[pctstart[i]+2]
  if (Ateam=="Red Sox") {
    Ateam <- "RSox"
  }
  Ateam <- name_abbrev(gsub(".* ", "", Ateam))
  fivethirtyeight$AwayTeam[i] <- Ateam
  
  Hteam <- fivethirtyeight$pcts[pctstart[i]+4]
  Hteam <- gsub(" $", "", Hteam)
  if (Hteam=="Red Sox") {
    Hteam <- "RSox"
  }
  Hteam <- name_abbrev(gsub(".* ", "", Hteam))
  fivethirtyeight$HomeTeam[i] <- Hteam
  
  starters <- fivethirtyeight$pcts[pctstart[i]+6]
  starters <- gsub(" Jr.", "", starters)
  Astarter <- gsub(" vs.*", "", starters)
  teampitchers <- pitching[pitching$Tm==Ateam,]
  Astarter <- teampitchers$Name[grep(Astarter, teampitchers$Name)]
  fivethirtyeight$AwayP[i] <- Astarter
  
  Hstarter <- gsub(".*vs\\. ", "", starters)
  teampitchers <- pitching[pitching$Tm==Hteam,]
  Hstarter <- teampitchers$Name[grep(Hstarter, teampitchers$Name)]
  fivethirtyeight$HomeP[i] <- Hstarter
  
  Apct <- fivethirtyeight$pcts[pctstart[i]+7]
  Apct <- as.numeric(gsub("%", "", Apct))
  fivethirtyeight$Away538Pct[i] <- Apct
  
  Hpct <- fivethirtyeight$pcts[pctstart[i]+8]
  Hpct <- as.numeric(gsub("%", "", Hpct))
  fivethirtyeight$Home538Pct[i] <- Hpct
}

fivethirtyeight <- fivethirtyeight[1:i,-1]




##############    WRITE DATA TO CSV   ##############
# date of folder corresponds to data before that day's games are played

dir.create(paste("~/Documents/School/Yale/Sports Analytics/MLB 2017 Daily Predictions/Historical Data", today, sep="/"))
setwd(paste("~/Documents/School/Yale/Sports Analytics/MLB 2017 Daily Predictions/Historical Data",today,sep="/"))
write.csv(pitching, "Pitching.csv", row.names=FALSE)
write.csv(bullpen, "Bullpen.csv", row.names=FALSE)
write.csv(data, "TeamData.csv", row.names=FALSE)
write.csv(games, "ScheduledGames.csv", row.names=FALSE)
write.csv(bigtable, "BigTable.csv", row.names=FALSE)
write.csv(fivethirtyeight, "FiveThirtyEight.csv", row.names=FALSE)





##############    PREVIOUS DAY RESULTS   ##############

### Store winners of previous day's games
urlR <- "https://www.baseball-reference.com/boxes/"
code <- readLines(urlR)
yesterday <- Sys.Date()-1
setwd(paste("~/Documents/School/Yale/Sports Analytics/MLB 2017 Daily Predictions/Historical Data",yesterday,sep="/"))
results <- read.csv("ScheduledGames.csv", as.is=TRUE)
if (length(which(colnames(results)=="X"))>0) {
  results <- results[,-which(colnames(results)=="X")]
}
res <- grep("<table class=\"teams\">", code)
for (i in 1:length(res)) {
  gameres <- code[res[i]:(res[i]+17)]
  winner <- grep("class=\"winner\">", gameres)+1
  Wteam <- gsub("</a>.*", "", gsub(".*.shtml\">", "", gameres[winner]))
  if (Wteam=="Boston Red Sox") {
    Wteam <- "Boston RSox"
  } else if (Wteam=="LA Angels of Anaheim") {
    Wteam <- "LA Angels"
  }
  Wteam <- gsub(".* ", "", Wteam)
  Wteam <- name_abbrev(Wteam)
  
  
  urlP <- paste("https://www.baseball-reference.com/boxes/", gsub("\">Final.*","",gsub(".*/boxes/","",gameres[grep("Final",gameres)])), sep="")
  codeP <- readLines(urlP)
  starterName <- gsub("</a>.*","",gsub(".*.shtml\">","",codeP[grep("Base-Out Runs Saved", codeP)[2]+5]))

  line <- which((results$AwayTeam==Wteam | results$HomeTeam==Wteam) & starterName==results$HomeP)
  results$Winner[line] <- Wteam
  
  Ascore <- as.numeric(gsub("</td>.*","",gsub(".*\"right\">","",gameres[5])))
  Hscore <- as.numeric(gsub("</td>.*","",gsub(".*\"right\">","",gameres[13])))
  results$Rundiff[line] <- Hscore - Ascore
}

if (length(which(is.na(results$Winner)))>0) {
  postponed <- which(is.na(results$Winner))
  results <- results[-postponed,]  # NA means game was postponed
}

results$HomeWin <- results$HomeTeam==results$Winner

write.csv(results, "ScheduledGames.csv", row.names=FALSE)

ybigtable <- read.csv("BigTable.csv", as.is=TRUE)
if (nrow(results) != nrow(ybigtable)) {
  ybigtable <- ybigtable[-postponed,]
}
ybigtable[,1:11] <- results
  
write.csv(ybigtable, "BigTable.csv", row.names=FALSE)




########## UPDATE HISTORICAL TABLE ##########

setwd("~/Documents/School/Yale/Sports Analytics/MLB 2017 Daily Predictions/Historical Data")
histtable <- read.csv("HistoricalTable.csv", as.is=TRUE)
names(ybigtable) <- names(histtable)
histtable <- rbind(histtable, ybigtable)
write.csv(histtable, "HistoricalTable.csv", row.names=FALSE)

names(bigtable) <- names(histtable)
histwtoday <- rbind(histtable, bigtable)
write.csv(histwtoday, "HistTable_wToday.csv", row.names=FALSE)

setwd(paste("~/Documents/School/Yale/Sports Analytics/MLB 2017 Daily Predictions/Historical Data",today,sep="/"))
write.csv(histtable, "HistoricalTable.csv", row.names=FALSE)
write.csv(histwtoday, "HistTable_wToday.csv", row.names=FALSE)


# quit(save="no")

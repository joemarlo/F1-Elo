library(tidyverse)
library(plotly)
library(ggridges)

#data source: https://www.kaggle.com/cjgdev/formula-1-race-data-19502017

#remove all objects
rm(list = ls())

# data imports ------------------------------------------------------------

#where the data is housed
project.path <- "/Users/joemarlo/Dropbox/Data/Projects/F1-Elo" #Mac

#function to import data
import_multiple_csv <- function(myPath){
  tmp.list.1 <- list.files(myPath, pattern = ".csv")
  tmp.list.2 <- list(length = length(tmp.list.1))
  for (i in 1:length(tmp.list.1)){tmp.list.2[[i]] <- read_csv(paste(myPath, tmp.list.1[i], sep = "/"))}
  names(tmp.list.2) <- tmp.list.1
  tmp.list.2
}

#import the data, fix the file name, and "delist" the object
tmp <- import_multiple_csv(paste0(project.path, "/Data"))
names(tmp) <- lapply(names(tmp), function(name){substr(name, start = 0, stop = nchar(name) - 4) %>% tools::toTitleCase()})
list2env(tmp, envir = .GlobalEnv)
rm(tmp, import_multiple_csv)


# ELO ---------------------------------------------------------------------

rookieElo <- 1300
kFactor <- 24 #starting value for K factor; 538 starts theirs at 24
cFactor <- 400 #set to 400 in chess system. means that an opponent that is 200 pts greater has a 75% chance of winning
uElo <- 1500 #mean score to normalize to
SDstandard <- 200 #standard deviation that the Elo scores are normalized to after each race

##key formulas
#new score  = old score + K factor * (score - expected score)
#expected score = 1 / (1 + 10 ^ ((opponent score - player score) / cFactor))

calcExpWin <- function(Driver.A.Score, Driver.B.Score){
  ret <- 1 / (1 + 10 ^ ((Driver.B.Score - Driver.A.Score) / cFactor))
  return(ret)
}

calcScore <- function(Tenure, wins, expectedWin, oldScore){
  kFac <- kFactor * (1 / (Tenure ^ (1/10)))
  ret <- oldScore + kFac * (wins - expectedWin)
  return(ret)
}

#curve for kFactor; varies with race not season
plot((1 / (1:200)^(1/10)) * kFactor)

#create basic data frame with results
EloDF <- Results %>%
  select(raceId, driverId, position) %>%
  left_join(y = Races[c("raceId", "year", "round")], by = "raceId") %>%
  arrange(year, round)

#remove duplicates of drivers and races (i.e. where driver shows up twice in one race)
EloDF <- EloDF[!duplicated(EloDF[c("raceId","driverId")]),]

#remove Indy 500
IndyIds <- Races %>% filter(name == "Indianapolis 500") %>% pull(raceId)
EloDF <- EloDF %>% filter(!(raceId %in% IndyIds))
rm(IndyIds)

#add raceId that is consecutive
EloDF <- EloDF %>%
  group_by(raceId, year, round) %>%
  summarize() %>%
  arrange(year, round) %>%
  rowid_to_column() %>%
  ungroup() %>%
  rename(EloRaceId = rowid) %>%
  select(EloRaceId, raceId) %>%
  right_join(y = EloDF, by = "raceId") %>%
  mutate(EloScoreBgn = ifelse(EloRaceId == 1, rookieElo, NA),
         EloScoreEnd = NA) %>%
  rowid_to_column() %>%
  rename(EloId = rowid)

#create dataframe of tenures by driver and round
TenureDF <- lapply(sort(unique(EloDF$EloRaceId)), function(Round){
  EloDF %>%
    filter(EloRaceId <= Round) %>%
    count(driverId) %>%
    rename(Tenure = n) %>%
    mutate(EloRaceId = Round)
}) %>% bind_rows()

#merge Tenure DF with main DF
EloDF <- left_join(x = EloDF, y = TenureDF, by = c("driverId", "EloRaceId"))
rm(TenureDF)

#DF to hold driver Bgn Elo ratings
EloHoldings <- EloDF %>%
  select(driverId) %>%
  distinct() %>%
  mutate(EloHold = rookieElo,
         EloRaceId = NA)

###main function that calculates the Elo scores per round
calculateRound <- function(Round) {
  
  thisRoundDF <- EloDF %>%
    filter(EloRaceId == Round)
  
  #initalize empty holding arrays for loops
  ExpWinFinal <- array(data = NA, dim = length(thisRoundDF$driverId))
  EloArray <- array(data = NA, dim = length(thisRoundDF$driverId))
  
  for (i in seq_along(thisRoundDF$driverId)) {
    
    #replace Bgn score with End score of previous round
    if (is.na(thisRoundDF$EloScoreBgn[i])) {thisRoundDF$EloScoreBgn[i] <- as.numeric(EloHoldings[which(EloHoldings$driverId == thisRoundDF$driverId[i]),"EloHold"])}
    
    #if no previous round then replace with Rookie Value; probably unneccessary because EloHoldings now contains rookie value
    if (is.na(thisRoundDF$EloScoreBgn[i])) {thisRoundDF$EloScoreBgn[i] <- rookieElo}
    
    #list of all drivers but the one to calculate
    competitorDrivers <- unique(thisRoundDF$driverId)[unique(thisRoundDF$driverId) != thisRoundDF$driverId[i]]

    #initalize array
    ExpWin <- array(data = NA, dim = length(competitorDrivers))
    
    #calculate the expected win rate for each driver
    for (j in seq_along(competitorDrivers)) {
      
      #do only if the competitor driver finishes
      if (!is.na(as.numeric(thisRoundDF[thisRoundDF$driverId == competitorDrivers[j], "position"]))) {
        ExpWin[j] <- calcExpWin(as.numeric(thisRoundDF[thisRoundDF$driverId == thisRoundDF$driverId[i], "EloScoreBgn"]),
                                as.numeric(thisRoundDF[thisRoundDF$driverId == competitorDrivers[j], "EloScoreBgn"]))
      }
      
      #array of total expected round robin wins for each driver
      ExpWinFinal[i] <- sum(ExpWin, na.rm = TRUE)
    }
    
    #calculate the End score
    EloArray[i] <- calcScore(as.numeric(thisRoundDF[i, "Tenure"]), #tenure
                             max(thisRoundDF$position, na.rm = TRUE) - as.numeric(thisRoundDF[i, "position"]), #actual wins
                             ExpWinFinal[i], #expected win
                             as.numeric(thisRoundDF[i, "EloScoreBgn"]) #oldscore
    )
    
    #replace NAs with old score; i.e. if the driver didn't finish then use the starting Elo score
    if (is.na(EloArray[i])) {EloArray[i] <- as.numeric(thisRoundDF[i, "EloScoreBgn"])}
    
    next
    
    return(EloArray)
  }
  
  ###normalize to prevent Elo inflation
  #recenter around Elo mean
  EloArray <- EloArray * uElo / (sum(EloArray) / length(EloArray))
  
  #normalize the array by setting the SD = scaleFactor and scaling all the values
  scaleFactor <- SDstandard / sd(EloArray)
  EloArray <- lapply(1:length(EloArray), function(x){uElo + (scaleFactor * scale(EloArray)[x] * sd(EloArray))}) %>% unlist()
  
  ###clean up and store the data
  #append scores to table
  thisRoundDF$EloScoreEnd <- EloArray

  #write scores to holding DF
  EloHoldings <<- EloHoldings
  
  EloHoldings$EloHold[match(thisRoundDF$driverId, EloHoldings$driverId)] <<- thisRoundDF$EloScoreEnd
  EloHoldings$EloRaceId[match(thisRoundDF$driverId, EloHoldings$driverId)] <<- thisRoundDF$EloRaceId
  
  #merge with master DF
  EloDF <<- EloDF
  
  EloDF$EloScoreBgn[match(thisRoundDF$EloId, EloDF$EloId)] <<- thisRoundDF$EloScoreBgn
  EloDF$EloScoreEnd[match(thisRoundDF$EloId, EloDF$EloId)] <<- thisRoundDF$EloScoreEnd
}


#calculate scores for all rounds
# lapply(1:100, calculateRound)
lapply(sort(unique(EloDF$EloRaceId)), calculateRound)

#add race date
EloDF <- EloDF %>%
  left_join(y = Races[, c("raceId", "date")], by = "raceId")

# Visuals -----------------------------------------------------------------

seashell.theme <- theme(legend.position = "none",
                        panel.grid.minor = element_line(color = NA),
                        panel.background = element_rect(fill = "seashell2"),
                        plot.background = element_rect(fill = "seashell",
                                                       color = NA),
                        axis.title = element_text(color = "gray30",
                                                  size = 12),
                        strip.background = element_rect(fill = "seashell3"),
                        plot.title = element_text(color = "gray30",
                                                  size = 14,
                                                  face = "bold"))

#visualization of Elos over time
EloDF %>%
  # filter(DescTools::Year(date) >=2010) %>%
  ggplot(aes(x = date, y = EloScoreEnd, group = driverId, color = as.factor(driverId)), alpha = .3) +
  geom_step(alpha = 0.5) +
  scale_y_continuous(breaks = seq(250 * floor(min(EloDF$EloScoreEnd) / 250),
                                  250 * ceiling(max(EloDF$EloScoreEnd) / 250),
                                  250)) +
  labs(x = "Year", y = "Elo Score") +
  seashell.theme

ggsave(filename = "F1_Elo_rainbow.png",
       plot = last_plot(),
       path = project.path,
       device = "png",
       width = 9,
       height = 5)

p <- ggplotly(ggplot2::last_plot())
api_create(p, filename = "F1-Elo-time-series")
rm(p)

#top drivers based on top five years of Elo
EloDF %>%
  group_by(year, driverId) %>%
  summarize(EloScore = mean(EloScoreEnd)) %>%
  ungroup() %>%
  group_by(driverId) %>%
  mutate(Rank = rank(-EloScore)) %>%
  ungroup() %>%
  filter(Rank <= 5) %>%
  group_by(driverId) %>%
  summarize(EloScore = mean(EloScore)) %>%
  left_join(y = Drivers[, c("driverId", "driverRef")], by = "driverId") %>%
  arrange(desc(EloScore)) %>%
  View()

#distribution of Elos for end of each year with champion Elo vline
EloDF %>%
  group_by(year) %>%
  filter(round == max(round)) %>%
         # year >= 1990) %>%
  ungroup() %>%
  left_join(y = driverStandings[, c("raceId", "driverId", "points")], by = c("raceId", "driverId")) %>%
  group_by(EloRaceId) %>%
  mutate(Champion = EloScoreEnd[match(max(points, na.rm = TRUE), points)]) %>%
  ungroup() %>%
  ggplot(aes(x = EloScoreEnd)) +
    geom_density(fill = "grey75", color = "white") +
    # geom_histogram(color = "white") +
    facet_wrap(~ year, scale = "free_x") +
    geom_vline(aes(xintercept = Champion), color = "grey60", size = 1.05) +
    guides(fill = FALSE) +
    labs(x = "Elo Score") +
    theme(
      legend.position = "none",
      plot.subtitle = element_text(vjust = 1),
      plot.caption = element_text(vjust = 1),
      axis.ticks = element_line(linetype = "blank"),
      panel.grid.major = element_line(colour = NA),
      panel.grid.minor = element_line(colour = NA),
      panel.background = element_rect(fill = "seashell"),
      plot.background = element_rect(fill = "seashell",
                                     colour = NA),
      axis.title = element_text(colour = "gray30"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())

#percentile rank of the champion Elo
EloDF %>%
  group_by(year) %>%
  filter(round == max(round)) %>%
  ungroup() %>%
  left_join(y = driverStandings[, c("raceId", "driverId", "points")], by = c("raceId", "driverId")) %>%
  group_by(EloRaceId) %>%
  mutate(Champion = EloScoreEnd[match(max(points, na.rm = TRUE), points)],
         ChampionPercentile = percent_rank(EloScoreEnd)[match(Champion, EloScoreEnd)]) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = ChampionPercentile)) +
    geom_step()

#plot fastest lap speed over time
Results %>%
  group_by(raceId) %>%
  summarize(fastestLap = max(fastestLapSpeed, na.rm = TRUE)) %>%
  select(raceId, fastestLap) %>%
  left_join(y = Races, by = "raceId") %>%
  filter(fastestLap > 0) %>%
  ggplot(aes(x = date, y = fastestLap, color = fastestLap)) +
  geom_point() +
  facet_wrap(~ name, ncol = 9) +
  guides(color = FALSE)

# scratch work ------------------------------------------------------------

#schumacher, vettel, senna, alonso, hamilton
topDrivers <- c(30, 20, 102, 4, 1)

EloDF %>%
  filter(driverId == 20 | driverId == 1) %>%
  ggplot(aes(x = EloRaceId, y = EloScoreEnd, group = driverId, color = as.factor(driverId))) +
  geom_line() +
  guides(color = FALSE)

#distribution of Elos for end of each year
EloDF %>%
  group_by(year) %>%
  filter(round == max(round),
         year >= 1990) %>%
  ungroup() %>%
  ggplot(aes(x = EloScoreEnd)) +
  geom_density_ridges_gradient(aes(y = as.factor(year), fill = ..x..), color = "white") +
  scale_fill_viridis_c(begin = .2, end = .8) +
  guides(fill = FALSE) +
  labs(x = "Elo Score", y = "Year") +
  scale_x_continuous(breaks = seq(1000, 2200, 200)) +
  coord_cartesian(xlim = c(1000, 2200)) +
  theme(
    plot.subtitle = element_text(vjust = 1),
    plot.caption = element_text(vjust = 1),
    axis.ticks = element_line(linetype = "blank"),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_line(colour = NA),
    panel.background = element_rect(fill = "antiquewhite"),
    plot.background = element_rect(fill = "antiquewhite",
                                   colour = NA),
    axis.title = element_text(colour = "gray30")
  )

#distribution of Elos for each race
EloDF %>%
  filter(year > 2008) %>%
  ggplot(aes(x = EloScoreEnd)) +
  geom_density_ridges_gradient(aes(y = as.factor(EloRaceId), fill = ..x..), color = "white") +
  scale_fill_viridis_c() +
  facet_wrap(~year, scales = "free_y") +
  guides(fill = FALSE)

#wins by constructors segmented by country
Results %>%
  filter(position == 1) %>% 
  group_by(constructorId) %>% 
  summarize(Wins = n()) %>%
  left_join(y = Constructors[c("constructorId", "name", "nationality")], by = "constructorId") %>%
  filter(Wins > 10) %>%
  ggplot(aes(x = reorder(name, -Wins), y = Wins, fill = nationality)) +
  geom_col() +
  facet_wrap(~ nationality, scale = "free_x") +
  scale_fill_brewer(palette = "Paired") +
  guides(fill = FALSE) +
  labs(x = "")
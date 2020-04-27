library(data.table)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggplotify)
library(gtable) 
library(dplyr)


path <- c("R/march-madness-analytics-2020/MDataFiles_Stage2/")
path_play <- c("R/march-madness-analytics-2020/MPlayByPlay_Stage2/")

source("R/march-madness-analytics-2020/court_plot.R")
source("R/march-madness-analytics-2020/NCAA_functions.R")

#dat <- fread(paste0(path_play,"MEvents2016.csv"))
#dat <- rbind(dat,fread(paste0(path_play,"MEvents2015.csv")))
fileLoad <- function(year){
  fread(paste0(path_play,"MEvents",year,".csv"))
}

regresults <- fread(paste0(path,"MRegularSeasonDetailedResults.csv"))
results <- fread(paste0(path,"MNCAATourneyDetailedResults.csv"))
seeds <- fread(paste0(path,"MNCAATourneySeeds.csv"))
teams <- fread(paste0(path, "Mteams.csv"))
conf <- fread(paste0(path, "MTeamConferences.csv"))
seeds$Seed = as.numeric(substring(seeds$Seed,2,3))


plotgame <- function(WTeam,LTeam, year){ 
  
  # Load data for the basic game information (Teams,seeds,year,conferences)
  # create basic game information grob
  # data load for the game plot
  # join the team names
  # track each teams score via the Event types for the shots made.
  
  datGame <-  
    seeds %>%
      filter(Season == year) %>%
      left_join(select(teams,TeamID,TeamName), by = c('TeamID')) %>%
      filter(TeamName == WTeam | TeamName == LTeam) %>%
      inner_join(conf,by = c('Season','TeamID')) %>%
      select(Teams = TeamName,Seed = Seed,Conference = ConfAbbrev) 
  
  g <- tableGrob(datGame,rows = NULL, theme = ttheme_default(7)) 
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 1, l = 1, r = ncol(g))
      
    plota <- as.grob(g)
   
  subtit <-  
  results %>%
    filter(Season == year) %>%
    select(WTeamID,LTeamID,WScore,LScore) %>%
    left_join(select(teams,TeamID,TeamName), by = c('WTeamID' = 'TeamID')) %>%
    left_join(select(teams,TeamID,TeamName), by = c('LTeamID' = 'TeamID')) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>% 
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    select(WTeamName,WScore,LTeamName,LScore) %>%
    mutate(game = paste(year,WTeamName,"=",WScore,LTeamName,"=",LScore,sep = " " )) %>%
    select(game)
  
  subtit <- subtit[1]$game
  
  dat <- fileLoad(year)  
  
  Labels <- data.table(x = c(600, 1800, 2700), y = c(100,100,100), label = c('1st Half', '2nd Half', 'Overtime'))
  
  title <- "NCAA March Madness Classic Games"
  lim <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    summarise(gameTime = max(ElapsedSeconds)) %>%
    pull(gameTime)
  
  plotb <-
    dat %>% 
      left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
      left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
      rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
      filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
      filter(EventType %in% c('made1', 'made2','made3')) %>%
      select(WCurrentScore, LCurrentScore, ElapsedSeconds,EventTeamID,EventType,WTeamName,LTeamName) %>% 
      mutate(EventTeamID = as.character(EventTeamID)) %>%
      ggplot(aes(x = ElapsedSeconds)) +
        ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit[1]))))))+
        geom_point(shape = 1, aes(y = WCurrentScore, color= WTeam))+
        geom_point(shape = 1,aes(y = LCurrentScore, color= LTeam)) +
        geom_vline(xintercept = c(1200,2400)) +
        geom_label(size = 3, data= Labels,aes(x=x,y=y, label = label)) +
        scale_x_continuous(limits = c(0,lim + 50)) +
        ylab(label="Score")+
        xlab(label="Elapsed Time (seconds)")+
        scale_colour_manual("Teams",
                          breaks=c(WTeam,LTeam),
                          values=c("blue","red"))
  
  plotc <- as.grob(plotb)
  
  grid.newpage()
  grid.draw(plotc)
  vp = viewport(x = 0.20,y=0.75,width = 0.2,height = 0.2)
  pushViewport(vp)
  grid.draw(plota)
  upViewport()
}

plotgame(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016)
plotgame(WTeam = "Kentucky" ,LTeam = "Notre Dame", 2015)
plotgame(WTeam = "Michigan" ,LTeam = "Oklahoma St", 2017)
plotgame(WTeam = "Purdue" ,LTeam = "Tennessee", 2019)
plotgame(WTeam = "Kansas" ,LTeam = "Duke", 2018)
plotgame(WTeam = "MTSU" ,LTeam = "Michigan St", 2016)
plotgame(WTeam = "Villanova" ,LTeam = "North Carolina", 2016)
plotgame(WTeam = "UMBC" ,LTeam = "Virginia", 2018)
plotgame(WTeam = "Buffalo" ,LTeam = "Arizona", 2018)
plotgame(WTeam = "Marshall" ,LTeam = "Wichita St", 2018)
plotgame(WTeam = "UC Irvine" ,LTeam = "Kansas St", 2019)
plotgame(WTeam = "Michigan St",LTeam = "Duke",2019)

############################################################################
# Plot game events / scorce diff / vs. time
############################################################################

plotgameEvents <- function(WTeam,LTeam, year){
  
  # Create grob of seed and conferences
 
statSum <-  
  rbind (
  regresults %>%
    filter(Season == year) %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam) %>%
    select(
    T1=WTeamID,
    T1_Name = WTeamName,
    T1_fgm=WFGM,
    T1_fga=WFGA,
    T1_fgm3=WFGM3,
    T1_fga3=WFGA3,
    T1_or=WOR,
    T1_ftm=WFTM, 
    T1_fta=WFTA,
    T1_to=WTO, 
    T1_stl=WStl, 
    T1_blk=WBlk, 
    ),
  regresults %>%
    filter(Season == year) %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(LTeamName == WTeam) %>%
    select(
      T1=LTeamID,
      T1_Name = LTeamName,
      T1_fgm=LFGM,
      T1_fga=LFGA,
      T1_fgm3=LFGM3,
      T1_fga3=LFGA3,
      T1_or=LOR,
      T1_ftm=LFTM, 
      T1_fta=LFTA,
      T1_to=LTO, 
      T1_stl=LStl, 
      T1_blk=LBlk, 
    ),
  
  regresults %>%
    filter(Season == year) %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(LTeamName == LTeam) %>%
    select(
      T1=LTeamID,
      T1_Name = LTeamName,
      T1_fgm=LFGM,
      T1_fga=LFGA,
      T1_or=LOR,
      T1_fgm3=LFGM3,
      T1_fga3=LFGA3,
      T1_ftm=LFTM, 
      T1_fta=LFTA,
      T1_to=LTO, 
      T1_stl=LStl, 
      T1_blk=LBlk, 
    ),
  regresults %>%
    filter(Season == year) %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == LTeam) %>%
    select(
      T1=WTeamID,
      T1_Name = WTeamName,
      T1_fgm=WFGM,
      T1_fga=WFGA,
      T1_fgm3=WFGM3,
      T1_fga3=WFGA3,
      T1_ftm=WFTM, 
      T1_or=WOR,
      T1_fta=WFTA,
      T1_to=WTO, 
      T1_stl=WStl, 
      T1_blk=WBlk, 
    )) %>%
    group_by(T1_Name) %>%
    summarise(fgm_mean=round(mean(T1_fgm),0),     
              fga_mean=round(mean(T1_fga),0), 
              fgp=round(sum(T1_fgm)/sum(T1_fga),3),
              fgm3=round(mean(T1_fgm3),0), 
              fga3=round(mean(T1_fga3),0),
              fg3p=round(sum(T1_fgm3)/sum(T1_fga3),3),    
              ftm=round(mean(T1_ftm),0), 
              fta=round(mean(T1_fta),0),
              ftp=round(sum(T1_ftm)/sum(T1_fta),3),        
              to_mean=round(mean(T1_to),0),
              stl_mean=round(mean(T1_stl),0), 
              blk_mean=round(mean(T1_blk),0), 
              off_eff=round(sum(T1_fgm + T1_fgm3)/sum(T1_fga+T1_fga3+T1_or+0.4*T1_fta),3))
 
  datGame <-  
    seeds %>%
    filter(Season == year) %>%
    left_join(select(teams,TeamID,TeamName), by = c('TeamID')) %>%
    filter(TeamName == WTeam | TeamName == LTeam) %>%
    inner_join(conf,by = c('Season','TeamID')) %>%
    select(Teams = TeamName,Seed = Seed,Conference = ConfAbbrev) %>%
    inner_join(statSum, by = c("Teams" = "T1_Name"))
  
  g <- tableGrob(datGame,rows = NULL, theme = ttheme_default(7)) 
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 1, l = 1, r = ncol(g))
  
  plota <- as.grob(g)
  
  subtit <-  
    results %>%
    filter(Season == year) %>%
    select(WTeamID,LTeamID,WScore,LScore) %>%
    left_join(select(teams,TeamID,TeamName), by = c('WTeamID' = 'TeamID')) %>%
    left_join(select(teams,TeamID,TeamName), by = c('LTeamID' = 'TeamID')) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>% 
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    select(WTeamName,WScore,LTeamName,LScore) %>%
    mutate(game = paste(year,WTeamName,"=",WScore,LTeamName,"=",LScore,sep = " " )) %>%
    select(game)
  
  subtit <- subtit[1]$game
  
  dat <- fileLoad(year)  
  
  datEvents <-
  dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    filter(EventType %in% setdiff(unique(dat$EventType),c('reb','sub', 'timeout','assist', 'miss1','miss2','miss3','made1','made2','made3'))) %>%
    select(ElapsedSeconds,EventTeamID,EventType) %>% 
    mutate(count = 1,
           row = row_number()) %>%
    group_by(EventType) %>%
    pivot_wider(names_from = EventType, values_from = count, values_fill = list(count = 0)) %>% 
    select(-row) %>%
    group_by(EventTeamID) %>%
    mutate(block = cumsum(block),
           turnover = cumsum(turnover),
           foul = cumsum(foul),
           steal = cumsum(steal)) %>%
    pivot_longer(cols = c('block','turnover','foul','steal'), names_to = "EventID", values_to = "count") %>%
    left_join(select(teams,TeamID,TeamName), by = c("EventTeamID" = "TeamID")) %>%
    ungroup() %>%
    mutate(EventTeamID = as.character(EventTeamID),
           count = ifelse(TeamName == LTeam, count*-1,count)) %>%
    data.table()
  
  Labels <- data.table(x = c(600, 1800, 2700), y = c(40,40,40), label = c('1st Half', '2nd Half', 'Overtime'))
  LabelTeam <- data.table(x=c(200,200),y=c(15,-15), label = c(WTeam,LTeam))
  
  title <- "NCAA March Madness Classic Games Event Comparisions"
  
  lim <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    summarise(gameTime = max(ElapsedSeconds)) %>%
    pull(gameTime)
  
  plotb <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    filter(EventType %in% c('made1', 'made2','made3')) %>%
    select(WCurrentScore, LCurrentScore, ElapsedSeconds,EventTeamID,EventType,WTeamName,LTeamName) %>% 
    mutate(EventTeamID = as.character(EventTeamID),
           WScoreDiff = WCurrentScore - LCurrentScore,
           LScoreDiff = LCurrentScore - WCurrentScore) %>%
    ggplot() +
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit[1]))))))+
    geom_line(size = I(2), color = "blue", aes(x = ElapsedSeconds,y = WScoreDiff))+
    geom_line(size = I(2),color = 'red',aes(x = ElapsedSeconds,y = LScoreDiff)) +
    geom_point(size = I(3),shape = 1, data = datEvents[TeamName == WTeam,], aes(x = ElapsedSeconds,y = count, color= EventID)) +
    geom_line(size = I(0), data = datEvents[TeamName == WTeam,], aes(x = ElapsedSeconds,y = count, color= EventID)) +
    geom_point(size = I(3),shape = 1, data = datEvents[TeamName == LTeam,], aes(x = ElapsedSeconds,y = count, color= EventID)) +
    geom_line(size = I(0),linetype = 2, data = datEvents[TeamName == LTeam,], aes(x = ElapsedSeconds,y = count, color= EventID)) +
    geom_vline(xintercept = c(1200,2400)) +
    geom_text(size = 3, data= Labels,aes(x=x,y=y, label = label)) +
    geom_label(size = 3, data= LabelTeam,aes(x=x,y=y, label = label)) +
    scale_x_continuous(limits = c(0,lim + 50)) +
    ylab(label="Score Difference / Event Count")+
    xlab(label="Elapsed Time (seconds)")
  
  plotc <- as.grob(plotb)
  
  grid.newpage()
  grid.draw(plotc)
  vp = viewport(x = 0.47,y=0.76,width = 0.10,height = 0.2)
  pushViewport(vp)
  grid.draw(plota)
  upViewport()

}

plotgameEvents(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016)
plotgameEvents(WTeam = "Kentucky" ,LTeam = "Notre Dame", 2015)
plotgameEvents(WTeam = "Michigan" ,LTeam = "Oklahoma St", 2017)
plotgameEvents(WTeam = "Purdue" ,LTeam = "Tennessee", 2019)
plotgameEvents(WTeam = "Kansas" ,LTeam = "Duke", 2018)
plotgameEvents(WTeam = "MTSU" ,LTeam = "Michigan St", 2016)
plotgameEvents(WTeam = "Villanova" ,LTeam = "North Carolina", 2016)
plotgameEvents(WTeam = "UMBC" ,LTeam = "Virginia", 2018)
plotgameEvents(WTeam = "Buffalo" ,LTeam = "Arizona", 2018)
plotgameEvents(WTeam = "Marshall" ,LTeam = "Wichita St", 2018)
plotgameEvents(WTeam = "UC Irvine" ,LTeam = "Kansas St", 2019)
plotgameEvents("Michigan St","Duke",2019)

#########################################################################
#Histograms and clump test maybe
#########################################################################

year <- 2016
WTeam <- "Texas A&M"
LTeam <- "Northern Iowa"

plotgameDens <- function(WTeam, LTeam, year){

dat <- fileLoad(year)

title <- "NCAA March Madness Classic Game Event Density"

subtit <-  
  results %>%
  filter(Season == year) %>%
  select(WTeamID,LTeamID,WScore,LScore) %>%
  left_join(select(teams,TeamID,TeamName), by = c('WTeamID' = 'TeamID')) %>%
  left_join(select(teams,TeamID,TeamName), by = c('LTeamID' = 'TeamID')) %>%
  rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>% 
  filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
  select(WTeamName,WScore,LTeamName,LScore) %>%
  mutate(game = paste(year,WTeamName,"=",WScore,LTeamName,"=",LScore,sep = " " )) %>%
  select(game)

subtit <- subtit[1]$game

rbind(
rbind(
dat %>% 
  left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
  left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
  rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
  filter(WTeamName == WTeam ) %>%
  filter(EventType %in% setdiff(unique(dat$EventType),c('reb','sub', 'timeout','assist', 'miss1','miss2','miss3'))) %>%
  select(ElapsedSeconds,EventTeamID,EventType,DayNum,WTeamID),

dat %>% 
  left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
  left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
  rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
  filter(LTeamName == WTeam ) %>%
  filter(EventType %in% setdiff(unique(dat$EventType),c('reb','sub', 'timeout','assist', 'miss1','miss2','miss3'))) %>%
  select(ElapsedSeconds,EventTeamID,EventType,DayNum,WTeamID = LTeamID)
) %>% 
  left_join(select(teams,TeamID,TeamName), by = c('EventTeamID' = 'TeamID')) %>%
  filter(TeamName == WTeam) %>%
  mutate(count = 1,
         row = row_number()) %>%
  mutate(count = ifelse(EventType == 'made1', 1, count ),
         count = ifelse(EventType == 'made2', 2, count),
         count = ifelse(EventType == 'made3', 3, count)) %>%
  mutate(EventType = ifelse(EventType == 'made1' |EventType =='made2'|EventType =='made3','point',EventType)) %>%
  group_by(EventType) %>%
  pivot_wider(names_from = EventType, values_from = count, values_fill = list(count = 0)) %>%
  group_by(DayNum) %>%
  mutate(block = cumsum(block),
         turnover = cumsum(turnover),
         foul = cumsum(foul),
         steal = cumsum(steal),
         point = cumsum(point),
         maxTime = max(ElapsedSeconds)) %>%
  pivot_longer(cols = c('block','turnover','foul','steal','point'), names_to = "EventID", values_to = "count") %>%
  select(-row) %>%
  #left_join(select(teams,TeamID,TeamName), by = c("EventTeamID" = "TeamID")) %>%
  ungroup() %>%
  mutate(EventTeamID = as.character(EventTeamID)) %>%
  data.table() %>% # this line will have vs. time  next block will take the maximum count by event for histagram plots.
  filter(ElapsedSeconds == maxTime),

rbind(
  dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == LTeam ) %>%
    filter(EventType %in% setdiff(unique(dat$EventType),c('reb','sub', 'timeout','assist', 'miss1','miss2','miss3'))) %>%
    select(ElapsedSeconds,EventTeamID,EventType,DayNum,WTeamID),
  
  dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(LTeamName == LTeam ) %>%
    filter(EventType %in% setdiff(unique(dat$EventType),c('reb','sub', 'timeout','assist', 'miss1','miss2','miss3'))) %>%
    select(ElapsedSeconds,EventTeamID,EventType,DayNum,WTeamID = LTeamID)
) %>% 
  left_join(select(teams,TeamID,TeamName), by = c('EventTeamID' = 'TeamID')) %>%
  filter(TeamName == LTeam) %>%
  mutate(count = 1,
         row = row_number()) %>%
  mutate(count = ifelse(EventType == 'made1', 1, count ),
         count = ifelse(EventType == 'made2', 2, count),
         count = ifelse(EventType == 'made3', 3, count)) %>%
  mutate(EventType = ifelse(EventType == 'made1' |EventType =='made2'|EventType =='made3','point',EventType)) %>%
  group_by(EventType) %>%
  pivot_wider(names_from = EventType, values_from = count, values_fill = list(count = 0)) %>%
  group_by(DayNum) %>%
  mutate(block = cumsum(block),
         turnover = cumsum(turnover),
         foul = cumsum(foul),
         steal = cumsum(steal),
         point = cumsum(point),
         maxTime = max(ElapsedSeconds)) %>%
  pivot_longer(cols = c('block','turnover','foul','steal','point'), names_to = "EventID", values_to = "count") %>%
  select(-row) %>%
  #left_join(select(teams,TeamID,TeamName), by = c("EventTeamID" = "TeamID")) %>%
  ungroup() %>%
  mutate(EventTeamID = as.character(EventTeamID)) %>%
  data.table() %>% # this line will have vs. time  next block will take the maximum count by event for histagram plots.
  filter(ElapsedSeconds == maxTime) 
)%>%
  ggplot(aes(x=count,fill =EventID)) +
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit[1]))))))+
    #geom_histogram()+
    geom_density() +
    facet_grid(TeamName~EventID, scales = "free") 

}

plotgameDens(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016)
plotgameDens(WTeam = "Kentucky" ,LTeam = "Notre Dame", 2015)
plotgameDens(WTeam = "Michigan" ,LTeam = "Oklahoma St", 2017)
plotgameDens(WTeam = "Purdue" ,LTeam = "Tennessee", 2019)
  
  
#################################################################################################
# Regression analysis
###############################################################################################

plotRegressComp <- function(WTeam,LTeam, year) {
  
  #year <- 2016
  #WTeam <- "Texas A&M"
  #LTeam <- "Northern Iowa"
  
  #WTeam = "Kentucky" 
  #LTeam = "Notre Dame"
  #year = 2015
  
  dat <- fileLoad(year)
  
  # data for winning team season regression
  datRegWin <- 
  rbind(
  dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName") %>%
    filter(WTeamName == WTeam,
           EventType %in% c('made1', 'made2','made3'),
           WTeamID == EventTeamID) %>%
    select(WTeamID,WCurrentScore,ElapsedSeconds,WTeamName),
  
  dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName") %>%
    filter(WTeamName == WTeam,
           EventType %in% c('made1', 'made2','made3'),
           LTeamID == EventTeamID) %>%
    select(LTeamID,LCurrentScore,ElapsedSeconds,WTeamName) %>%
    rename(WTeamID = LTeamID, WCurrentScore = LCurrentScore)
  )
  # Points agains switching W and L in current score
  datRegWinPointsAgainst <- 
    rbind(
      dat %>% 
        left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
        rename("WTeamName" = "TeamName") %>%
        filter(WTeamName == WTeam,
               EventType %in% c('made1', 'made2','made3'),
               WTeamID == EventTeamID) %>%
        select(WTeamID,LCurrentScore,ElapsedSeconds,WTeamName),
      
      dat %>% 
        left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
        rename("WTeamName" = "TeamName") %>%
        filter(WTeamName == WTeam,
               EventType %in% c('made1', 'made2','made3'),
               LTeamID == EventTeamID) %>%
        select(LTeamID,WCurrentScore,ElapsedSeconds,WTeamName) %>%
        rename(WTeamID = LTeamID, LCurrentScore = WCurrentScore)
    )
  
  datRegLose <- 
    rbind(
      dat %>% 
        left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
        rename("LTeamName" = "TeamName") %>%
        filter(LTeamName == LTeam,
               EventType %in% c('made1', 'made2','made3'),
               LTeamID == EventTeamID) %>%
        select(LTeamID,LCurrentScore,ElapsedSeconds,LTeamName),
      
      dat %>% 
        left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
        rename("LTeamName" = "TeamName") %>%
        filter(LTeamName == LTeam,
               EventType %in% c('made1', 'made2','made3'),
               WTeamID == EventTeamID) %>%
        select(WTeamID,WCurrentScore,ElapsedSeconds,LTeamName) %>%
        rename(LTeamID = WTeamID, LCurrentScore = WCurrentScore)
    )
  
  datRegLosePointsAgainst <- 
    rbind(
      dat %>% 
        left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
        rename("LTeamName" = "TeamName") %>%
        filter(LTeamName == LTeam,
               EventType %in% c('made1', 'made2','made3'),
               LTeamID == EventTeamID) %>%
        select(LTeamID,WCurrentScore,ElapsedSeconds,LTeamName),
      
      dat %>% 
        left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
        rename("LTeamName" = "TeamName") %>%
        filter(LTeamName == LTeam,
               EventType %in% c('made1', 'made2','made3'),
               WTeamID == EventTeamID) %>%
        select(WTeamID,LCurrentScore,ElapsedSeconds,LTeamName) %>%
        rename(LTeamID = WTeamID, WCurrentScore = LCurrentScore)
    )
  
  # Model of points for vs game time
  robust_model_win = MASS::rlm(WCurrentScore ~ 0 + ElapsedSeconds, data = datRegWin, method = "MM", init = "lts")
  robust_model_Lose = MASS::rlm(LCurrentScore ~ 0 + ElapsedSeconds, data = datRegLose, method = "MM", init = "lts")
  # Model of points against vs game time
  robust_model_win_pa = MASS::rlm(LCurrentScore ~ 0 + ElapsedSeconds, data = datRegWinPointsAgainst, method = "MM", init = "lts")
  robust_model_Lose_pa = MASS::rlm(WCurrentScore ~ 0 + ElapsedSeconds, data = datRegLosePointsAgainst, method = "MM", init = "lts")
  
  #summary(robust_model_win)
  #summary(robust_model_Lose)
  #summary(robust_model_win_pa)
  #summary(robust_model_Lose_pa)
  
  subtit <-  
    results %>%
    filter(Season == year) %>%
    select(WTeamID,LTeamID,WScore,LScore) %>%
    left_join(select(teams,TeamID,TeamName), by = c('WTeamID' = 'TeamID')) %>%
    left_join(select(teams,TeamID,TeamName), by = c('LTeamID' = 'TeamID')) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>% 
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    select(WTeamName,WScore,LTeamName,LScore) %>%
    mutate(game = paste(year,WTeamName,"=",WScore,LTeamName,"=",LScore,sep = " " )) %>%
    select(game)
  
  subtit <- subtit[1]$game
  
  lim <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    summarise(gameTime = max(ElapsedSeconds)) %>%
    pull(gameTime)
  
  datGame <-  
    seeds %>%
    filter(Season == year) %>%
    left_join(select(teams,TeamID,TeamName), by = c('TeamID')) %>%
    filter(TeamName == WTeam | TeamName == LTeam) %>%
    inner_join(conf,by = c('Season','TeamID')) %>%
    select(Teams = TeamName,Seed = Seed,Conference = ConfAbbrev) %>%
    mutate(E_pf = c(round(robust_model_win$coefficients[1]*lim,0),round(robust_model_Lose$coefficients[1]*lim,0)),
           E_pa = c(round(robust_model_win_pa$coefficients[1]*lim,0),round(robust_model_Lose_pa$coefficients[1]*lim,0))
           )
  
  g <- tableGrob(datGame,rows = NULL, theme = ttheme_default(7)) 
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 1, l = 1, r = ncol(g))
  
  plota <- as.grob(g)
  
  title <- "NCAA March Madness Classic Games Regression Analysis"
  
  
  
  Labels <- data.table(x = c(600, 1800, 2700), y = c(3,3,3), label = c('1st Half', '2nd Half', 'Overtime'))

  plotb <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    filter(EventType %in% c('made1', 'made2','made3')) %>%
    select(WCurrentScore, LCurrentScore, ElapsedSeconds,EventTeamID,EventType,WTeamName,LTeamName) %>% 
    mutate(EventTeamID = as.character(EventTeamID)) %>%
    ggplot(aes(x = ElapsedSeconds)) +
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit[1]))))))+
    geom_point(shape = 1, aes(y = WCurrentScore, color= WTeam))+
    geom_point(shape = 1,aes(y = LCurrentScore, color= LTeam)) +
    #geom_smooth(method = MASS::rlm, formula = y ~ 0 + x, se = FALSE, color = "Blue", size = 1, aes(x = ElapsedSeconds,y=WCurrentScore)) +
    #geom_smooth(method = MASS::rlm, formula = y ~ 0 + x, se = FALSE, color = "Red", size = 1, aes(x = ElapsedSeconds,y=LCurrentScore)) +
    geom_abline(intercept=0, slope=robust_model_win$coefficients[1], color='Blue', size=1, linetype = 1) + 
    geom_abline(intercept=0, slope=robust_model_Lose$coefficients[1], color='Red', size=1, linetype = 1) + 
    geom_abline(intercept=0, slope=robust_model_win_pa$coefficients[1], color='Blue', size=1, linetype = 2) + 
    geom_abline(intercept=0, slope=robust_model_Lose_pa$coefficients[1], color='Red', size=1, linetype = 2) + 
    geom_vline(xintercept = c(1200,2400)) +
    geom_text(size = 3, data= Labels,aes(x=x,y=y, label = label)) +
    scale_x_continuous(limits = c(0,lim + 50)) +
    ylab(label="Score")+
    xlab(label="Elapsed Time (seconds)")+
    scale_colour_manual("Teams",
                        breaks=c(WTeam,LTeam),
                        values=c("blue","red"))
  
  plotc <- as.grob(plotb)
  
  grid.newpage()
  grid.draw(plotc)
  vp = viewport(x = 0.25,y=0.80,width = 0.2,height = 0.2)
  pushViewport(vp)
  grid.draw(plota)
  upViewport()
  
}

plotRegressComp(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016)
plotRegressComp(WTeam = "Kentucky" ,LTeam = "Notre Dame", 2015)
plotRegressComp(WTeam = "Michigan" ,LTeam = "Oklahoma St", 2017)
plotRegressComp(WTeam = "Purdue" ,LTeam = "Tennessee", 2019)
plotRegressComp(WTeam = "Kansas" ,LTeam = "Duke", 2018)
plotRegressComp(WTeam = "MTSU" ,LTeam = "Michigan St", 2016)
plotRegressComp(WTeam = "Villanova" ,LTeam = "North Carolina", 2016)
plotRegressComp(WTeam = "UMBC" ,LTeam = "Virginia", 2018)
plotRegressComp(WTeam = "Buffalo" ,LTeam = "Arizona", 2018)
plotRegressComp(WTeam = "Marshall" ,LTeam = "Wichita St", 2018)
plotRegressComp(WTeam = "UC Irvine" ,LTeam = "Kansas St", 2019)
plotRegressComp(WTeam ="Michigan St",LTeam ="Duke",2019)

##########################################################################################################
# Shot Charts
##########################################################################################################

shotChart <- function(WTeam,LTeam, year,tstart = 0, tend = 2400) {
  
  #  get the game data
  #  select made and missed 2 and 3 point shots
  #  move all shot the "left side of the court"
  #  rotate the shots to align with the court plot.
  
  #WTeam ="Michigan St"
  #LTeam = "Duke"
  #year = 2019
  #tstart = 0
  #tend = 2400
  
  statSum <-  EventProbCalc(WTeam = WTeam,LTeam = LTeam,year = year, tstart = tstart, tend = tend)
  
 
  datGame <-  
    seeds %>%
    filter(Season == year) %>%
    left_join(select(teams,TeamID,TeamName), by = c('TeamID')) %>%
    filter(TeamName == WTeam | TeamName == LTeam) %>%
    inner_join(conf,by = c('Season','TeamID')) %>%
    select(Teams = TeamName,Seed = Seed,Conference = ConfAbbrev) %>%
    inner_join(statSum, by = c("Teams" = "TeamName"))
  
  g <- tableGrob(datGame,rows = NULL, theme = ttheme_default(7)) 
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 1, l = 1, r = ncol(g))
  
  plota <- as.grob(g)

  #year <- 2019
  #WTeam <- "UC Irvine"
  #LTeam <- "Kansas St"
  
  dat <- fileLoad(year)
  
  gameShotData <-
  dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    filter(EventType %in% c('made2','made3','miss2','miss3' )) %>%
    mutate(EventType = ifelse(grepl('made',EventType),'Made','Miss')) %>%
    select(EventTeamID,X,Y,EventType) %>% 
    left_join(select(teams,TeamID,TeamName), by = c("EventTeamID" = "TeamID")) %>%
    mutate(team_basket = ifelse(X > 50,'right','left')) %>%
    mutate(X = ifelse(team_basket == 'right',  100 - X, X),
           Y = ifelse(team_basket == 'right', 100 - Y,Y)) %>%
    mutate(coord_X = (47/50)*X,
           coord_Y = (50 - Y)/2) %>%
    mutate(X = -1 * coord_Y,
           Y = coord_X - (50/47)*5.3 ) 
    
  subtit <-  
    results %>%
    filter(Season == year) %>%
    select(WTeamID,LTeamID,WScore,LScore) %>%
    left_join(select(teams,TeamID,TeamName), by = c('WTeamID' = 'TeamID')) %>%
    left_join(select(teams,TeamID,TeamName), by = c('LTeamID' = 'TeamID')) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>% 
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    select(WTeamName,WScore,LTeamName,LScore) %>%
    mutate(game = paste(year,WTeamName,"=",WScore,LTeamName,"=",LScore,sep = " " )) %>%
    select(game)
  
  subtit <- subtit[1]$game
  
  title <- "NCAA March Madness Classic Games Regression Analysis"
  
  plotb <-
    college_court +
       geom_point(data = gameShotData, size = I(3),aes(x=X,y=Y,color = TeamName,shape = EventType)) +
      ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit[1]))))))
    
    plotc <- as.grob(plotb)
  
  grid.newpage()
  grid.draw(plotc)
  vp = viewport(x = 0.40,y=0.80,width = 0.2,height = 0.2)
  pushViewport(vp)
  grid.draw(plota)
  upViewport()       
    
    
}

shotChart("UC Irvine","Kansas St",2019)
shotChart("Michigan St","Duke",2019)
shotChart("Virginia","Auburn",2019)

#####################################################################
# poison dist plot for key games point and blocks and turnovers
#####################################################################


poisPlotProb <- function(WTeam, LTeam, year, tstart, tend, Block = 0, Steal = 0,  Turnover = 0, maxEvent = 15) {
  
  #WTeam = "Texas A&M" 
  #LTeam = "Northern Iowa"
  #year = 2016
  #tstart = 2350
  #tend = 2398
  
  results %>%
    filter(Season == year) %>%
    filter(Season == year) %>%
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam)
  
  dat <- fileLoad(year)  
  
  lambdaCalc <-
    rbind(
      rbind(
        regresults %>% 
          filter(Season == year) %>%
          left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
          left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
          rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
          mutate(gametime = ifelse(NumOT == 0, 2400, 2400 + NumOT * 300)) %>%
          filter(WTeamName == WTeam) %>%
          select(WTeamID, WScore,WTO,WStl,WBlk,gametime,WTeamName),
        
        regresults %>% 
          filter(Season == year) %>%
          left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
          left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
          rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
          mutate(gametime = ifelse(NumOT == 0, 2400, 2400 + NumOT * 300)) %>%
          filter(LTeamName == WTeam) %>%
          select(WTeamID = LTeamID, WScore = LScore,WTO = LTO,WStl=LStl,WBlk=LBlk,gametime,WTeamName = LTeamName)
      ) , 
      
      rbind(
        regresults %>% 
          filter(Season == year) %>%
          left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
          left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
          rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
          mutate(gametime = ifelse(NumOT == 0, 2400, 2400 + NumOT * 300)) %>%
          filter(LTeamName == LTeam) %>%
          select(WTeamID = LTeamID, WScore = LScore,WTO = LTO,WStl = LStl,WBlk = LBlk,gametime,WTeamName = LTeamName),
        
        regresults %>% 
          filter(Season == year) %>%
          left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
          left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
          rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
          mutate(gametime = ifelse(NumOT == 0, 2400, 2400 + NumOT * 300)) %>%
          filter(WTeamName == LTeam) %>%
          select(WTeamID , WScore ,WTO,WStl,WBlk,gametime,WTeamName )
      )
    ) %>%
    group_by(WTeamID) %>%
    summarise(Turnover = sum(WTO)/sum(gametime),
              Block = sum(WBlk)/sum(gametime),
              Steal = sum(WStl)/sum(gametime)
    ) %>%
    inner_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    select(TeamName,Turnover,Block,Steal) %>%
    pivot_longer(-TeamName,names_to = "Event_Type", values_to = "rate") %>%
    mutate(prob_3 = 1- ppois(q = 3,lambda = rate*(tend - tstart)),
           prob_12 = 1- ppois(q = 12,lambda = rate*(tend - tstart)))
  
  events <- data.table(x = c(0:maxEvent)) 
  
  for(i in 1:nrow(lambdaCalc)){
    events[, paste(lambdaCalc$TeamName[i],lambdaCalc$Event_Type[i],sep = "_") := dpois(x=x, lambda = lambdaCalc$rate[i]*(tend - tstart))]
  }
  
  subtit <-  
    results %>%
    filter(Season == year) %>%
    select(WTeamID,LTeamID,WScore,LScore) %>%
    left_join(select(teams,TeamID,TeamName), by = c('WTeamID' = 'TeamID')) %>%
    left_join(select(teams,TeamID,TeamName), by = c('LTeamID' = 'TeamID')) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>% 
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    select(WTeamName,WScore,LTeamName,LScore) %>%
    mutate(game = paste(year,WTeamName,"=",WScore,LTeamName,"=",LScore,sep = " " )) %>%
    select(game)
  
  subtit <- subtit[1]$game
  
  title <- "NCAA March Madness Classic Games Event Probablity"
  
  vline.data <- data.table(Event_Type = c("Block","Steal","Turnover") , z =c(Block,Steal,Turnover))
  
  events %>%
    pivot_longer(-x, names_to = 'id', values_to = 'Probablity') %>%
    mutate(Event_Type = word(id, -1, sep = fixed('_')),
           Team_Name = word(id, 1, sep = fixed('_'))) %>%
    ggplot(aes(x=x,y=Probablity,color=Team_Name)) +
    geom_point(shape = 1, size = I(3)) +
    geom_line()+
    geom_vline(aes(xintercept = z), vline.data, colour = "red") +
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit[1])))))) +
    ylab(label="Probablity")+
    xlab(label="Event Count") +
    facet_grid(Event_Type ~.)
}  

poisPlotProb(WTeam = "Texas A&M" , LTeam = "Northern Iowa", year = 2016, tstart = 0, tend = 2398,Block = 0, Steal = 6, Turnover = 14) 
poisPlotProb(WTeam = "Texas A&M" , LTeam = "Northern Iowa", year = 2016, tstart = 1000, tend = 1500,Block = 0, Steal = 2, Points = 3, Turnover = 2) 

poisPlotProb(WTeam = "Texas A&M" , LTeam = "Northern Iowa", year = 2016, tstart = 2350, tend = 2400,Block = 0, Steal = 3, Turnover = 3,maxEvent = 10) 

#####################################
# Function to plot specfic game times and scorce
####################################
  
plotgametime <- function(WTeam,LTeam, year, tstart = 2000, tend = 2401 , minscore = 0, maxscore = 100){ 
  
  # Load data for the basic game information (Teams,seeds,year,conferences)
  # create basic game information grob
  # data load for the game plot
  # join the team names
  # track each teams score via the Event types for the shots made.
  
  datGame <-  
    seeds %>%
    filter(Season == year) %>%
    left_join(select(teams,TeamID,TeamName), by = c('TeamID')) %>%
    filter(TeamName == WTeam | TeamName == LTeam) %>%
    inner_join(conf,by = c('Season','TeamID')) %>%
    select(Teams = TeamName,Seed = Seed,Conference = ConfAbbrev) 
  
  g <- tableGrob(datGame,rows = NULL, theme = ttheme_default(7)) 
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 1, l = 1, r = ncol(g))
  
  plota <- as.grob(g)
  
  subtit <-  
    results %>%
    filter(Season == year) %>%
    select(WTeamID,LTeamID,WScore,LScore) %>%
    left_join(select(teams,TeamID,TeamName), by = c('WTeamID' = 'TeamID')) %>%
    left_join(select(teams,TeamID,TeamName), by = c('LTeamID' = 'TeamID')) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>% 
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    select(WTeamName,WScore,LTeamName,LScore) %>%
    mutate(game = paste(year,WTeamName,"=",WScore,LTeamName,"=",LScore,sep = " " )) %>%
    select(game)
  
  subtit <- subtit[1]$game
  
  dat <- fileLoad(year)  
  
  Labels <- data.table(x = c(600, 1800, 2700), y = c(100,100,100), label = c('1st Half', '2nd Half', 'Overtime'))
  
  title <- "NCAA March Madness Classic Games"
  lim <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    summarise(gameTime = max(ElapsedSeconds)) %>%
    pull(gameTime)
  
  plotb <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    filter(EventType %in% c('made1', 'made2','made3')) %>%
    select(WCurrentScore, LCurrentScore, ElapsedSeconds,EventTeamID,EventType,WTeamName,LTeamName) %>% 
    mutate(EventTeamID = as.character(EventTeamID)) %>%
    ggplot(aes(x = ElapsedSeconds)) +
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit[1]))))))+
    geom_point(shape = 1, aes(y = WCurrentScore, color= WTeam))+
    geom_point(shape = 1,aes(y = LCurrentScore, color= LTeam)) +
    geom_line(size = I(0), aes(y = WCurrentScore, color= WTeam))+
    geom_line(size = I(0),aes(y = LCurrentScore, color= LTeam)) +
    geom_vline(xintercept = c(1200,2400)) +
    geom_label(size = 3, data= Labels,aes(x=x,y=y, label = label)) +
    scale_x_continuous(limits = c(tstart,tend)) +
    scale_y_continuous(limits = c(minscore-5,maxscore+5)) +
    ylab(label="Score")+
    xlab(label="Elapsed Time (seconds)")+
    scale_colour_manual("Teams",
                        breaks=c(WTeam,LTeam),
                        values=c("blue","red"))
  
  plotc <- as.grob(plotb)
  
  grid.newpage()
  grid.draw(plotc)
  vp = viewport(x = 0.20,y=0.75,width = 0.2,height = 0.2)
  pushViewport(vp)
  grid.draw(plota)
  upViewport()
}

plotgametime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016, tstart = 2350) 
plotgametime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016, tstart = 780 , tend = 1450, minscore = 15, maxscore = 45 ) 


############################################################################
# Plot game events / scorce diff / vs. time / look at specific game times
############################################################################

plotgameEventsTime <- function(WTeam,LTeam, year,tstart = 2000, tend = 2401, mindiff = -20, maxdiff = 20){
  
  # Create grob of seed and conferences

  datGame <-  EventProbCalc(WTeam = WTeam,LTeam = LTeam,year = year, tstart = tstart, tend = tend)
  
  g <- tableGrob(datGame,rows = NULL, theme = ttheme_default(7)) 
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g,
                       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 1, l = 1, r = ncol(g))
  
  plota <- as.grob(g)
  
  subtit <-  
    results %>%
    filter(Season == year) %>%
    select(WTeamID,LTeamID,WScore,LScore) %>%
    left_join(select(teams,TeamID,TeamName), by = c('WTeamID' = 'TeamID')) %>%
    left_join(select(teams,TeamID,TeamName), by = c('LTeamID' = 'TeamID')) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>% 
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    select(WTeamName,WScore,LTeamName,LScore) %>%
    mutate(game = paste(year,WTeamName,"=",WScore,LTeamName,"=",LScore,sep = " " )) %>%
    select(game)
  
  subtit <- subtit[1]$game
  
  dat <- fileLoad(year)  
  
  datEvents <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    filter(EventType %in% setdiff(unique(dat$EventType),c('reb','sub', 'timeout','assist', 'miss1','miss2','miss3','made1','made2','made3'))) %>%
    select(ElapsedSeconds,EventTeamID,EventType) %>% 
    mutate(count = 1,
           row = row_number()) %>%
    group_by(EventType) %>%
    pivot_wider(names_from = EventType, values_from = count, values_fill = list(count = 0)) %>% 
    select(-row) %>%
    group_by(EventTeamID) %>%
    mutate(block = cumsum(block),
           turnover = cumsum(turnover),
           steal = cumsum(steal)) %>%
    pivot_longer(cols = c('block','turnover','steal'), names_to = "EventID", values_to = "count") %>%
    left_join(select(teams,TeamID,TeamName), by = c("EventTeamID" = "TeamID")) %>%
    ungroup() %>%
    mutate(EventTeamID = as.character(EventTeamID),
           count = ifelse(TeamName == LTeam, count*-1,count)) %>%
    data.table()
  
  Labels <- data.table(x = c(tstart,tend), y = c(0,0), label = c('Start Time', 'End Time'))
  LabelTeam <- data.table(x=c(tstart + 10,tstart + 10),y=c(15,-15), label = c(WTeam,LTeam))
  
  title <- "NCAA March Madness Classic Games Event Comparisions"
  
  lim <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    summarise(gameTime = max(ElapsedSeconds)) %>%
    pull(gameTime)
  
  plotb <-
    dat %>% 
    left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
    left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
    rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
    filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
    filter(EventType %in% c('made1', 'made2','made3')) %>%
    select(WCurrentScore, LCurrentScore, ElapsedSeconds,EventTeamID,EventType,WTeamName,LTeamName) %>% 
    mutate(EventTeamID = as.character(EventTeamID),
           WScoreDiff = WCurrentScore - LCurrentScore,
           LScoreDiff = LCurrentScore - WCurrentScore) %>%
    ggplot() +
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit[1]))))))+
    geom_line(size = I(2), color = "blue", aes(x = ElapsedSeconds,y = WScoreDiff))+
    geom_line(size = I(2),color = 'red',aes(x = ElapsedSeconds,y = LScoreDiff)) +
    geom_point(size = I(3),shape = 1, data = datEvents[TeamName == WTeam,], aes(x = ElapsedSeconds,y = count, color= EventID)) +
    geom_line(size = I(0), data = datEvents[TeamName == WTeam,], aes(x = ElapsedSeconds,y = count, color= EventID)) +
    geom_point(size = I(3),shape = 1, data = datEvents[TeamName == LTeam,], aes(x = ElapsedSeconds,y = count, color= EventID)) +
    geom_line(size = I(0),linetype = 2, data = datEvents[TeamName == LTeam,], aes(x = ElapsedSeconds,y = count, color= EventID)) +
    geom_vline(xintercept = c(tstart,tend)) +
    geom_label(size = 3, data= Labels,aes(x=x,y=y, label = label)) +
    geom_label(size = 3, data= LabelTeam,aes(x=x,y=y, label = label)) +
    scale_x_continuous(limits = c(tstart,tend)) +
    scale_y_continuous(limits = c(mindiff,maxdiff)) +
    ylab(label="Score Difference / Event Count")+
    xlab(label="Elapsed Time (seconds)")
  
  plotc <- as.grob(plotb)
  
  grid.newpage()
  grid.draw(plotc)
  vp = viewport(x = 0.32,y=0.83,width = 0.10,height = 0.2)
  pushViewport(vp)
  grid.draw(plota)
  upViewport()
  
}
plotgameEventsTime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016,tstart = 0, tend = 3000)

plotgameEventsTime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016,tstart = 1000, tend = 1500)

plotgameEventsTime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016,tstart = 780, tend = 1410, maxdiff = 25,mindiff = -20)
plotgameEventsTime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016,tstart = 2350, tend = 2400, maxdiff = 25,mindiff = -20)






plotgameEventsTime(WTeam = "UMBC" ,LTeam = "Virginia", 2018,tstart = 2000, tend = 2400, maxdiff = 25,mindiff = -20)

plotgameEventsTime(WTeam ="Michigan St",LTeam ="Duke",2019,tstart = 875, tend = 1300, maxdiff = 25,mindiff = -20)
plotgameEventsTime(WTeam ="Michigan St",LTeam ="Duke",2019,tstart = 0, tend = 2400, maxdiff = 25,mindiff = -20)

(1300 -875)/60

plotgameEventsTime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016,tstart = 2300)
plotgameEventsTime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016,tstart = 2355)
plotgameEventsTime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016,tstart = 2355)

plotgametime(WTeam = "Texas A&M" ,LTeam = "Northern Iowa", 2016, tstart = 2350) 
 
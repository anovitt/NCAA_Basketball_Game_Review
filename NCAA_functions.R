library(data.table)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggplotify)
library(RColorBrewer)
library(gtable) 
library(dplyr)

###########################################
# fileLoad = load the play by play data
###########################################

# enter year
# path play is the path to the data directory

fileLoad <- function(year){
  fread(paste0(path_play,"MEvents",year,".csv"))
}
EventProbCalc("Texas A&M","Northern Iowa",year = 2016, tstart = 780, tend = 1410)

EventProbCalc <- function(WTeam,LTeam,year,tstart,tend){
  
  dat <- fileLoad(year)   
#  Calculate season shooting percentages.
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
        T1_ftm=WFTM, 
        T1_fta=WFTA,
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
        T1_ftm=LFTM, 
        T1_fta=LFTA,
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
        T1_fgm3=LFGM3,
        T1_fga3=LFGA3,
        T1_ftm=LFTM, 
        T1_fta=LFTA,
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
        T1_fta=WFTA,
      )) %>%
  group_by(T1_Name) %>%
  summarise(fgp=round(sum(T1_fgm)/sum(T1_fga),3),
            fg3p=round(sum(T1_fgm3)/sum(T1_fga3),3),    
            ftp=round(sum(T1_ftm)/sum(T1_fta),3))

#  Get the game event counts
# subset to made and misss, and the start time and end time.
# add a columns for shop type and made and missed

dat %>%
  left_join(select(teams,TeamID,TeamName), by = c("WTeamID" = "TeamID")) %>%
  left_join(select(teams,TeamID,TeamName), by = c("LTeamID" = "TeamID")) %>%
  rename("WTeamName" = "TeamName.x","LTeamName" = "TeamName.y" ) %>%
  filter(WTeamName == WTeam & LTeamName == LTeam ) %>%
  filter(EventType %in% setdiff(unique(dat$EventType),c('reb','sub', 'timeout','assist','foul','turnover','steal','block'))) %>%
  select(ElapsedSeconds,EventTeamID,EventType) %>% 
  filter(ElapsedSeconds >= tstart & ElapsedSeconds <= tend) %>%
  mutate(result = substr(EventType,1,4),
         shotType = substr(EventType,5,5) ,
         Points = ifelse(result == 'made',as.integer(shotType),0),
         Made = ifelse(result == 'made',1,0)) %>%
  group_by(EventTeamID) %>%
  summarise(Attempts = n(),
            Made = sum(Made),
            Points = sum(Points))%>%
  left_join(select(teams,TeamID,TeamName), by = c("EventTeamID" = "TeamID")) %>%
  inner_join(statSum, by = c("TeamName" = "T1_Name")) %>%
  mutate(Prob = round(pbinom(q=Made,size = Attempts,prob = fgp),5)) %>%
  select(TeamName, Attempts,Made,Points,fgp,fg3p,ftp,Prob) %>%
  as.data.table()

}

EventProbCalc("Texas A&M","Northern Iowa",year = 2016, tstart = 780, tend = 1410)
  
(1410-780)/60

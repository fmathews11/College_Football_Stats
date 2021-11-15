library(tidyverse)
library(cfbfastR)
library(anytime)

#Leaving off the last 2 digits of my collegefootballstats API key as this will live in github

Sys.setenv(CFBD_API_KEY = "ZmYrC0A2qVl/S+8M0W0ygbpyMtpTvGYn2lDZexO8WPP5ok0IKvxkYTJItmgNgBhh")

#Reading in games from the original NCAAF_L1 pipeline stored in github

data <- read.csv('https://raw.githubusercontent.com/fmathews11/College_Football_Stats/main/Allplayedgames.csv')

teamlist <- read.csv('https://raw.githubusercontent.com/fmathews11/College_Football_Stats/main/teamlist.csv')

teamlist <- teamlist%>%
  select(-X)


data<-data[3:19]

data<- data%>%select(-OT)

data <- data%>%select(-X)

data$Date <- as.Date(data$Date)

#Only when the regular as.Date() doesn't work
data<-data%>%
  mutate(Date = anydate(Date))


#Mandatory brute-force cleaning:

data[data$Team == "Ucf", "Team"] <- "UCF"
data[data$Opponent == "Ucf", "Opponent"] <- "UCF"

data[data$Team == "UTSA", "Team"] <- "UT San Antonio"
data[data$Opponent == "UTSA", "Opponent"] <- "UT San Antonio"

data[data$Team == "Smu", "Team"] <- "SMU"
data[data$Opponent == "Smu", "Opponent"] <- "SMU"

data[data$Team == "Unlv", "Team"] <- "UNLV"
data[data$Opponent == "Unlv", "Opponent"] <- "UNLV"

data[data$Team == "Louisiana-Monroe", "Team"] <- "UL Monroe"
data[data$Opponent == "Louisiana-Monroe", "Opponent"] <- "UL Monroe"

data[data$Team == "Sam Houston", "Team"] <- "Sam Houston State"
data[data$Opponent == "Sam Houston", "Opponent"] <- "Sam Houston State"

data[data$Team == "Southern Miss", "Team"] <- "Southern Mississippi"
data[data$Opponent == "Southern Miss", "Opponent"] <- "Southern Mississippi"

data[data$Team == "Tarleton", "Team"] <- "Tarleton State"
data[data$Opponent == "Tarleton", "Opponent"] <- "Tarleton State"

data[data$Team == "CSU Northridge", "Team"] <- "CSU-Northridge"
data[data$Opponent == "CSU Northridge", "Opponent"] <- "CS-Northridge"

data[data$Team == "Nicholls State", "Team"] <- "Nicholls"
data[data$Opponent == "Nicholls State", "Opponent"] <- "Nicholls"

data[data$Team == "Presbyterian College", "Team"] <- "Presbyterian"
data[data$Opponent == "Presbyterian College", "Opponent"] <- "Presbyterian"

data[data$Team == "North Carolina Central", "Team"] <- "NC Central"
data[data$Opponent == "North Carolina Central", "Opponent"] <- "NC Central"

data[data$Team == "San José State", "Team"] <- "San Jose St"
data[data$Opponent == "San José State", "Opponent"] <- "San Jose St"


# Getting this past week's games using collegefootballdata's API:

recently_played_games <- cfbfastR::cfbd_game_media(year = 2021,week = 11)%>%
  select(game_id,start_time,home_team,away_team)

#Getting basic stats from each game
needed_stats <- data.frame()

for (gm in unique(recently_played_games$game_id)){
  tempdf <- cfbfastR::cfbd_game_info(year = 2021,game_id = gm)
  tempdf2 <- tempdf%>%
    select(start_date,game_id,neutral_site,home_team,away_team,home_points,away_points)
  needed_stats <- rbind(needed_stats,tempdf2)
}

#Making a new df of just home teams
recently_played_games_home <- needed_stats%>%
  mutate(Date = as.Date(start_date))%>%
  mutate(Played = TRUE)%>%
  mutate(Season = 2021)%>%
  mutate(Team = home_team)%>%
  mutate(Opponent = away_team)%>%
  mutate(Team_FBS = ifelse(
    Team %in% teamlist$Team[teamlist$FBS == 1],1,0
  ))%>%
  mutate(Opp_FBS = ifelse(
    Opponent %in% teamlist$Team[teamlist$FBS == 1],1,0
  ))%>%
  mutate(Points_For = home_points)%>%
  mutate(Points_Against = away_points)%>%
  mutate(Neutral_Site = neutral_site)%>%
  mutate(Spread = Points_For-Points_Against)%>%
  mutate(Home = 1)%>%
  mutate(Result = ifelse(Points_For > Points_Against,1,0))%>%
  select(Date,Season,Team,Opponent,Result,
         Team_FBS,Opp_FBS,Points_For,Points_Against,Spread,Neutral_Site,Home,game_id)

recently_played_games_away <- needed_stats%>%
  mutate(Date = as.Date(start_date))%>%
  mutate(Played = TRUE)%>%
  mutate(Season = 2021)%>%
  mutate(Team = away_team)%>%
  mutate(Opponent = home_team)%>%
  mutate(Team_FBS = ifelse(
    Team %in% teamlist$Team[teamlist$FBS == 1],1,0
  ))%>%
  mutate(Opp_FBS = ifelse(
    Opponent %in% teamlist$Team[teamlist$FBS == 1],1,0
  ))%>%
  mutate(Points_For = away_points)%>%
  mutate(Points_Against = home_points)%>%
  mutate(Spread = Points_For-Points_Against)%>%
  mutate(Neutral_Site = neutral_site)%>%
  mutate(Home = 0)%>%
  mutate(Result = ifelse(Points_For > Points_Against,1,0))%>%
  select(Date,Season,Team,Opponent,Result,
         Team_FBS,Opp_FBS,Points_For,Points_Against,Spread,Neutral_Site,Home,game_id)

  all_recently_played_games <- rbind(recently_played_games_home,recently_played_games_away)%>%
  arrange(game_id)%>%
  mutate(ELO = 0)%>%
  mutate(Opp_ELO = 0)


data<- data %>% filter(Game_ID != "Postponed", Game_ID != "Canceled", Game_ID != "Live")

NCAAF_L1 <- data


NCAAF_L1_Future <- NCAAF_L1 %>% 
  filter(Played == FALSE, Game_ID != "Canceled", Game_ID != "Postponed")

NCAAF_L1 <- NCAAF_L1 %>% 
  filter(Played == TRUE) %>% 
  arrange(Date, Game_ID)

NCAAF_L1 <- NCAAF_L1 %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result == "T", 0.5, Result),
  Result = as.numeric(Result)
)

teamlist<- teamlist %>% mutate(
  ELO = ifelse(FBS == 1, 1500, 1200),)

#Making sure there aren't any weird FCS or DII schols that have popped up since last time
for (tm in unique(NCAAF_L1$Team)){
  if (!tm %in% unique(teamlist$Team)){
    print(paste0("Adding ",tm," to the teamlist dataframe"))
    tempdf <- data.frame(Team = tm,FBS = 0,ELO = 1200)
    teamlist <- rbind(teamlist,tempdf)
  }
}

#Write the updated teamlist to a .csv for to have for future use

teamlist<- teamlist %>% mutate(
  ELO = ifelse(FBS == 1, 1500, 1200),)

#Loop to write the ELO

for(i in 1:nrow(NCAAF_L1)){
  if(i %% 2 != 0){
    # i = 1
    #print(i)
    
    # View(head(NCAAF_L1))
    
    Team_A <- NCAAF_L1$Team[i]
    Team_B <- NCAAF_L1$Team[i+1]
    
    Result_A <- NCAAF_L1$Result[i]
    Result_B <- NCAAF_L1$Result[i+1]
    
    ## Get Current ELO ##
    
    ELO_A <- as.numeric(teamlist[teamlist$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(teamlist[teamlist$Team == Team_B, "ELO"])
    #Find which schools are causing issues...
    if(length(ELO_A) == 0 | length(ELO_B) == 0){
      print(paste0(Team_A,": ",ELO_A))
      print(paste0(Team_B,": ",ELO_B))
      break
    }
    
    ## Load current ELO into the main NCAAF_L1set ##
    
    NCAAF_L1$ELO[i] <- ELO_A
    NCAAF_L1$Opp_ELO[i] <- ELO_B
    
    NCAAF_L1$ELO[i+1] <- ELO_B
    NCAAF_L1$Opp_ELO[i+1] <- ELO_A
    
    ## Update ELOs
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 40 * (Result_A - E_A)
    Elo_Updated_B <- ELO_B + 40 * (Result_B - E_B)
    
    ## Update Team ELOs
    
    teamlist[teamlist$Team == Team_A, "ELO"] <- Elo_Updated_A
    teamlist[teamlist$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}

write.csv(teamlist,"teamlist.csv")

# Writing the file to a .csv with the appended ELO

write.csv(NCAAF_L1,"C:/Users/Frank/OneDrive/R Scripts/College Football/Allplayedgames.csv")




#Update recent games with new ELO

#Add in new team(s)

for (tm in all_recently_played_games$Team%>%unique()){
  if(!tm %in% teamlist$Team){
    print(tm)
  }
}

#Adding in what is necessary and updating the ELO appropriately:
all_recently_played_games[all_recently_played_games$Team == "San José State", "Team"] <- "San Jose St"
all_recently_played_games[all_recently_played_games$Opponent == "San José State", "Opponent"] <- "San Jose St"

all_recently_played_games$ELO <-0
all_recently_played_games$Opp_ELO <-0

for(i in 1:nrow(all_recently_played_games)){
  if(i %% 2 != 0){
    # i = 1
    print(i)
    
    # View(head(NCAAF_L1))
    
    Team_A <- all_recently_played_games$Team[i]
    Team_B <- all_recently_played_games$Team[i+1]
    
    Result_A <- all_recently_played_games$Result[i]
    Result_B <- all_recently_played_games$Result[i+1]
    
    ## Get Current ELO ##
    
    ELO_A <- as.numeric(teamlist[teamlist$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(teamlist[teamlist$Team == Team_B, "ELO"])
    #Find which schools are causing issues...
    if(length(ELO_A) == 0 | length(ELO_B) == 0){
      print(paste0(Team_A,": ",ELO_A))
      print(paste0(Team_B,": ",ELO_B))
      break
    }
    
    ## Load current ELO into the main NCAAF_L1set ##
    
    all_recently_played_games$ELO[i] <- ELO_A
    all_recently_played_games$Opp_ELO[i] <- ELO_B
    
    all_recently_played_games$ELO[i+1] <- ELO_B
    all_recently_played_games$Opp_ELO[i+1] <- ELO_A
    
    ## Update ELOs
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 40 * (Result_A - E_A)
    Elo_Updated_B <- ELO_B + 40 * (Result_B - E_B)
    
    ## Update Team ELOs
    
    teamlist[teamlist$Team == Team_A, "ELO"] <- Elo_Updated_A
    teamlist[teamlist$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}

write.csv(teamlist,"teamlist.csv")

all_recently_played_games$Played <- TRUE

names(data)

all_recently_played_games <- all_recently_played_games%>%
  rename("Neutral_Location" = "Neutral_Site")%>%
  rename("Game_ID" = "game_id")%>%
  select(Date,Season,Team,Opponent,Result,Points_For,Points_Against,Spread,Played,Home,Neutral_Location,
         Team_FBS,Opp_FBS,Game_ID,ELO,Opp_ELO)


    
data<- rbind(NCAAF_L1,all_recently_played_games)

data <- data%>%
  arrange(Date,Game_ID)

write.csv(data,"C:/Users/Frank/OneDrive/R Scripts/College Football/Allplayedgames.csv")


#### Getting next week's games

nwgames <- cfbfastR::cfbd_game_media(year = 2021,week = 12)%>%
  select(game_id,start_time,home_team,away_team)

#Getting basic stats from each game
needed_stats <- data.frame()

for (gm in unique(nwgames$game_id)){
  tempdf <- cfbfastR::cfbd_game_info(year = 2021,game_id = gm)
  tempdf2 <- tempdf%>%
    select(start_date,game_id,neutral_site,home_team,away_team)
  needed_stats <- rbind(needed_stats,tempdf2)
}

print("DONE")

#Making a new df of just home teams
nwgames_home <- needed_stats%>%
  mutate(Date = as.Date(start_date))%>%
  mutate(Played = TRUE)%>%
  mutate(Season = 2021)%>%
  mutate(Team = home_team)%>%
  mutate(Opponent = away_team)%>%
  mutate(Neutral_Location = neutral_site)%>%
  mutate(Team_FBS = ifelse(
    Team %in% teamlist$Team[teamlist$FBS == 1],1,0
  ))%>%
  mutate(Opp_FBS = ifelse(
    Opponent %in% teamlist$Team[teamlist$FBS == 1],1,0
  ))%>%
  mutate(Home = 1)%>%
  select(Date,Season,Team,Opponent,Team_FBS,Opp_FBS,Neutral_Location,Home,game_id)

nwgames_away <- needed_stats%>%
  mutate(Date = as.Date(start_date))%>%
  mutate(Played = TRUE)%>%
  mutate(Season = 2021)%>%
  mutate(Team = away_team)%>%
  mutate(Opponent = home_team)%>%
  mutate(Team_FBS = ifelse(
    Team %in% teamlist$Team[teamlist$FBS == 1],1,0
  ))%>%
  mutate(Opp_FBS = ifelse(
    Opponent %in% teamlist$Team[teamlist$FBS == 1],1,0
  ))%>%
  mutate(Neutral_Location = neutral_site)%>%
  mutate(Home = 0)%>%
  select(Date,Season,Team,Opponent,Team_FBS,Opp_FBS,Neutral_Location,Home,game_id)

all_nwgames <- rbind(nwgames_home,nwgames_away)%>%
  arrange(game_id)

#Getting each team's most recent ELO:

last_elo <- data%>%
  group_by(Team)%>%
  summarise(ELO = last(ELO))

all_nwgames$ELO <- teamlist$ELO[match(all_nwgames$Team,teamlist$Team)]
all_nwgames$Opp_ELO <- teamlist$ELO[match(all_nwgames$Opponent,teamlist$Team)]

teamlist%>% arrange(-ELO)%>%
  View()


write.csv(all_nwgames,'next_weeks_games.csv')



library(tidyverse)
library(tidymodels)
library(readr)
library(tidyr)
library(lubridate)
library(rvest)

#NCAAF_L1 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NCAAF/NCAAF_Level_One.csv")
NCAAF_L1_Teams <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NCAAF/NCAAF_Team_List.csv")


####### DATA PIPELINE #####
####### THANKS TO MATTC317####


library(tidyr)
library(dplyr)
library(stringr)
library(rvest)
library(lubridate)

ncaaf <- read_html("https://www.espn.com/college-football/teams")


ids <- ncaaf %>% str_extract_all('(?<=college-football/team/_/id/)(.*?)(?=")') %>% unlist()
ids <- ids[str_detect(ids, "/")]
ids <- ids %>% str_split("/", simplify = T)
ids <- as.data.frame(ids)
names(ids) <- c('ID', 'Name')

ids <- ids %>% 
  filter(Name != "", ID != "") %>% 
  mutate(UID = paste0(Name, ID)) %>% 
  arrange(UID) %>% 
  mutate(DUP = ifelse(UID == lag(UID), 1, 0))

ids[1, "DUP"] <- 0

ids <- ids %>% filter(DUP == 0)


nrow(ids)
length(unique(ids$ID))
length(unique(ids$Name))


id_vector <- unique(ids$ID) %>% as.character()
str_vector <- unique(ids$Name) %>% as.character()

ids <- data.frame(ID = ids$ID, Name = ids$Name)

ids$FBS <- 1
ids$ID <- as.character(ids$ID)
ids$Name <- as.character(ids$Name)

teams <- ncaaf %>% str_extract_all('(?<=" title=")(.*?)(?=" data-mptype=")') %>% unlist()
seasons <- c(2021:2000)

Schedule <- as.data.frame(matrix(NA, nrow = 0, ncol = 14))
names(Schedule) <- c('Date', 'Opponent', 'Result', 'Team', 'Team_ID', 'Season', 'Home', 'Points_For', 'Points_Against', 'Played', 'OT', 'Opp_ID', 'Opp_Name_ID', 'Game_ID')

ids_temp <- ids

# Grab some coffee, mow the yard, etc.  This takes a little while
while(nrow(ids_temp) > 0){
  # i = 1
  id <- ids_temp[1, ]
  # id <- ids[ids$ID == "333", ]
  print(id)
  
  for(j in 1:length(seasons)){
    # j = 1
    season <- seasons[j]
    print(season)
    
    # Add Try
    team_games <- try({read_html(paste0("https://www.espn.com/college-football/team/schedule/_/id/", id[1, 1], "/season/", season))})
    
    if(class(team_games)[1] != "try-error"){
      schedule_ids <- team_games %>% str_extract_all('(?<=team/_/id/)(.*?)(?="><img alt=")') %>% unlist()
      
      schedule_ids <- as.data.frame(schedule_ids) %>% separate(schedule_ids, c("ID", "Name"), sep = "/") %>% filter(ID != id[1, 1])
      
      schedule_ids_ID <- schedule_ids$ID
      schedule_ids_Name <- schedule_ids$Name
      
      ## Handle Teams without an ID on ESPN
      
      #schedule_ids_blank_teams <- team_games %>% str_extract_all('(?<=height:20px)(.*?)(?="><img alt=")') %>% unlist()
      schedule_ids_blank_teams <- team_games %>% str_extract_all('(?<=height:20px)(.*?)(?=<span)') %>% unlist()
      schedule_ids_blank_teams <- which(!str_detect(schedule_ids_blank_teams, "/id/"))
      
      if(length(schedule_ids_blank_teams) > 0){
        for(i in 1:length(schedule_ids_blank_teams)){
          schedule_ids_ID <- append(schedule_ids_ID, NA, schedule_ids_blank_teams[i]-1)
          schedule_ids_Name <- append(schedule_ids_Name, NA, schedule_ids_blank_teams[i]-1)
        }
        
        nrow(team_games)
        
        schedule_ids <- data.frame(ID = schedule_ids_ID, Name = schedule_ids_Name)
        schedule_ids$FBS <- 0 
      }
      
      if(nrow(schedule_ids) == 0){
        schedule_ids <- as.data.frame(matrix(NA, ncol = 3, nrow = 0))
        names(schedule_ids) <- c("ID", "Name", "FBS")
      }else(
        schedule_ids$FBS <- 0
      )
      
      ##
      
      game_ids <- team_games %>% str_extract_all('(?<=gameId/)(.*?)(?=<!-- -->)') %>% unlist() %>% str_split('"', simplify = T)
      
      #### If no games in the current season for the current team go to next sesason ####
      if(length(game_ids) > 0 & !is.null(game_ids)){
        game_ids <- game_ids[ ,1]
        
        additional_ids <- as.data.frame(matrix(NA, nrow = 0, ncol = 3))
        names(additional_ids) <- c("ID", "Name", "FBS")
        
        for(i in 1:nrow(schedule_ids)){
          # i = 1
          if(!any(schedule_ids[i, "ID"] %in% ids$ID)){
            ids <- rbind(ids, schedule_ids[i, ])
            ids_temp <- rbind(ids_temp, schedule_ids[i, ])
          }
        }
        
        team_games <- team_games %>% html_nodes('table') %>% html_table(fill = T)
        
        team_games <- team_games[[1]]
        
        team_games <- team_games[, c(1:3)]
        
        names(team_games) <- c("Date", "Opponent", "Result")
        
        team_games <- team_games[!(team_games$Date %in% c("Regular Season", "DATE")), ]
        
        team_games <- team_games[str_detect(team_games$Date, ", "), ]
        
        team_games <- team_games %>% 
          mutate(
            Team = id[1, 2],
            Team_ID = id[1, 1],
            Season = season,
            Home = ifelse(str_detect(Opponent, "vs"), TRUE, FALSE),
            Result_Delete = Result,
            Result = case_when(
              str_detect(Result, "W") & str_detect(Result, "-") ~ "W",
              str_detect(Result, "L") & str_detect(Result, "-") ~ "L",
              str_detect(Result, "T") & str_detect(Result, "-") ~ "T",
              TRUE ~ "TBD"
            ),
            Played = ifelse(Result %in% c("W", "L", "T"), "TRUE", "FALSE"),
            Neutral_Location = ifelse(str_detect(Opponent, "\\*"), TRUE, FALSE),
            Opponent = str_replace(Opponent, " \\*", ""),
            Opponent = str_replace(Opponent, "\\*", ""),
            Opponent = str_replace(Opponent, "vs", ""),
            Opponent = str_replace(Opponent, "@", ""),
            Opponent = str_replace(Opponent, "1 ", ""),
            Opponent = str_replace(Opponent, "2 ", ""),
            Opponent = str_replace(Opponent, "3 ", ""),
            Opponent = str_replace(Opponent, "4 ", ""),
            Opponent = str_replace(Opponent, "5 ", ""),
            Opponent = str_replace(Opponent, "6 ", ""),
            Opponent = str_replace(Opponent, "7 ", ""),
            Opponent = str_replace(Opponent, "8 ", ""),
            Opponent = str_replace(Opponent, "9 ", ""),
            Opponent = str_replace(Opponent, "10 ", ""),
            Opponent = str_replace(Opponent, "11 ", ""),
            Opponent = str_replace(Opponent, "12 ", ""),
            Opponent = str_replace(Opponent, "13 ", ""),
            Opponent = str_replace(Opponent, "14 ", ""),
            Opponent = str_replace(Opponent, "15 ", ""),
            Opponent = str_replace(Opponent, "16 ", ""),
            Opponent = str_replace(Opponent, "17 ", ""),
            Opponent = str_replace(Opponent, "18 ", ""),
            Opponent = str_replace(Opponent, "19 ", ""),
            Opponent = str_replace(Opponent, "20 ", ""),
            Opponent = str_replace(Opponent, "21 ", ""),
            Opponent = str_replace(Opponent, "22 ", ""),
            Opponent = str_replace(Opponent, "23 ", ""),
            Opponent = str_replace(Opponent, "24 ", ""),
            Opponent = str_replace(Opponent, "25 ", ""),
            Opponent = str_replace(Opponent, "1", ""),
            Opponent = str_replace(Opponent, "2", ""),
            Opponent = str_replace(Opponent, "3", ""),
            Opponent = str_replace(Opponent, "4", ""),
            Opponent = str_replace(Opponent, "5", ""),
            Opponent = str_replace(Opponent, "6", ""),
            Opponent = str_replace(Opponent, "7", ""),
            Opponent = str_replace(Opponent, "8", ""),
            Opponent = str_replace(Opponent, "9", ""),
            Opponent = str_replace(Opponent, "10", ""),
            Opponent = str_replace(Opponent, "11", ""),
            Opponent = str_replace(Opponent, "12", ""),
            Opponent = str_replace(Opponent, "13", ""),
            Opponent = str_replace(Opponent, "14", ""),
            Opponent = str_replace(Opponent, "15", ""),
            Opponent = str_replace(Opponent, "16", ""),
            Opponent = str_replace(Opponent, "17", ""),
            Opponent = str_replace(Opponent, "18", ""),
            Opponent = str_replace(Opponent, "19", ""),
            Opponent = str_replace(Opponent, "20", ""),
            Opponent = str_replace(Opponent, "21", ""),
            Opponent = str_replace(Opponent, "22", ""),
            Opponent = str_replace(Opponent, "23", ""),
            Opponent = str_replace(Opponent, "24", ""),
            Opponent = str_replace(Opponent, "25", ""),
            Result_Delete = ifelse(Played, str_replace(Result_Delete, Result, ""), Result_Delete),
            OT = ifelse(str_detect(Result_Delete, " OT"), TRUE, FALSE),
            Result_Delete = str_replace(Result_Delete, " OT", ""),
            Result_Delete = str_replace(Result_Delete, " 2OT", ""),
            Result_Delete = str_replace(Result_Delete, " 3OT", ""),
            Result_Delete = str_replace(Result_Delete, " 4OT", ""),
            Result_Delete = str_replace(Result_Delete, " 5OT", ""),
            Result_Delete = str_replace(Result_Delete, " 6OT", ""),
            Result_Delete = str_replace(Result_Delete, " 7OT", ""),
            Result_Delete = str_replace(Result_Delete, " 8OT", ""),
            Result_Delete = str_replace(Result_Delete, " 9OT", ""),
            Result_Delete = str_replace(Result_Delete, " 10OT", ""),
            Result_Delete = str_replace(Result_Delete, " 11OT", ""),
            Result_Delete = str_replace(Result_Delete, " 12OT", ""),
            Result_Delete = case_when(
              Result_Delete == "Postponed" ~ "Postponed",
              Result_Delete == "Canceled" ~ "Canceled",
              Result_Delete == "LIVE" ~ "LIVE",
              !str_detect(Result_Delete, "-") ~ "TBD",
              TRUE ~ Result_Delete
            ),
            Result_Delete = str_replace(Result_Delete, "Postponed", "Postponed-Postponed"),
            Result_Delete = str_replace(Result_Delete, "Canceled", "Canceled-Canceled"),
            Result_Delete = str_replace(Result_Delete, "LIVE", "LIVE-LIVE"),
            Result_Delete = str_replace(Result_Delete, "TBD", "TBD-TBD"),
            Date = str_replace(Date, "Sat, ", ""),
            Date = str_replace(Date, "Sun, ", ""),
            Date = str_replace(Date, "Mon, ", ""),
            Date = str_replace(Date, "Tue, ", ""),
            Date = str_replace(Date, "Wed, ", ""),
            Date = str_replace(Date, "Thu, ", ""),
            Date = str_replace(Date, "Fri, ", ""),
            Date = paste0(Date, ", ", season),
            Date = as.Date(Date, format = "%b %d, %Y"),
            Date = ifelse(month(Date) == 1 | month(Date) == 2 | month(Date) == 3, as.Date(Date + years(1)), as.Date(Date))
            
            # Opp_ID = ifelse(length(ids[str_detect(ids$Name, str_replace_all(str_to_lower(Opponent), " ", "-")), 1]) == 0, NA, ids[str_detect(ids$Name, str_replace_all(str_to_lower(Opponent), " ", "-")), 1])
          )
        
        team_games <- team_games %>% separate(Result_Delete, sep = "-", into = c("Points_For", "Points_Against"))
        
        team_games <- team_games %>% cbind(schedule_ids) %>% rename(Opp_ID = ID, Opp_Name_ID = Name)
        
        postponed_ids <- which(team_games$Points_For == "Postponed")
        canceled_ids <- which(team_games$Points_For == "Canceled")
        live_ids <- which(team_games$Points_For == "LIVE")
        
        insert_ids <- sort(c(postponed_ids, canceled_ids, live_ids))
        
        if(length(insert_ids)){
          for(i in 1:length(insert_ids)){
            game_ids <- append(game_ids, values = "Postponed/Canceled/Live", after = insert_ids[i]-1)
          }
        }
        
        if(length(postponed_ids)){
          for(i in 1:length(postponed_ids)){
            game_ids[postponed_ids[i]] <- "Postponed"
          }
        }
        
        if(length(canceled_ids)){
          for(i in 1:length(canceled_ids)){
            game_ids[canceled_ids[i]] <- "Canceled"
          }
        }
        
        if(length(live_ids)){
          for(i in 1:length(live_ids)){
            game_ids[live_ids[i]] <- "Live"
          }
        }
        
        team_games$Game_ID <- game_ids
        
        Schedule <- rbind(Schedule, team_games)
      }
    }
  }
  
  ids_temp <- ids_temp[-1, ]
  ids_temp <- ids_temp %>% filter(!is.na(ID))
  ids <- ids %>% filter(!is.na(ID))
  
}

ids <- ids %>% filter(!is.na(ID))

###

Schedule_raw <- Schedule

###

Schedule <- Schedule_raw

# Schedule$Date <- as.Date(Schedule$Date, origin = "1970-01-01")

Name_Mapping <- Schedule %>% select(Name = Opponent, Team_ID = Opp_ID, Name_ID = Opp_Name_ID) %>% arrange(Name)
Name_Mapping <- Name_Mapping %>% mutate(
  dup = ifelse(Name == lag(Name, 1), TRUE, FALSE)
)

Name_Mapping$dup[1] <- FALSE

Name_Mapping <- Name_Mapping %>% filter(dup == FALSE) %>% select(-dup)

Name_Mapping[Name_Mapping$Name == "Ucf", "Name"] <- "UCF"

Name_Mapping[Name_Mapping$Name == "UTSA", "Name"] <- "UT San Antonio"

Name_Mapping[Name_Mapping$Name == "Smu", "Name"] <- "SMU"

Name_Mapping[Name_Mapping$Name == "Unlv", "Name"] <- "UNLV"

Name_Mapping[Name_Mapping$Name == "Louisiana-Monroe", "Name"] <- "UL Monroe"

Name_Mapping[Name_Mapping$Team_ID == "23" & !is.na(Name_Mapping$Team_ID), "Name"] <- "San Jose St"

Name_Mapping_NA <- Name_Mapping %>% filter(is.na(Team_ID))
Name_Mapping <- Name_Mapping %>% filter(!is.na(Team_ID))

Name_Mapping <- Name_Mapping %>% arrange(Team_ID) %>%  mutate(
  dup = ifelse(Team_ID == lag(Team_ID, 1), TRUE, FALSE)
)

Name_Mapping$dup[1] <- FALSE

Name_Mapping <- Name_Mapping %>% filter(dup == FALSE) %>% select(-dup)

Name_Mapping <- rbind(Name_Mapping, Name_Mapping_NA)

Name_Mapping <- Name_Mapping %>% left_join(ids %>% select(ID, FBS), by = c("Team_ID" = "ID"))

Name_Mapping <- Name_Mapping %>% mutate(
  FBS = ifelse(is.na(FBS), 0, FBS)
)

# Update Team Name
Schedule <- Schedule %>% left_join(Name_Mapping %>% filter(!is.na(Team_ID)) %>% select(Name, Team_FBS = FBS, Team_ID), by = c("Team_ID" = "Team_ID"))

Schedule$Team <- Schedule$Name

Schedule$Name <- NULL

Schedule <- Schedule %>% left_join(Name_Mapping %>% filter(!is.na(Team_ID)) %>% select(Name, Name_ID), by = c("Team" = "Name"))

# Update Opp Team Name
Schedule <- Schedule %>% left_join(Name_Mapping %>% filter(!is.na(Team_ID)) %>% select(Name, Opp_FBS = FBS, Team_ID), by = c("Opp_ID" = "Team_ID"))

Schedule$Opponent <- Schedule$Name

Schedule$Name <- NULL

# Correct FBS for teams with no ID
Schedule <- Schedule %>% mutate(
  Opp_FBS = ifelse(is.na(Opp_ID), 0, Opp_FBS),
  Team_FBS = ifelse(is.na(Team_ID), 0, Team_FBS)
) 

Schedule$FBS <- NULL

# Check for team names
length(unique(Schedule$Team))
length(unique(Name_Mapping$Team_ID))
length(unique(Schedule$Opponent))
length(unique(Name_Mapping$Name))

Schedule <- Schedule %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Played, Home, Neutral_Location, OT, Game_ID, Team_FBS, Opp_FBS, Team_ID, Name_ID, Opp_ID, Opp_Name_ID)

#### Duplicate games with NA in Opp_ID ####

Schedule_Singles <- Schedule %>% group_by(Game_ID) %>% tally() %>% filter(n == 1) %>% select(Game_ID)
Schedule_Singles <- Schedule_Singles$Game_ID

Schedule_NA <- Schedule %>% filter(Game_ID %in% Schedule_Singles)
names(Schedule_NA) <- c('Date', 'Season', 'Opponent', 'Team', 'Result', 'Points_For', 'Points_Against', 'Played', 'Home', 'Neutral_Location', 'OT', 'Game_ID', 'Opp_FBS', 'Team_FBS', 'Opp_ID', 'Opp_Name_ID', 'Team_ID', 'Name_ID')

Schedule_NA <- Schedule_NA %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Played, Home, Neutral_Location, OT, Game_ID, Team_FBS, Opp_FBS, Team_ID, Name_ID, Opp_ID, Opp_Name_ID)

Schedule_NA <- Schedule_NA %>% mutate(
  Result = case_when(
    Result == "L" ~ "W",
    Result == "W" ~ "L",
    TRUE ~ Result
  ),
  Home = ifelse(TRUE, FALSE, TRUE)
)

Schedule <- rbind(Schedule, Schedule_NA)

Schedule <- Schedule %>% mutate(
  Date = as.Date(Date, origin = "1970-01-01"),
  Points_For = ifelse(Played, as.numeric(Points_For), 0),
  Points_Against = ifelse(Played, as.numeric(Points_Against), 0),
  points_for_temp = ifelse(Result == "W", Points_For, Points_Against),
  points_against_temp = ifelse(Result == "L", Points_For, Points_Against),
  Points_For = points_for_temp,
  Points_Against = points_against_temp,
  Spread = Points_For - Points_Against,
  Home = ifelse(Home, 1, 0),
  # Home = ifelse(Neutral_Location == TRUE, 0.5, Home)
)

Schedule <- Schedule %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Spread, Played, Home, Neutral_Location, OT, Team_FBS, Opp_FBS, Game_ID, Team_ID, Name_ID, Opp_ID, Opp_Name_ID) %>% 
  arrange(desc(Played), desc(Date), Game_ID) %>% filter(complete.cases(.))

NCAAF_Level_One <- Schedule %>% select(Date, Season, Team, Opponent, Result, Points_For, Points_Against, Spread, Played, Home, Neutral_Location, OT, Team_FBS, Opp_FBS, Game_ID) 

# Rename again so that the ELO loop doesn't crumble in on itself...

NCAAF_Level_One[NCAAF_Level_One$Team == "Ucf", "Team"] <- "UCF"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Ucf", "Opponent"] <- "UCF"

NCAAF_Level_One[NCAAF_Level_One$Team == "UTSA", "Team"] <- "UT San Antonio"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "UTSA", "Opponent"] <- "UT San Antonio"

NCAAF_Level_One[NCAAF_Level_One$Team == "Smu", "Team"] <- "SMU"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Smu", "Opponent"] <- "SMU"

NCAAF_Level_One[NCAAF_Level_One$Team == "Unlv", "Team"] <- "UNLV"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Unlv", "Opponent"] <- "UNLV"

NCAAF_Level_One[NCAAF_Level_One$Team == "Louisiana-Monroe", "Team"] <- "UL Monroe"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Louisiana-Monroe", "Opponent"] <- "UL Monroe"

NCAAF_Level_One[NCAAF_Level_One$Team == "Sam Houston", "Team"] <- "Sam Houston State"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Sam Houston", "Opponent"] <- "Sam Houston State"

NCAAF_Level_One[NCAAF_Level_One$Team == "Southern Miss", "Team"] <- "Southern Mississippi"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Southern Miss", "Opponent"] <- "Southern Mississippi"

NCAAF_Level_One[NCAAF_Level_One$Team == "Tarleton", "Team"] <- "Tarleton State"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Tarleton", "Opponent"] <- "Tarleton State"

NCAAF_Level_One[NCAAF_Level_One$Team == "CSU Northridge", "Team"] <- "CS-Northridge"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "CSU Northridge", "Opponent"] <- "CS-Northridge"

NCAAF_Level_One[NCAAF_Level_One$Team == "Nicholls State", "Team"] <- "Nicholls"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Nicholls State", "Opponent"] <- "Nicholls"

NCAAF_Level_One[NCAAF_Level_One$Team == "Presbyterian College", "Team"] <- "Presbyterian"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "Presbyterian College", "Opponent"] <- "Presbyterian"

NCAAF_Level_One[NCAAF_Level_One$Team == "North Carolina Central", "Team"] <- "NC Central"
NCAAF_Level_One[NCAAF_Level_One$Opponent == "North Carolina Central", "Opponent"] <- "NC Central"


NCAAF_Level_One <- NCAAF_Level_One %>% filter(Game_ID != "Postponed", Game_ID != "Canceled", Game_ID != "Live")

write.csv(NCAAF_Level_One, "Level_One.csv", row.names = F)
write.csv(Name_Mapping %>% select(Team = Name, FBS), "NCAAF_Team_List.csv", row.names = F)

Played <- Schedule %>% filter(Played == TRUE)
length(unique(Played$Game_ID)) * 2 == nrow(Played)

NCAAF_L1 <- NCAAF_Level_One


################### Create ELO ###############
# NCAAFL1 <- NCAAF_L1%>%
#   filter(Team %in% NCAAF_L1_Teams$Team)%>%
#   filter(Opponent %in% NCAAF_L1_Teams$Team)
#NCAAF_L1 <- read_csv("https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NCAAF/NCAAF_Level_One.csv")

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

NCAAF_L1_Teams <- NCAAF_L1_Teams %>% mutate(
  ELO = ifelse(FBS == 1, 1500, 1200),)

#elo_loop_df <- NCAAF_L1%>%filter(Date < "2020-09-01")
#i <-2207

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
    
    ELO_A <- as.numeric(NCAAF_L1_Teams[NCAAF_L1_Teams$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(NCAAF_L1_Teams[NCAAF_L1_Teams$Team == Team_B, "ELO"])
    #Find which schools are causing issues...
    if(is.na(ELO_A)| is.na(ELO_B)){
      print(i)
      break
    }
    
    ## Load current ELO into the main dataset ##
    
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
    
    NCAAF_L1_Teams[NCAAF_L1_Teams$Team == Team_A, "ELO"] <- Elo_Updated_A
    NCAAF_L1_Teams[NCAAF_L1_Teams$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}

NCAAF_L1_Teams %>% filter(FBS == 1) %>% arrange(desc(ELO)) %>% top_n(100)%>%View()

write.csv(NCAAF_L1,'Level_One_With_ELO.csv')

##### Creating the test set.  Need to change date each week when data pipeline is re-run
NCAAF_L1_Future$Date <- as.Date(NCAAF_L1_Future$Date, origin = "1970-01-01")
NCAAF_This_Week <- NCAAF_L1_Future %>% filter(Date > "2021-09-30", Date <= "2021-10-07") %>% 
  select(Date, Season, Team, Opponent, Home, Neutral_Location, Game_ID)

NCAAF_This_Week <- NCAAF_This_Week %>% 
  left_join(NCAAF_L1_Teams %>% select(Team, ELO), by = c("Team" = "Team"))

NCAAF_This_Week <- NCAAF_This_Week %>% 
  left_join(NCAAF_L1_Teams %>% select(Team, ELO), by = c("Opponent" = "Team"))

names(NCAAF_This_Week) <- c("Date", "Season", "Team", "Opponent", "Home", 
                            "Neutral_Location", "Game_ID", "ELO", "Opp_ELO")

NCAAF_This_Week <- NCAAF_This_Week %>% mutate(
  Elo_Difference = ELO - Opp_ELO
)

write.csv(NCAAF_This_Week,"ThisWeeksGames_L1.csv")

data <- NCAAF_L1

advanced_stats_df <- data.frame()
teamlist <- cfbd_game_info(2021, week = 1)$home_team
counter <- 1
for (t in unique(data$Team)){
  for (s in 2000:2021){
    print(paste0('Iteration: ',counter,' of 1869'))
    try(tempdf <- cfbd_stats_season_team(year = s,team = t)%>%
          select(team,time_of_poss_pg,completion_pct,pass_ypr,int_pct,rush_ypc,turnovers_pg,third_conv_rate,fourth_conv_rate,penalty_yds_pg))
    tempdf <- tempdf%>%
      mutate(Team = t)%>%
      mutate(Season = s)
    advanced_stats_df <- rbind(advanced_stats_df,tempdf)
    counter <- counter +1
    
  }
}

fulldata<-inner_join(data,advanced_stats_df, by = c('Team','Season'))

fulldata<-unique(fulldata)

mldata<-fulldata%>%
  drop_na()%>%
  mutate(opp_time_of_poss_gm = fulldata$time_of_poss_pg[match(Opponent,fulldata$Team)])%>%
  mutate(opp_completion_pct = fulldata$completion_pct[match(Opponent,fulldata$Team)])%>%
  mutate(opp_pass_ypr = fulldata$pass_ypr[match(Opponent,fulldata$Team)])%>%
  mutate(opp_int_pct =  fulldata$int_pct[match(Opponent,fulldata$Team)])%>%
  mutate(opp_rush_ypc = fulldata$rush_ypc[match(Opponent,fulldata$Team)])%>%
  mutate(opp_turnovers_pg = fulldata$turnovers_pg[match(Opponent,fulldata$Team)])%>%
  mutate(opp_third_conv_rate = fulldata$third_conv_rate[match(Opponent,fulldata$Team)])%>%
  mutate(opp_fourth_conv_rate = fulldata$fourth_conv_rate[match(Opponent,fulldata$Team)])%>%
  mutate(opp_penalty_yds_pg = fulldata$penalty_yds_pg[match(Opponent,fulldata$Team)])%>%
  na.omit()

mldata <- mldata%>%
  select(-team)

write.csv(mldata,'mldata.csv')  

# Add additional stats to this week's games

df2021 <- advanced_stats_df%>%
  filter(Season == 2021)

NCAAF_This_Week <- inner_join(NCAAF_This_Week,df2021,by = c("Team","Season"))%>%na.omit()

ref_table <- mldata%>%
  mutate(Date = as.Date(Date))%>%
  filter(Date > "2021-09-01")%>%
  select(Team,time_of_poss_pg,completion_pct,pass_ypr,int_pct,rush_ypc,turnovers_pg,third_conv_rate,
         fourth_conv_rate,penalty_yds_pg)


NCAAF_This_Week <- NCAAF_This_Week%>%
  mutate(opp_time_of_poss_gm = ref_table$time_of_poss_pg[match(NCAAF_This_Week$Opponent,ref_table$Team)])%>%
  mutate(opp_completion_pct = ref_table$completion_pct[match(Opponent,ref_table$Team)])%>%
  mutate(opp_pass_ypr = ref_table$pass_ypr[match(Opponent,ref_table$Team)])%>%
  mutate(opp_int_pct =  ref_table$int_pct[match(Opponent,ref_table$Team)])%>%
  mutate(opp_rush_ypc = ref_table$rush_ypc[match(Opponent,ref_table$Team)])%>%
  mutate(opp_turnovers_pg = ref_table$turnovers_pg[match(Opponent,ref_table$Team)])%>%
  mutate(opp_third_conv_rate = ref_table$third_conv_rate[match(Opponent,ref_table$Team)])%>%
  mutate(opp_fourth_conv_rate = ref_table$fourth_conv_rate[match(Opponent,ref_table$Team)])%>%
  mutate(opp_penalty_yds_pg = ref_table$penalty_yds_pg[match(Opponent,ref_table$Team)])%>%
  na.omit()

NCAAF_This_Week <- unique(NCAAF_This_Week)

NCAAF_This_Week <- NCAAF_This_Week%>%
  select(-team)

write.csv(NCAAF_This_Week,"ThisWeeksGames.csv")



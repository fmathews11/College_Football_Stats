library(cfbfastR)
library(tidyverse)
library(ggthemes)
library(extrafont)
library(showtext)
library(tidytext)
library(png)
library(ggpubr)
font_add_google('Quicksand')
showtext_auto()
library(grid)
Sys.setenv(CFBD_API_KEY = "ZmYrC0A2qVl/S+8M0W0ygbpyMtpTvGYn2lDZexO8WPP5ok0IKvxkYTJItmgNgBhh")




purdata<- cfbd_pbp_data(year = 2021 ,team = "Purdue", season_type = 'regular', week =1)

purdata = data.frame()



for (i in 2000:2021){
  for (j in 1:4){
    tempdf <- cfbd_pbp_data(year = i ,team = "Purdue", season_type = 'regular',week = j)
    if(typeof(tempdf) != "NULL"){
      tempdf2 <- tempdf%>%
      select(season,wk,offense_play,play_type,play_text)
      purdata <- rbind(purdata,tempdf2)
}}}

purdata<- data.frame()
for(j in 1:4){
  tempdf <- cfbd_pbp_data(year = 2021 ,team = "Purdue", season_type = 'regular',week = j)
  purdata <- rbind(purdata,tempdf)
}

purdata%>%
  filter(stringr::str_detect(play_text,'Karlaftis'))

data<- purdata%>%
  filter(offense_play == 'Purdue')%>%
  filter(play_type == 'Pass Interception' | play_type == 'Pass Interception Return' |
           play_type == "Interception Return Touchdown")%>%
  count(season)
                            
(purdata%>%colnames())  %in% (tempdf%>%colnames())

g <- purdata%>%
  filter(grepl('Plummer',play_text))%>%
  filter(offense_play == 'Purdue')%>%
  filter(play_type == 'Pass Incompletion' | play_type == 'Pass Reception')
         
g<-cfbd_play_stats_player(year = 2021,week = 1,team = 'Purdue')

g <- cfbd_team_roster(2021,'Purdue')


k <- cfbd_pbp_data(2021,team = "Purdue")%>%
  filter(grepl('Karlaftis',play_text))


k <- cfbd_play_stats_player(2021,week = 1,athlete_id = 4426659, team = 'Purdue')

g <- cfbd_stats_game_advanced(2021,week = 1,team = 'Purdue')

usage <- cfbd_player_usage(year = 2021, team = "Purdue",athlete_id = 4426659, excl_garbage_time = TRUE)

cfbd_player

purdata%>%
  filter(offense_play == "Purdue")%>%
  filter(down > 0)%>%
  filter(play_type != 'Kickoff' & play_type != 'Timeout')%>%
  group_by(down)%>%
  count(play_type)%>%
  ggplot(aes(x = reorder(play_type,n),y = n))+geom_col(fill= 'black',color = 'white')+
  coord_flip()+
  facet_wrap(vars(down), scales = 'free')+
  labs(
    title = 'Total Count of Play Type Per Down',
    x = NULL,
    y = NULL,
    subtitle = "Purdue Offense, 2021")+
  theme_hc()+theme(title=element_text(size=14,  family="Quicksand",colour = 'black'),
                 plot.background = element_rect(fill = "#CEB888", color = NA),
                 axis.title = element_text(size = 10,color = 'black'),
                 axis.title.y = element_text(face = 'bold'),
                 axis.title.x = element_text(face = 'bold'),
                 axis.text = element_text(face = 'bold',size = 9),
                 plot.caption = element_text(face = "italic",hjust = 0.5, size = 8),
                 strip.background =  element_rect(fill = "#CEB888"),
                 strip.text = element_text(face = 'bold'))

cfbd_metrics_ppa_players_games(year = 2021,team = 'Purdue')%>%
  group_by(name)%>%
  summarise(avg_pass_ppa = mean(avg_PPA_all))%>%
  arrange(-avg_pass_ppa)

cfbd_plays(year = 2021, week = 3, team = 'Purdue',defense = 'Purdue')

cfbd_game_box_advanced(401282706)


library(extrafont)
library(showtext)
library(tidytext)
font_add_google('Quicksand')
showtext_auto()

### PPA PLOT
cfbd_metrics_ppa_players_season(year = "2021", team = "Purdue")%>%
  left_join(cfbd_player_usage(year = '2021',team = 'Purdue'),by = 'athlete_id')%>%
  select(name.x,avg_PPA_all,usg_overall)%>%
  mutate(adj_ppa = avg_PPA_all/usg_overall)%>%
  filter(!is.na(adj_ppa))%>%
  ggplot(aes(x = reorder(name.x,adj_ppa),y = adj_ppa))+
  geom_col(fill = 'black',color = 'white')+
  coord_flip()+
  theme_hc()+labs(
    title = "Average PPA Values For Offensive Players - 2021",
    caption = "Frank Mathews - Purdue Sports Analytics\n@jishguy",
    y = "Average Game PPA Value",
    x = "Player Name"
  )+theme(title=element_text(size=14,  family="Quicksand",colour = 'black'),
          plot.background = element_rect(fill = "#CEB888", color = NA),
          axis.title = element_text(size = 10,color = 'black'),
          axis.title.y = element_text(face = 'bold'),
          axis.title.x = element_text(face = 'bold'),
          axis.text = element_text(face = 'bold',size = 9),
          plot.caption = element_text(face = "italic",hjust = 0.5, size = 8))+
          annotate("text",x =4.5,  y = 48.5, label = "Adjusted for usage percentage",colour = "black",  vjust = 0.5, size = 4, fontface = "italic")
img.file <- png::readPNG('motionp.png')        
getwd()
  
cfbd_player_usage(year = '2021',team = 'Purdue')%>%
  filter(position != "QB")%>%
  pivot_longer(usg_1st_down:usg_3rd_down,names_to = "down", values_to = 'value')%>%
  select(name,down,value)%>%
  filter(value >0)%>%
  ggplot(aes(x = reorder_within(name,value,down),y = value))+
  scale_x_reordered()+
  geom_col(fill = 'black',color = 'white')+
  facet_wrap(vars(down), scales = 'free')+
  coord_flip()+
  theme_hc()+
  theme(plot.title=element_text(size=14,  family="Quicksand",colour = 'black',hjust = 0.5),
plot.background = element_rect(fill = "#CEB888", color = NA),
axis.title = element_text(size = 10,color = 'black',face ='bold'),
axis.title.y = element_text(face = 'bold'),
axis.title.x = element_text(face = 'bold'),
axis.text = element_text(face = 'bold',size = 9),
plot.caption = element_text(face = "italic",hjust = 0.5, size = 8),
strip.background =  element_rect(fill = "#CEB888"),
strip.text = element_text(face = 'bold'))+
  labs(
  title = 'Player Usage % Per Down',
  x = 'Player Name',
  y = 'Usage %',
  caption = "Frank Mathews - Purdue Sports Analytics\n@jishguy"
)






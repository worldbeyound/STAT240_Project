nfl_2022 = read.csv("../../data/play_by_play_2022.csv")
nfl_2021 = read.csv("../../data/play_by_play_2021.csv")
nfl_2020 = read.csv("../../data/play_by_play_2020.csv")
nfl_2019 = read.csv("../../data/play_by_play_2019.csv")
nfl_2018 = read.csv("../../data/play_by_play_2018.csv")
nfl_2017 = read.csv("../../data/play_by_play_2017.csv")
nfl_2016 = read.csv("../../data/play_by_play_2016.csv")
nfl_2015 = read.csv("../../data/play_by_play_2015.csv")
nfl_2014 = read.csv("../../data/play_by_play_2014.csv")
nfl_2013 = read.csv("../../data/play_by_play_2013.csv")
nfl_2012 = read.csv("../../data/play_by_play_2012.csv")

library(tidyverse)
library(kableExtra)
library(broman)
source("../../scripts/viridis.R")
source("../../scripts/ggprob.R")

nfl_2022_champ = nfl_2022 %>%
  filter(str_detect(game_id, "KC"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2021_champ = nfl_2021 %>%
  filter(str_detect(game_id, "LA"))%>%
  filter(!(str_detect(game_id, "LAC")))%>%
  filter(str_detect(season_type, "REG"))

nfl_2020_champ = nfl_2020 %>%
  filter(str_detect(game_id, "TB"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2019_champ = nfl_2019 %>%
  filter(str_detect(game_id, "KC"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2018_champ = nfl_2018 %>%
  filter(str_detect(game_id, "NE"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2017_champ = nfl_2017 %>%
  filter(str_detect(game_id, "DEN"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2016_champ = nfl_2016 %>%
  filter(str_detect(game_id, "NE"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2015_champ = nfl_2015 %>%
  filter(str_detect(game_id, "SEA"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2014_champ = nfl_2014 %>%
  filter(str_detect(game_id, "BAL"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2013_champ = nfl_2013 %>%
  filter(str_detect(game_id, "NYG"))%>%
  filter(str_detect(season_type, "REG"))

nfl_2012_champ = nfl_2012 %>%
  filter(str_detect(game_id, "GB"))%>%
  filter(str_detect(season_type, "REG"))


nfl_2022_champ_filt = nfl_2022_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2021_champ_filt = nfl_2021_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2020_champ_filt = nfl_2020_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2019_champ_filt = nfl_2019_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2018_champ_filt = nfl_2018_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2017_champ_filt = nfl_2017_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2016_champ_filt = nfl_2016_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2015_champ_filt = nfl_2015_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2014_champ_filt = nfl_2014_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2013_champ_filt = nfl_2013_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

nfl_2012_champ_filt = nfl_2012_champ %>%
  select(game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa )

last_10_champs = full_join(nfl_2012_champ_filt,nfl_2013_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2015_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2016_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2017_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2018_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2019_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2020_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2021_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2022_champ_filt)






last_10_champs = full_join(last_10_champs,nfl_2014_champ_filt)first_game = filtered %>%
  filter(week == 1)

ggplot(first_game, aes(x = game_seconds_remaining, y= away_wp))+
  geom_line() 


ggplot(filtered, aes(x = game_seconds_remaining, y= away_wp))+
  geom_line()+
  facet_wrap(vars(week))


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
#nfl_2012 = read.csv("../../data/play_by_play_2012.csv")

library(tidyverse)
library(kableExtra)
library(broman)
source("../../scripts/viridis.R")
source("../../scripts/ggprob.R")

nfl_2022_champ = nfl_2022 %>%
  filter(str_detect(game_id, "KC"))%>%
  filter(str_detect(season_type, "REG"))%>%
  filter(!(week == 18))%>%
  mutate(team = "KC", year = 2022)

nfl_2021_champ = nfl_2021 %>%
  filter(str_detect(game_id, "LA"))%>%
  filter(!(str_detect(game_id, "LAC")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "LA", year = 2021)

nfl_2020_champ = nfl_2020 %>%
  filter(str_detect(game_id, "TB"))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "TB",  year = 2020)

nfl_2019_champ = nfl_2019 %>%
  filter(str_detect(game_id, "KC"))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "KC",  year = 2019)

nfl_2018_champ = nfl_2018 %>%
  filter(str_detect(game_id, "NE"))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "NE", year = 2018)

nfl_2017_champ = nfl_2017 %>%
  filter(str_detect(game_id, "DEN"))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "DEN",  year = 2017)

nfl_2016_champ = nfl_2016 %>%
  filter(str_detect(game_id, "NE"))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "NE", year = 2016)

nfl_2015_champ = nfl_2015 %>%
  filter(str_detect(game_id, "SEA"))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "SEA",  year = 2015)

nfl_2014_champ = nfl_2014 %>%
  filter(str_detect(game_id, "BAL"))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "BAL",  year = 2014)

nfl_2013_champ = nfl_2013 %>%
  filter(str_detect(game_id, "NYG"))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(team = "NYG",  year = 2013)

# nfl_2012_champ = nfl_2012 %>%
#   filter(str_detect(game_id, "GB"))%>%
#   filter(str_detect(season_type, "REG"))%>%
#   mutate(team = "GB",  year = 2012)


nfl_2022_champ_filt = nfl_2022_champ %>%
  select(team,year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2021_champ_filt = nfl_2021_champ %>%
  select(team, year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2020_champ_filt = nfl_2020_champ %>%
  select(team, year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2019_champ_filt = nfl_2019_champ %>%
  select(team, year,game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2018_champ_filt = nfl_2018_champ %>%
  select(team, year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2017_champ_filt = nfl_2017_champ %>%
  select(team,year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2016_champ_filt = nfl_2016_champ %>%
  select(team, year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2015_champ_filt = nfl_2015_champ %>%
  select(team, year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2014_champ_filt = nfl_2014_champ %>%
  select(team, year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

nfl_2013_champ_filt = nfl_2013_champ %>%
  select(team, year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
         game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed  )

# nfl_2012_champ_filt = nfl_2012_champ %>%
#   select(team, year, game_id, home_team, away_team, week, posteam, defteam, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, 
#          game_half, play_type, score_differential, td_prob, fg_prob, wp, def_wp,home_wp, away_wp, wpa, touchdown, third_down_converted, third_down_failed )

last_10_champs = full_join(nfl_2022_champ_filt,nfl_2013_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2014_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2015_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2016_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2017_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2018_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2019_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2020_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2021_champ_filt)
#last_10_champs = full_join(last_10_champs,nfl_2012_champ_filt)



total_games = 16 

champ_offense = last_10_champs%>%
  filter(team == posteam)%>%
  select(team, year, third_down_converted, third_down_failed, touchdown, week)%>%
  arrange(year)

champ_td = champ_offense%>%
  select(team, year, touchdown)%>%
  filter(touchdown == 1)%>%
  group_by(year)%>%
  mutate(total_td = n())%>%
  mutate(td_per_game = total_td/total_games)%>%
  distinct()%>%
  ungroup(year)%>%
  mutate(mean = mean(td_per_game))

champ_third_down_converted = champ_offense%>%
  select(team, year, third_down_converted)%>%
  filter(third_down_converted == 1)%>%
  group_by(year)%>%
  mutate(total_converted = n())%>%
  select(-third_down_converted)%>%
  distinct()
  
champ_third_down_failed = champ_offense%>%
  select(team, year, third_down_failed)%>%
  filter(third_down_failed == 1)%>%
  group_by(year)%>%
  mutate(total_failed = n())%>%
  select(-third_down_failed)%>%
  distinct()

champ_third_down_rate = full_join(champ_third_down_converted, champ_third_down_failed, by = c("team","year"))

champ_third_down_rate = champ_third_down_rate%>%
  group_by(year)%>%
  mutate(conversion_rate = total_converted/(total_converted+total_failed))%>%
  ungroup(year)%>%
  mutate(mean = mean(conversion_rate))





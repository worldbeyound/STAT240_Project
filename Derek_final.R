install.packages("nflreadr")
install.packages("nflfastR")
library(nflreadr)
library(nflfastR)
library(tidyverse)
library(kableExtra)
library(broman)
source("../../scripts/viridis.R")
source("../../scripts/ggprob.R")

nfl_2022 = load_pbp(2022)
nfl_2021 = load_pbp(2021)
nfl_2020 = load_pbp(2020)
nfl_2019 = load_pbp(2019)
nfl_2018 = load_pbp(2018)
nfl_2017 = load_pbp(2017)
nfl_2016 = load_pbp(2016)
nfl_2015 = load_pbp(2015)
nfl_2014 = load_pbp(2014)
nfl_2013 = load_pbp(2013)




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


nfl_2022_champ_filt = nfl_2022_champ %>%
  select(team, year, week, posteam,third_down_converted, third_down_failed, touchdown)

nfl_2021_champ_filt = nfl_2021_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2020_champ_filt = nfl_2020_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2019_champ_filt = nfl_2019_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2018_champ_filt = nfl_2018_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2017_champ_filt = nfl_2017_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2016_champ_filt = nfl_2016_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2015_champ_filt = nfl_2015_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2014_champ_filt = nfl_2014_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2013_champ_filt = nfl_2013_champ %>%
  select(team, year, week, posteam, third_down_converted, third_down_failed, touchdown)


last_10_champs = full_join(nfl_2022_champ_filt,nfl_2013_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2014_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2015_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2016_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2017_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2018_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2019_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2020_champ_filt)
last_10_champs = full_join(last_10_champs,nfl_2021_champ_filt)



total_games = 16 

champ_offense = last_10_champs%>%
  filter(team == posteam)%>%
  select(team, year, week, third_down_converted, third_down_failed, touchdown)%>%
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


t.test(champ_td$td_per_game)
t.test(champ_third_down_rate$conversion_rate)

ggplot(champ_td, aes(x=year, y=td_per_game))+
  geom_col()

ggplot(champ_third_down_rate, aes(x=year, y =conversion_rate))+
  geom_col()





nfl_2022_rest = nfl_2022 %>%
  filter(!(str_detect(game_id, "KC")))%>%
  filter(str_detect(season_type, "REG"))%>%
  filter(!(week == 18))%>%
  mutate(year = 2022)

nfl_2021_rest = nfl_2021 %>%
  filter(!(str_detect(game_id, "LA")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(year = 2021)

nfl_2020_rest = nfl_2020 %>%
  filter(!(str_detect(game_id, "TB")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(year = 2020)

nfl_2019_rest = nfl_2019 %>%
  filter(!(str_detect(game_id, "KC")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(year = 2019)

nfl_2018_rest = nfl_2018 %>%
  filter(!(str_detect(game_id, "NE")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate( year = 2018)

nfl_2017_rest = nfl_2017 %>%
  filter(!(str_detect(game_id, "DEN")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(year = 2017)

nfl_2016_rest = nfl_2016 %>%
  filter(!(str_detect(game_id, "NE")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(year = 2016)

nfl_2015_rest = nfl_2015 %>%
  filter(!(str_detect(game_id, "SEA")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(year = 2015)

nfl_2014_rest = nfl_2014 %>%
  filter(!(str_detect(game_id, "BAL")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(year = 2014)

nfl_2013_rest = nfl_2013 %>%
  filter(!(str_detect(game_id, "NYG")))%>%
  filter(str_detect(season_type, "REG"))%>%
  mutate(year = 2013)

nfl_2022_rest_filt = nfl_2022_rest %>%
  select(year, week, posteam,third_down_converted, third_down_failed, touchdown)

nfl_2021_rest_filt = nfl_2021_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2020_rest_filt = nfl_2020_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2019_rest_filt = nfl_2019_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2018_rest_filt = nfl_2018_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2017_rest_filt = nfl_2017_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2016_rest_filt = nfl_2016_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2015_rest_filt = nfl_2015_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2014_rest_filt = nfl_2014_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)

nfl_2013_rest_filt = nfl_2013_rest %>%
  select(year, week, posteam, third_down_converted, third_down_failed, touchdown)


last_10 = full_join(nfl_2022_rest_filt,nfl_2013_rest_filt)
last_10 = full_join(last_10,nfl_2014_rest_filt)
last_10 = full_join(last_10,nfl_2015_rest_filt)
last_10 = full_join(last_10,nfl_2016_rest_filt)
last_10 = full_join(last_10,nfl_2017_rest_filt)
last_10 = full_join(last_10,nfl_2018_rest_filt)
last_10 = full_join(last_10,nfl_2019_rest_filt)
last_10 = full_join(last_10,nfl_2020_rest_filt)
last_10 = full_join(last_10,nfl_2021_rest_filt)

last_10 = last_10%>%arrange(year)

total_games_rest = 240 * 2

rest_td = last_10%>%
  select(year, touchdown)%>%
  filter(touchdown == 1)%>%
  group_by(year)%>%
  mutate(total_td = n())%>%
  mutate(td_per_game = total_td/total_games_rest)%>%
  distinct()%>%
  ungroup(year)%>%
  mutate(mean = mean(td_per_game))

rest_third_down_converted = last_10%>%
  select(year, third_down_converted)%>%
  filter(third_down_converted == 1)%>%
  group_by(year)%>%
  mutate(total_converted = n())%>%
  select(-third_down_converted)%>%
  distinct()

rest_third_down_failed = last_10%>%
  select(year, third_down_failed)%>%
  filter(third_down_failed == 1)%>%
  group_by(year)%>%
  mutate(total_failed = n())%>%
  select(-third_down_failed)%>%
  distinct()

rest_third_down_rate = full_join(rest_third_down_converted, rest_third_down_failed, by = "year")

rest_third_down_rate = rest_third_down_rate%>%
  group_by(year)%>%
  mutate(conversion_rate = total_converted/(total_converted+total_failed))%>%
  ungroup(year)%>%
  mutate(mean = mean(conversion_rate))


ggplot(rest_td, aes(x=year, y=td_per_game))+
  geom_col()

ggplot(champ_td, aes(x=year, y=td_per_game))+
  geom_col()

ggplot(rest_third_down_rate, aes(x=year, y =conversion_rate))+
  geom_col()

ggplot(champ_third_down_rate, aes(x=year, y =conversion_rate))+
  geom_col()


# Plots to compare touchdowns and down rate
third_down_rate = full_join(champ_third_down_rate, rest_third_down_rate) %>%
  mutate(group = case_when(
    !is.na(team) ~ "superbowl winner",
    TRUE ~ "the rest"
  )) %>%
  mutate(year = year - 2000)

td = full_join(champ_td, rest_td) %>%
  mutate(group = case_when(
    !is.na(team) ~ "superbowl winner",
    TRUE ~ "the rest"
  )) %>%
  mutate(year = year - 2000)

ggplot(third_down_rate, aes(x = year, y = conversion_rate)) +
  facet_wrap(vars(group)) +
  geom_col() +
  scale_x_continuous(breaks = c(13:22))

ggplot(td, aes(x = year, y = td_per_game)) +
  facet_wrap(vars(group)) +
  geom_col() +
  scale_x_continuous(breaks = c(13:22))

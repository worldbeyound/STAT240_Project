data_2022 = read_csv("../../data/play_by_play_2022.csv")
data_2021 = read_csv("../../data/play_by_play_2021.csv")
data_2020 = read_csv("../../data/play_by_play_2020.csv")
data_2019 = read_csv("../../data/play_by_play_2019.csv")
data_2018 = read_csv("../../data/play_by_play_2018.csv")
data_2017 = read_csv("../../data/play_by_play_2017.csv")
data_2016 = read_csv("../../data/play_by_play_2016.csv")
data_2015 = read_csv("../../data/play_by_play_2015.csv")
data_2014 = read_csv("../../data/play_by_play_2014.csv")
data_2013 = read_csv("../../data/play_by_play_2013.csv")
data_2022_joined=data_2022%>%
  full_join(data_2021)%>%
  full_join(data_2020)%>%
  full_join(data_2018)%>%
  full_join(data_2017)%>%
  full_join(data_2015)%>%
  full_join(data_2013)%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2022") & defteam=="KC")| ((substr(game_id, 0, 4)=="2021") & defteam=="LA") | ((substr(game_id, 0, 4)=="2020") & defteam=="TB") | ((substr(game_id, 0, 4)=="2018") & defteam=="NE") | ((substr(game_id, 0, 4)=="2017") & defteam=="PHI") | ((substr(game_id, 0, 4)=="2015") & defteam=="DEN") | ((substr(game_id, 0, 4)=="2013") & defteam=="SEA"), yardline_100<80, !is.na(tackled_for_loss))%>%
  group_by(defteam)%>%
  summarise(hello=sum((tackled_for_loss)/n()))
data_2022_joined_2=data_2019%>%
  mutate(defteam=str_replace(defteam, "KC", "KC_19"))%>%
  filter(!month(game_date)==01, season_type=="REG",  ((substr(game_id, 0, 4)=="2019") & defteam=="KC_19"), yardline_100<80, !is.na(tackled_for_loss))%>%
  group_by(defteam)%>%
  summarise(hello=sum((tackled_for_loss)/n()))

data_2022_joined_7=data_2016%>%
  mutate(defteam=str_replace(defteam, "NE", "NE_16"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2016") & defteam=="NE_16") , yardline_100<80, !is.na(tackled_for_loss))%>%
  group_by(defteam)%>%
  summarise(hello=sum((tackled_for_loss)/n()))
  
data_2022_joined_3=data_2014%>%
  mutate(defteam=str_replace(defteam, "NE", "NE_14"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2014") & defteam=="NE_14") , yardline_100<80, !is.na(tackled_for_loss))%>%
  group_by(defteam)%>%
  summarise(hello=sum((tackled_for_loss)/n()))
data_2022_joined_4=data_2022_joined%>%
  full_join(data_2022_joined_2)%>%
  full_join(data_2022_joined_3)%>%
  full_join(data_2022_joined_7)

data_2022_joined_80=data_2022%>%
  full_join(data_2021)%>%
  full_join(data_2020)%>%
  full_join(data_2018)%>%
  full_join(data_2017)%>%
  full_join(data_2015)%>%
  full_join(data_2013)%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2022") & defteam=="KC")| ((substr(game_id, 0, 4)=="2021") & defteam=="LA") | ((substr(game_id, 0, 4)=="2020") & defteam=="TB") | ((substr(game_id, 0, 4)=="2018") & defteam=="NE") | ((substr(game_id, 0, 4)=="2017") & defteam=="PHI") | ((substr(game_id, 0, 4)=="2015") & defteam=="DEN") | ((substr(game_id, 0, 4)=="2013") & defteam=="SEA"), yardline_100>80, !is.na(tackled_for_loss))%>%
  group_by(defteam)%>%
  summarise(hello_2=sum((tackled_for_loss)/n()))
data_2022_joined_2_80=data_2019%>%
  mutate(defteam=str_replace(defteam, "KC", "KC_19"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2019") & defteam=="KC_19"), yardline_100>80, !is.na(tackled_for_loss))%>%
  group_by(defteam)%>%
  summarise(hello_2=sum((tackled_for_loss)/n()))

data_2022_joined_7_80=data_2016%>%
  mutate(defteam=str_replace(defteam, "NE", "NE_16"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2016") & defteam=="NE_16") , yardline_100>80, !is.na(tackled_for_loss))%>%
  group_by(defteam)%>%
  summarise(hello_2=sum((tackled_for_loss)/n()))
data_2022_joined_3_80=data_2014%>%
  mutate(defteam=str_replace(defteam, "NE", "NE_14"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2014") & defteam=="NE_14") , yardline_100>80, !is.na(tackled_for_loss))%>%
  group_by(defteam)%>%
  summarise(hello_2=sum((tackled_for_loss)/n()))
data_2022_joined_4_80=data_2022_joined_80%>%
  full_join(data_2022_joined_2_80)%>%
  full_join(data_2022_joined_3_80)%>%
  full_join(data_2022_joined_7_80)
  
joined_zone_data=data_2022_joined_4%>%
  full_join(data_2022_joined_4_80)%>%
  mutate(official_stat=hello/hello_2)%>%
  mutate(joiner=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))




sec_data_2022_joined=data_2022%>%
  full_join(data_2021)%>%
  full_join(data_2020)%>%
  full_join(data_2018)%>%
  full_join(data_2017)%>%
  full_join(data_2015)%>%
  full_join(data_2013)%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2022") & !defteam=="KC")| ((substr(game_id, 0, 4)=="2021") & !defteam=="LA") | ((substr(game_id, 0, 4)=="2020") & !defteam=="TB") | ((substr(game_id, 0, 4)=="2018") & !defteam=="NE") | ((substr(game_id, 0, 4)=="2017") & !defteam=="PHI") | ((substr(game_id, 0, 4)=="2015") & !defteam=="DEN") | ((substr(game_id, 0, 4)=="2013") & !defteam=="SEA"), yardline_100<80, !is.na(tackled_for_loss))%>%
  mutate(year=(substr(game_id, 0, 4)))%>%
  group_by(year)%>%
  summarise(hello=sum((tackled_for_loss)/n()))
sec_data_2022_joined_2=data_2019%>%
  mutate(defteam=str_replace(defteam, "KC", "KC_19"))%>%
  filter(!month(game_date)==01, season_type=="REG",  ((substr(game_id, 0, 4)=="2019") & !defteam=="KC_19"), yardline_100<80, !is.na(tackled_for_loss))%>%
  mutate(year=(substr(game_id, 0, 4)))%>%
  group_by(year)%>%
  summarise(hello=sum((tackled_for_loss)/n()))

sec_data_2022_joined_7=data_2016%>%
  mutate(defteam=str_replace(defteam, "NE", "NE_16"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2016") & defteam=="NE_16") , yardline_100<80, !is.na(tackled_for_loss))%>%
  mutate(year=(substr(game_id, 0, 4)))%>%
  group_by(year)%>%
  summarise(hello=sum((tackled_for_loss)/n()))

sec_data_2022_joined_3=data_2014%>%
  mutate(defteam=str_replace(defteam, "NE", "NE_14"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2014") & !defteam=="NE_14") , yardline_100<80, !is.na(tackled_for_loss))%>%
  mutate(year=(substr(game_id, 0, 4)))%>%
  group_by(year)%>%
  summarise(hello=sum((tackled_for_loss)/n()))
sec_data_2022_joined_4=sec_data_2022_joined%>%
  full_join(sec_data_2022_joined_2)%>%
  full_join(sec_data_2022_joined_3)%>%
  full_join(sec_data_2022_joined_7)

sec_data_2022_joined_80=data_2022%>%
  full_join(data_2021)%>%
  full_join(data_2020)%>%
  full_join(data_2018)%>%
  full_join(data_2017)%>%
  full_join(data_2015)%>%
  full_join(data_2013)%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2022") & !defteam=="KC")| ((substr(game_id, 0, 4)=="2021") & !defteam=="LA") | ((substr(game_id, 0, 4)=="2020") & !defteam=="TB") | ((substr(game_id, 0, 4)=="2018") & !defteam=="NE") | ((substr(game_id, 0, 4)=="2017") & !defteam=="PHI") | ((substr(game_id, 0, 4)=="2015") & !defteam=="DEN") | ((substr(game_id, 0, 4)=="2013") & !defteam=="SEA"), yardline_100>80, !is.na(tackled_for_loss))%>%
  mutate(year=(substr(game_id, 0, 4)))%>%
  group_by(year)%>%
  summarise(hello_2=sum((tackled_for_loss)/n()))
sec_data_2022_joined_2_80=data_2019%>%
  mutate(defteam=str_replace(defteam, "KC", "KC_19"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2019") & !defteam=="KC_19"), yardline_100>80, !is.na(tackled_for_loss))%>%
  mutate(year=(substr(game_id, 0, 4)))%>%
  group_by(year)%>%
  summarise(hello_2=sum((tackled_for_loss)/n()))

sec_data_2022_joined_7_80=data_2016%>%
  mutate(defteam=str_replace(defteam, "NE", "NE_16"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2016") & !defteam=="NE_16") , yardline_100>80, !is.na(tackled_for_loss))%>%
  mutate(year=(substr(game_id, 0, 4)))%>% 
  group_by(year)%>%
  summarise(hello_2=sum((tackled_for_loss)/n()))
sec_data_2022_joined_3_80=data_2014%>%
  mutate(defteam=str_replace(defteam, "NE", "NE_14"))%>%
  filter(!month(game_date)==01, season_type=="REG", ((substr(game_id, 0, 4)=="2014") & !defteam=="NE_14") , yardline_100>80, !is.na(tackled_for_loss))%>%
  mutate(year=(substr(game_id, 0, 4)))%>%
  group_by(year)%>%
  summarise(hello_2=sum((tackled_for_loss)/n()))
sec_data_2022_joined_4_80=sec_data_2022_joined_80%>%
  full_join(sec_data_2022_joined_2_80)%>%
  full_join(sec_data_2022_joined_3_80)%>%
  full_join(sec_data_2022_joined_7_80)

lev_dataset_2=sec_data_2022_joined_4%>%
  full_join(sec_data_2022_joined_4_80)%>%
  mutate(official_stat=hello/hello_2)%>%
  rename(hi=hello, hi_2=hello_2, official_stat_2=official_stat)%>%
  mutate(joiner=c(6, 1, 5, 4, 7, 3, 2, 8, 9, 10))%>%
  right_join(joined_zone_data, by="joiner")%>%
  select(defteam, official_stat, official_stat_2)
write.csv(final_dataset_2, file = "../../data/final_dataset_2.csv")







  

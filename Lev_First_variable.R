bbballl_2021 = read_csv("../../data/play_by_play_2021.csv")
LA=bbballl_2021%>%
  filter(posteam=="LA")
not_LA=bbballl_2021%>%
  filter(!posteam=="LA")

foot_1=LA%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

foot_2=LA%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(foot_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

foot_1_NFL=not_LA%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

foot_2_NFL=not_LA%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(foot_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(foot_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)

bbballl_2020 = read_csv("../../data/play_by_play_2020.csv")
TB=bbballl_2020%>%
  filter(posteam=="TB")
not_TB=bbballl_2020%>%
  filter(!posteam=="TB")

ffoot_1=TB%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

ffoot_2=TB%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(ffoot_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

ffoot_1_NFL=not_TB%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

ffoot_2_NFL=not_TB%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(ffoot_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(ffoot_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(foot_2_NFL)

bbballl_2019 = read_csv("../../data/play_by_play_2019.csv")
KC=bbballl_2019%>%
  filter(posteam=="KC")
not_KC=bbballl_2020%>%
  filter(!posteam=="KC")

fffoot_1=KC%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

fffoot_2=KC%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(fffoot_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

fffoot_1_NFL=not_KC%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

fffoot_2_NFL=not_KC%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(ffoot_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(fffoot_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(ffoot_2_NFL)
bbballl_2018 = read_csv("../../data/play_by_play_2018.csv")
NE=bbballl_2018%>%
  filter(posteam=="NE")
not_NE=bbballl_2020%>%
  filter(!posteam=="NE")

foott_1=NE%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

foott_2=NE%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(foott_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

foott_1_NFL=not_NE%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

foott_2_NFL=not_NE%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(foott_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(foott_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(fffoot_2_NFL)
bbballl_2017 = read_csv("../../data/play_by_play_2017.csv")
PHI=bbballl_2017%>%
  filter(posteam=="PHI")
not_PHI=bbballl_2017%>%
  filter(!posteam=="PHI")

foottt_1=PHI%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

foottt_2=PHI%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(foottt_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

foottt_1_NFL=not_PHI%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

foottt_2_NFL=not_PHI%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(foottt_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(foottt_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(foott_2_NFL)

bbballl_2016 = read_csv("../../data/play_by_play_2016.csv")
NE16=bbballl_2016%>%
  filter(posteam=="NE")
not_NE16=bbballl_2016%>%
  filter(!posteam=="NE")

ffoottt_1=NE16%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

ffoottt_2=NE16%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(ffoottt_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

ffoottt_1_NFL=not_NE16%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

ffoottt_2_NFL=not_NE16%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(ffoottt_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(ffoottt_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(foottt_2_NFL)

bbballl_2015 = read_csv("../../data/play_by_play_2015.csv")
DEN=bbballl_2015%>%
  filter(posteam=="DEN")
not_DEN=bbballl_2015%>%
  filter(!posteam=="DEN")

ball_1=DEN%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

ball_2=DEN%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(ball_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

ball_1_NFL=not_DEN%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

ball_2_NFL=not_DEN%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(ball_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(ball_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(ffoottt_2_NFL)

bbballl_2014 = read_csv("../../data/play_by_play_2014.csv")
NE14=bbballl_2014%>%
  filter(posteam=="NE")
not_NE14=bbballl_2014%>%
  filter(!posteam=="NE")

bball_1=NE14%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

bball_2=NE14%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(bball_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

bball_1_NFL=not_NE14%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

ball_2_NFL=not_NE14%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(bball_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(bball_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(ball_2_NFL)

bbballl_2013 = read_csv("../../data/play_by_play_2013.csv")
SEA=bbballl_2013%>%
  filter(posteam=="SEA")
not_SEA=bbballl_2013%>%
  filter(!posteam=="SEA")

bballl_1=SEA%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

bballl_2=SEA%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(bballl_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

bball_1_NFL=not_SEA%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

bball_2_NFL=not_SEA%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(bball_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(bball_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(ball_2_NFL)

bbballl_2022 = read_csv("../../data/play_by_play_2022.csv")
KC22=bbballl_2022%>%
  filter(posteam=="KC")
not_KC22=bbballl_2022%>%
  filter(!posteam=="KC")

bbball_1=KC22%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(ay=mean(air_yards))

bbball_2=KC22%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  group_by(posteam)%>%
  summarise(yac=mean(yards_after_catch))%>%
  full_join(bbballl_1, by="posteam")%>%
  mutate(new_stat=ay/yac)%>%
  mutate(hello="hello")

bbball_1_NFL=not_KC22%>%
  filter(play_type=="pass", !is.na(air_yards))%>%
  select(posteam, air_yards, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(ay_2=mean(air_yards))

final_dataset=not_KC22%>%
  filter(play_type=="pass", !is.na(yards_after_catch))%>%
  select(posteam, yards_after_catch, yardline_100)%>%
  filter(yardline_100>70)%>%
  mutate(posteam="hello")%>%
  group_by(posteam)%>%
  summarise(yac_2=mean(yards_after_catch))%>%
  full_join(bbball_1_NFL, by="posteam")%>%
  mutate(stat_2=ay_2/yac_2, hello="hello")%>%
  right_join(bbball_2, by="hello")%>%
  mutate(off_stat=stat_2/new_stat)%>%
  full_join(bball_2_NFL)%>%
  select(posteam.y, off_stat)
write.csv(final_dataset, file = "../../data/final_dataset.csv")







































































































































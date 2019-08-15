master_team_list <- read.csv("App/data/master.csv", header = T, stringsAsFactors = F) %>%
  arrange(., team)

full_schedules <- read.csv("App/data/full_schedules.csv", header = T, stringsAsFactors = F) %>%
  mutate(., date_str = date,
         date = mdy(date),
         team_id = as.integer(team_id)) %>%
  select(., team_id, org_id, date_str, date, opponent, location, result, score,
         everything()) %>%
  filter(., date < '2018-11-30') %>%
  mutate(., uni_id = group_indices(., team_id, org_id, date_str))

wl <- list()

stan_opp <- full_schedules %>%
  filter(., team_id == 545) %>%
  select(., org_id, uni_id)

opp_sch <- full_schedules %>%
  filter(., team_id == stan_opp[1,1],
         uni_id != stan_opp[1,2]) %>%
  select(., team_id, org_id, result)

wl[[1]] <- full_schedules %>%
  filter(., team_id == opp_sch[1,2]) %>%
  pull(., result) %>%
  table(.) %>%
  as.vector(.) %>%
  as.data.frame(.) %>%
  setNames(., c("L", "W"))

schedule_minus_game <- full_schedules %>%
  filter(., team_id == "156",
         !(opponent == "LSU"))# & date == date_of_game_to_replace))


opponents <- schedule_minus_game$org_id

opp_schedules <- full_schedules %>%
  filter(., team_id %in% opponents,
         !(org_id == "156"))
opp_wl <- (opp_schedules$result == "W")
mean(opp_wl)

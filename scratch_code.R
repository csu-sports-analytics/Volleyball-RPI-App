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

##############

stan_opp <- full_schedules %>%
  filter(., team_id == 545) %>%
  select(., org_id, uni_id)

opp_sch <- NULL

for(i in seq(1:nrow(stan_opp))){
  opp_sch <- rbind(opp_sch, full_schedules %>%
    filter(., team_id == stan_opp[i,1],
           uni_id != stan_opp[i,2]) %>%
    select(., team_id, org_id, result))
}
opp_wl <- (opp_sch$result == "W")

mean(opp_wl, na.rm = TRUE)

##############

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
opp_wl_2 <- (opp_schedules$result == "W")
mean(opp_wl_2)




start_team_id <- 545

list_opp <- full_schedules %>%
  filter(., team_id == start_team_id) %>%
  select(., org_id, uni_id)

opp_sch <- NULL
opp_opp_sch <- NULL

for(i in seq(1:nrow(list_opp))){
  opp_sch <- rbind(opp_sch, full_schedules %>%
                     filter(., team_id == list_opp[i,1],
                            uni_id != list_opp[i,2]) %>%
                     select(., team_id, org_id, result))
}

for(i in seq(1:nrow(list_opp))){
  
  opp <- list_opp[i,1]
  
  opp_opp_list <- full_schedules %>%
    filter(., team_id == opp) %>%
    select(., org_id, uni_id)
  
  for(j in seq(1:nrow(opp_opp_list))){
    
    opp_opp_sch <- rbind(opp_opp_sch, full_schedules %>%
                           filter(., team_id == opp_opp_list[j,1]) %>%
                           select(., team_id, org_id, result))
    
  }
  
}

mean(opp_opp_sch$result == "W", na.rm = TRUE)


###################




# get date object to replace
date_of_game_to_replace_format <- mdy(date_of_game_to_replace)

replace_org <- filter(master_team_list, team == game_to_replace)

new_row <- c(as.integer(start_team_id), as.integer(replace_org$team_id), date_of_game_to_replace, date_of_game_to_replace_format, game_to_replace, NA, result, NA, NA)

# # get selected teams full schedule without game to remove
# schedule_minus_game <- full_schedules %>%
#   filter(., team_id == start_team_id,
#          !(opponent == game_to_replace & date == date_of_game_to_replace))
# 
# 
# 
# opponents <- schedule_minus_game$org_id
# 
# opp_schedules <- opp_schedules <- full_schedules %>%
#   filter(., team_id %in% opponents,
#          !(org_id == start_team_id))
# 
# opp_wl <- (opp_schedules$result == "W")
# mean(opp_wl)
# 
###

list_opp <- full_schedules %>%
  filter(., team_id == start_team_id,
         !(opponent == game_to_replace & date == date_of_game_to_replace)) %>%
  rbind(., new_row) %>%
  select(., org_id, uni_id)

team_sch <- full_schedules %>%
  filter(., team_id == start_team_id) %>%
  select(., team_id, org_id, result)

opp_sch <- NULL
opp_opp_sch <- NULL

for(i in seq(1:nrow(list_opp))){
  opp_sch <- rbind(opp_sch, full_schedules %>%
                     filter(., team_id == list_opp[i,1],
                            uni_id != list_opp[i,2]) %>%
                     select(., team_id, org_id, result))
}

for(i in seq(1:nrow(list_opp))){
  
  opp <- list_opp[i,1]
  
  opp_opp_list <- full_schedules %>%
    filter(., team_id == opp) %>%
    select(., org_id, uni_id)
  
  for(j in seq(1:nrow(opp_opp_list))){
    
    opp_opp_sch <- rbind(opp_opp_sch, full_schedules %>%
                           filter(., team_id == opp_opp_list[j,1]) %>%
                           select(., team_id, org_id, result))
    
  }
  
}





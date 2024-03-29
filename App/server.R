#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

#-------------------------------------------------------------------------------
# SETUP
#-------------------------------------------------------------------------------
options(shiny.fullstacktrace = TRUE)

master_team_list <- read.csv("data/master.csv", header = T, stringsAsFactors = F) %>%
  arrange(., team)

# Add unique ID to each game
full_schedules <- read.csv("data/full_schedules.csv", header = T, stringsAsFactors = F) %>%
  mutate(., date_str = date,
         date = mdy(date),
         team_id = as.integer(team_id)) %>%
  select(., team_id, org_id, date_str, date, opponent, location, result, score,
         everything()) %>%
  filter(., date < '2018-11-30') %>%
  mutate(., uni_id = group_indices(., team_id, org_id, date_str))


#-------------------------------------------------------------------------------
# DEFINE HELPER FUNCTIONS
#-------------------------------------------------------------------------------
calculate_rpi <- function(start_team_id){
  
  list_opp <- full_schedules %>%
    filter(., team_id == start_team_id) %>%
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
  
  # .25(team_sch) + .5(opp_sch) + .25(opp_opp_sch)
  
  0.25*mean(team_sch$result == "W", na.rm = TRUE) + 0.5*mean(opp_sch$result == "W", na.rm = TRUE) + 0.25*mean(opp_opp_sch$result == "W", na.rm = TRUE)
  
  #mean(opp_opp_sch$result == "W", na.rm = TRUE)
  
}

recalculate_rpi <- function(start_team_id, game_to_replace, date_of_game_to_replace, new_game, des_result){
  
  # get date object to replace
  start_team_id <- as.integer(start_team_id)
  game_to_replace <- as.character(game_to_replace)
  date_of_game_to_replace_format <- mdy(date_of_game_to_replace)
  
  replace_org <- filter(master_team_list, team == game_to_replace) %>%
    pull(., team_id) %>%
    as.integer(.)
  
  list_opp <- full_schedules %>%
    filter(., team_id == start_team_id,
           !(opponent == game_to_replace & date == date_of_game_to_replace_format)) %>%
    add_row(., team_id = as.integer(start_team_id),
            org_id = replace_org,
            date_str = date_of_game_to_replace,
            date = date_of_game_to_replace_format,
            opponent = game_to_replace,
            location = "",
            result = des_result,
            score = "",
            uni_id = NULL) %>%
    select(., org_id, uni_id)
  
  team_sch <- full_schedules %>%
    filter(., team_id == start_team_id) %>%
    select(., team_id, org_id, result)
  
  opp_sch <- NULL
  opp_opp_sch <- NULL
  
  
  for(i in seq(1:nrow(list_opp))){
    opp_sch <- rbind(opp_sch, full_schedules %>%
                       filter(., team_id == list_opp[i,1],
                              as.integer(uni_id) != list_opp[i,2]) %>%
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
  
  # .25(team_sch) + .5(opp_sch) + .25(opp_opp_sch)
  
  0.25*mean(team_sch$result == "W", na.rm = TRUE) + 0.5*mean(opp_sch$result == "W", na.rm = TRUE) + 0.25*mean(opp_opp_sch$result == "W", na.rm = TRUE)
  
}



# Define server logic
shinyServer(function(input, output) {
  
  # get schedule of selected team
  output$output_schedule <- renderTable(
    {
      
      # get selected team's team_id
      start_team_id <- master_team_list %>%
        filter(., team == input$start_team) %>%
        pull(., team_id)
      
      # render schedule
      full_schedules %>%
        filter(., team_id == start_team_id) %>%
        select(., Date = date_str, Opponent = opponent, Location = location, 
               Result = result, Score = score)
    }
  )
  
  # get opponent to replace
  output$selected_game_replace <- renderUI(
    {
      start_team_id <- master_team_list %>%
        filter(., team == input$start_team) %>%
        pull(., team_id)
      
      opponent_to_replace <- full_schedules %>%
        filter(., team_id == start_team_id) %>%
        pull(., opponent)
      
      selectInput("game_to_replace", 
                  "Choose a game to replace", 
                  opponent_to_replace, 
                  selected = NULL)
    }
  )
  
  # get date of opponent to replace
  output$selected_date_replace <- renderUI(
    {
      start_team_id <- master_team_list %>%
        filter(., team == input$start_team) %>%
        pull(., team_id)
      
      list_of_opponent_dates <- full_schedules %>%
        filter(., team_id == start_team_id, opponent == input$game_to_replace) %>%
        pull(., date_str)
      
      selectInput("date_of_game_replaced", 
                  "Pick a date", 
                  list_of_opponent_dates)
    }
  )
  
  # get header for unmodified rpi results
  output$rpi_unmod_header <- renderText(
    {
      "Unmodified RPI"
    }
  )
  
  # get header for modified rpi results
  output$rpi_mod_header <- renderText(
    {
      "Modified RPI"
    }
  )
  
  # calculate and format unmodified rpi results
  output$rpi_unmod_calculated <- renderUI(
    {
      
      team_id <- filter(master_team_list, team == input$start_team) %>%
        pull(., team_id) %>%
        as.integer(.)
      
      game_to_replace <- input$game_to_replace
      date_of_game_replaced <- input$date_of_game_replaced
      selected_team_new <- input$selected_team_new
      result <- input$win_lose
      
      rpi_before <- calculate_rpi(team_id)
      rpi_after <- recalculate_rpi(team_id, game_to_replace, date_of_game_replaced, selected_team_new, result)
      #rpi_after <- 1.123456789
      
      HTML(
        paste0("Before: ", round(rpi_before, 8), "<br/>", 
               "After: ", round(rpi_after, 8), "<br/>")
      )
    }
  )
  
  # calculate and format modified rpi results
  output$rpi_mod_calculated <- renderUI(
    {
      rpi_before <- master_team_list %>%
        filter(., team == input$start_team) %>%
        pull(., unmod_rpi)
      
      HTML(
        paste0("Before: ", round(rpi_before, 4), "<br/>", 
               "After: ", "None", "<br/>")
      )
    }
  )
  
  # get bugging information
  output$DEBUG <- renderText(
    {
      
    }
  )
  
})

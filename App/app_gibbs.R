#-------------------------------------------------------------------------------
# LOAD DEPENDENCIES
#-------------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)

#-------------------------------------------------------------------------------
# SETUP
#-------------------------------------------------------------------------------
master_team_list <- read.csv("./data/master.csv", header = T, stringsAsFactors = F) %>%
  arrange(., team)
full_schedules <- read.csv("./data/full_schedules.csv", header = T, stringsAsFactors = F) %>%
  mutate(., date_str = date,
         date = mdy(date),
         team_id = as.integer(team_id)) %>%
  select(., team_id, org_id, date_str, date, opponent, location, result, score,
         everything()) %>%
  filter(., date < '2018-11-30')

#-------------------------------------------------------------------------------
# DEFINE HELPER FUNCTIONS
#-------------------------------------------------------------------------------
recalculate_rpi <- function(start_team_id, game_to_replace, date_of_game_to_replace, new_game, result){
  
  # get date object to replace
  date_of_game_to_replace <- mdy(date_of_game_to_replace)
  
  # get selected teams full schedule without game to remove
  schedule_minus_game <- full_schedules %>%
    filter(., team_id == start_team_id,
           !(opponent == game_to_replace & date == date_of_game_to_replace))
  
}

calculate_rpi <- function(start_team_id, game_to_replace, date_of_game_to_replace, new_game, result){
  
  # get date object to replace
  date_of_game_to_replace <- mdy(date_of_game_to_replace)
  
}

#-------------------------------------------------------------------------------
# DEFINE SERVER LOGIC
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
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
      rpi_before <- master_team_list %>%
        filter(., team == input$start_team) %>%
        pull(., unmod_rpi)
      
      HTML(
        paste0("Before: ", round(rpi_before, 4), "<br/>", 
               "After: ", "round(rpi_after, 4)", "<br/>")
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
               "After: ", "round(rpi_after, 4)", "<br/>")
      )
    }
  )
  
  # get bugging information
  output$DEBUG <- renderText(
    {
      
    }
  )
}

#-------------------------------------------------------------------------------
# DEFINE USER INTERFACE
#-------------------------------------------------------------------------------
ui <- fluidPage(
  
  # application title
  titlePanel("Volleyball RPI Calculator"),
  
  # input section
  sidebarLayout(
    sidebarPanel(
      selectInput("start_team", "Choose a Team", master_team_list$team),
      uiOutput("selected_game_replace"),
      uiOutput("selected_date_replace"),
      selectInput("selected_team_new", "Choose new opponent", master_team_list$team),
      selectInput("win_lose", "Win or Lose", c("W", "L")),
      # show final results
      tags$b(textOutput("rpi_unmod_header")),
      htmlOutput("rpi_unmod_calculated"),
      tags$br(),
      tags$b(textOutput("rpi_mod_header")),
      htmlOutput("rpi_mod_calculated"),
      # render bugging info
      textOutput("DEBUG")
    ),
    mainPanel(
      tableOutput("output_schedule")
    )
  )
)

#-------------------------------------------------------------------------------
# RUN APPLICATION
#-------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

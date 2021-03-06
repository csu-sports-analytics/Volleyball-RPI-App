#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
#library(DT)

## Setup

master_team_list <- read.csv("data/master.csv", header = T, stringsAsFactors = F) %>%
  arrange(.,team)
full_schedules <- read.csv("data/full_schedules.csv", header = T, stringsAsFactors = F) %>%
  mutate(., date = lubridate::mdy(date)) %>%
  filter(., date < '2018-11-30') #%>%
  #group_by(., team_id) %>%
  #mutate(., index = row_number()) %>%
  #ungroup(.)

## Functions

# make two functions: one for unmodified RPI with original schedule, one for unmodified with "new" schedule.

recalculate_rpi <- function(start_team_id, game_to_replace, date_of_game_to_replace, new_game, result) {
  
  schedule <- filter(full_schedules, team_id == start_team_id) %>%
    mutate(., date = paste(date))
  game_to_remove <- filter(schedule, date == date_of_game_to_replace) %>%
    filter(., opponent == game_to_replace) %>%
    mutate(., date = paste(date))
  
  game_to_remove

  schedule_minus_game <- schedule %>% 
    filter(., . != game_to_remove)

  schedule_minus_game
  
}

calculate_rpi <- function(start_team_id){
  
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Volleyball RPI Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("start_team", "Choose a Team", master_team_list$team),
         
         textOutput("rpi_calculated"),
         
         uiOutput("selected_game_replace"),
         
         uiOutput("selected_date_replace"),
         
         selectInput("selected_team_new", "Choose new opponent", master_team_list$team),
         
         selectInput("win_lose", "Win or Lose", c("W", "L")),
         
         textOutput("DEBUG")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("output_schedule")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$output_schedule <- renderTable({

    start_team_id <- filter(master_team_list, team == input$start_team)$team_id
    
    team_schedule <- filter(full_schedules, team_id == start_team_id) %>%
      mutate(., date = paste(date))
    
    team_schedule$team_id <- NULL
    team_schedule$org_id <- NULL
    team_schedule$location <- NULL
    
    team_schedule

    team_id <- filter(master_team_list, team == input$start_team)$team_id
    game_to_replace <- input$game_to_replace
    date_of_game_replaced <- input$date_of_game_replaced
    selected_team_new <- input$selected_team_new
    result <- input$win_lose

    recalculate_rpi(team_id, game_to_replace, date_of_game_replaced, selected_team_new, result) #%>%
      #mutate(., date = paste(date))
  })
  
  output$selected_game_replace <- renderUI({
    
    start_team_id <- filter(master_team_list, team == input$start_team)$team_id
    
    selectInput("game_to_replace", "Choose a game to replace", filter(full_schedules, team_id == start_team_id)$opponent, selected = NULL)
    
  })
  
  output$selected_date_replace <- renderUI({
    
    start_team_id <- filter(master_team_list, team == input$start_team)$team_id
    
    list_of_opponent_dates <- full_schedules %>%
      filter(., team_id == start_team_id) %>%
      filter(., opponent == input$game_to_replace)
    
    selectInput("date_of_game_replaced", "Pick a date", list_of_opponent_dates$date)
    
  })
  
  output$rpi_calculated <- renderText({
    
    rpi_before <- filter(master_team_list, team == input$start_team)$unmod_rpi
    # Once calculate_rpi() is working, this line will replace the hard coded rpi.
    #rpi_before <- calculate_rpi(filter(master_team_list, team == input$start_team)$team_id)
    
    team_id <- filter(master_team_list, team == input$start_team)$team_id
    game_to_replace <- input$game_to_replace
    date_of_game_replaced <- input$date_of_game_replaced
    selected_team_new <- input$selected_team_new
    result <- input$win_lose
    
    paste0("Unmodified RPI before: ", rpi_before, "\n", "Unmodified RPI after: ", "testing")
    
  })
  
  output$DEBUG <- renderText({
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


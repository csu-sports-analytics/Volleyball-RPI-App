#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
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
))
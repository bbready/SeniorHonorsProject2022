#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

league_details <- read_csv("country_options.csv", show_col_types = FALSE)

countries = league_details$country

# Define UI for application 
shinyUI(fluidPage(
    
    # Application title
    titlePanel("BIFA"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("country","Choose a league", choices=countries),
            numericInput("year", label = "Enter a season (ex. for 2020-2021 season, enter 2020)", value = 2021)
        ),
        
        mainPanel(
            
            tabsetPanel(
                tabPanel(
                    "Standings", dataTableOutput('standings'),
                    downloadButton("downloadStandings", "Download Standings Table")
                ),
                tabPanel(
                    "Games", dataTableOutput('games'),
                    downloadButton("downloadGames", "Download Games File")
                ),
                tabPanel(
                    "RPI", dataTableOutput('rpi'),
                    downloadButton("downloadRPI", "Download RPI Table")
                ),
                tabPanel(
                    "BIFA Rankings", 
                    textOutput('BIFAtext'),
                    "-----------------",
                    dataTableOutput('poisson'),
                    downloadButton("downloadBIFA", "Download BIFA Rankings")
                ),
                tabPanel(
                    "Forecasts", dataTableOutput('forecasts'),
                    downloadButton("downloadForecasts", "Download Forecasts")
                ),
                
                tabPanel(
                    "About",
                    h3("About BIFA", align = "center"),
                    h5("Bready’s International Football App, or BIFA, is an R shiny app designed to download current and 
                       past match results from any of Europe’s top 25 leagues. In addition to individual match results, 
                       BIFA provides up to date standings tables, RPI based rankings, and offensive and defensive rankings 
                       based on a Poisson probability model. Using the model, BIFA generates the expected goals for each team 
                       for past and future matches."),
                    h5("This app was developed by Brenden Bready for an honors Senior Year Experience in Statistics at St. Lawrence 
                       University during the Spring of 2022. The project was supervised by Dr. Robin Lock, Burry Professor of Statistics.
                       A special thank you to Dr. Lock for all of his guidance during this project."),
                    h5("All data for this project is scraped from transfermarkt.us, with a citation to them listed below."),
                    h5("Transfermarkt. (2022). From https://www.transfermarkt.us/ ")
                )
            )
        )
    )
)) 

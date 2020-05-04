#################################################
#                                               #
# STA 404 - Final Project                       #
# Brad Schmitz                                  #
#                                               #
# TMBb Database - The Simpsons                  #
#                                               #
# Shiny App                                     #
#                                               #
#################################################

# ====================================================== 1.) Load the necessary libraries and settings
# Load libraries
library(TMDb)
library(shiny)
library(tidyverse)
require(forcats)
library(plotly)


# ====================================================== 2.) Load Datasets
# Set the working directory
setwd("~/Classes (Spring 2020)/STA 404")

# Load the simpsons dataset
load("Simpsons_Episodes.RData")


# ====================================================== 3.) Data Handling
# Group by episode and season
simpsons_episodes <- simpsons_episodes %>%
    group_by(season_number, episode_number)

# Create a list of plot types
plot_types <- c("Ratings Over Time (Per Episode)" = "ratings",
                "Guest Stars Over Time (Per Season)" = "guest_stars",
                "Popularity of Guest Stars" = "guest_star_pop",
                "Popularity of Directors" = "director_pop",
                "Popularity of Writers" = "writer_pop")

# ====================================================== 4.) Create shiny app
##### Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel(title = "The Simpsons: Popularity Over Time",
               windowTitle = "Brad Schmitz - STA 404 Final Project"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            # Selection menu for choosing plot
            selectInput(inputId="plottype", label="Plot Type", choices = plot_types),
            
            # Slider panel for choosing season
            sliderInput(inputId="season", label="Season", 
                        min = 1,
                        max = 27,
                        value = c(1, 27)),
            
            # Extra UI output
            uiOutput(outputId = "ui")

        ),

        # Show a plot
        mainPanel(
           plotlyOutput("plot")
        )
    )
)

##### Define server logic
server <- function(input, output) {
    
    # Render the plot to the window
    output$plot <- renderPlotly({
        
        # ====================================== 1.) Create a line plot of the average rating between two seasons
        if (input$plottype == "ratings") {
            
            # Create the basic plot structure
            line_plot_ratings <- ggplot(subset(simpsons_episodes, season_number >= input$season[1] & 
                                                   season_number <= input$season[2])) +
                labs(x="Episode Number (Overall)", y="Average Rating", caption="Source: TMDb Database")
            
            # If we should colorize by season, do so
            if (input$separateseasons == TRUE) {
                line_plot_ratings <- line_plot_ratings + 
                    geom_path(aes(x=total_episode_num, y=imdb_rating, group=1, color=season_number,
                                  text=paste("Season", season_number,"- Episode", episode_number,
                                             "<br>Episode Name:", episode_title,
                                             "<br>Total Episode Number", total_episode_num,
                                             "<br>Air Date", air_date,
                                             "<br>Average Rating:", imdb_rating)), 
                              size=0.5)
            }
            else {
                line_plot_ratings <- line_plot_ratings + 
                    geom_path(aes(x=total_episode_num, y=imdb_rating, group=1,
                                  text=paste("Season", season_number,"- Episode", episode_number,
                                             "<br>Episode Name:", episode_title,
                                             "<br>Total Episode Number", total_episode_num,
                                             "<br>Air Date", air_date,
                                             "<br>Average Rating:", imdb_rating)), 
                              size=0.5)
            }
            
            # Check if seasons are the same
            if (input$season[1] == input$season[2]) {
                
                # Convert to a plotly object
                ggplotly(line_plot_ratings, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Rating Per Episode",
                                                      "<br>",
                                                      "<sup>",
                                                      "Season ", input$season[1],
                                                      "</sup>")),
                           margin=list(t=75))

            }
            # If not, adjust title properly
            else {
                
                # Convert to a plotly object
                ggplotly(line_plot_ratings, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Rating Per Episode",
                                                      "<br>",
                                                      "<sup>",
                                                      "Seasons ", input$season[1], " Through ", input$season[2],
                                                      "</sup>")),
                           margin=list(t=75))
            }
            
        # ====================================== 2.) Create a plot comparing guest stars per season
        } else if (input$plottype == "guest_stars") {
            
            # Create the basic plot structure
            line_plot_guest_stars <- ggplot(subset(simpsons_episodes, season_number >= input$season[1] & 
                                                   season_number <= input$season[2])) +
                labs(x="Season Number", y="Number of Guest Stars", caption="Source: TMDb Database")
            
            # If we should colorize by season, do so
            if (input$separateseasons == TRUE) {
                line_plot_guest_stars <- line_plot_guest_stars + 
                    geom_path(aes(x=season_number, y=avg_guest_stars, group=1, color=season_number,
                                  text=paste("Season", season_number,
                                             "<br>Avg. Number of Guest Stars:", avg_guest_stars, 
                                             "<br>Total Number of Guest Stars:", total_guest_stars, 
                                             "<br>Total Number of Episodes:", season_episode_num, 
                                             "<br>Average Rating:", avg_season_rating)), 
                              size=0.5)
            }
            else {
                line_plot_guest_stars <- line_plot_guest_stars + 
                    geom_path(aes(x=season_number, y=avg_guest_stars, group=1,
                                  text=paste("Season", season_number,
                                             "<br>Avg. Number of Guest Stars:", avg_guest_stars, 
                                             "<br>Total Number of Guest Stars:", total_guest_stars, 
                                             "<br>Total Number of Episodes:", season_episode_num, 
                                             "<br>Average Rating:", avg_season_rating)), 
                size=0.5)
            }
            
            # Check if seasons are the same
            if (input$season[1] == input$season[2]) {
                
                # Convert to a plotly object
                ggplotly(line_plot_guest_stars, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Number of Guest Stars (and Ratings) per Season",
                                                      "<br>",
                                                      "<sup>",
                                                      "Season ", input$season[1],
                                                      "</sup>")),
                           margin=list(t=75))
                
            }
            # If not, adjust title properly
            else {
                
                # Convert to a plotly object
                ggplotly(line_plot_guest_stars, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Number of Guest Stars (and Ratings) per Season",
                                                      "<br>",
                                                      "<sup>",
                                                      "Seasons ", input$season[1], " Through ", input$season[2],
                                                      "</sup>")),
                           margin=list(t=75))
            }
        }

    })
    
    # Renders the UI dynamically
    output$ui <- renderUI({

        # If the ratings plot...
        if (input$plottype == "ratings") {
            # Checkbox for differentiating by season
            checkboxInput(inputId = "separateseasons", label="Separate by Season?")
            
            # Checkbox for averaging by season
            checkboxInput(inputId = "averageseasons", label="Average Seasons?")
        }
        # If the guest stars over time plot...
        else if (input$plottype == "guest_stars") {
                # Checkbox for differentiating by season
                checkboxInput(inputId = "separateseasons", label="Separate by Season?")
        } 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

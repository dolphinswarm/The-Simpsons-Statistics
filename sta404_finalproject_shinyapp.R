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
# Load the simpsons dataset
load("Simpsons_Episodes.RData")
load("Simpsons_Episodes_Guest_Stars.RData")
load("Simpsons_Episodes_Crew.RData")


# ====================================================== 3.) Data Handling
# Group by episode and season
simpsons_episodes <- simpsons_episodes %>%
    group_by(season_number, episode_number)

# Create a list of plot types
plot_types <- c("Ratings Over Time (Per Season)" = "ratings_season",
                "Ratings Over Time (Per Episode)" = "ratings_episode",
                "Guest Stars Over Time (Per Season)" = "guest_stars",
                "Most Popular Guest Stars" = "guest_star_pop",
                "Most Frequent Guest Stars" = "guest_star_freq",
                "Most Popular Directors" = "director_pop",
                "Most Popular Writers" = "writer_pop")

# ====================================================== 4.) Create shiny app
######
# UI #
######
ui <- fluidPage(

    # Application title
    titlePanel(title = "The Simpsons In Statistics",
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
            textOutput(outputId="error"),
            plotlyOutput(outputId="plot")
        )
    )
)

##########
# Server #
##########
server <- function(input, output) {
    
    # DYNAMIC UI AREA RENDERING
    output$ui <- renderUI({
        
        # If the ratings plot...
        if (input$plottype == "ratings_season" || input$plottype == "guest_stars") {
            
            # Checkbox for averaging by season
            checkboxInput(inputId = "variability", label="Show Variability?")
        }
        # If the guest stars over time plot...
        # else if () {
        #     
        #     # Checkbox for differentiating by season
        #     checkboxInput(inputId = "correlatebyaveragerating", label="Correlate by Rating?")
        # } 
    })
    
    # ERROR RENDERING
    output$error <- renderText({
        if (is.na(input$variability) | is.null(input$variability) | length(input) == 0) return()
        
        # Do we have an invalid plot? If so, render the text
        if ((input$plottype == "ratings_season" & input$season[1] == input$season[2] & input$variability == FALSE)) {
            "This plot type is invalid - please select a different plot!"
        }
    })
    
    # RENDER PLOT
    output$plot <- renderPlotly({
        # ====================================== 1.) Ratings Over Time (Per Season)
        if (input$plottype == "ratings_season") {
            
            # Create the basic plot structure
            line_plot_ratings <- ggplot(subset(simpsons_episodes, season_number >= input$season[1] & 
                                                                  season_number <= input$season[2])) +
                labs(x="Season", y="Average Rating", caption="Source: TMDb Database") + 
                scale_y_continuous(limits=c(5,9.3), labels=seq(5,9,1))
            
            # Add line, if we should
            if (input$season[1] != input$season[2]) {
                line_plot_ratings <- line_plot_ratings + 
                    geom_path(aes(x=season_number, y=avg_season_rating, group=1,
                                  text=paste("Season", season_number,
                                             "<br>Total Number of Episodes:", season_episode_num, 
                                             "<br>Average Rating:", avg_season_rating)), 
                              size=0.5)
            }
            
            # Add distributions, if we should
            if (input$variability == TRUE) {
                line_plot_ratings <- line_plot_ratings + 
                    geom_boxplot(aes(x=season_number, y=imdb_rating,
                                     text=paste("Season", season_number,
                                                "<br>Total Number of Episodes:", season_episode_num, 
                                                "<br>Average Rating:", avg_season_rating)),
                                 color="gray50")
            }
            
            line_plot_ratings <- line_plot_ratings 
            
            # ADJUST TITLE
            # Seasons are not equal
            if (input$season[1] != input$season[2]) {
                
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
            # Seasons are equal but variability on
            else if (input$season[1] == input$season[2] & input$variability == TRUE) {
                # Convert to a plotly object
                ggplotly(line_plot_ratings, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Variablity In Episode Ratings Per Season",
                                                      "<br>",
                                                      "<sup>",
                                                      "Seasons ", input$season[1],
                                                      "</sup>")),
                           margin=list(t=75))
            }
            # Seasons are equal - BAD!
            else {
                ggplotly(ggplot() + theme_minimal()) %>%
                    config(displayModeBar = F)
            }
            
        # ====================================== 2.) Ratings Over Time (Per Episode)
        } else if (input$plottype == "ratings_episode") { 
            line_plot_ratings <- ggplot(subset(simpsons_episodes, season_number >= input$season[1] & 
                                                                  season_number <= input$season[2])) +
                geom_path(aes(x=total_episode_num, y=imdb_rating, group=1,
                              text=paste("Season", season_number,"- Episode", episode_number,
                                         "<br>Episode Name:", episode_title,
                                         "<br>Total Episode Number:", total_episode_num,
                                         "<br>Air Date:", air_date,
                                         "<br>Average Rating:", imdb_rating)), 
                          size=0.5)  +
                labs(x="Episode Number (Overall)", y="Average Rating", caption="Source: TMDb Database")
            
            # Seasons are equal
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
            # Seasons are not equal
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
            
        # ====================================== 3.) Guest Stars Over Time (Per Season)
        } else if (input$plottype == "guest_stars") {
            
            # Create the basic plot structure
            line_plot_ratings <- ggplot(subset(simpsons_episodes, season_number >= input$season[1] & 
                                                   season_number <= input$season[2])) +
                labs(x="Season Number", y="Avg. Number of Guest Stars\nPer Episode", caption="Source: TMDb Database") 
                # scale_y_continuous(limits=c(5,9.3), labels=seq(5,9,1))
            
            # Add line, if we should
            if (input$season[1] != input$season[2]) {
                line_plot_ratings <- line_plot_ratings + 
                    geom_path(aes(x=season_number, y=avg_guest_stars, group=1,
                                  text=paste("Season", season_number,
                                             "<br>Avg. Number of Guest Stars:", avg_guest_stars, 
                                             "<br>Total Number of Guest Stars:", season_guest_star_num, 
                                             "<br>Total Number of Episodes:", season_episode_num, 
                                             "<br>Average Rating:", avg_season_rating)), 
                              size=0.5)
                    
            }
            
            # Add distributions, if we should
            if (input$variability == TRUE) {
                line_plot_ratings <- line_plot_ratings + 
                    geom_boxplot(aes(x=season_number, y=episode_guest_star_num,
                                     text=paste("Season", season_number,
                                                "<br>Total Number of Episodes:", season_episode_num, 
                                                "<br>Average Rating:", avg_season_rating)),
                                 color="gray50")
            }
            
            # ADJUST TITLE
            # Seasons are not equal
            if (input$season[1] != input$season[2]) {
                
                # Convert to a plotly object
                ggplotly(line_plot_ratings, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Number of Guest Stars per Season",
                                                      "<br>",
                                                      "<sup>",
                                                      "Seasons ", input$season[1], " Through ", input$season[2],
                                                      "</sup>")),
                           margin=list(t=75))
            } 
            # Seasons are equal but variability on
            else if (input$season[1] == input$season[2] & input$variability == TRUE) {
                # Convert to a plotly object
                ggplotly(line_plot_ratings, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Number of Guest Stars per Season",
                                                      "<br>",
                                                      "<sup>",
                                                      "Seasons ", input$season[1],
                                                      "</sup>")),
                           margin=list(t=75))
            }
            # Seasons are equal - BAD!
            else {
                ggplotly(ggplot() + theme_minimal()) %>%
                    config(displayModeBar = F)
            }
            
        # ====================================== 3.) Create a plot comparing guest stars per season
        } else if (input$plottype == "guest_star_freq") {
            
            # Create an on-the-fly dataset for finding the top viewers per season
            tmp_guest_star_data <- subset(simpsons_episodes, season_number >= input$season[1] & 
                                              season_number <= input$season[2]) %>%
                select(guest_name, episode_title, imdb_rating) %>%
                distinct() %>% 
                group_by(guest_name) %>%
                summarize(total_appearances = n(),
                          average_ratings = round(mean(imdb_rating),digits=2)) %>%
                drop_na() %>%
                mutate(overall_avg = mean(total_appearances)) %>%
                filter(total_appearances > overall_avg)
            
            # Create the basic plot structure
            plot_guest_stars_2 <- ggplot(tmp_guest_star_data) 
            
            plot_guest_stars_2 <- plot_guest_stars_2 + 
                geom_bar(aes(x=reorder(guest_name, -total_appearances), y=total_appearances,
                               text=paste("Name:", guest_name,
                                          "<br>Total Appearances:", total_appearances, 
                                          "<br>Average Episode Rating:", average_ratings)), 
                           stat="identity") +
                labs(x="Guest Star", y="Total Appearances", caption="Source: TMDb Database") +
                theme(axis.text.x=element_blank())
        }

    })
}

# Run the application 
shinyApp(ui = ui, server = server)

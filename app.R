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
library(shinyWidgets)


# ====================================================== 2.) Load Datasets
# Load the simpsons dataset
load("Simpsons_Episodes.RData")


# ====================================================== 3.) Data Handling
# Group by episode and season
simpsons_episodes <- simpsons_episodes %>%
    group_by(season_number, episode_number)

# Create a list of plot types
plot_types <- c("Ratings Over Time (Per Season)" = "ratings_season",
                "Ratings Over Time (Per Episode)" = "ratings_episode",
                "Guest Stars Over Time (Per Season)" = "guest_stars",
                "Guest Stars - More Info" = "Guest Star",
                "Writers - More Info" = "Writer",
                "Directors - More Info" = "Director")

# Create a list of sorting variables
extra_types <-c("Most Popular" = "popular_most",
                "Least Popular" = "popular_least",
                "Frequency" = "frequency",
                "Popularity by Frequency" = "both")

# ====================================================== 4.) Create shiny app
######
# UI #
######
ui <- fluidPage(
    
    # Redo themes
    setBackgroundColor("skyblue"),

    # Application title
    titlePanel(title=div(img(src="title.png", width=200), "In Statistics")),
    
    # Subtitles
    headerPanel(title="Brad Schmitz - STA 404 Final Project"),
    headerPanel(title="Data Sources: TMDb and data.world"),
    tags$h1(tags$style("h1 { font-size: 16px; margin: 2px; }")),

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
        
        # If a by-season plot...
        if (input$plottype == "ratings_season" | input$plottype == "guest_stars") {
            
            # Checkbox for averaging by season
            checkboxInput(inputId = "variability", label="Show Variability?")
        }
        # If an extra info plot
        else if (input$plottype == "Guest Star" | input$plottype == "Writer" | input$plottype == "Director") {

            # Selection menu for choosing plot
            selectInput(inputId="extratype", label="Extra Type", choices = extra_types)
        }
    })
    
    # ERROR RENDERING
    output$error <- renderText({
        # Do we have an invalid plot? If so, render the text
        if (((input$plottype == "ratings_season" | input$plottype == "guest_stars") & 
              input$season[1] == input$season[2] & input$variability == FALSE)) {
            "This plot type is invalid - please select a different plot!"
        }
    })
    
    # RENDER PLOT
    output$plot <- renderPlotly({
        # ====================================== 1.) Ratings Over Time (Per Season)
        if (input$plottype == "ratings_season") {
            
            # Handle error case
            if (input$season[1] == input$season[2] & input$variability == FALSE) {
                
                ggplotly(ggplot() + theme_minimal()) %>%
                    config(displayModeBar = F)
            } 
            # Else, good plot
            else {
                
                # Create the basic plot structure
                line_plot_ratings <- ggplot(subset(simpsons_episodes, season_number >= input$season[1] & 
                                                       season_number <= input$season[2])) +
                    labs(x="Season", y="Average Rating", caption="Source: TMDb Database") + 
                    scale_y_continuous(limits=c(5,9.3), labels=seq(5,9,1)) +
                    geom_path(aes(x=season_number, y=avg_season_rating, group=1,
                                  text=paste("Season", season_number,
                                             "<br>Total Number of Episodes:", season_episode_num, 
                                             "<br>Average Rating:", avg_season_rating)), 
                              size=0.5, color="darkblue")
                
                # Add distributions, if we should
                if (input$variability == TRUE) {
                    line_plot_ratings <- line_plot_ratings + 
                        geom_boxplot(aes(x=season_number, y=imdb_rating,
                                         text=paste("Season", season_number,
                                                    "<br>Total Number of Episodes:", season_episode_num, 
                                                    "<br>Average Rating:", avg_season_rating)),
                                     color="gray50")
                }
                
                
                # Create plotly object and adjust title
                ggplotly(line_plot_ratings, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Rating Per Episode",
                                                      "<br>",
                                                      "<sup>",
                                                      ifelse(input$season[1] != input$season[2], 
                                                             paste0("Seasons ", input$season[1], " Through ", input$season[2]), 
                                                             paste0("Season ", input$season[1])),
                                                      "</sup>")),
                           hoverlabel=list(bgcolor="white", font.color="black"),
                           margin=list(t=75))
            }
            
        # ====================================== 2.) Ratings Over Time (Per Episode)
        } else if (input$plottype == "ratings_episode") { 
            
            # Create the basic plot structure
            line_plot_ratings <- ggplot(subset(simpsons_episodes, season_number >= input$season[1] & 
                                                                  season_number <= input$season[2])) +
                geom_path(aes(x=total_episode_num, y=imdb_rating, group=1,
                              text=paste("Season", season_number,"- Episode", episode_number,
                                         "<br>Episode Name:", episode_title,
                                         "<br>Total Episode Number:", total_episode_num,
                                         "<br>Air Date:", air_date,
                                         "<br>Average Rating:", imdb_rating)), 
                          size=0.5, color="darkblue")  +
                labs(x="Episode Number (Overall)", y="Average Rating", caption="Source: TMDb Database")
            
            # Create plotly object and adjust title
            ggplotly(line_plot_ratings, tooltip="text") %>%
                config(displayModeBar = F) %>%
                layout(title = list(text = paste0("Average Rating Per Episode",
                                                  "<br>",
                                                  "<sup>",
                                                  ifelse(input$season[1] != input$season[2], 
                                                         paste0("Seasons ", input$season[1], " Through ", input$season[2]), 
                                                         paste0("Season ", input$season[1])),
                                                  "</sup>")),
                       hoverlabel=list(bgcolor="white", font.color="black"),
                       margin=list(t=75))
            
        # ====================================== 3.) Guest Stars Over Time (Per Season)
        } else if (input$plottype == "guest_stars") {
            
            # Handle error case
            if (input$season[1] == input$season[2] & input$variability == FALSE) {
                
                ggplotly(ggplot() + theme_minimal()) %>%
                    config(displayModeBar = F)
            } 
            # Else, good plot
            else {
                # Create the basic plot structure
                line_plot_ratings <- ggplot(subset(simpsons_episodes, season_number >= input$season[1] & 
                                                       season_number <= input$season[2])) +
                    labs(x="Season Number", y="Avg. Number of Guest Stars\nPer Episode", caption="Source: TMDb Database") +
                    geom_path(aes(x=season_number, y=avg_guest_stars, group=1,
                                      text=paste("Season", season_number,
                                                 "<br>Avg. Number of Guest Stars:", avg_guest_stars, 
                                                 "<br>Total Number of Guest Stars:", season_guest_star_num, 
                                                 "<br>Total Number of Episodes:", season_episode_num, 
                                                 "<br>Average Rating:", avg_season_rating)), 
                                  size=0.5, color="darkblue")
                    
                # Add distributions, if we should
                if (input$variability == TRUE) {
                    line_plot_ratings <- line_plot_ratings + 
                        geom_boxplot(aes(x=season_number, y=episode_guest_star_num,
                                         text=paste("Season", season_number,
                                                    "<br>Total Number of Episodes:", season_episode_num, 
                                                    "<br>Average Rating:", avg_season_rating)),
                                     color="gray50")
                }
                    
                # Create plotly object and adjust title
                ggplotly(line_plot_ratings, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Number of Guest Stars per Season",
                                                      "<br>",
                                                      "<sup>",
                                                      ifelse(input$season[1] != input$season[2], 
                                                             paste0("Seasons ", input$season[1], " Through ", input$season[2]), 
                                                             paste0("Season ", input$season[1])),
                                                      "</sup>")),
                           hoverlabel=list(bgcolor="white", font.color="black"),
                           margin=list(t=75))
            }
            
        # ====================================== 4-6.) GUEST STARS / WRITERS / DIRECTORS PER SEASON
        } else if (input$plottype == "Guest Star" | input$plottype == "Writer" | input$plottype == "Director") {
            
            # Create an on-the-fly dataset for finding the top viewers per season
            tmp_data <- subset(simpsons_episodes, season_number >= input$season[1] & 
                                              season_number <= input$season[2])
            
            # Choose writers/ directors
            if (input$plottype != "Guest Star") {
                tmp_data <- tmp_data %>%
                    filter(job==input$plottype) %>%
                    select(crew_name, episode_title, imdb_rating) %>%
                    distinct() %>% 
                    group_by(crew_name) %>%
                    summarize(total_appearances = n(),
                              average_ratings = round(mean(imdb_rating),digits=2)) %>%
                    drop_na() %>%
                    rename(name=crew_name)
            }
            # Else, guest
            else {
                tmp_data <- tmp_data %>%
                select(guest_name, episode_title, imdb_rating) %>%
                    distinct() %>% 
                    group_by(guest_name) %>%
                    summarize(total_appearances = n(),
                              average_ratings = round(mean(imdb_rating),digits=2)) %>%
                    drop_na() %>%
                    rename(name=guest_name)
            }

            
            # Create the basic plot structure
            plot_data <- NULL
            
            #  Plot popularity (most)
            if (input$extratype == "popular_most") {
                
                # Combine rows of same popularity and filter top 12
                tmp_data <- tmp_data %>%
                    group_by(average_ratings) %>%
                    mutate(all_of_rating = paste0(name, collapse = "\n- ")) %>%
                    ungroup() %>%
                    select(all_of_rating, average_ratings) %>%
                    distinct() %>%
                    top_n(n=12, wt=average_ratings)
                
                # Add to plot                
                plot_data <- ggplot(tmp_data) +
                    geom_bar(aes(x=reorder(all_of_rating, -average_ratings), y=average_ratings,
                                 text=paste0(input$plottype, "s:<br>- ", all_of_rating,
                                            "<br>Average Episode Rating of ", input$plottype, ": " , average_ratings)), 
                             stat="identity", fill="darkblue") +
                    labs(x=input$plottype, y=paste0("Average Episode Rating of ", input$plottype), caption="Source: TMDb Database") +
                    theme(axis.text.x=element_blank())
                
                # Create plotly object and adjust title
                ggplotly(plot_data, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Episode Rating of<br>Most Popular ", input$plottype, "s",
                                                      "<br>",
                                                      "<sup>",
                                                      ifelse(input$season[1] != input$season[2], 
                                                             paste0("Seasons ", input$season[1], " Through ", input$season[2]), 
                                                             paste0("Season ", input$season[1])),
                                                      "</sup>")),
                           hoverlabel=list(bgcolor="white", font.color="black"),
                           margin=list(t=90))
            }
            # Plot popularity (least)
            else if (input$extratype == "popular_least") {
                
                # Combine rows of same popularity and filter top 12
                tmp_data <- tmp_data %>%
                    group_by(average_ratings) %>%
                    mutate(all_of_rating = paste0(name, collapse = "\n- ")) %>%
                    ungroup() %>%
                    select(all_of_rating, average_ratings) %>%
                    distinct() %>%
                    top_n(n=-12, wt=average_ratings)
                
                # Add to plot                
                plot_data <- ggplot(tmp_data) +
                    geom_bar(aes(x=reorder(all_of_rating, average_ratings), y=average_ratings,
                                 text=paste0(input$plottype, "s:<br>- ", all_of_rating,
                                             "<br>Average Episode Rating of ", input$plottype, ": " , average_ratings)), 
                             stat="identity", fill="darkblue") +
                    labs(x=input$plottype, y=paste0("Average Episode Rating of ", input$plottype), caption="Source: TMDb Database") +
                    theme(axis.text.x=element_blank())
                
                # Create plotly object and adjust title
                ggplotly(plot_data, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Average Episode Rating of<br>Least Popular ", input$plottype, "s",
                                                      "<br>",
                                                      "<sup>",
                                                      ifelse(input$season[1] != input$season[2], 
                                                             paste0("Seasons ", input$season[1], " Through ", input$season[2]), 
                                                             paste0("Season ", input$season[1])),
                                                      "</sup>")),
                           hoverlabel=list(bgcolor="white", font.color="black"),
                           margin=list(t=90))
            } 
            # Plot frequency
            else if (input$extratype == "frequency") {
                
                # Filter data
                tmp_data <- tmp_data %>%
                    top_n(15, wt=total_appearances)
                
                # Add to plot                
                plot_data <- ggplot(tmp_data) +
                    geom_bar(aes(x=reorder(name, -total_appearances), y=total_appearances,
                             text=paste("Name:", name,
                                        "<br>Total Appearances:", total_appearances, 
                                        "<br>Average Episode Rating:", average_ratings)), 
                         stat="identity", fill="darkblue") +
                    labs(x=input$plottype, y="Total Appearances", caption="Source: TMDb Database") +
                    theme(axis.text.x=element_blank())
                
                # Create plotly object and adjust title
                ggplotly(plot_data, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Total Frequency of ", input$plottype, "s",
                                                      "<br>",
                                                      "<sup>",
                                                      ifelse(input$season[1] != input$season[2], 
                                                             paste0("Seasons ", input$season[1], " Through ", input$season[2]), 
                                                             paste0("Season ", input$season[1])),
                                                      "</sup>")),
                           hoverlabel=list(bgcolor="white", font.color="black"),
                           margin=list(t=75))
            }

                
            # Plot correlation
            else {
                # Add to plot                
                plot_data <- ggplot(tmp_data) +
                    geom_jitter(aes(x=total_appearances, y=average_ratings,
                                 text=paste("Name:", name,
                                            "<br>Total Appearances:", total_appearances, 
                                            "<br>Average Episode Rating:", average_ratings)), 
                             stat="identity", size=0.5, alpha=0.4, color="darkblue") +
                    labs(x="Total Appearances", y="Average Ratings", caption="Source: TMDb Database") +
                    coord_flip()
                
                # Create plotly object and adjust title
                ggplotly(plot_data, tooltip="text") %>%
                    config(displayModeBar = F) %>%
                    layout(title = list(text = paste0("Frequency vs. Popularity of ", input$plottype, "s",
                                                      "<br>",
                                                      "<sup>",
                                                      ifelse(input$season[1] != input$season[2], 
                                                             paste0("Seasons ", input$season[1], " Through ", input$season[2]), 
                                                             paste0("Season ", input$season[1])),
                                                      "</sup>")),
                           hoverlabel=list(bgcolor="white", font.color="black"),
                           margin=list(t=75))
            } 
        }

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
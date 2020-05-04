#################################################
#                                               #
# STA 404 - Final Project                       #
# Brad Schmitz                                  #
#                                               #
# TMBb Database - The Simpsons                  #
#                                               #
# Data Handler                                  #
#                                               #
#################################################

# ====================================================== 1.) Load the necessary libraries and settings
# Load libraries
library(TMDb)
library(tidyverse)

# ====================================================== 2.) Load Datasets
# Set the working directory
setwd("~/Classes (Spring 2020)/STA 404")

# Get each season of the Simpsons
seasons <- tv(api_key = "3d1b240ce44c8b72696e9a3c43e3f992", id = 456)$seasons
seasons <- seasons %>%
  # Filter out the Tracey Ullman shorts
  filter(season_number > 0 & season_number < 31)
  # Change the episodes in the most recent season to include only ones that have aired

# Create a blank dataset for all the episodes, guest stars, and crew
episodes <- data.frame()
guest_stars <- data.frame()
crew <- data.frame()

# Create a tracking variable for the total episode number
total_ep_num = 1

# Do a nested for-loop to get each episode
# For each season...
season_number = max(seasons$season_number)
for (i in 1:season_number) {
  
  # For each episode in the season
  for (j in 1:seasons[i,]$episode_count) {
    
    # Print the episode being acquired
    print(paste("### Getting Season", i, "- Episode",j))
    
    # Get the episode
    ep <- tv_episode("3d1b240ce44c8b72696e9a3c43e3f992", 456, season_number = i, episode_number = j)
      
    # If this episode had guest stars, add it to the guest stars database
    if (length(ep$guest_stars) > 0) {
      guest_stars_list <- data.frame(ep$guest_stars) %>%
        mutate(episode_id = ep$id,
               episode_name = ep$name,
               season_num = i,
               episode_num = j)
      guest_stars <- rbind(guest_stars, guest_stars_list)
    }
    
    # If this episode's crew is listed, add it to the database
    if (length(ep$crew) > 0) {
      crew_list <- data.frame(ep$crew) %>%
        mutate(episode_id = ep$id,
               episode_name = ep$name,
               season_num = i,
               episode_num = j)
      crew <- rbind(crew, crew_list)
    }
    
    # Convert the list to a dataframe
    ep <- data.frame(ep[-c(2,4)])
    ep <- ep %>%
      mutate(total_episode_num = total_ep_num)
    total_ep_num = total_ep_num + 1
    
    # Merge to the existing dataset
    episodes <- rbind(episodes, ep)
  }
}

# Load the Simpsons Ratings and Viewings data
# SOURCE: https://data.world/data-society/the-simpsons-by-the-data
ratings <- read_csv("simpsons_episodes.csv") %>%
  select(imdb_rating, imdb_votes, us_viewers_in_millions, season, number_in_season)
  

# ====================================================== 3.) Data Handling
# Calculate the number of guest stars per episode
guest_stars <- guest_stars %>%
  left_join(guest_stars %>%
            group_by(season_num) %>%
            summarize(total_guest_stars = n()))

# Add the number of episodes per season as a column
episodes <- episodes %>%
  group_by(season_number) %>%
  mutate(season_episode_num = n())

# Create a large dataset of the guest stars and crew, left-joined
simpsons_episodes <- episodes %>%
  left_join(crew, by = c("id" = "episode_id", "name" = "episode_name",
                         "season_number" = "season_num", "episode_number" = "episode_num")) %>%
  left_join(guest_stars, by = c("id" = "episode_id", "name" = "episode_name",
                                "season_number" = "season_num", "episode_number" = "episode_num")) %>%
  left_join(ratings, by = c("season_number" = "season", "episode_number" = "number_in_season")) %>%
  rename(crew_name = name.y,
         guest_name = name.y.y,
         episode_title = name) %>%
  group_by(season_number) %>%
  mutate(total_guest_stars = ifelse(is.na(total_guest_stars), max(total_guest_stars, na.rm = TRUE), total_guest_stars),
         avg_guest_stars = round(total_guest_stars / season_episode_num, digits=2),
         avg_season_rating = mean(imdb_rating))

# Save the dataset
save(simpsons_episodes, file="Simpsons_Episodes.RData")
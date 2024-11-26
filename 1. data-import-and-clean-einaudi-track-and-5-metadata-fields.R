######## 1. Data import and cleaning: Einaudi track titles for the 3 most
######## recent albums, and metadata fields: track_name, duration_ms, tempo, 
######## acousticness, instrumentalness, key, mode).

#Set up the environment.

library(spotifyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(caret)
library(usethis)
devtools::install_github('charlie86/spotifyr')

###### Authenticate with the API
edit_r_environ()

###### Pull data for our single artist slowly so as not to overwhelm the API
#If we request too much data it will say "! Too Many Requests (RFC 6585)"
artist_name <- "Ludovido Einaudi"

#Create function to fetch and filter track data for the latest 3 albums

fetch_latest_album_features <- function(artist_name) {
  # Step 1: Get all albums for the artist
  albums <- get_artist_albums(artist_name, include_groups = "album")
  
  # Step 2: Select the latest 3 albums
  latest_albums <- albums %>%
    arrange(desc(release_date)) %>%
    slice(1:3)
  
  # Step 3: Initialize an empty list to store track features
  all_features <- list()
  
  # Step 4: Loop through the latest albums and fetch track features
  for (album_id in latest_albums$id) {
    # Get tracks from the album
    tracks <- get_album_tracks(album_id)
    
    # Fetch audio features for the tracks
    track_features <- get_track_audio_features(tracks$id) %>%
      select(track_name, duration_ms, tempo, acousticness, instrumentalness, key, mode)
    
    # Append to the list
    all_features[[album_id]] <- track_features
    
    # Pause to avoid hitting rate limit
    Sys.sleep(1)  # Adjust this as needed to stay within Spotify's rate limits
  }
  
  # Combine all features into a single data frame
  combined_features <- bind_rows(all_features)
  
  return(combined_features)
}

artist_name <- 'Ludovico Einaudi'
artist_id <- '2uFUBdaVGtyMqckSeCl0Qj'
latest_features <- fetch_latest_album_features(artist_id) #Even after that
#function slowing things down to avoid hitting rate limit, I still get this 
#error, uggggh!



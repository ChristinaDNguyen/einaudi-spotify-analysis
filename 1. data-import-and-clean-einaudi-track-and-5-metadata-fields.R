#Trying to pull data again

library(spotifyr)
library(tidyverse)
library(lubridate)

Sys.setenv(SPOTIFY_CLIENT_ID = 'fe8ba20e8c9a4a46b7bf35281257e83d')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b2affa13f0fb45e995e1f3fbc03bd748')

access_token <- get_spotify_access_token()
print(access_token)


#### Find out what Einaudi's ID is on Spotify so we can refer to him as his
#### ID from now on.
einaudi <- search_spotify("Ludovico Einaudi", type = "artist")
artist_id <- einaudi$id[1]

#### Get the 50 most recent albums of his
einaudi_albums <- get_artist_albums(artist_id, include_groups = "album", limit = 50)

#### Clean that data from the albums before we can add more audio_features
simplified_track_data <- map_df(einaudi_albums$id, function(album_id) {
  get_album_tracks(album_id) %>%
    select(id, name, duration_ms, external_urls.spotify) %>%
    mutate(album_id = album_id)  # Add album_id to each track's data
})
write_csv(simplified_track_data, "Step1.simplified_track_data.csv")

#### Add the audio features in to the data so we have a whole set

# Function to get audio features in batches

fetch_audio_features_for_tracks <- function(track_ids) {
  audio_features <- get_track_audio_features(track_ids)
  return(audio_features)}

# Split track data into manageable batches of 50
batch_size <- 50
library(purrr)
all_audio_features <- map_df(seq(1, nrow(simplified_track_data), by = batch_size), function(i) {
  track_ids_batch <- simplified_track_data$id[i:min(i + batch_size - 1, nrow(simplified_track_data))]
  audio_features_batch <- fetch_audio_features_for_tracks(track_ids_batch)
  left_join(simplified_track_data[i:min(i + batch_size - 1, nrow(simplified_track_data)), ], audio_features_batch, by = "id")
})

# View the first few rows of merged data
glimpse(all_audio_features)

write_csv(all_audio_features, "Step2.all_audio_features.csv")

latest_features <- all_audio_features

#### That's our cleaned data, ready for analysis, latest_features

#### Later on in our analysis, if we want, we can isolate for a couple of columns
#### again.

#### Ongoing issues with rate limits: https://developer.spotify.com/documentation/web-api/concepts/rate-limits
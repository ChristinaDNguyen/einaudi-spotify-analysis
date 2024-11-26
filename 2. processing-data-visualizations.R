######## 2. Processing 

#### Basic check and exploration of the cleaned data, called latest_features

# Check the first few rows of the dataset
head(latest_features)

# Summary statistics
summary(latest_features)

# Check for missing values
sum(is.na(latest_features))

#### Turn track length (duration_ms) into minutes rather than milliseconds, 
#### which is unwieldly.

latest_features <- latest_features %>%
  mutate(duration_minutes = duration_ms / 60000)  

#### [Visualisation 1] Distribution of track feature across three albums:
#### distribution of track duration (in minutes) across three albums

ggplot(latest_features, aes(x = factor(album_name), y = duration_minutes)) +
  geom_boxplot() +
  labs(title = "Distribution of track duration across albums",
       x = "Album",
       y = "Duration (Minutes)") +
  theme_minimal()

#### [Visualisation 2] Distribution of track features across three albums:
#### distribution of tempo across three albums

ggplot(latest_features, aes(x = factor(album_name), y = tempo)) +
  geom_boxplot() +
  labs(title = "Distribution of tempo Across albums",
       x = "Album",
       y = "Tempo (BPM)") +
  theme_minimal()

#### [Visualization 3] Distribution of track features across three albums:
#### distribution of acousticness and instrumentalness over three albums in
#### a single box-plot

# Convert the data to long format for easier plotting
long_features <- latest_features %>%
  select(album_name, acousticness, instrumentalness) %>%
  pivot_longer(cols = c(acousticness, instrumentalness), names_to = "feature", values_to = "value")

# Create the boxplot for both acousticness and instrumentalness
ggplot(long_features, aes(x = factor(album_name), y = value, fill = feature)) +
  geom_boxplot() +
  labs(title = "Distribution of Acousticness and Instrumentalness Across Albums",
       x = "Album",
       y = "Feature Value") +
  theme_minimal() +
  scale_fill_manual(values = c("acousticness" = "purple", "instrumentalness" = "salmon"))  # Custom colors

#### [Visualization 4]  Distribution of track features across three albums:
#### distribution of key, mode over three albums in a bar graph

#Since key and mode are categorical variables, I'm using a bar plot 
#to track how the categories of key and mode are distributed across the
#three albums

#plot the distribution of keys across the three albums
ggplot(latest_features, aes(x = factor(album_name), fill = factor(key))) +
  geom_bar(position = "dodge") +  
  labs(title = "Distribution of Key Across Albums",
       x = "Album",
       y = "Count of Tracks",
       fill = "Key") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")  # Set3 color palette for better distinction

# plotting distribution of mode (Major/Minor) across the three albums
ggplot(latest_features, aes(x = factor(album_name), fill = factor(mode))) +
  geom_bar(position = "dodge") +  # Dodge position for side-by-side bars
  labs(title = "Distribution of Mode (Major/Minor) Across Albums",
       x = "Album",
       y = "Count of Tracks",
       fill = "Mode") +
  theme_minimal() +
  scale_fill_manual(values = c("major" = "purple", "minor" = "salmon")) 

#### Correlation of the features: is there a relationship between features
#### like tempo, acousticness, instrumentalness, etc? 
#### heatmaps are cool for things like this

#calculate correlation
correlation_matrix <- latest_features %>%
  select(duration_minutes, tempo, acousticness, instrumentalness, key, mode) %>%
  cor()

#make pretty heatmap to show correlations between all six variables with
#each other

library(reshape2)
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +  # Draw the tiles
  scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", midpoint = 0) +  #Set color gradient to show correlation's strength
  labs(title = "Correlation Heatmap of Features",
       x = "Features", y = "Features", fill = "Correlation") +
  theme_minimal() +  # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  #rotate x-axis labs to be readabile
  coord_fixed()  #Make the heatmap square-shaped
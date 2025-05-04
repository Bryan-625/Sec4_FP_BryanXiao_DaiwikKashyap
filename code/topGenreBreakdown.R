# Load libraries
library(tidyverse)
library(stringr)
library(tidytext)

# Import datasets
MostPlayedDataset <- read.csv("~/GitHub/Sec4_FP_BryanXiao_DaiwikKashyap/data/data.csv", header = TRUE, row.names = 1)
SteamStoreDataset <- read.csv("~/GitHub/Sec4_FP_BryanXiao_DaiwikKashyap/data/steam.csv", header = TRUE)

# Preprocess peak players
MostPlayedDataset$All_time.peak <- str_replace_all(MostPlayedDataset$All_time.peak, ",", "")
MostPlayedDataset$All_time.peak <- as.numeric(as.character(MostPlayedDataset$All_time.peak))

# Clean and prepare Steam store dataset
SteamStoreDataset <- SteamStoreDataset %>% rename(Name = name)

# Merge datasets
MergedData <- merge(SteamStoreDataset, MostPlayedDataset)

clean <- MergedData %>%
  rename(game_name = Name) %>%
  filter(!is.na(genres), genres != "", !is.na(game_name), game_name != "")

# Top 3 genres in the data
TopGenres <- c("Indie", "Action", "Adventure")

# Get top 5 sub-genres for each top genre
TopSubGenres <- clean %>%
  filter(str_detect(genres, paste(TopGenres, collapse = "|"))) %>%
  select(game_name, genres, steamspy_tags) %>%
  separate_rows(genres, sep = ";") %>%
  filter(genres %in% TopGenres) %>%
  separate_rows(steamspy_tags, sep = ";") %>%
  filter(steamspy_tags != "") %>%
  group_by(genres, steamspy_tags) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(genres, desc(count)) %>%
  group_by(genres) %>%
  slice_max(count, n = 5)

# Plot
ggplot(TopSubGenres, aes(x = genres, y = count, fill = steamspy_tags)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 5 Sub-Genres Within Top 3 Genres",
    x = "Primary Genre",
    y = "Number of Games",
    fill = "Sub-Genre Tags"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

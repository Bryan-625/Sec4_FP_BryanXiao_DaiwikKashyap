# Load necessary libraries
library(tidyverse)
library(stringr)
library(scales)

# Load datasets
MostPlayedDataset <- read.csv("~/GitHub/Sec4_FP_BryanXiao_DaiwikKashyap/data/data.csv", header = TRUE, row.names = 1)
SteamStoreDataset <- read.csv("~/GitHub/Sec4_FP_BryanXiao_DaiwikKashyap/data/steam.csv", header = TRUE)

# Wrangle most played dataset
MostPlayedDataset$All_time.peak <- str_replace_all(MostPlayedDataset$All_time.peak, ",", "")
MostPlayedDataset$All_time.peak <- as.numeric(as.character(MostPlayedDataset$All_time.peak))

# Clean Steam store data
SteamStoreDataset <- SteamStoreDataset %>% rename(Name = name)

# Merge datasets and tidy
MergedData <- merge(SteamStoreDataset, MostPlayedDataset)

MergedDataTidy <- MergedData %>% 
  arrange(desc(All_time.peak), .by_group = TRUE) %>%
  select("Name", "genres", "All_time.peak") %>%
  rename(
    Genres = genres,
    All_Time_Peak = All_time.peak
  ) %>%
  slice(1:400) %>%
  separate(
    col = "Genres",
    sep = ";",
    into = c("Genre 1", "Genre 2", "Genre 3"),
    fill = "right"
  )

# Top 10 Most Played Games
Top10Games <- MergedDataTidy %>%
  arrange(desc(All_Time_Peak)) %>%
  slice(1:10)

# Plot
ggplot(Top10Games, aes(x = reorder(Name, All_Time_Peak), y = All_Time_Peak)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Played Steam Games",
    x = "Game",
    y = "All Time Peak Players"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

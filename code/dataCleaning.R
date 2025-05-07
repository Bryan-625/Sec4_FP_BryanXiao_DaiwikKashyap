library(tidyverse)
library(stringr)

# Wrangle Most Played dataset
MostPlayedDataset$All_time.peak <- str_replace_all(MostPlayedDataset$All_time.peak, ",", "")
MostPlayedDataset$All_time.peak <- as.numeric(as.character(MostPlayedDataset$All_time.peak))

# Making sure SteamStoreDataset and MostPlayedDataset have a common column
SteamStoreDataset <- SteamStoreDataset %>%
  rename(
    Name = name
  )

# Merge datasets and tidy
MergedData <- merge(SteamStoreDataset, MostPlayedDataset)

MergedDataTidy <- MergedData %>% 
  arrange(desc(All_time.peak), .by_group = TRUE) %>%
  select("Name", "genres", "All_time.peak", "positive_ratings", "negative_ratings") %>%
  rename(
    Genres = genres,
    All_Time_Peak = All_time.peak,
    Positive_Ratings = positive_ratings,
    Negative_Ratings = negative_ratings
  ) %>%
  slice(1:400) %>%
  separate(
    col = "Genres",
    sep = ";",
    into = c("Genre 1", "Genre 2", "Genre 3"),
    fill = "right"
  )

# Saving merged dataset as CSV file
#write.csv(
#  MergedDataTidy,
#  file = "cleanedData.csv",
#  row.names = TRUE
#)
library(ggplot2)
library(tidyverse)
library(scales)

MergedDataReviews <- MergedDataTidy %>% 
  slice(1:400) %>%
  select(Name, All_Time_Peak, Positive_Ratings, Negative_Ratings) %>%
  mutate(
    PercentageOfPositiveReviews = Positive_Ratings / (Negative_Ratings + Positive_Ratings) * 100
  )
ggplot(
  data = MergedDataReviews,
  mapping = aes(
    x = All_Time_Peak,
    y = PercentageOfPositiveReviews
  )
) + geom_point(size = 3) +
  geom_smooth(method = "lm") +
  labs(
    x = "All Time Peak",
    y = "Percentage of Positive Reviews",
    title = "Percentage of Positive Reviews vs All Time Peak"
  ) + scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_bw()
#Imported file in rstudio "import data set" through environment 

# Load necessary libraries
library(tidyverse)

# Summarizes dataset
summary(netflix_titles)

# Count the number of Movies vs. TV Shows
type_count <- netflix_titles %>%
  count(type) %>%
  arrange(desc(n))
print(type_count)
# Movies = 6131 and TV Shows = 2676

# Graph of the number of Movies vs. TV Shows
ggplot(type_count, aes(x = type, y = n, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Movies vs. TV Shows on Netflix",
       x = "Type",
       y = "Count") +
  theme_minimal() # Can be different types of themes
# Use this code to show different aspects of the literature that you reviewed

# Load libraries:

library(tidyverse)
library(maps)
library(countrycode)
library(usmap)
library(choroplethr)
library(choroplethrMaps)
library(ggthemes)
library(readr)

# Set working directory
setwd("ADDYOURWORKINGDIRECTORY") #laptop

# Read in example data
data <- read_csv("example_data.csv")

# Data columns:
# CitationID: A unique number assigned to each citation.
# Location: Country where the study was conducted.
# Year: Year of publication.
# Theme: Themes identified during coding.
# Number: Each citation receives the number '1' for aggregation.

# Remember that you will need to ensure that your columns have the same header as those used in the code, and you must save your data as a .csv to import.

#-----------------------------------------------
# Line chart of publications through time
#-----------------------------------------------

# Summarize observations by year (ignoring location)
by_year <- data %>%
  group_by(Year) %>%
  summarise(Observations = sum(Number), .groups = "drop")

# Plot publications through time
ggplot(by_year, aes(x = Year, y = Observations)) +
  geom_line(color = "grey77", size = 1) +
  stat_smooth(method = "glm", formula = y ~ poly(x, 2), se = FALSE,
              size = 1, color = "red3") +
  labs(x = "Year", y = "Number of publications (n)") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

#-----------------------------------------------
# Map of publication locations
#-----------------------------------------------

# Prepare map data
world_map <- map_data("world") %>%
  filter(region != "Antarctica")

# Summarize the number of studies per country
location_summary <- data %>%
  group_by(Location) %>%
  summarise(NumberOfStudies = sum(Number), .groups = "drop")

# Merge publication data with world map
locations <- left_join(world_map, location_summary, by = c("region" = "Location"))

# Plot map
ggplot() +
  geom_map(data = world_map, map = world_map, aes(map_id = region), fill = "white", color = "#7f7f7f", size = 0.25) +
  geom_map(data = locations, map = world_map, aes(map_id = region, fill = NumberOfStudies), size = 0.25) +
  scale_fill_gradient(low = "#FFFFFF", high = "#00441b", name = "Publications") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_fixed(1.3) +
  labs(title = "Publication Locations") +
  theme_map() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

#-----------------------------------------------
# Pie chart of publication themes
#-----------------------------------------------

# Summarize themes in publications
theme_summary <- data %>%
  group_by(Theme) %>%
  summarise(Count = sum(Number), .groups = "drop")

# Plot themes as a pie chart
ggplot(theme_summary, aes(x = "", y = Count, fill = Theme)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "Theme") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

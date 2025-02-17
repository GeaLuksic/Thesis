getwd()
list.files()
View(gdinv)
library(readxl)
gdinv=read_excel("Book1.xlsx")
library(ggplot2)
library(dplyr)


#weigh the traps
head(gdinv)

gdinv <- gdinv %>%
  mutate(weight = 1 / no_traps)

lake_summary <- gdinv %>%
  group_by(site_season) %>%
  summarize(
    total_predators = sum(Tot_predators, na.rm = TRUE),  # Total predators for the lake
    total_traps = sum(no_traps, na.rm = TRUE),           # Total number of traps
    total_weight = sum(weight, na.rm = TRUE)             # Total weight (sum of weights)
  )

lake_summary <- lake_summary %>%
  mutate(weighted_predators = total_predators / total_weight)
print(lake_summary)


ggplot(lake_summary, aes(x = site_season, y = weighted_predators)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Weighted Invertebrate Predators by Lake",
       x = "Lake (Site by season)",
       y = "Weighted Number of Invertebrate Predators")

#by water/land level
head(gdinv)

gdinv <- gdinv %>%
  mutate(weight = 1 / no_traps)

distance_summary <- gdinv %>%
  group_by(pitfall_type) %>%
  summarize(
    total_predators = sum(Tot_predators, na.rm = TRUE),  # Total predators for the lake
    total_traps = sum(no_traps, na.rm = TRUE),           # Total number of traps
    total_weight = sum(weight, na.rm = TRUE)             # Total weight (sum of weights)
  )

distance_summary <- distance_summary %>%
  mutate(weighted_predators = total_predators / total_weight)
print(distance_summary)


ggplot(distance_summary, aes(x = pitfall_type, y = weighted_predators)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Weighted Invertebrate Predators by Lake",
       x = "Pitfall type",
       y = "Weighted Number of Invertebrate Predators")

#for fish
head(gdinv)

gdinv <- gdinv %>%
  mutate(weight = 1 / no_traps)

fish_summary <- gdinv %>%
  group_by(fish) %>%
  summarize(
    total_predators = sum(Tot_predators, na.rm = TRUE),  # Total predators for the lake
    total_traps = sum(no_traps, na.rm = TRUE),           # Total number of traps
    total_weight = sum(weight, na.rm = TRUE)             # Total weight (sum of weights)
  )

fish_summary <- fish_summary %>%
  mutate(weighted_predators = total_predators / total_weight)
print(fish_summary)


ggplot(fish_summary, aes(x = fish, y = weighted_predators)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Weighted Invertebrate Predators by Lake",
       x = "Fish presence",
       y = "Weighted Number of Invertebrate Predators")


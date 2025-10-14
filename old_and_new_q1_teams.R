week6 <- readRDS("Archived_Data/2025_7_combined_rankings.rds")
week7 <- readRDS("Archived_Data/2025_8_combined_rankings.rds")

# Extract top 40 teams
week6_top40 <- week6 %>% filter(Total_Rank <= 40) %>% pull(team)
week7_top40 <- week7 %>% filter(Total_Rank <= 40) %>% pull(team)

# New entrants in week 7
new_teams <- setdiff(week7_top40, week6_top40)

# Dropouts from week 6
dropped_teams <- setdiff(week7_top40, week6_top40)

# Optional: create summary table
summary_table <- tibble(
  team = c(new_teams, dropped_teams),
  status = c(rep("New In Top 40", length(new_teams)),
             rep("Dropped From Top 40", length(dropped_teams)))
)

summary_table


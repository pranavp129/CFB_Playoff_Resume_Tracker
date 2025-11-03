old <- readRDS("Archived_Data/2025_10_combined_rankings.rds")
new <- readRDS("Archived_Data/2025_11_combined_rankings.rds")

# Extract top 40 teams
old_top40 <- old %>% filter(Total_Rank <= 40) %>% pull(team)
new_top40 <- new %>% filter(Total_Rank <= 40) %>% pull(team)

# New entrants in new week
new_teams <- setdiff(new_top40, old_top40)

# Dropouts from old week
dropped_teams <- setdiff(old_top40, new_top40)

# Optional: create summary table
summary_table <- tibble(
  team = c(new_teams, dropped_teams),
  status = c(rep("New In Top 40", length(new_teams)),
             rep("Dropped From Top 40", length(dropped_teams)))
)

summary_table

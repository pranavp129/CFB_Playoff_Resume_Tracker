# install (if not already installed)
# remotes::install_github("sportsdataverse/cfbfastR")

library(cfbfastR)
library(dplyr)
library(tidyr)

# Get Schedules
games <- cfbd_game_info(year = 2025)

# Get list of playoff contenders
contenders <- c("Ohio State", "Oregon", "Alabama", "Georgia", "Texas", "LSU", "Clemson")

# Conference List
fbs_conf <- c("ACC", "Big 12", "Big Ten", "SEC", "Pac-12", "Conference USA", "Mid-American", "Mountain West", "FBS Independents", "Sun Belt", "American Athletic")

# Get AP Rankings
ap_poll <- cfbd_rankings(year = 2025, season_type = "regular", week = 5) %>%
  filter(poll == "AP Top 25") %>% 
  select("team" = school, "AP_Rank" = rank)

# Get ELO Rankings 
elo_rankings <- cfbd_ratings_elo(year = 2025) %>% 
  filter(conference %in% fbs_conf) %>%
  select(team, elo) %>%
  arrange(desc(elo)) %>%
  mutate(rank = row_number()) %>%
  select(team, "ELO_Rank" = rank)

# Get SRS Rankings
srs_rankings <- cfbd_ratings_srs(year = 2025) %>% 
  filter(conference %in% fbs_conf) %>% 
  select(team, "SRS_Rank" = ranking)

sp_rankings <- cfbd_ratings_sp(year = 2025) %>% 
  filter(conference %in% fbs_conf) %>% 
  select(team, "SP_Rank" = ranking)

# Generate combined rankings
combined_rankings <- ap_poll %>%
  full_join(elo_rankings, by = "team") %>%
  full_join(srs_rankings, by = "team") %>%
  full_join(sp_rankings, by = "team") %>%
  rowwise() %>%
  mutate(
    Avg_Rank = mean(c(AP_Rank, ELO_Rank, SRS_Rank, SP_Rank), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Avg_Rank) %>%
  mutate(Total_Rank = row_number())

# Find games for all contenders
games_ranked <- games %>%
  filter(home_team %in% contenders | away_team %in% contenders) %>%
  # Join total rank
  left_join(combined_rankings %>% select(team, Total_Rank), by = c("home_team" = "team")) %>%
  rename(home_total_rank = Total_Rank) %>%
  left_join(combined_rankings %>% select(team, Total_Rank), by = c("away_team" = "team")) %>%
  rename(away_total_rank = Total_Rank) %>%
  # Create opponent info before pivot
  mutate(
    home_opp = away_team,
    home_opp_rank = away_total_rank,
    away_opp = home_team,
    away_opp_rank = home_total_rank
  ) %>%
  # Pivot longer
  pivot_longer(cols = c(home_team, away_team),
               names_to = "home_away",
               values_to = "team") %>%
  # Assign opponent and rank based on home/away
  mutate(
    opp = ifelse(home_away == "home_team", home_opp, away_opp),
    opp_total_rank = ifelse(home_away == "home_team", home_opp_rank, away_opp_rank),
    result_type = case_when(
      completed & home_away == "home_team" & home_points > away_points & opp_total_rank <= 34 ~ "Quad1_Win",
      completed & home_away == "away_team" & away_points > home_points & opp_total_rank <= 34 ~ "Quad1_Win",
      completed & ((home_away == "home_team" & home_points < away_points) |
                     (home_away == "away_team" & away_points < home_points)) & opp_total_rank <= 34 ~ "Quad1_Loss",
      completed & ((home_away == "home_team" & home_points < away_points) |
                     (home_away == "away_team" & away_points < home_points)) & opp_total_rank > 34 ~ "Other_Loss",
      !completed & opp_total_rank <= 34 ~ "Remaining_Q1",
      !completed & opp_total_rank > 34 & opp_total_rank <= 68 ~ "Remaining_Q2",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(team %in% contenders)

# Build the resume table
resume <- games_ranked %>%
  group_by(team, result_type) %>%
  summarise(opponents = paste(opp, collapse = ", "), .groups = "drop") %>%
  pivot_wider(names_from = result_type, values_from = opponents)

# Ensure all expected columns exist
for(col in c("Quad1_Win", "Quad1_Loss", "Other_Loss", "Remaining_Q1", "Remaining_Q2")) {
  if(!col %in% names(resume)) resume[[col]] <- ""
}

# Replace NA with empty string and rename columns
resume <- resume %>%
  mutate(across(c(Quad1_Win, Quad1_Loss, Other_Loss, Remaining_Q1, Remaining_Q2), ~coalesce(.x, ""))) %>%
  select(team, 
         "Quad 1 Wins" = Quad1_Win,
         "Quad 1 Losses" = Quad1_Loss,
         "Other Losses" = Other_Loss,
         "Remaining Quad 1 Games" = Remaining_Q1,
         "Remaining Quad 2 Games" = Remaining_Q2)

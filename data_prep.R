# ----------------------------
# 1) Load Libraries
# ----------------------------
library(cfbfastR)
library(dplyr)
library(tidyr)

# VALUES THAT CAN CHANGED:
year_to_use <- 2025
week_to_use <- 8

# ----------------------------
# 2) Fetch Data
# ----------------------------
# Get 2025 games
games <- cfbd_game_info(year = year_to_use)

# Get AP rankings
ap_poll <- cfbd_rankings(year = year_to_use, season_type = "regular", week = week_to_use) %>%
  filter(poll == "AP Top 25") %>% 
  select(team = school, AP_Rank = rank)

# List of playoff contenders (AP Top 25 teams)
contenders <- ap_poll$team

# Conferences to include
fbs_conf <- c("ACC", "Big 12", "Big Ten", "SEC", "Pac-12", 
              "Conference USA", "Mid-American", "Mountain West", 
              "FBS Independents", "Sun Belt", "American Athletic")

# ELO rankings
elo_rankings <- cfbd_ratings_elo(year = year_to_use) %>%
  filter(conference %in% fbs_conf) %>%
  arrange(desc(elo)) %>%
  mutate(ELO_Rank = row_number()) %>%
  select(team, ELO_Rank)

# SRS rankings
srs_rankings <- cfbd_ratings_srs(year = year_to_use) %>%
  filter(conference %in% fbs_conf) %>%
  select(team, SRS_Rank = ranking)

# SP+ rankings
sp_rankings <- cfbd_ratings_sp(year = year_to_use) %>%
  filter(conference %in% fbs_conf) %>%
  select(team, SP_Rank = ranking)

# FPI rankings
fpi_rankings <- cfbd_ratings_fpi(year = year_to_use) %>% 
  filter(conference %in% fbs_conf) %>%
  arrange(resume_ranks_fpi) %>% 
  select(team, FPI_rank = resume_ranks_fpi) 

# ----------------------------
# 3) Combine Rankings
# ----------------------------
combined_rankings <- ap_poll %>%
  full_join(elo_rankings, by = "team") %>%
  full_join(srs_rankings, by = "team") %>%
  full_join(sp_rankings, by = "team") %>%
  full_join(fpi_rankings, by = "team") %>% 
  mutate(AP_Rank = ifelse(is.na(AP_Rank), 136, AP_Rank)) %>%  # count all unranked teams as 136
  rowwise() %>%
  mutate(Avg_Rank = mean(c(AP_Rank, ELO_Rank, SRS_Rank, SP_Rank), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Avg_Rank) %>%
  mutate(Total_Rank = row_number())

# Save combined ranking data
combined_rankings_save_file_name <- paste0("Archived_Data/", year_to_use, "_", week_to_use, "_combined_rankings.rds")
saveRDS(combined_rankings, combined_rankings_save_file_name)

# ----------------------------
# 4) Process Games for Resume Table
# ----------------------------
games_ranked <- games %>%
  filter(home_team %in% contenders | away_team %in% contenders) %>%
  left_join(combined_rankings %>% select(team, Total_Rank), by = c("home_team" = "team")) %>%
  rename(home_total_rank = Total_Rank) %>%
  left_join(combined_rankings %>% select(team, Total_Rank), by = c("away_team" = "team")) %>%
  rename(away_total_rank = Total_Rank) %>%
  mutate(
    home_opp = away_team,
    home_opp_rank = away_total_rank,
    away_opp = home_team,
    away_opp_rank = home_total_rank
  ) %>%
  pivot_longer(cols = c(home_team, away_team),
               names_to = "home_away",
               values_to = "team") %>%
  mutate(
    opp = ifelse(home_away == "home_team", home_opp, away_opp),
    opp_total_rank = ifelse(home_away == "home_team", home_opp_rank, away_opp_rank),
    location = case_when(
      neutral_site ~ "Neutral",
      home_away == "home_team" ~ "Home",
      home_away == "away_team" ~ "Away"
    ),
    # Quad1 cutoffs by location
    quad1_cutoff = case_when(
      location == "Home" ~ 30,
      location == "Neutral" ~ 35,
      location == "Away" ~ 40
    ),
    # Result type
    result_type = case_when(
      completed & home_away == "home_team" & home_points > away_points & opp_total_rank <= quad1_cutoff ~ "Quad1_Win",
      completed & home_away == "away_team" & away_points > home_points & opp_total_rank <= quad1_cutoff ~ "Quad1_Win",
      completed & ((home_away == "home_team" & home_points < away_points) |
                     (home_away == "away_team" & away_points < home_points)) & opp_total_rank <= quad1_cutoff ~ "Quad1_Loss",
      completed & ((home_away == "home_team" & home_points < away_points) |
                     (home_away == "away_team" & away_points < home_points)) & opp_total_rank > quad1_cutoff ~ "Other_Loss",
      !completed & opp_total_rank <= quad1_cutoff ~ "Remaining_Q1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(team %in% contenders)

# ----------------------------
# 5) Build Resume Table
# ----------------------------
resume <- games_ranked %>%
  group_by(team, result_type) %>%
  summarise(opponents = paste(opp, collapse = ", "), .groups = "drop") %>%
  pivot_wider(names_from = result_type, values_from = opponents) %>%
  slice(match(contenders, team))

# Ensure all expected columns exist
for(col in c("Quad1_Win", "Quad1_Loss", "Other_Loss", "Remaining_Q1")) {
  if(!col %in% names(resume)) resume[[col]] <- ""
}

resume <- resume %>%
  mutate(across(c(Quad1_Win, Quad1_Loss, Other_Loss, Remaining_Q1), ~coalesce(.x, ""))) %>%
  select(team, 
         "Q1 Wins" = Quad1_Win,
         "Q1 Losses" = Quad1_Loss,
         "Other Losses" = Other_Loss,
         "Remaining Q1 Games" = Remaining_Q1)

# ----------------------------
# 6) Prepare Plot-Ready Data
# ----------------------------
teams_logos <- cfbd_team_info() %>%
  select(team = school, logo)

plot_data <- resume %>%
  pivot_longer(cols = c("Q1 Wins", "Q1 Losses", "Other Losses",
                        "Remaining Q1 Games"),
               names_to = "ResultType", values_to = "Opponents") %>%
  mutate(Opponents = ifelse(Opponents == "", NA, Opponents)) %>%
  separate_rows(Opponents, sep = ",\\s*") %>%
  filter(!is.na(Opponents)) %>%
  left_join(teams_logos, by = c("Opponents" = "team")) %>%
  rename(OppLogo = logo) %>%
  left_join(teams_logos %>% rename(TeamLogo = logo), by = c("team" = "team")) %>%
  group_by(team, ResultType) %>%
  mutate(n_logos = n()) %>%
  ungroup()

# Save for plotting
saveRDS(plot_data, "plot_data.rds")
saveRDS(year_to_use, "year_to_use.rds")
saveRDS(week_to_use, "week_to_use.rds")


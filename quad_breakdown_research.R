library(cfbfastR)
library(dplyr)
library(purrr)
library(tidyr)

# --- Settings ---
years <- 2021:2025
fbs_conf <- c("ACC","Big 12","Big Ten","SEC","Pac-12",
              "Conference USA","Mid-American","Mountain West",
              "FBS Independents","Sun Belt","American Athletic")

# --- Helper function to get combined rankings for one year/week ---
get_combined_rankings <- function(year, week) {
  
  # AP
  ap_poll <- cfbd_rankings(year = year, season_type = "regular", week = week) %>%
    filter(poll == "AP Top 25") %>%
    select(team = school, AP_Rank = rank)
  
  # ELO
  elo <- cfbd_ratings_elo(year = year, week = week) %>%
    filter(conference %in% fbs_conf) %>%
    arrange(desc(elo)) %>%
    mutate(ELO_Rank = row_number()) %>%
    select(team, ELO_Rank)
  
  # SRS
  srs <- cfbd_ratings_srs(year = year) %>%
    filter(conference %in% fbs_conf) %>%
    select(team, SRS_Rank = ranking)
  
  # SP+
  sp <- cfbd_ratings_sp(year = year) %>%
    filter(conference %in% fbs_conf) %>%
    select(team, SP_Rank = ranking)
  
  # FPI
  fpi <- cfbd_ratings_fpi(year = year) %>%
    filter(conference %in% fbs_conf) %>%
    arrange(resume_ranks_fpi) %>%
    select(team, FPI_rank = resume_ranks_fpi)
  
  # Combine
  full_join(ap_poll, elo, by = "team") %>%
    full_join(srs, by = "team") %>%
    full_join(sp, by = "team") %>%
    full_join(fpi, by = "team") %>%
    mutate(AP_Rank = ifelse(is.na(AP_Rank), 136, AP_Rank)) %>%
    rowwise() %>%
    mutate(Avg_Rank = mean(c(AP_Rank, ELO_Rank, SRS_Rank, SP_Rank), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Avg_Rank) %>%
    mutate(Total_Rank = row_number(),
           year = year,
           week = week)
}

# Generate rankings
weeks <- 1:15
all_rankings <- expand.grid(year = years, week = weeks) %>%
  mutate(data = map2(year, week, safely(get_combined_rankings))) %>%
  mutate(data = map(data, "result")) %>%
  unnest(data, names_sep = "_")

# get fbs vs fbs games
games <- map_df(years, ~ cfbd_game_info(year = .x)) %>% 
  filter(home_division == "fbs", away_division == "fbs")

# merge games and rankings
games_ranked <- games %>%
  left_join(
    all_rankings %>%
      select(year, week, team = data_team, Total_Rank = data_Total_Rank),
    by = c("season" = "year", "week", "home_team" = "team")
  ) %>%
  rename(home_rank = Total_Rank) %>%
  left_join(
    all_rankings %>%
      select(year, week, team = data_team, Total_Rank = data_Total_Rank),
    by = c("season" = "year", "week", "away_team" = "team")
  ) %>%
  rename(away_rank = Total_Rank)

# get game results and location
games_results <- games_ranked %>%
  mutate(
    # outcome for home team
    home_win = case_when(
      home_points > away_points ~ 1,
      home_points < away_points ~ 0,
      TRUE ~ NA_real_
    ),
    # which team is being evaluated and whether they won
    location = case_when(
      neutral_site ~ "Neutral",
      TRUE ~ "Home"
    ),
    # create a mirror record for the away team
    away_location = case_when(
      neutral_site ~ "Neutral",
      TRUE ~ "Away"
    )
  )

# Make long form so each row = one team's perspective
games_long <- games_results %>%
  select(season, week, home_team, away_team, home_rank, away_rank,
         home_points, away_points, neutral_site) %>%
  mutate(
    winner = ifelse(home_points > away_points, home_team, away_team)
  ) %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "side",
    values_to = "team"
  ) %>%
  mutate(
    location = case_when(
      neutral_site ~ "Neutral",
      side == "home_team" ~ "Home",
      side == "away_team" ~ "Away"
    ),
    opp_rank = ifelse(side == "home_team", away_rank, home_rank),
    win = ifelse(team == winner, 1, 0)
  )

# Compute Win % by Opponent Rank and Location 
games_summary <- games_long %>%
  mutate(
    opp_rank_group = cut(
      opp_rank,
      breaks = seq(0, 150, by = 5),   # 0–10, 11–20, ..., 141–150
      labels = paste(seq(1, 141, by = 5), seq(5, 150, by = 5), sep = "–"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(location, opp_rank_group) %>%
  summarise(
    games = n(),
    wins = sum(win, na.rm = TRUE),
    win_pct = wins / games,
    .groups = "drop"
  )

# look at win %'s
home_summary <- games_summary %>%
  filter(location == "Home") %>%
  arrange(desc(win_pct))


# plot
library(ggplot2)

ggplot(games_summary, aes(x = opp_rank_group, y = win_pct, fill = location)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Win Percentage by Opponent Rank and Location",
    x = "Opponent Rank Group",
    y = "Win Percentage",
    fill = "Location"
  ) +
  theme_minimal(base_size = 13)

# logistic regression for win ~ opponent rank
glm(win ~ opp_rank, data = filter(games_long, location == "Home"), family = "binomial")

library(dplyr)
library(ggplot2)

# Fit separate logistic models for each location
models <- games_long %>%
  group_by(location) %>%
  group_map(~ glm(win ~ opp_rank, data = .x, family = "binomial"), .keep = TRUE) %>%
  setNames(unique(games_long$location))

# Create a sequence of opponent ranks to predict
opp_ranks <- 1:150  # adjust max rank as needed

# Predict probabilities
pred_df <- lapply(names(models), function(loc) {
  model <- models[[loc]]
  data.frame(
    location = loc,
    opp_rank = opp_ranks,
    win_prob = predict(model, newdata = data.frame(opp_rank = opp_ranks), type = "response")
  )
}) %>%
  bind_rows()

# Plot
ggplot(pred_df, aes(x = opp_rank, y = win_prob, color = location)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(
    title = "Predicted Win Probability vs Opponent Rank",
    x = "Opponent Rank",
    y = "Predicted Win Probability",
    color = "Location"
  ) +
  theme_minimal()

library(dplyr)

games_long %>%
  filter(location == "Home") %>%
  summarise(
    total = n(),
    missing_win = sum(is.na(win)),
    missing_opp_rank = sum(is.na(opp_rank))
  )

games_long %>%
  group_by(location, opp_rank_group = cut(opp_rank, breaks = seq(0,150,5))) %>%
  summarise(win_pct = mean(win, na.rm = TRUE), n = n()) %>%
  ggplot(aes(x = opp_rank_group, y = win_pct, color = location)) +
  geom_point() +
  geom_hline(yintercept = 0.34, color = "black", size = 1) +
  geom_line(aes(group = location)) +
  labs(title = "Raw Win % vs Opponent Rank (FBS only)", y = "Win %")


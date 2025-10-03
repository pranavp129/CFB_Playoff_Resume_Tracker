# install (if not already installed)
# remotes::install_github("sportsdataverse/cfbfastR")

library(cfbfastR)
library(dplyr)
library(tidyr)

# Get Schedules
games <- cfbd_game_info(year = 2025)

# Get list of playoff contenders for testing
test_contenders <- c("Ohio State", "Oregon", "Alabama", "Georgia", "Texas", "LSU", "Clemson")

# Conference List
fbs_conf <- c("ACC", "Big 12", "Big Ten", "SEC", "Pac-12", "Conference USA", "Mid-American", "Mountain West", "FBS Independents", "Sun Belt", "American Athletic")

# Get AP Rankings
ap_poll <- cfbd_rankings(year = 2025, season_type = "regular", week = 5) %>%
  filter(poll == "AP Top 25") %>% 
  select("team" = school, "AP_Rank" = rank)

contenders <- ap_poll$team

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

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggimage)
library(cfbfastR)

# Get logos
teams_logos <- cfbd_team_info() %>%
  select(team = school, logo)

# Prepare resume_long
resume_long <- resume %>%
  pivot_longer(cols = c("Quad 1 Wins", "Quad 1 Losses", "Other Losses",
                        "Remaining Quad 1 Games", "Remaining Quad 2 Games"),
               names_to = "ResultType", values_to = "Opponents") %>%
  mutate(Opponents = ifelse(Opponents == "", NA, Opponents)) %>%
  separate_rows(Opponents, sep = ",\\s*") %>%
  filter(!is.na(Opponents))

# Join logos
plot_data <- resume_long %>%
  left_join(teams_logos, by = c("Opponents" = "team")) %>%
  rename(OppLogo = logo) %>%
  left_join(teams_logos %>% rename(TeamLogo = logo), by = c("team" = "team"))

# Assign horizontal offsets to stack logos in each cell left-to-right
plot_data <- plot_data %>%
  group_by(team, ResultType) %>%
  mutate(offset = 0.2 * (row_number() - 1)) %>% # horizontal offset
  ungroup()



# ------------------------------

library(dplyr)
library(ggplot2)
library(ggimage)

# -------------------
# 1) Prep row + col ids
# -------------------

# Stable ordering of teams (descending like before)
team_levels <- sort(unique(plot_data$team), decreasing = TRUE)

row_lookup <- tibble(
  team = team_levels,
  row_id = seq_along(team_levels)
) %>%
  left_join(teams_logos %>% rename(team_logo = logo), by = "team") %>%
  mutate(shade = ifelse(row_id %% 2 == 0, "grey95", "white"))  # alternating stripes

# Map ResultType to numeric x positions
result_levels <- c("Quad 1 Wins", "Quad 1 Losses", "Other Losses",
                   "Remaining Quad 1 Games", "Remaining Quad 2 Games")

plot_data2 <- plot_data %>%
  mutate(ResultType = factor(ResultType, levels = result_levels)) %>%
  left_join(row_lookup, by = "team") %>%
  group_by(team, ResultType) %>%
  mutate(offset = 0.2 * (row_number() - 1)) %>%
  ungroup() %>%
  mutate(col_id = as.numeric(ResultType) + offset)

plot_data_centered <- plot_data2 %>%
  group_by(ResultType, row_id) %>%
  mutate(n_logos = n()) %>%
  group_by(ResultType) %>%
  mutate(
    offset_center = 0.2 * (row_number() - 1) - 0.2 * (n()/2 - 0.5),
    col_id_centered = as.numeric(ResultType) + offset_center
  ) %>%
  ungroup()

# x-axis max
max_offset <- plot_data_centered %>%
  group_by(ResultType) %>%
  summarise(max_col = max(col_id_centered), .groups = "drop")
x_max <- max(max_offset$max_col) + 0.5

plot_data_correct <- plot_data2 %>%
  group_by(team, ResultType) %>%
  mutate(
    # Stack logos horizontally centered relative to team’s column
    n_logos = n(),
    offset_center = 0.2 * (row_number() - 1) - 0.2 * (n_logos - 1)/2,
    col_id_centered = as.numeric(ResultType) + offset_center
  ) %>%
  ungroup()

# Dynamic spacing + adaptive logo size
plot_data_dynamic <- plot_data_correct %>%
  group_by(team, ResultType) %>%
  mutate(
    n_logos = n(),
    # Max total width per column ~0.8 units; shrink spacing if many logos
    spacing = ifelse(n_logos > 4, 0.8 / (n_logos - 1), 0.2),
    offset_center = spacing * (row_number() - 1) - spacing * (n_logos - 1)/2,
    col_id_centered = as.numeric(ResultType) + offset_center,
    # Adaptive logo size: shrink slightly for dense columns
    logo_size = ifelse(n_logos > 4, 0.06 - 0.01*(n_logos-4), 0.06)
  ) %>%
  ungroup()

# x-axis max based on largest offset
x_max <- max(plot_data_dynamic$col_id_centered) + 0.5

# number of teams
n_teams <- nrow(row_lookup)

# scale logo sizes dynamically based on row count
team_logo_size <- 0.8 / n_teams   # for team logos
opp_logo_size  <- 0.7 / n_teams   # for opponent logos


# plot
ggplot(plot_data_dynamic, aes(x = col_id_centered, y = row_id)) +
  # alternating stripes (slightly taller to fully hold team logo)
  geom_tile(data = row_lookup, aes(x = 3, y = row_id, fill = shade),
            width = Inf, height = 1.05, inherit.aes = FALSE) +
  scale_fill_identity() +
  
  # horizontal grid lines
  geom_hline(yintercept = 0.5:(max(plot_data_dynamic$row_id)+0.5), 
             color = "grey60", size = 0.3) +
  
  # vertical grid lines
  geom_vline(xintercept = 0.5 + 0:length(result_levels), 
             color = "grey85", size = 0.3) +
  
  # thicker line between team logos and opponents
  geom_vline(xintercept = 0.5, color = "black", size = 1) +
  
  # opponent logos
  geom_image(
    aes(image = OppLogo),
    size = opp_logo_size
  ) +
  
  # team logos
  geom_image(
    data = row_lookup,
    aes(x = 0.25, y = row_id),
    image = row_lookup$team_logo,
    size = team_logo_size,
    inherit.aes = FALSE
  ) +
  
  scale_x_continuous(
    breaks = 1:length(result_levels),
    labels = result_levels,
    limits = c(0, x_max),
    expand = c(0,0)
  ) +
  scale_y_continuous(breaks = NULL, expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  labs(x = "", y = "", title = "CFB Playoff Resume by Team") +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank()
  )


# ----------------------------
# 1) Load Libraries
# ----------------------------
library(cfbfastR)
library(dplyr)

# VALUES THAT CAN CHANGED:
year_to_use <- 2025
week_to_use <- 11
saveRDS(year_to_use, "data/year_to_use.rds")
saveRDS(week_to_use, "data/week_to_use.rds")


# Get 2025 games
games <- cfbd_game_info(year = year_to_use)
saveRDS(games, "data/games.rds")

# Get Poll data
polls <- cfbd_rankings(year = year_to_use, season_type = "regular", week = week_to_use)
saveRDS(polls, "data/polls.rds")

# Get ELO data
elo <- cfbd_ratings_elo(year = year_to_use)
saveRDS(elo, "data/elo.rds")

# Get SRS data
srs <- cfbd_ratings_srs(year = year_to_use)
saveRDS(srs, "data/srs.rds")

# Get SP data
sp <- cfbd_ratings_sp(year = year_to_use)
saveRDS(sp, "data/sp.rds")

# Get FPI data
fpi <- cfbd_ratings_fpi(year = year_to_use)
saveRDS(fpi, "data/fpi.rds")

# Get team logo data
teams_logos <- cfbd_team_info() %>%
  select(team = school, logo)
saveRDS(fpi, "data/teams_logos.rds")


  
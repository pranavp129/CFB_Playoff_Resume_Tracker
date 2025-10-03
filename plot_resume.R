# plot_resume.R
# ----------------------------
# 1) Load required libraries
# ----------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggimage)

# ----------------------------
# 2) Load plot-ready data
# ----------------------------
plot_data <- readRDS("plot_data.rds")

# ----------------------------
# 3) Row + Column Setup
# ----------------------------

# Order teams as they appear in the data
team_levels <- unique(plot_data$team)

# Row lookup: assigns row IDs and alternating row shading
row_lookup <- tibble(
  team = team_levels,
  row_id = seq_along(team_levels)
) %>%
  left_join(plot_data %>% select(team, TeamLogo) %>% distinct(), by = "team") %>%
  mutate(shade = ifelse(row_id %% 2 == 0, "grey95", "white"))

# ResultType order for columns
result_levels <- c("Quad 1 Wins", "Quad 1 Losses", "Other Losses",
                   "Remaining Quad 1 Games")

# Base logo sizes relative to number of rows
base_team_logo_size <- 0.8 / nrow(row_lookup)  # team logos
base_opp_logo_size  <- 0.7 / nrow(row_lookup)  # opponent logos

# ----------------------------
# 4) Assign numeric positions for opponent logos
# ----------------------------

plot_data <- plot_data %>%
  mutate(ResultType = factor(ResultType, levels = result_levels)) %>%
  left_join(row_lookup, by = "team") %>%
  group_by(team, ResultType) %>%
  mutate(
    n_logos = n(),
    # spacing adjusts based on number of logos in a cell
    spacing = ifelse(n_logos > 4, 0.8 / (n_logos - 1), 0.2),
    # center logos horizontally in the column
    offset_center = spacing * (row_number() - 1) - spacing * (n_logos - 1)/2,
    col_id_centered = as.numeric(ResultType) + offset_center,
    # scale logos relative to row count; shrink if crowded
    logo_size = ifelse(n_logos > 4,
                       base_opp_logo_size * (4 / n_logos),
                       base_opp_logo_size)
  ) %>%
  ungroup()

# Maximum x-axis value for plot limits
x_max <- max(plot_data$col_id_centered) + 0.5

# ----------------------------
# 5) Create Plot
# ----------------------------

ggplot(plot_data, aes(x = col_id_centered, y = row_id)) +
  
  # alternating shaded rows
  geom_tile(data = row_lookup, aes(x = 3, y = row_id, fill = shade),
            width = Inf, height = 1.05, inherit.aes = FALSE) +
  scale_fill_identity() +
  
  # horizontal and vertical grid lines
  geom_hline(yintercept = 0.5:(nrow(row_lookup)+0.5), color = "grey60", size = 0.3) +
  geom_vline(xintercept = 0.5 + 0:length(result_levels), color = "grey85", size = 0.3) +
  
  # vertical divider between team logos and result columns
  geom_vline(xintercept = 0.5, color = "black", size = 1) +
  
  # opponent logos
  geom_image(aes(image = OppLogo, size = I(logo_size))) +
  
  # team logos
  geom_image(data = row_lookup,
             aes(x = 0.25, y = row_id, image = TeamLogo),
             size = base_team_logo_size,
             inherit.aes = FALSE) +
  
  # x-axis setup
  scale_x_continuous(
    breaks = 1:length(result_levels),
    labels = result_levels,
    limits = c(0, x_max),
    expand = c(0,0)
  ) +
  
  # y-axis setup
  scale_y_continuous(breaks = NULL, expand = c(0,0), trans = "reverse") +
  
  # allow plotting outside the clipping region
  coord_cartesian(clip = "off") +
  
  # theme and labels
  theme_minimal() +
  labs(
    x = "", 
    y = "", 
    title = "AP Top 25 CFB Playoff Resumes",
    subtitle = "Quad 1 = Teams ranked 1-34; Rankings from AP, SRS, SP+, ELO"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank()
  )

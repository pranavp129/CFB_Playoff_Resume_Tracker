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
  left_join(plot_data %>% select(team, TeamLogo) %>% distinct(), by = "team")

# ResultType order for columns
result_levels <- c("Q1 Wins", "Q1 Losses", "Other Losses",
                   "Remaining Q1 Games")

# Base logo sizes relative to number of rows
base_team_logo_size <- 1.0 / nrow(row_lookup)  # team logos
base_opp_logo_size  <- .9 / nrow(row_lookup)  # opponent logos

# ----------------------------
# 4) Assign numeric positions for opponent logos
# ----------------------------\
col_borders <- list(
  "Q1 Wins" = 0.25,
  "Q1 Losses" = 0.4,
  "Other Losses" = 0.55,
  "Remaining Q1 Games" = 0.7
)

plot_data <- plot_data %>%
  mutate(ResultType = factor(ResultType, levels = result_levels)) %>%
  left_join(row_lookup, by = "team") %>%
  group_by(team, ResultType) %>%
  mutate(
    n_logos = n(),
    # spacing adjusts based on number of logos in a cell
    spacing = 0.05, #ifelse(n_logos > 4, 0.85 / (n_logos - 1), 0.2),
    # left justify logos horizontally in the column
    offset_left = spacing * (row_number() - 1),
    col_id_left = as.numeric(col_borders[ResultType]) + offset_left,
  ) %>%
  ungroup()

# Maximum x-axis value for plot limits
x_max <- max(plot_data$col_id_left) + .05

# ----------------------------
# 5) Create Plot
# ----------------------------

ggplot(plot_data, aes(x = col_id_left, y = row_id)) +
  # horizontal and vertical grid lines
  geom_hline(yintercept = 0.5:(nrow(row_lookup)+0.5), color = "grey30", size = 0.3) +
  geom_vline(xintercept = unlist(col_borders[result_levels]) - 0.05, color = "grey60", size = 0.3) +

  # vertical divider between team logos and result columns
  geom_vline(xintercept = 0.2, color = "black", size = 1) +
  
  # opponent logos
  geom_image(aes(image = OppLogo, size = I(base_opp_logo_size))) +
  
  # team logos
  geom_image(data = row_lookup,
             aes(x = 0.1, y = row_id, image = TeamLogo),
             size = base_team_logo_size,
             inherit.aes = FALSE) +
  
  # x-axis setup
  scale_x_continuous(
    #breaks = unlist(col_borders[result_levels]) - 0.02,
    breaks = c(0.275, 0.425, 0.575, 0.775),
    labels = result_levels,
    limits = c(0, x_max),
    expand = c(0,0),
    position = "top"
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
    title = "AP Top 25 CFB Playoff Resumes (Week 6)",
    subtitle = "Quad 1 (Q1) = Teams ranked 1-34; Rankings are a composite of AP, SRS, SP+, and ELO ratings",
    caption = "*Data: CFBFastR*
    **Pranav Pitchala**"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    plot.background  = element_rect(fill = "grey75", color = NA),
  )

ggsave("Graphics/Week6_Resumes.png", dpi = 500, width = 10, height = 10)


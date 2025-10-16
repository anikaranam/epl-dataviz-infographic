# Data loading and preprocessing

library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyverse)
library(ggbeeswarm)
library(gt)
library(scales)
library(tidyr)
library(reshape2)

folder <- "~/Downloads/Datasets/"

years <- c("2000-01","2001-02","2002-03","2003-04","2004-05",
           "2005-06","2006-07","2007-08","2008-09","2009-10",
           "2010-11","2011-12","2012-13","2013-14","2014-15",
           "2015-16","2016-17","2017-18","2018-19","2019-20",
           "2020-21","2021-22")

# Required columns
columns_req <- c("Date","HomeTeam","AwayTeam","FTHG","FTAG","HTHG","HTAG",
                 "FTR","HTR","HS","AS","HST","HC","AC","AST","HF","AF",
                 "HY","AY","HR","AR","Referee")

playing_statistics <- map(years, function(yr) {
  file_path <- paste0(folder, yr, ".csv")
  
  read_csv(file_path, show_col_types = FALSE) |>
    select(any_of(columns_req)) |>
    mutate(Season = yr)
})

df <- bind_rows(playing_statistics)

df <- df |>
  mutate(
    FTR = case_when(
      FTR == "H" ~ "HOME",
      FTR == "A" ~ "AWAY",
      TRUE ~ "DRAW"
    ),
    
    Winner = case_when(
      FTR == "HOME" ~ HomeTeam,
      FTR == "AWAY" ~ AwayTeam,
      TRUE ~ "Draw"
    ),

    Result = case_when(
      FTR == "HOME" ~ "Home Team Win",
      FTR == "AWAY" ~ "Away Team Win",
      TRUE ~ "Draw"
    ),
    
    Year = year(dmy(Date)),
    
    TotalGoal = FTHG + FTAG,
    GoalDif = FTHG - FTAG
  )

write_csv(df, "~/Combined_stats.csv")


# Code for the stacked bar chart for half time to full time result

df = read.csv('~/Combined_stats.csv')

transition_matrix <- df |>
  count(HTR, FTR) |>
  group_by(HTR) |>
  mutate(perc = 100 * n / sum(n))

ht_labels <- c("H" = "Home Team leading", "D" = "Score Tied", "A" = "Away Team leading")
ft_labels <- c("HOME" = "Home Win", "DRAW" = "Draw", "AWAY" = "Away Win")

colors <- c("HOME" = "#1a9641", "DRAW" = "#F18F01", "AWAY" = "#d7191c")

ggplot(transition_matrix, aes(x = HTR, y = perc, fill = FTR)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", linewidth = 1.2) +
  
  geom_text(aes(label = ifelse(perc > 5, sprintf("%.1f%%", perc), "")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.5) +
  
  scale_x_discrete(labels = ht_labels) +
  scale_fill_manual(values = colors, labels = ft_labels, name = "FT Result") +
  
  labs(
    x = "Half-Time Result",
    y = "Percentage (%)",
    title = "From Half-Time to Full-Time",
    subtitle = "When home teams are leading at half time, they go on to win the game about 80% of the time\nWhen scores are tied, the home team comes away with at least a draw around 75% of the time"
  ) + theme_minimal() + 
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(face = "bold"),
  )

# Code for facetted bar chart

df = read.csv('~/Combined_stats.csv')

calculate_stats <- function(data) {
  mean_val <- mean(data, na.rm = TRUE)
  sd_val   <- sd(data, na.rm = TRUE)
  n        <- sum(!is.na(data))
  se       <- sd_val / sqrt(n)
  t_value  <- 1.96
  ci       <- se * t_value
  data.frame(mean = mean_val, ci = ci)
}

metrics <- list(
  "Goals per Game"   = list(Home = calculate_stats(df$FTHG), Away = calculate_stats(df$FTAG)),
  "Shots per Game"   = list(Home = calculate_stats(df$HS),   Away = calculate_stats(df$AS)),
  "Corners per Game" = list(Home = calculate_stats(df$HC),   Away = calculate_stats(df$AC))
)

stat_data <- do.call(rbind, lapply(names(metrics), function(metric) {
  home <- metrics[[metric]]$Home
  away <- metrics[[metric]]$Away
  data.frame(
    Metric   = metric,
    Location = c("Home", "Away"),
    Mean     = c(home$mean, away$mean),
    CI       = c(home$ci, away$ci)
  )
}))

plot_metric <- function(data, title) {
  ggplot(data, aes(x = Location, y = Mean, fill = Location)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.8, width = 0.6) +
    geom_errorbar(aes(ymin = Mean - CI, ymax = Mean + CI), width = 0.2, size = 1.1) +
    geom_text(aes(label = sprintf("%.2f", Mean),
                  y = Mean + CI + 0.05 * max(Mean)),
              fontface = "bold", size = 5.5) +
    scale_fill_manual(values = c("red", "#1a9641")) +
    labs(title = title, y = "Average per Game", x = NULL) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none",
      axis.title.y = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.text.x = element_text(face = 'bold'),
      axis.text.y = element_text(face = 'bold')
    )
}

plots <- lapply(unique(stat_data$Metric), function(metric) {
  plot_metric(filter(stat_data, Metric == metric), metric)
})

grid.arrange(
  grobs = plots,
  ncol = 3,
  top = textGrob(
    "Home vs Away Performance Statistics (with 95% CI)",
    gp = gpar(fontsize = 16, fontface = "bold")
  ),
  bottom = textGrob(
    "Home teams show a clear advantage, recording more goals, shots, \nand corners per game than away teams.",
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

## Code for beeswarm plot

df = read.csv('~/Combined_stats.csv')

home_conversion <- df |>
  filter(HST > 0) |>
  group_by(Team = HomeTeam, Year) |>
  summarise(
    Goals = sum(FTHG),
    ShotsOnTarget = sum(HST),
    .groups = 'drop'
  ) |>
  mutate(
    ConversionRate = Goals / ShotsOnTarget,
    Venue = "Home"
  )

away_conversion <- df |>
  filter(AST > 0) |> 
  group_by(Team = AwayTeam, Year) |>
  summarise(
    Goals = sum(FTAG),
    ShotsOnTarget = sum(AST),
    .groups = 'drop'
  ) |>
  mutate(
    ConversionRate = Goals / ShotsOnTarget,
    Venue = "Away"
  )

full_conversion_data <- bind_rows(home_conversion, away_conversion) |>
  filter(ConversionRate <= 1)

shot_quality_plot <- ggplot(full_conversion_data, aes(x = Venue, y = ConversionRate, color = Venue)) +
  geom_boxplot(width = 0.2, alpha = 0.3, outlier.shape = NA) +
  geom_quasirandom(alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c("Home" = "#1a9641", "Away" = "#d7191c")) +
  guides(color = "none") +
  labs(
    title = "Shot Quality: Home vs. Away Performance",
    subtitle = "Each dot represents a team's goal conversion rate in a single season. There seems to \nbe no significant difference in conversion rate between home and away teams",
    y = "Conversion Rate (Goals per Shot on Target)",
    x = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(face = 'bold')
  )

print(shot_quality_plot)


# Code for premier league standings

# didn't have time to find a package and do this automatically so i had to hardcode it
pl_table <- tibble::tribble(
  ~Pos, ~Club, ~Pts,
  1, "Liverpool", 99,
  2, "Manchester City", 81,
  3, "Manchester United", 66,
  4, "Chelsea", 66,
  5, "Leicester City", 62,
  6, "Tottenham Hotspur", 59,
  7, "Wolves", 59,
  8, "Arsenal", 56,
  9, "Sheffield United", 54,
  10, "Burnley", 54,
  11, "Southampton", 52,
  12, "Everton", 49,
  13, "Newcastle United", 44,
  14, "Crystal Palace", 43,
  15, "Brighton", 41,
  16, "West Ham", 39,
  17, "Aston Villa", 35,
  18, "Bournemouth", 34,
  19, "Watford", 34,
  20, "Norwich City", 21
)

pl_gt <- pl_table |>
  gt() |>
  cols_label(
    Pos = "Pos",
    Club = "Club",
    Pts = "Points"
  ) |>
  data_color(
    columns = Pts,
    colors = col_numeric(
      palette = c("#d7191c", "#1a9641"),
      domain = range(pl_table$Pts)
    )
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "white"),
      cell_fill(color = "#002244")
    ),
    locations = cells_column_labels(everything())
  ) |>
  tab_options(
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    column_labels.font.size = px(14),
    data_row.padding = px(6),
    table.font.names = "Helvetica",
    heading.align = "center"
  )

pl_gt

# Code for home win percentage over the years

df = read.csv('~/Combined_stats.csv')

home_advantage_time <- df |>
  group_by(Year) |>
  summarise(
    total_matches = n(),
    home_wins = sum(FTR == 'HOME'),
    home_win_pct = (home_wins / total_matches) * 100,
    .groups = 'drop'
  ) |>

overall_avg <- sum(df$FTR == 'HOME') / nrow(df) * 100

circle <- data.frame(
  x = 2020 + 1.5 * cos(seq(0, 2*pi, length.out = 200)),
  y = 40 + 3.5 * sin(seq(0, 2*pi, length.out = 200))
)

ggplot(home_advantage_time, aes(x = Year, y = home_win_pct)) +
  geom_polygon(data = circle, aes(x = x, y = y),
               fill = "grey70", alpha = 0.8, color = NA) +
  geom_line(color = '#d7191c', size = 1.5) +
  geom_point(color = '#d7191c', size = 3, shape = 19) +
  geom_hline(yintercept = overall_avg, linetype = 'dashed', 
             color = '#1a9641', size = 1, alpha = 0.7) +
  annotate('text', x = 2020, y = 35.5, label = 'Pandemic period',
           color = 'black', fontface = 'bold', size = 3) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  scale_y_continuous(limits = c(35, 55), breaks = seq(35, 55, by = 5)) +
  labs(
    title = 'Home Advantage Over Time in Premier League (2000–2022)',
    subtitle = 'Teams win ~45% of all home games, with the pandemic years\n(where fans were not allowed in the stadium) being notable outliers',
    x = 'Year',
    y = 'Home Win Percentage (%)'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title.x = element_text(size = 12, face = 'bold', margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = 'bold', margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = 'gray90'),
    plot.margin = margin(20, 20, 20, 20)
  )


# Code for heatmap

df = read.csv('~/Combined_stats.csv')

df_2020 <- df |> filter(Year == 2020)

teams_2020 <- unique(c(df_2020$HomeTeam, df_2020$AwayTeam))
teams_2020 <- sort(teams_2020)

df <- df |>
  filter(HomeTeam %in% teams_2020 & AwayTeam %in% teams_2020)

home_stats <- df |>
  group_by(HomeTeam) |>
  summarise(
    HomeFouls = sum(HF, na.rm = TRUE),
    HomeYellows = sum(HY, na.rm = TRUE),
    HomeReds = sum(HR, na.rm = TRUE),
    HomeGames = n()
  )

away_stats <- df |>
  group_by(AwayTeam) |>
  summarise(
    AwayFouls = sum(AF, na.rm = TRUE),
    AwayYellows = sum(AY, na.rm = TRUE),
    AwayReds = sum(AR, na.rm = TRUE),
    AwayGames = n()
  )

combined <- home_stats |>
  full_join(away_stats, by = c("HomeTeam" = "AwayTeam")) |>
  rename(Team = HomeTeam)

combined <- combined |>
  mutate(
    FoulsPerHomeGame = HomeFouls / HomeGames,
    FoulsPerAwayGame = AwayFouls / AwayGames,
    YellowsPerHomeGame = HomeYellows / HomeGames,
    YellowsPerAwayGame = AwayYellows / AwayGames,
    RedsPerHomeGame = HomeReds / HomeGames,
    RedsPerAwayGame = AwayReds / AwayGames
  )

combined <- combined |>
  mutate(
    YellowsPerHomeFoul = ifelse(HomeFouls > 0, HomeYellows / HomeFouls, 0),
    YellowsPerAwayFoul = ifelse(AwayFouls > 0, AwayYellows / AwayFouls, 0)
  )

heatmap_data <- combined |>
  mutate(
    `Yellow cards per Foul` = YellowsPerHomeFoul / YellowsPerAwayFoul,
    `Yellow cards per Game` = YellowsPerHomeGame / YellowsPerAwayGame
  ) |>
  select(Team, `Yellow cards per Foul`, `Yellow cards per Game`)

heatmap_data <- heatmap_data |>
  mutate(
    `Yellow cards per Foul` = ifelse(is.infinite(`Yellow cards per Foul`) | is.nan(`Yellow cards per Foul`), 1, `Yellow cards per Foul`),
    `Yellow cards per Game` = ifelse(is.infinite(`Yellow cards per Game`) | is.nan(`Yellow cards per Game`), 1, `Yellow cards per Game`)
  )

heatmap_data <- heatmap_data |>
  arrange(desc(`Yellow cards per Foul`))

heatmap_matrix <- heatmap_data |>
  column_to_rownames("Team") |>
  as.matrix()

heatmap_melted <- melt(heatmap_matrix)
colnames(heatmap_melted) <- c("Team", "Metric", "Value")

plot_height <- max(8, nrow(heatmap_data) * 0.4)

p <- ggplot(heatmap_melted, aes(x = Metric, y = factor(Team, levels = rownames(heatmap_matrix)), fill = Value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Value)), color = "black", size = 3.5) +
  scale_fill_gradient2(
    low = "#1a9641",
    mid = "white",
    high = "#d7191c",
    midpoint = 1.0,
    limits = c(0.5, 1.5),
    name = "Ratio of \nHome Card Rate ÷ \nAway Card Rate\n"
  ) +
  labs(
    title = "Referee Bias Analysis: Home vs Away",
    subtitle = 'Ratio below 1.0 = fewer yellow cards at home (treated favorably at home);\nabove 1.0 = more yellow cards at home (treated unfavorably at home)',
    x = "",
    y = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank()
  )

print(p)

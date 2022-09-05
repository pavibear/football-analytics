library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsoccer)
library(grid)

rm(list = ls())

source("helpers.r")

### Declaring constants ----

match_id = 3795506
pitch_length = 105
pitch_width = 68
# choose possessions to represent in plot
possession_selection = c(6, 119)
# cutoff value to exclude events with little movement of the ball
cutoff <- 0.5

### Import data ----

match <- load_match(match_id)
teams <- unique(match$team.name)

### Extract possession chain information ----

len = length(possession_selection)
possessions <- list()
for (i in 1:len) {
  # filter for this possession
  possession <- match[match$possession == possession_selection[i],]
  # normalize coordinates to meters
  possession[, c("x", "y")] <-
    lapply(possession[, c("x", "y")], as.numeric)
  possession$x <- possession$x * pitch_length / 120
  possession$y <- possession$y * pitch_width / 80
  # correct for actions of other team as pitch coordinates are inverted in data
  possession$type.name[possession$team.name != possession$possession_team.name] <- "Opponent"
  possession$x[possession$type.name == "Opponent"] <-
    pitch_length - possession$x[possession$type.name == "Opponent"]
  possession$y[possession$type.name == "Opponent"] <-
    pitch_width - possession$y[possession$type.name == "Opponent"]
  # create end locations by using start location of consecutive event
  possession$x_end <- c(possession$x[-1], NA)
  possession$y_end <- c(possession$y[-1], NA)
  possession <- head(possession,-1)
  # select relevant columns
  possession <-
    select(
      possession,
      c(
        "possession_team.name",
        "possession",
        "type.name",
        "x",
        "y",
        "x_end",
        "y_end"
      )
    )
  # rename columns
  colnames(possession) <- c("team","possession","type","x","y","x_end", "y_end")
  # exclude events with little movement of the ball
  filter <-
    (abs(possession$x - possession$x_end) > cutoff) |
    (abs(possession$y - possession$y_end) > cutoff)
  possession <- possession[filter, ]
  # group interception, pressure, ball receipt etc. as Other
  possession$type[!(possession$type %in%  c("Pass", "Carry", "Shot", "Opponent"))] <- "Other"
  # enumerate passes
  possession$nr[possession$type == "Pass"] = 1:sum(possession$type == "Pass")
  # put result to list
  possessions[[i]] <- possession
}
# merge all possessions to one list
possessions <-
  Reduce(function(x, y)
    merge(x, y, all = TRUE), possessions)
# split by team
possessions_t1 <- possessions[possessions$team == teams[1],]
possessions_t2 <- possessions[possessions$team == teams[2],]

### Plotting ----

# load pitch measures
pitch_custom <- pitch_measures(pitch_length, pitch_width)
# fix colors for both plots
color_map <-
  c(
    "Pass" = "mediumturquoise",
    "Carry" = "darkorange",
    "Other" = "grey",
    "Opponent" = "lightslateblue",
    "Shot" = "red"
  )
# plotting function
chain_plot <- function(data, team) {
  return(
    ggplot(
    data,
    aes(
      x = x,
      xend = x_end,
      y = y,
      yend = y_end,
      color = type,
      label = nr
    )
  ) +
    annotate_pitch(dimensions = pitch_custom) +
    geom_segment(arrow = arrow(length = unit(0.01, "npc")), size = 0.5) +
    geom_label(
      size = 2,
      label.padding = unit(0.1, "lines"),
      color = "black"
    ) +
    ggtitle(team) +
    scale_colour_manual(values = color_map) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.key = element_rect(fill = "transparent"),
      legend.title = element_blank()
    ) +
    coord_flip() +
    theme_pitch(aspect_ratio = pitch_ratio)
  )
}

# actual plotting
plot_t1 <- chain_plot(possessions_t1, teams[1])
plot_t2 <- chain_plot(possessions_t2, teams[2])
ggarrange(
  plot_t1,
  plot_t2,
  ncol = 2,
  common.legend = TRUE,
  legend = "bottom"
)
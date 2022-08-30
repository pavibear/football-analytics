library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsoccer)
library(grid)
library(rjson)

rm(list = ls())

### Declaring constants ###

statsbomb_event_path = "/home/patrick/ownCloud/17_Soccer Analytics/uefa-euro-2020/data/match09/events_statsbomb.json"
pitch_length = 105
pitch_width = 68
# choose possessions to represent in plot
possession_selection = c(20, 62, 94)

### Import data ###

match <- fromJSON(file = statsbomb_event_path)
match <-
  lapply(match, function(x)
    data.frame(t(unlist(x)), stringsAsFactors = FALSE))
match <- rbindlist(match, fill = TRUE)
teams <- unique(match$team.name)

### Extract possession chain information ###

len = length(possession_selection)
possessions <- list()
for (i in 1:len) {
  # filter for this possession
  possession <- match[match$possession == possession_selection[i]]
  # normalize coordinates to meters
  possession[, c("location1", "location2")] <-
    lapply(possession[, c("location1", "location2")], as.numeric)
  possession$location1 <- possession$location1 * pitch_length / 120
  possession$location2 <- possession$location2 * pitch_width / 80
  # correct for actions of other team as pitch coordinates are inverted in data
  possession$type.name[possession$team.name != possession$possession_team.name] <- "Opponent"
  possession$location1[possession$type.name == "Opponent"] <-
    pitch_length - possession$location1[possession$type.name == "Opponent"]
  possession$location2[possession$type.name == "Opponent"] <-
    pitch_width - possession$location2[possession$type.name == "Opponent"]
  # create end locations by using start location of consecutive event
  possession$end_location1 <- c(possession$location1[-1], NA)
  possession$end_location2 <- c(possession$location2[-1], NA)
  possession <- head(possession,-1)
  # select relevant columns
  possession <-
    select(
      possession,
      c(
        "possession_team.name",
        "possession",
        "type.name",
        "location1",
        "location2",
        "end_location1",
        "end_location2"
      )
    )
  # exclude events with little movement of the ball
  cutoff <- 0.5
  filter <-
    (abs(possession$location1 - possession$end_location1) > cutoff) |
    (abs(possession$location2 - possession$end_location2) > cutoff)
  possession <- possession[filter, ]
  # group interception, pressure, ball receipt etc. as Other
  possession$type.name[!(possession$type.name %in%  c("Pass", "Carry", "Shot", "Opponent"))] <- "Other"
  # enumerate passes
  possession$nr[possession$type.name == "Pass"] = 1:sum(possession$type.name == "Pass")
  # put result to list
  possessions[[i]] <- possession
}
# merge all possessions to one list
possessions <-
  Reduce(function(x, y)
    merge(x, y, all = TRUE), possessions)
# split by team
possessions_t1 <- possessions[possession_team.name == teams[1]]
possessions_t2 <- possessions[possession_team.name == teams[2]]

### Plotting ###

pitch_custom <- list(
  length = pitch_length,
  width = pitch_width,
  penalty_box_length = 16.5,
  penalty_box_width = 16.5 * 2 + 7.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 5.5 * 2 + 7.32,
  penalty_spot_distance = 11,
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)
# fix colors for both plots
color_map <-
  c(
    "Pass" = "mediumturquoise",
    "Carry" = "darkorange",
    "Other" = "grey",
    "Opponent" = "lightslateblue",
    "Shot" = "red"
  )
# aspect_ratio pitch_length/pitch_width does not result in correct scales with theme_pitch, unclear why?
pitch_ratio = 1.46
# actual plotting
plot_t1 <-
  ggplot(
    possessions_t1,
    aes(
      x = location1,
      xend = end_location1,
      y = location2,
      yend = end_location2,
      color = type.name,
      label = nr
    )
  ) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length = unit(0.01, "npc")), size = 0.5) +
  geom_segment(size = 0.5) +
  geom_label(
    size = 2,
    label.padding = unit(0.1, "lines"),
    color = "black"
  ) +
  ggtitle(teams[1], " Olmo 15'") +
  scale_colour_manual(values = color_map) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_blank()
  ) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)
plot_t1

plot_t2 <-
  ggplot(
    possessions_t2,
    aes(
      x = location1,
      xend = end_location1,
      y = location2,
      yend = end_location2,
      color = type.name,
      label = nr
    )
  ) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length = unit(0.01, "npc")), size = 0.5) +
  geom_segment(size = 0.5) +
  geom_label(
    size = 2,
    label.padding = unit(0.1, "lines"),
    color = "black"
  ) +
  ggtitle(teams[2], " Berg 60', Isak 40'") +
  scale_colour_manual(values = color_map) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_blank()
  ) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)
plot_t2

ggarrange(
  plot_t1,
  plot_t2,
  ncol = 2,
  common.legend = TRUE,
  legend = "bottom"
)
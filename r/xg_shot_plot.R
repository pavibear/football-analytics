library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggsoccer)
library(grid)
library(stringr)

rm(list = ls())

source("helpers.r")

### Declaring constants ----

match_id = 3795506
pitch_length = 105
pitch_width = 68

### Import data ----

match <- load_match(match_id)
lineup <- load_lineup(match_id)
teams <- unique(match$team.name)

### Extract shot information ----

# filter events to get shots only
shots <- match[match$type.name == "Shot",]
# exclude penalties as too strong overlap for penalty shootouts
shots <- shots[!(shots$shot.type.name == "Penalty"),]
# merge with lineup information to get player nickname
nas <- is.na(lineup$player_nickname)
lineup$player_nickname[nas] <- lineup$player_name[nas]
shots <-
  merge(shots, lineup, by.x = "player.id", by.y = "player_id")
# calculate missing columns
shots$goal = shots$shot.outcome.name == "Goal"
shots$info = paste(word(shots$player_nickname,-1), " ", shots$minute, "'", sep = "")
# select relevant columns
shots <-
  select(shots,
         c(
           "shot.statsbomb_xg",
           "goal",
           "possession_team.name",
           "info",
           "x",
           "y"
         ))
# rename columns
colnames(shots) <- c("xg", "goal", "team", "info", "x", "y")
# convert strings to numerics
shots[, c("xg", "x", "y")] <-
  lapply(shots[, c("xg", "x", "y")], as.numeric)
# normalize coordinates to meters
shots$x <- shots$x * pitch_length / 120
shots$y <- shots$y * pitch_width / 80


### Plotting ----

# sort by descending xG in order for small circles to be on top
shots <- shots[order(-shots$xg),]
# load pitch measures
pitch_custom <- pitch_measures(pitch_length, pitch_width)
# variables for xlim and ylim
dist_center = max(16.5 + 7.32 / 2, max(abs(shots$y - pitch_width / 2)))
dist_groundline = max(11 + 9.15, min(shots$x))
buffer = 0.05
xlim_low = dist_groundline * (1 - buffer)
xlim_high =  pitch_length * 1.02
ylim_low = (pitch_width / 2 - dist_center) * (1 - buffer)
ylim_high = (pitch_width / 2 + dist_center) * (1 + buffer)
ratio = (xlim_high - xlim_low) / (ylim_high - ylim_low)
# actual plotting
ggplot(shots, aes(x = x, y = y)) +
  ggtitle("Statsbomb expected goals (xG) for shots") +
  annotate_pitch(dimensions = pitch_custom) +
  geom_point(aes(size = xg, color = team, shape = goal), fill = "white") +
  geom_text_repel(aes(label = info), size=2.5, max.overlaps = 5) +
  # modify scales and legends
  scale_shape_manual(labels=c("No goal", "Goal"), values=c(21, 19)) +
  scale_size_continuous(breaks=c(0.05, 0.1, 0.25, 0.5)) +
  labs(color = "", size = "", shape = "") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key = element_rect(fill = "transparent"),
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  # adjust pitch inputs
  coord_flip() +
  theme_pitch(aspect_ratio = ratio) +
  xlim(xlim_low, xlim_high) +
  ylim(ylim_low, ylim_high)
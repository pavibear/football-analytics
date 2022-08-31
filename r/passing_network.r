library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsoccer)
library(grid)
library(rjson)

rm(list = ls())

### Declaring constants ###

# global paths to source files
statsbomb_event_path <-
  "/home/patrick/ownCloud/17_Soccer Analytics/uefa-euro-2020/data/match09/events_statsbomb.json"
statsbomb_lineups_path <-
  "/home/patrick/ownCloud/17_Soccer Analytics/uefa-euro-2020/data/match09/lineups_statsbomb.json"
# pitch dimensions
pitch_length <- 105
pitch_width <- 68
# elimination constants in terms of share of total passes and passes or receptions
pass_pair_threshold <- 0.003
player_threshold <- 0.015
# decide whether to include passes/receipts when calculating average position and pairs (1 incl, 0 excl)
incl_passers <- 1
incl_recipients <- 1

### Import data ###

match <- fromJSON(file = statsbomb_event_path)
match <-
  lapply(match, function(x)
    data.frame(t(unlist(x)), stringsAsFactors = FALSE))
match <- rbindlist(match, fill = TRUE)
teams <- unique(match$team.name)
lineup <- fromJSON(file = statsbomb_lineups_path)
lineup <- rbind(lineup[[1]][[3]], lineup[[2]][[3]])
lineup <-
  lapply(lineup, function(x)
    data.frame(t(unlist(x)), stringsAsFactors = FALSE))
lineup <- rbindlist(lineup, fill = TRUE)[, 1:6]

### Extract passing information ###

# filter events to get passes only
passes <- match[match$type.name == "Pass", ]
# select relevant columns
passes <-
  passes[, c(
    "team.name",
    "player.id",
    "pass.recipient.id",
    "location1",
    "location2",
    "pass.end_location1",
    "pass.end_location2"
  )]
colnames(passes) <-
  c("team",
    "passer_id",
    "recipient_id",
    "x_start",
    "y_start",
    "x_end",
    "y_end")
# filter passes without target
passes <- passes[!(is.na(passes$recipient_id)), ]
# normalize coordinates to meters
passes[, c("x_start", "y_start", "x_end", "y_end")] <-
  lapply(passes[, c("x_start", "y_start", "x_end", "y_end")], as.numeric)
passes[, c("x_start", "x_end")] <-
  lapply(passes[, c("x_start", "x_end")], function(x)
    x * pitch_length / 120)
passes[, c("y_start", "y_end")] <-
  lapply(passes[, c("y_start", "y_end")], function(x)
    x * pitch_width / 80)
# loop over both teams to calculate pass pairs
pairs <- list()
players <- list()
for (i in 1:2) {
  # split data into two teams
  passes_ti <- passes[passes$team == teams[i], ]
  # get player average passing/reception position
  passers <- passes_ti %>%
    group_by(passer_id) %>%
    summarise(
      nr_passes = n(),
      x_pass_sum = sum(x_start),
      y_pass_sum = sum(y_start)
    )
  recipients <- passes_ti %>%
    group_by(recipient_id) %>%
    summarise(
      nr_receptions = n(),
      x_reception_sum = sum(x_end),
      y_reception_sum = sum(y_end)
    )
  players[[i]] <-
    merge(passers,
          recipients,
          by.x = "passer_id",
          by.y = "recipient_id",
          all = TRUE)
  names(players[[i]])[names(players[[i]]) == "passer_id"] <-
    "player_id"
  players[[i]][is.na(players[[i]])] <- 0
  players[[i]]$nr_actions <-
    incl_passers * players[[i]]$nr_passes + incl_recipients * players[[i]]$nr_receptions
  players[[i]]$x_mean <-
    (incl_passers * players[[i]]$x_pass_sum + incl_recipients * players[[i]]$x_reception_sum) / players[[i]]$nr_actions
  players[[i]]$y_mean <-
    (incl_passers * players[[i]]$y_pass_sum + incl_recipients * players[[i]]$y_reception_sum) / players[[i]]$nr_actions
  # get player share of total passes/receptions
  players[[i]]$action_share <-
    players[[i]]$nr_actions / sum(players[[i]]$nr_actions)
  # threshold for unimportant players
  players[[i]] <-
    players[[i]][players[[i]]$action_share > player_threshold, ]
  passes_ti <-
    passes_ti[(
      passes_ti$passer_id %in% players[[i]]$player_id &
        passes_ti$recipient_id %in% players[[i]]$player_id
    )]
  # add player info
  players[[i]] <-
    merge(players[[i]], lineup, by = "player_id")
  # group by player pairs
  pairs[[i]] <- passes_ti %>%
    group_by(passer_id, recipient_id) %>%
    summarise(count = n())
  # add x and y coordinates to pass pairs
  pairs[[i]] <-
    merge(pairs[[i]], players[[i]][, c("player_id", "x_mean", "y_mean")], by.x = "passer_id", by.y = "player_id")
  pairs[[i]] <-
    merge(pairs[[i]], players[[i]][, c("player_id", "x_mean", "y_mean")], by.x = "recipient_id", by.y = "player_id")
  colnames(pairs[[i]]) <-
    c("recipient_id",
      "passer_id",
      "count",
      "x_start",
      "y_start",
      "x_end",
      "y_end")
  # sort by count for plotting z order
  pairs[[i]] <- pairs[[i]][order(pairs[[i]]$count), ]
  # get pass pair share of total passes
  pairs[[i]]$pass_share <- pairs[[i]]$count / sum(pairs[[i]]$count)
  # threshold for unimportant pass pairs
  pairs[[i]] <-
    pairs[[i]][pairs[[i]]$pass_share > pass_pair_threshold, ]
  # put result in list
  pairs[[i]] <- pairs[[i]]
}

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
# aspect_ratio pitch_length/pitch_width does not result in correct scales with theme_pitch, unclear why?
pitch_ratio = 1.46
# factors to shift segments for better visibility
length_factor = 2
offset_factor = 0.5
# loop over both teams to create plots
plots <- list()
for (i in 1:2) {
  # calculate angles of segments and direction
  pairs[[i]]$angle <-
    atan((pairs[[i]]$y_end - pairs[[i]]$y_start) / (pairs[[i]]$x_end - pairs[[i]]$x_start))
  pairs[[i]] <-
    pairs[[i]] %>% mutate(direction = case_when(x_start < x_end ~ 1,
                                                TRUE ~ -1))
  # shorten segments to make space for labels
  pairs[[i]]$x_start <-
    pairs[[i]]$x_start + pairs[[i]]$direction * length_factor * cos(pairs[[i]]$angle)
  pairs[[i]]$x_end <-
    pairs[[i]]$x_end - pairs[[i]]$direction * length_factor * cos(pairs[[i]]$angle)
  pairs[[i]]$y_start <-
    pairs[[i]]$y_start + pairs[[i]]$direction * length_factor * sin(pairs[[i]]$angle)
  pairs[[i]]$y_end <-
    pairs[[i]]$y_end - pairs[[i]]$direction * length_factor * sin(pairs[[i]]$angle)
  # find unique pairings not to offset
  passer_recipient <- pairs[[i]]$passer_id < pairs[[i]]$recipient_id
  pairs[[i]]$ordered <-
    paste(pairs[[i]]$recipient_id, " ", pairs[[i]]$passer_id)
  pairs[[i]]$ordered[passer_recipient] <-
    paste(pairs[[i]]$passer_id[passer_recipient], " ",
          pairs[[i]]$recipient_id[passer_recipient])
  pairs[[i]]$unique <- 1
  pairs[[i]]$unique[!pairs[[i]]$ordered %in% pairs[[i]]$ordered[duplicated(pairs[[i]]$ordered)]] <-
    0
  # offset segments between same pair of players
  pairs[[i]]$x_start <-
    pairs[[i]]$x_start + pairs[[i]]$unique * pairs[[i]]$direction * offset_factor * sin(pairs[[i]]$angle)
  pairs[[i]]$x_end <-
    pairs[[i]]$x_end + pairs[[i]]$unique * pairs[[i]]$direction * offset_factor * sin(pairs[[i]]$angle)
  pairs[[i]]$y_start <-
    pairs[[i]]$y_start - pairs[[i]]$unique * pairs[[i]]$direction * offset_factor * cos(pairs[[i]]$angle)
  pairs[[i]]$y_end <-
    pairs[[i]]$y_end - pairs[[i]]$unique * pairs[[i]]$direction * offset_factor * cos(pairs[[i]]$angle)
  # actual plotting
  plots[[i]] <- ggplot() +
    annotate_pitch(dimensions = pitch_custom) +
    geom_segment(
      aes(
        x = x_start,
        y = y_start,
        xend = x_end,
        yend = y_end,
        color = count
      ),
      pairs[[i]],
      arrow = arrow(length = unit(0.01, "npc"), type = "closed")
    ) +
    geom_text(aes(x = x_mean, y = y_mean, label = jersey_number),
              players[[i]],
              size = unit(3, "lines")) +
    viridis::scale_color_viridis(name = "# passes",
                                 option = "C",
                                 direction = -1) +
    ggtitle(teams[i],) +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_flip() +
    theme_pitch(aspect_ratio = pitch_ratio)
}
ggarrange(plots[[1]], plots[[2]], ncol = 2)

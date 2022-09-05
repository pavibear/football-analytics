library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsoccer)
library(grid)
library(rjson)

rm(list = ls())

source("helpers.r")

### Declaring constants ----

match_id = 3795506
binwidth = 5

### Import data ----

match <- load_match(match_id)
teams <- unique(match$team.name)

### Extract passing information ----

# filter events to get passes only
passes <- match[match$type.name == "Pass", ]
# select relevant columns
passes <- select(passes, c("possession_team.name", "possession"))
# separate by team and whether possession led to shot or not
shots <- match[match$type.name == "Shot", "possession"]
pass_att_t1 <-
  passes[(passes$possession %in% shots) &
           passes$possession_team.name == teams[1], ]
pass_noatt_t1 <-
  passes[!(passes$possession %in% shots) &
           passes$possession_team.name == teams[1], ]
pass_att_t2 <-
  passes[(passes$possession %in% shots) &
           passes$possession_team.name == teams[2], ]
pass_noatt_t2 <-
  passes[!(passes$possession %in% shots) &
           passes$possession_team.name == teams[2], ]
# summarize number of passes per possession
pass_att_t1 <-
  pass_att_t1 %>% group_by(possession) %>% summarise(seq_len = n())
pass_noatt_t1 <-
  pass_noatt_t1 %>% group_by(possession) %>% summarise(seq_len = n())
pass_att_t2 <-
  pass_att_t2 %>% group_by(possession) %>% summarise(seq_len = n())
pass_noatt_t2 <-
  pass_noatt_t2 %>% group_by(possession) %>% summarise(seq_len = n())
# recombine possessions leading to shots and possessions not leading to shots
pass_sequ_t1 <-
  rbind(pass_att_t1 %>% mutate(attempt = "Shot"),
        pass_noatt_t1 %>% mutate(attempt = "No shot"))
pass_sequ_t2 <-
  rbind(pass_att_t2 %>% mutate(attempt = "Shot"),
        pass_noatt_t2 %>% mutate(attempt = "No shot"))

### Histogram plotting ----

# plotting function
pass_plot <- function(data, team) {
  return(ggplot(data, aes(x = seq_len, fill = attempt)) +
    geom_histogram(binwidth = binwidth, color = "black", boundary = 0) +
    ggtitle(team) +
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position ="none") +
    xlab("Length of a pass sequence") +
    ylab("Count")
  )
}
# plot by team, second team with additional legend
plot_t1 <- pass_plot(pass_sequ_t1, teams[1])
plot_t2 <- pass_plot(pass_sequ_t2, teams[2]) +
  theme(legend.position = c(0.8, 0.9)) +
  labs(fill = "")
# extract automatic x and y limits to get the same scale
x_limit <-
  max(layer_scales(plot_t1)$x$range$range,
      layer_scales(plot_t2)$x$range$range)
y_limit <-
  max(layer_scales(plot_t1)$y$range$range,
      layer_scales(plot_t2)$y$range$range)
plot_t1 <- plot_t1 + xlim(0, x_limit) + ylim(0, y_limit)
plot_t2 <- plot_t2 + xlim(0, x_limit) + ylim(0, y_limit)
# combine plots
ggarrange(plot_t1, plot_t2, ncol = 2)
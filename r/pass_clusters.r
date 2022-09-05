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
nr_clusters <- 25

### Import data ----

match <- load_match(match_id)
teams <- unique(match$team.name)

### Extract passing information ----

# filter events to get passes only
passes <- match[match$type.name == "Pass", ]
# select relevant columns
passes <-
  select(
    passes,
    c(
      "possession_team.name",
      "x",
      "y",
      "x_end",
      "y_end"
    )
  )
colnames(passes) <-
  c(
    "Team",
    "X.Pass",
    "Y.Pass",
    "X.Receive",
    "Y.Receive"
  )
# normalize coordinates to meters
passes[, c(2:5)] <-
  lapply(passes[, c(2:5)], as.numeric)
passes[, c(2, 4)] <-
  lapply(passes[, c(2, 4)], function(x)
    x * pitch_length / 120)
passes[, c(3, 5)] <-
  lapply(passes[, c(3, 5)], function(x)
    x * pitch_width / 80)
# group by team
passes_t1 <- passes[passes$Team == teams[1], ]
passes_t2 <- passes[passes$Team == teams[2], ]

### Clustering ----

# kmeans with random starting points
passes_t1$cluster <-
  kmeans(passes_t1[, c(2, 3, 4, 5)], nr_clusters)$cluster
passes_t2$cluster <-
  kmeans(passes_t2[, c(2, 3, 4, 5)], nr_clusters)$cluster
# summarizing clusters with average passing and receiving positions
clusters_t1 <-
  passes_t1 %>% group_by(cluster) %>% summarise(
    X.Pass = mean(X.Pass),
    Y.Pass = mean(Y.Pass),
    X.Receive = mean(X.Receive),
    Y.Receive = mean(Y.Receive),
    count = n()
  )
clusters_t2 <-
  passes_t2 %>% group_by(cluster) %>% summarise(
    X.Pass = mean(X.Pass),
    Y.Pass = mean(Y.Pass),
    X.Receive = mean(X.Receive),
    Y.Receive = mean(Y.Receive),
    count = n()
  )
# rank by cluster size
clusters_t1$rank <- rank(-clusters_t1$count, ties.method = "first")
clusters_t2$rank <- rank(-clusters_t2$count, ties.method = "first")

### Plotting ----

# load pitch measures
pitch_custom <- pitch_measures(pitch_length, pitch_width)

plot_t1 <-
  ggplot(
    clusters_t1,
    aes(
      x = X.Pass,
      xend = X.Receive,
      y = Y.Pass,
      yend = Y.Receive,
      color = count,
      label = rank
    )
  ) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length = unit(0.03, "npc")), size = 1) +
  geom_label(
    size = 3,
    label.padding = unit(0.1, "lines"),
    color = "black"
  ) +
  viridis::scale_color_viridis(name = '# passes',
                               option = "C",
                               direction = -1) +
  ggtitle(teams[1],) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)

plot_t2 <-
  ggplot(
    clusters_t2,
    aes(
      x = X.Pass,
      xend = X.Receive,
      y = Y.Pass,
      yend = Y.Receive,
      color = count,
      label = rank
    )
  ) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length = unit(0.03, "npc")), size = 1) +
  geom_label(
    size = 3,
    label.padding = unit(0.1, "lines"),
    color = "black"
  ) +
  viridis::scale_color_viridis(name = '# passes',
                               option = "C",
                               direction = -1) +
  ggtitle(teams[2],) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)

ggarrange(plot_t1, plot_t2, ncol = 2)

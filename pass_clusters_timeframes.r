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
nr_clusters <- 25

### Import data ###

match <- fromJSON(file=statsbomb_event_path)
match <- lapply(match, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
match <- rbindlist(match,fill=TRUE)
teams <- unique(match$team.name)

### Extract passing information ###

# filter events to get passes for one team only
passes <- match[match$type.name=="Pass",]
passes <- passes[passes$possession_team.name==teams[1],]
# select relevant columns
passes <- select(passes, c("possession_team.name", "possession","player.name","location1","location2","pass.recipient.name","pass.end_location1","pass.end_location2", "pass.length", "pass.angle", "pass.height.name", "pass.type.name", "pass.body_part.name", "minute"))
colnames(passes) <- c("Team", "Possession", "Passer", "X.Pass", "Y.Pass", "Receiver", "X.Receive", "Y.Receive", "Pass.Length", "Pass.Angle", "Pass.Height", "Pass.Type", "Body.Part", "minute")
# normalize coordinates to meters
passes[,c(2,4,5,7:10)] <- lapply(passes[,c(2,4,5,7:10)], as.numeric)
passes[,c(4,7)] <- lapply(passes[,c(4,7)], function(x) x * pitch_length / 120)
passes[,c(5,8)] <- lapply(passes[,c(5,8)], function(x) x * pitch_width / 80)
# group by timeframe
passes_1 <- passes[passes$minute<15,]
passes_2 <- passes[passes$minute>=15&passes$minute<30,]
passes_3 <- passes[passes$minute>=30&passes$minute<45,]
passes_4 <- passes[passes$minute>=45&passes$minute<60,]
passes_5 <- passes[passes$minute>=60&passes$minute<75,]
passes_6 <- passes[passes$minute>=75,]

### Clustering ###

# kmeans with random starting points
passes_1$cluster <- kmeans(passes_1[,c(4,5,7,8)], nr_clusters)$cluster
passes_2$cluster <- kmeans(passes_2[,c(4,5,7,8)], nr_clusters)$cluster
passes_3$cluster <- kmeans(passes_3[,c(4,5,7,8)], nr_clusters)$cluster
passes_4$cluster <- kmeans(passes_4[,c(4,5,7,8)], nr_clusters)$cluster
passes_5$cluster <- kmeans(passes_5[,c(4,5,7,8)], nr_clusters)$cluster
passes_6$cluster <- kmeans(passes_6[,c(4,5,7,8)], nr_clusters)$cluster
# summarizing clusters with average passing and receiving positions
clusters_1 <- passes_1 %>% group_by(cluster) %>% summarise(X.Pass=mean(X.Pass), Y.Pass=mean(Y.Pass), X.Receive=mean(X.Receive), Y.Receive=mean(Y.Receive), count= n())
clusters_2 <- passes_2 %>% group_by(cluster) %>% summarise(X.Pass=mean(X.Pass), Y.Pass=mean(Y.Pass), X.Receive=mean(X.Receive), Y.Receive=mean(Y.Receive), count= n())
clusters_3 <- passes_3 %>% group_by(cluster) %>% summarise(X.Pass=mean(X.Pass), Y.Pass=mean(Y.Pass), X.Receive=mean(X.Receive), Y.Receive=mean(Y.Receive), count= n())
clusters_4 <- passes_4 %>% group_by(cluster) %>% summarise(X.Pass=mean(X.Pass), Y.Pass=mean(Y.Pass), X.Receive=mean(X.Receive), Y.Receive=mean(Y.Receive), count= n())
clusters_5 <- passes_5 %>% group_by(cluster) %>% summarise(X.Pass=mean(X.Pass), Y.Pass=mean(Y.Pass), X.Receive=mean(X.Receive), Y.Receive=mean(Y.Receive), count= n())
clusters_6 <- passes_6 %>% group_by(cluster) %>% summarise(X.Pass=mean(X.Pass), Y.Pass=mean(Y.Pass), X.Receive=mean(X.Receive), Y.Receive=mean(Y.Receive), count= n())
# rank by cluster size
clusters_1$rank <- rank(-clusters_1$count, ties.method="first")
clusters_2$rank <- rank(-clusters_2$count, ties.method="first")
clusters_3$rank <- rank(-clusters_3$count, ties.method="first")
clusters_4$rank <- rank(-clusters_4$count, ties.method="first")
clusters_5$rank <- rank(-clusters_5$count, ties.method="first")
clusters_6$rank <- rank(-clusters_6$count, ties.method="first")

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

plot_1 <- ggplot(clusters_1, aes(x=X.Pass, xend=X.Receive, y=Y.Pass, yend=Y.Receive, color=count, label=rank)) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length=unit(0.03, "npc")), size=1) +
  # geom_label(size=3, label.padding=unit(0.1, "lines"), color="black") +
  viridis::scale_color_viridis(name = '# passes', option = "C", direction = -1) +
  ggtitle("0-15") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)

plot_2 <- ggplot(clusters_2, aes(x=X.Pass, xend=X.Receive, y=Y.Pass, yend=Y.Receive, color=count, label=rank)) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length=unit(0.03, "npc")), size=1) +
  # geom_label(size=3, label.padding=unit(0.1, "lines"), color="black") +
  viridis::scale_color_viridis(name = '# passes', option = "C", direction = -1) +
  ggtitle("15-30") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)

plot_3 <- ggplot(clusters_3, aes(x=X.Pass, xend=X.Receive, y=Y.Pass, yend=Y.Receive, color=count, label=rank)) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length=unit(0.03, "npc")), size=1) +
  # geom_label(size=3, label.padding=unit(0.1, "lines"), color="black") +
  viridis::scale_color_viridis(name = '# passes', option = "C", direction = -1) +
  ggtitle("30-45") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)

plot_4 <- ggplot(clusters_4, aes(x=X.Pass, xend=X.Receive, y=Y.Pass, yend=Y.Receive, color=count, label=rank)) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length=unit(0.03, "npc")), size=1) +
  # geom_label(size=3, label.padding=unit(0.1, "lines"), color="black") +
  viridis::scale_color_viridis(name = '# passes', option = "C", direction = -1) +
  ggtitle("45-60") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)

plot_5 <- ggplot(clusters_5, aes(x=X.Pass, xend=X.Receive, y=Y.Pass, yend=Y.Receive, color=count, label=rank)) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length=unit(0.03, "npc")), size=1) +
  # geom_label(size=3, label.padding=unit(0.1, "lines"), color="black") +
  viridis::scale_color_viridis(name = '# passes', option = "C", direction = -1) +
  ggtitle("60-75") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)

plot_6 <- ggplot(clusters_6, aes(x=X.Pass, xend=X.Receive, y=Y.Pass, yend=Y.Receive, color=count, label=rank)) +
  annotate_pitch(dimensions = pitch_custom) +
  geom_segment(arrow = arrow(length=unit(0.03, "npc")), size=1) +
  # geom_label(size=3, label.padding=unit(0.1, "lines"), color="black") +
  viridis::scale_color_viridis(name = '# passes', option = "C", direction = -1) +
  ggtitle("75-end") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  theme_pitch(aspect_ratio = pitch_ratio)

ggarrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, nrow=2, ncol=3)
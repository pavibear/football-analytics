library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsoccer)
library(grid)
library(rjson)

rm(list = ls())

### Data path ###

statsbomb_event_path = "/home/patrick/ownCloud/17_Soccer Analytics/uefa-euro-2020/data/match09/events_statsbomb.json"

### Import data ###

match <- fromJSON(file=statsbomb_event_path)
match <- lapply(match, function(x) data.frame(t(unlist(x)), stringsAsFactors = FALSE))
match <- rbindlist(match,fill=TRUE)
teams <- unique(match$team.name)

### Extract passing information ###

# filter events to get passes only
passes <- match[match$type.name=="Pass",]
# select relevant columns
passes <- select(passes, c("possession_team.name", "possession"))
# separate by team and whether possession led to shot or not
shots <- match[match$type.name=="Shot"]$possession
pass_att_t1 <- passes[(passes$possession %in% shots)&passes$possession_team.name==teams[1]]
pass_noatt_t1 <- passes[!(passes$possession %in% shots)&passes$possession_team.name==teams[1]]
pass_att_t2 <- passes[(passes$possession %in% shots)&passes$possession_team.name==teams[2]]
pass_noatt_t2 <- passes[!(passes$possession %in% shots)&passes$possession_team.name==teams[2]]
# summarize number of passes per possession
pass_att_t1 <- pass_att_t1 %>% group_by(possession) %>% summarise(seq_len=n())
pass_noatt_t1 <- pass_noatt_t1 %>% group_by(possession) %>% summarise(seq_len=n())
pass_att_t2 <- pass_att_t2 %>% group_by(possession) %>% summarise(seq_len=n())
pass_noatt_t2 <- pass_noatt_t2 %>% group_by(possession) %>% summarise(seq_len=n())
# recombine possessions leading to shots and possessions not leading to shots
pass_sequ_t1 <- rbind(pass_att_t1 %>% mutate(attempt="Shot"), pass_noatt_t1 %>% mutate(attempt="No shot"))
pass_sequ_t2 <- rbind(pass_att_t2 %>% mutate(attempt="Shot"), pass_noatt_t2 %>% mutate(attempt="No shot"))

### Histogram plotting ###

plot_t1 <- ggplot(data=pass_sequ_t1, aes(x=seq_len, fill=attempt)) +
  geom_histogram(binwidth = 5, color="black") +
  ggtitle(teams[1]) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill="Outcome") +
  xlab("Length of a pass sequence") +
  ylab("Count")

plot_t2 <- ggplot(data=pass_sequ_t2, aes(x=seq_len, fill=attempt)) +
  geom_histogram(binwidth = 1, color="black") +
  ggtitle(teams[2]) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill="Outcome") +
  xlab("Length of a pass sequence") +
  ylab("Count")

ggarrange(plot_t1, plot_t2, ncol = 2, common.legend = TRUE, legend="right")
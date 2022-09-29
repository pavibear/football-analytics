# football-analytics

The following plots can be generated for all matches in the [StatsBomb Open Data repository](https://github.com/statsbomb/open-data) by adjusting the match_id variable in the source files.
Where deviating from the 105mx68m standard dimensions, pitch_length and pitch_width variables have to be adjusted for correct representation.

### XG shot plot (in r/python)
Does not include penalties and own goals\
![r_xg_shot_plot_3788750](https://user-images.githubusercontent.com/41541395/193134841-56b3be04-3a5b-464e-afaa-fde75ba5180c.png)

### Pass clusters (in r)
Adjustment of nr_clusters might be required depending on the number of total passes\
Careful: the starting configuration for the clusters depends on the random seed, repeated runs create different plots\
![r_pass_clusters_3788750](https://user-images.githubusercontent.com/41541395/193134622-59564dd0-b178-46ad-b6da-1117c495880b.png)

### Passing network (in r)
Adjustments of pass_pair_threshold and player_threshold might be required for better overview\
Default player position is the average of all passing and receiving positions for each player\
![r_passing_network_3788750](https://user-images.githubusercontent.com/41541395/193134684-e4794e52-dea9-4e1c-b3d2-6d3914d724b3.png)

### Possession chains (in r)
Requires a selection of possession ids in the possession_selection list variable (e.g. 6 and 119 in this plot)\
![r_possession_chains_3788750](https://user-images.githubusercontent.com/41541395/193134781-9b49d81c-1021-4f18-aa36-c81b5807a87d.png)

### Passes before attempt (in r)
Adjustment of binwidth might be required depending on the number of total pass sequences\
![r_passes_before_attempt_3788750](https://user-images.githubusercontent.com/41541395/193134855-cd95f880-824a-49c9-8f54-26311054e8ae.png)

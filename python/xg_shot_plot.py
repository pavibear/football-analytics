import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.axes import Axes
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
# Installation issue solved with https://github.com/coqui-ai/TTS/issues/1808
from statsbombpy import sb

import labels
import pitch


def prepare_data(match_id, pitch_length, pitch_width, orient_vertical=False):
    # load datasets into dataframes
    shots = sb.events(match_id, split=True)['shots']
    lineup_dict = sb.lineups(match_id)
    teams = list(lineup_dict.keys())
    lineup = pd.concat([lineup_dict[teams[0]], lineup_dict[teams[1]]])

    # extract and rename necessary columns
    shots = shots[['location', 'shot_statsbomb_xg', 'shot_outcome',
                   'possession_team', 'minute', 'player_id', 'player']]
    shots = shots.rename(
        columns={'shot_statsbomb_xg': 'xg', 'possession_team': 'team'})
    lineup = lineup[['player_id', 'player_nickname']]

    # calculate missing columns
    shots['isGoal'] = shots['shot_outcome'] == 'Goal'
    shots['x'], shots['y'] = zip(*shots['location'])
    shots = pd.merge(shots, lineup, on='player_id')
    shots['player'] = shots['player_nickname'].fillna(shots['player'])
    shots['info'] = shots['player'].str.split().str[-1] + ' ' + \
        shots['minute'].map(str) + '\''

    # normalize coordinates
    shots['x'] = shots['x'] * pitch_length / 120
    shots['y'] = pitch_width - shots['y'] * pitch_width / 80

    # unify column selection and indexing
    shots = shots[['x', 'y', 'xg', 'team', 'isGoal', 'info']]
    shots = shots.reset_index(drop=True)

    # switch 'x' and 'y' for orient_vertical
    if orient_vertical:
        shots = shots.rename(columns={'x': 'y', 'y': 'x'})
        shots['x'] = pitch_width - shots['x']

    return shots


def scatter_shots(
    ax: Axes,
    data: pd.DataFrame,
    shot_markersize_factor,
    colors=['tab:blue', 'tab:orange'],
    print_info=True
):
    # prepare data
    teams = data['team'].unique()
    data.loc[data['team'] == teams[0], 'edgecolor'] = colors[0]
    data.loc[data['team'] == teams[1], 'edgecolor'] = colors[1]
    data['facecolor'] = data['edgecolor']
    data.loc[data['isGoal'] == False, 'facecolor'] = 'white'
    data = data.sort_values('xg', ascending=False)

    # create scatter plot
    ax.scatter(data['x'], data['y'], marker='o', facecolor=data['facecolor'],
               edgecolor=data['edgecolor'], s=data['xg'] * shot_markersize_factor)

    # add labels
    if print_info:
        labels.repel(ax, data['x'], data['y'], data['info'], k=1.5)


def add_xg_legend(ax: Axes, title, shot_size_factor, bbox_to_anchor=(0, 1)):
    # choose different xg values to show
    xg_labels = [0.05, 0.25, 0.5]

    # create symbols
    xg_symbols = [Line2D([], [], marker='o', linestyle='none',
                         markerfacecolor='none', markeredgecolor='black',
                         markersize=np.sqrt(i*shot_size_factor)
                         ) for i in xg_labels]
    symbols = xg_symbols + [Line2D([], [], marker='o', linestyle='none',
                                   color='black',
                                   markersize=np.sqrt(0.25*shot_size_factor))]
    labels = xg_labels+['goal']

    # combine symbols and labels to legend
    legend_xg = ax.legend(symbols, labels, title=title, ncol=4, fontsize=8,
                          title_fontsize=8, loc='upper left', frameon=False,
                          bbox_to_anchor=bbox_to_anchor)
    legend_xg._legend_box.align = 'left'
    legend_xg._legend_box.sep = 10
    return legend_xg

def shot_plot(
    match_id,
    save_path='output/',
    pitch_length=105,
    pitch_width=68,
    orient_vertical=True,
    print_info=True,
    shot_size_factor=800,
    colors=['tab:red', 'tab:blue']
):
    # prepare variables
    shots = prepare_data(match_id, pitch_length, pitch_width, orient_vertical=orient_vertical)
    teams = shots['team'].unique()

    # plot on entire pitch
    fig, ax = plt.subplots(figsize=(10, 7))
    scatter_shots(ax, shots, shot_size_factor,
                  colors=colors, print_info=print_info)
    pitch.draw(ax, orient_vertical=orient_vertical)
    plt.axis('equal')
    ax.set_axis_off()

    # dynamic zoom on area of shots
    buffer = 0.05
    x = 'y' if orient_vertical else 'x'
    y = 'x' if orient_vertical else 'y'
    dist_center = max(
        16.5 + 7.32 / 2, np.max(abs(shots[y] - pitch_width / 2)))
    dist_groundline = max(11 + 9.15, np.min(shots[x]))
    xlim = [dist_groundline * (1 - buffer), pitch_length * 1.07]
    ylim = [(pitch_width / 2 - dist_center) * (1 - buffer),
            (pitch_width / 2 + dist_center) * (1 + buffer)]
    ax.set_xlim(ylim if orient_vertical else xlim)
    ax.set_ylim(xlim if orient_vertical else ylim)

    # add legends
    title = 'Statsbomb expected goals (xG) for shots'
    legend_xg = add_xg_legend(ax, title, shot_size_factor)
    ax.legend([Patch(color=colors[i]) for i in [0, 1]],
              teams, loc='lower left', fontsize=8, frameon=False)
    plt.gca().add_artist(legend_xg)

    plt.savefig(save_path+'xg_shot_plot.png',
                bbox_inches='tight')
import json
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.axes import Axes
from matplotlib.patches import Arc
from matplotlib.transforms import Affine2D
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import networkx as nx

# TODO add paths and pitch dimensions
save_path = '/home/patrick/ownCloud/Football Analytics/output/'
events_path = '/home/patrick/ownCloud/Football Analytics/sample_data/events_statsbomb.json'
lineup_path = '/home/patrick/ownCloud/Football Analytics/sample_data/lineups_statsbomb.json'
pitch_length = 105
pitch_width = 68


def get_json(path):
    with open(path, encoding='utf8') as f:
        js = json.load(f)
        return js


def prep_data(events_path, lineup_path, pitch_length, pitch_width, orient_vertical=False):
    # load datasets into dataframes
    events = pd.json_normalize(get_json(events_path))
    lineup = pd.json_normalize(get_json(lineup_path))
    lineup = pd.concat([pd.json_normalize(lineup['lineup'][0]),
                       pd.json_normalize(lineup['lineup'][1])])

    # filter for shots
    shots = events[events['type.name'] == 'Shot']

    # extract and rename necessary columns
    shots = shots[['location', 'shot.statsbomb_xg', 'shot.outcome.name',
                   'possession_team.name', 'minute', 'player.id', 'player.name']]
    shots = shots.rename(
        columns={'shot.statsbomb_xg': 'xg', 'possession_team.name': 'team'})
    lineup = lineup[['player_id', 'player_nickname']]

    # calculate missing columns
    shots['isGoal'] = shots['shot.outcome.name'] == 'Goal'
    shots['x'], shots['y'] = zip(*shots['location'])
    shots = pd.merge(shots, lineup, left_on='player.id', right_on='player_id')
    shots['player.name'] = shots['player_nickname'].fillna(
        shots['player.name'])
    shots['info'] = shots['player.name'].str.split().str[-1] + ' ' + \
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


def draw_pitch(
    ax: Axes,
    pitch_length: float = 105,
    pitch_width: float = 68,
    circle_radius: float = 9.15,
    penalty_distance: float = 11,
    box_length: float = 16.5,
    goalbox_length: float = 5.5,
    goal_width: float = 7.32,
    goal_length: float = 1.5,
    linewidth: float = 1.2,
    linecolor="black",
    zorder: int = -10,
    orient_vertical: bool = False,
):
    if orient_vertical:
        transform = Affine2D().rotate_deg(90).scale(-1, 1) + ax.transData
    else:
        transform = ax.transData

    plot_arguments = dict(
        color=linecolor, zorder=zorder, transform=transform, linewidth=linewidth
    )

    # reference distances based on lecture slides 22-03-02.pdf p. 19
    # c = center, gp = goalpost, b = box, h = half, w = width, l = length
    c_to_gp = goal_width / 2
    c_to_b = c_to_gp + box_length
    c_to_gb = c_to_gp + goalbox_length
    pitch_l_h = pitch_length / 2
    pitch_w_h = pitch_width / 2

    # pitch outline & centre line
    ax.plot([0, 0], [0, pitch_width], **plot_arguments)
    ax.plot([0, pitch_length], [pitch_width, pitch_width], **plot_arguments)
    ax.plot([pitch_length, pitch_length], [pitch_width, 0], **plot_arguments)
    ax.plot([pitch_length, 0], [0, 0], **plot_arguments)
    ax.plot([pitch_l_h, pitch_l_h], [0, pitch_width], **plot_arguments)

    # left penalty area
    ax.plot([box_length, box_length],
            [pitch_w_h - c_to_b, pitch_w_h + c_to_b], **plot_arguments)
    ax.plot([0, box_length],
            [pitch_w_h + c_to_b, pitch_w_h + c_to_b], **plot_arguments)
    ax.plot([box_length, 0],
            [pitch_w_h - c_to_b, pitch_w_h - c_to_b], **plot_arguments)

    # right penalty area
    ax.plot([pitch_length, pitch_length - box_length],
            [pitch_w_h + c_to_b, pitch_w_h + c_to_b], **plot_arguments)
    ax.plot([pitch_length - box_length, pitch_length - box_length],
            [pitch_w_h + c_to_b, pitch_w_h - c_to_b], **plot_arguments)
    ax.plot([pitch_length - box_length, pitch_length],
            [pitch_w_h - c_to_b, pitch_w_h - c_to_b], **plot_arguments)

    # left 6-yard box
    ax.plot([goalbox_length, goalbox_length],
            [pitch_w_h - c_to_gb, pitch_w_h + c_to_gb], **plot_arguments)
    ax.plot([0, goalbox_length],
            [pitch_w_h + c_to_gb, pitch_w_h + c_to_gb], **plot_arguments)
    ax.plot([goalbox_length, 0],
            [pitch_w_h - c_to_gb, pitch_w_h - c_to_gb], **plot_arguments)

    # right 6-yard box
    ax.plot([pitch_length, pitch_length - goalbox_length],
            [pitch_w_h + c_to_gb, pitch_w_h + c_to_gb], **plot_arguments)
    ax.plot([pitch_length - goalbox_length, pitch_length - goalbox_length],
            [pitch_w_h + c_to_gb, pitch_w_h - c_to_gb], **plot_arguments)
    ax.plot([pitch_length - goalbox_length, pitch_length],
            [pitch_w_h - c_to_gb, pitch_w_h - c_to_gb], **plot_arguments)

    # left goal
    ax.plot([0, -goal_length],
            [pitch_w_h - c_to_gp, pitch_w_h - c_to_gp], **plot_arguments)
    ax.plot([-goal_length, -goal_length],
            [pitch_w_h - c_to_gp, pitch_w_h + c_to_gp], **plot_arguments)
    ax.plot([-goal_length, 0],
            [pitch_w_h + c_to_gp, pitch_w_h + c_to_gp], **plot_arguments)

    # right goal
    ax.plot([pitch_length, pitch_length + goal_length],
            [pitch_w_h - c_to_gp, pitch_w_h - c_to_gp], **plot_arguments)
    ax.plot([pitch_length + goal_length, pitch_length + goal_length],
            [pitch_w_h - c_to_gp, pitch_w_h + c_to_gp], **plot_arguments)
    ax.plot([pitch_length + goal_length, pitch_length],
            [pitch_w_h + c_to_gp, pitch_w_h + c_to_gp], **plot_arguments)

    # prepare circles
    centre_circle = plt.Circle(
        (pitch_l_h, pitch_w_h), circle_radius, fill=False, **plot_arguments)
    centre_spot = plt.Circle(
        (pitch_l_h, pitch_w_h), linewidth / 4, **plot_arguments)
    left_pen_spot = plt.Circle(
        (penalty_distance, pitch_w_h), linewidth / 8, **plot_arguments)
    right_pen_spot = plt.Circle(
        (pitch_length - penalty_distance, pitch_w_h), linewidth / 8, **plot_arguments)

    # draw circles
    ax.add_patch(centre_circle)
    ax.add_patch(centre_spot)
    ax.add_patch(left_pen_spot)
    ax.add_patch(right_pen_spot)

    # prepare arcs
    # why is additional factor required?
    theta = np.rad2deg(np.cos((box_length-penalty_distance)/circle_radius))*1.1
    left_arc = Arc(
        (penalty_distance, pitch_w_h),
        height=circle_radius * 2,
        width=circle_radius * 2,
        angle=0,
        theta1=360-theta,
        theta2=theta,
        **plot_arguments,
    )
    right_arc = Arc(
        (pitch_length - penalty_distance, pitch_w_h),
        height=circle_radius * 2,
        width=circle_radius * 2,
        angle=0,
        theta1=180-theta,
        theta2=180+theta,
        **plot_arguments,
    )

    # draw arcs
    ax.add_patch(left_arc)
    ax.add_patch(right_arc)

# Source: https://stackoverflow.com/questions/14938541/how-to-improve-the-label-placement-for-matplotlib-scatter-chart-code-algorithm


def repel_labels(ax, x, y, labels, k=0.01):
    G = nx.DiGraph()
    data_nodes = []
    init_pos = {}
    for xi, yi, label in zip(x, y, labels):
        data_str = 'data_{0}'.format(label)
        G.add_node(data_str)
        G.add_node(label)
        G.add_edge(label, data_str)
        data_nodes.append(data_str)
        init_pos[data_str] = (xi, yi)
        init_pos[label] = (xi, yi)

    pos = nx.spring_layout(G, pos=init_pos, fixed=data_nodes, k=k)

    # undo spring_layout's rescaling
    pos_after = np.vstack([pos[d] for d in data_nodes])
    pos_before = np.vstack([init_pos[d] for d in data_nodes])
    scale, shift_x = np.polyfit(pos_after[:, 0], pos_before[:, 0], 1)
    scale, shift_y = np.polyfit(pos_after[:, 1], pos_before[:, 1], 1)
    shift = np.array([shift_x, shift_y])
    for key, val in pos.items():
        pos[key] = (val*scale) + shift

    for label, data_str in G.edges():
        ax.annotate(label,
                    xy=pos[data_str], xycoords='data',
                    xytext=pos[label], textcoords='data',
                    arrowprops=dict(arrowstyle="-",
                                    shrinkA=0, shrinkB=0,
                                    connectionstyle="arc3",
                                    color='black',
                                    linewidth=0.5),
                    fontsize=6
                    )
    # expand limits
    # pabeer: changed dict_values to list
    all_pos = np.vstack([i for i in pos.values()])
    x_span, y_span = np.ptp(all_pos, axis=0)
    mins = np.min(all_pos-x_span*0.15, 0)
    maxs = np.max(all_pos+y_span*0.15, 0)
    ax.set_xlim([mins[0], maxs[0]])
    ax.set_ylim([mins[1], maxs[1]])


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
        repel_labels(ax, data['x'], data['y'], data['info'], k=1.5)


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
    data: pd.DataFrame,
    name_provider='',
    pitch_length=105,
    pitch_width=68,
    orient_vertical=False,
    print_info=True,
    shot_size_factor=800,
    colors=['tab:red', 'tab:blue']
):
    # prepare variables
    teams = data['team'].unique()

    # plot on entire pitch
    fig, ax = plt.subplots(figsize=(10, 7))
    scatter_shots(ax, data, shot_size_factor,
                  colors=colors, print_info=print_info)
    draw_pitch(ax, orient_vertical=orient_vertical)
    plt.axis('equal')
    ax.set_axis_off()

    # dynamic zoom on area of shots
    buffer = 0.05
    x = 'y' if orient_vertical else 'x'
    y = 'x' if orient_vertical else 'y'
    dist_center = max(
        16.5 + 7.32 / 2, np.max(abs(shots[y] - pitch_width / 2)))
    dist_groundline = max(11 + 9.15, np.min(data[x]))
    xlim = [dist_groundline * (1 - buffer), pitch_length * 1.07]
    ylim = [(pitch_width / 2 - dist_center) * (1 - buffer),
            (pitch_width / 2 + dist_center) * (1 + buffer)]
    ax.set_xlim(ylim if orient_vertical else xlim)
    ax.set_ylim(xlim if orient_vertical else ylim)

    # add legends
    title = name_provider+' expected goals (xG) for shots'
    legend_xg = add_xg_legend(ax, title, shot_size_factor)
    ax.legend([Patch(color=colors[i]) for i in [0, 1]],
              teams, loc='lower left', fontsize=8, frameon=False)
    plt.gca().add_artist(legend_xg)

    plt.savefig(save_path+'plot_shots_'+name_provider.lower()+'_zoom.png',
                bbox_inches='tight')


def comparison_plot(
    data1: pd.DataFrame,
    data2: pd.DataFrame,
    name_provider1='',
    name_provider2='',
    pitch_length=105,
    pitch_width=68,
    orient_vertical=False,
    print_info=True,
    shot_size_factor=1000,
    colors=['tab:red', 'tab:blue']
):
    # prepare variables
    teams = data1['team'].unique()
    data2['x'] = pitch_length - data2['x']
    data2['y'] = pitch_width - data2['y']

    # plot on entire pitch
    fig, ax = plt.subplots(figsize=(13, 8.5))
    scatter_shots(ax, data1, shot_size_factor,
                  colors=colors, print_info=print_info)
    scatter_shots(ax, data2, shot_size_factor,
                  colors=colors, print_info=print_info)
    draw_pitch(ax, orient_vertical=orient_vertical)
    plt.axis('equal')
    ax.set_axis_off()

    # add legends
    title = 'Expected goals (xG) for shots'
    legend_xg = add_xg_legend(
        ax, title, shot_size_factor, bbox_to_anchor=(0.05, 0.95))
    ax.legend([Patch(color=colors[i]) for i in [0, 1]], teams, loc='upper left',
              fontsize=8, bbox_to_anchor=(0.05, 0.87), frameon=False)
    plt.gca().add_artist(legend_xg)
    ax.text(0.75, 0.1, name_provider1, transform=ax.transAxes,
            horizontalalignment='center')
    ax.text(0.25, 0.1, name_provider2, transform=ax.transAxes,
            horizontalalignment='center')

    plt.savefig(save_path+'plot_shots_comparison.png', bbox_inches='tight')


# actual plotting
orient_vertical = True
shots = prep_data(events_path, lineup_path, pitch_length, pitch_width,
                  orient_vertical=orient_vertical)
shot_plot(shots, name_provider='Statsbomb', orient_vertical=orient_vertical)

print(shots)

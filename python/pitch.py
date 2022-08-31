import matplotlib.pyplot as plt
import numpy as np
from matplotlib.axes import Axes
from matplotlib.patches import Arc
from matplotlib.transforms import Affine2D

def draw(
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
    # why is additional factor required to make it binding?
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
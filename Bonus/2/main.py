import os
import shutil
from typing import Callable
import numpy as np
from numpy.polynomial.polynomial import Polynomial
from scipy.interpolate import lagrange
import matplotlib.pyplot as plt

_OUT_DIR = "output/"

_DRAW_SAMPLE_SIZE = 1000  # for np.linspace to draw functions

_A4SIZE = np.array([4960, 7016])


def equidistant_nodes(a: float, b: float, n: int) -> np.ndarray:
    return np.linspace(a, b, n)


def chebyshev_nodes(a: float, b: float, n: int) -> np.ndarray:
    return 0.5 * (a + b) + 0.5 * (b - a) * np.cos(
        (2 * np.array(range(1, n + 1), dtype=float) - 1) * np.pi / (2 * n)
    )


def lagrange_poly(
    nodes: np.ndarray,
    fun: Callable[[float], float],
) -> Polynomial:
    return Polynomial(lagrange(nodes, fun(nodes)).coef[::-1])


def plot_fun(
    ax: plt.Axes,
    n: int,
    x: np.array,
    nodes: np.ndarray,
    fun_name: str,
    fun: Callable[[float], float],
    poly: Polynomial,
    **kwargs,
):
    ax.plot(x, fun(x), "k--")
    ax.plot(x, poly(x), kwargs.get("color", "b") + "-")
    ax.plot(nodes, fun(nodes), kwargs.get("color", "b") + "o")

    ax.set_title(f"$n$ = {n}")
    ax.set_xlabel("$x$")
    ax.set_ylabel(fun_name)


def plot_err(
    ax: plt.Axes,
    a: float,
    b: float,
    n: int,
    x: np.array,
    nodes: np.ndarray,
    delta_x: np.ndarray,
    delta_nodes: np.ndarray,
    fun_name: str,
    **kwargs,
):
    ax.hlines(np.max(delta_x * 100), a, b, "k", "dashed")
    ax.plot(x, delta_x * 100, kwargs.get("color", "b") + "-")
    ax.plot(nodes, delta_nodes * 100, kwargs.get("color", "b") + "o")

    ax.set_title(f"$n$ = {n}")
    ax.set_xlabel("$x$")
    ax.set_ylabel("Относ. погрешность (%)")


def make_fun_graphs(
    a: float,
    b: float,
    n_values: list[int],
    fun_name: str,
    fun: Callable[[float], float],
):
    fig, axs = plt.subplots(len(n_values), 2, figsize=(_A4SIZE * 2.5 / 1000))

    for i, n in enumerate(n_values):
        ax_l, ax_r = axs[i]
        # Sample enough x to graph a smooth continous line
        x = np.linspace(a, b, _DRAW_SAMPLE_SIZE)
        # Make interpolation with equidistant nodes
        nodes = equidistant_nodes(a, b, n)
        plot_fun(
            ax_l,
            n,
            x,
            nodes,
            fun_name,
            fun,
            lagrange_poly(nodes, fun),
            color="b",
        )
        # Make interpolation with Chebyshev nodes
        nodes = chebyshev_nodes(a, b, n)
        plot_fun(
            ax_r,
            n,
            x,
            nodes,
            fun_name,
            fun,
            lagrange_poly(nodes, fun),
            color="r",
        )

    # Make pretty and save :3
    non_tex_fun_name = fun_name.translate(str.maketrans("", "", "$\\"))

    fig.suptitle(
        "Интерполяция для "
        + non_tex_fun_name
        + "\n"
        + "(синий для равноудаленных узлов, красный для узлов Чебышева)",
        fontsize=20,
        fontweight="bold",
    )
    fig.subplots_adjust(hspace=0.35, wspace=0.3)
    fig.savefig(
        _OUT_DIR + non_tex_fun_name + ".png",
    )


def make_err_graphs(
    a: float,
    b: float,
    n_values: list[int],
    fun_name: str,
    fun: Callable[[float], float],
):
    fig, axs = plt.subplots(len(n_values), 2, figsize=(_A4SIZE * 2.5 / 1000))

    for i, n in enumerate(n_values):
        ax_l, ax_r = axs[i]
        # Sample enough x to graph a smooth continous line
        x = np.linspace(a, b, _DRAW_SAMPLE_SIZE)

        # Make interpolation with equidistant nodes
        nodes = equidistant_nodes(a, b, n)
        poly = lagrange_poly(nodes, fun)
        delta_x = np.abs((fun(x) - poly(x)) / fun(x))
        delta_nodes = np.abs((fun(nodes) - poly(nodes)) / fun(nodes))
        plot_err(
            ax_l,
            a,
            b,
            n,
            x,
            nodes,
            delta_x,
            delta_nodes,
            fun_name,
            color="b",
        )
        # Make interpolation with Chebyshev nodes
        nodes = chebyshev_nodes(a, b, n)
        poly = lagrange_poly(nodes, fun)
        delta_x = np.abs((fun(x) - poly(x)) / fun(x))
        delta_nodes = np.abs((fun(nodes) - poly(nodes)) / fun(nodes))
        plot_err(
            ax_r,
            a,
            b,
            n,
            x,
            nodes,
            delta_x,
            delta_nodes,
            fun_name,
            color="r",
        )

    # Make pretty and save :3
    non_tex_fun_name = fun_name.translate(str.maketrans("", "", "$\\"))

    fig.suptitle(
        "Абс. погрешность интерполяции "
        + non_tex_fun_name
        + "\n"
        + "(синий для равноудаленных узлов, красный для узлов Чебышева)",
        fontsize=20,
        fontweight="bold",
    )
    fig.subplots_adjust(hspace=0.35, wspace=0.3)
    fig.savefig(
        _OUT_DIR + non_tex_fun_name + "_err.png",
    )


def make_graphs(
    a: float,
    b: float,
    n_values: list[int],
    fun_name: str,
    fun: Callable[[float], float],
):
    make_fun_graphs(a, b, n_values, fun_name, fun)
    make_err_graphs(a, b, n_values, fun_name, fun)


def main(debug: bool = False):
    if debug:
        if os.path.exists(_OUT_DIR):
            shutil.rmtree(_OUT_DIR)
        os.mkdir(_OUT_DIR)

    a = -1.5
    b = 1.5
    n_values: list = [5, 10, 15, 25]
    functions: dict = {
        "$\\ln(x^2+2)$": lambda x: np.log(x**2 + 2),
        "round($\\sin(x)+2$)": lambda x: np.round(np.sin(x) + 2),
    }

    for fun_name, fun in functions.items():
        make_graphs(a, b, n_values, fun_name, fun)


if __name__ == "__main__":
    main(debug=True)

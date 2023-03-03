import numpy as np
from numpy.polynomial import Polynomial
from matplotlib import pyplot as plt


class Exercise1:
    """
    (x - 1)(x - 2)...(x - 20) = x**20 - 210 x**19 + ...,
    нули которого x1 = 1, x2 = 2, …, x20 = 20

    Найти нули полинома, погрешность 10**-7
    допущенатолько в коэффициенте при x**19;
    x**20 – (210 +10**-7) x**19 + ...,
    Нарисовать нули полиномов
    """

    def __plot_pol(self, p: Polynomial, **kwargs) -> None:
        x = np.linspace(0, 22, 1000)
        y = p(x)
        plt.plot(x, y, kwargs["color"], label=kwargs["label"])
        plt.plot(
            p.roots(),
            [0] * 20,
            f"{kwargs['color']}{kwargs['marker']}",
            label=f"{kwargs['label']} roots",
        )

    def __plot_roots(self, p: Polynomial, **kwargs) -> None:
        roots = p.roots()
        plt.plot(
            roots.real,
            roots.imag,
            f"{kwargs['color']}{kwargs['marker']}",
            label=f"{kwargs['label']} roots",
        )

    def solve(self) -> None:
        w = Polynomial.fromroots(range(1, 21))  # Wilkinson's polynomial w(x)
        e = Polynomial(
            [0] * 19 + [-(10**-7)]
        )  # error in coeff of x ** 19 = - 10 ** -7
        p = w + e

        plt.figure(1)
        plt.title("Графики для $w(x)$ и $p(x)$")
        plt.xlabel("x")
        plt.ylabel("f(x)")
        plt.axhline(0, c="black")
        plt.axvline(0, c="black")
        plt.ylim(-1e17, 1e17)
        self.__plot_pol(w, color="b", marker="o", label="Wilkinson's polynom.")
        self.__plot_pol(p, color="r", marker="^", label="Polynom. with error")
        plt.legend()
        plt.show()

        plt.figure()
        plt.title("Корни многочленов $w(x)$ и $p(x)$")
        plt.xlabel("Re(z)")
        plt.ylabel("Im(z)")
        plt.axhline(0, c="black")
        plt.axvline(0, c="black")
        self.__plot_roots(w, color="b", marker="o", label="Wilkinson's polynom.")
        self.__plot_roots(p, color="r", marker="^", label="Polynom. with error")
        plt.legend()
        plt.show()

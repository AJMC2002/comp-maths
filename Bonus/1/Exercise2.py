# from pandas import DataFrame # Used in line 65
from math import pi, sin, factorial as fact
from collections import defaultdict
import json


class Exercise2:
    """
    Из курса математического анализа известно, что функция
    синус представляется своим рядом Тейлора
    причем радиус сходимости ряда равен бесконечности — ряд
    сходится при любых значениях x.

    1 вариант:
        будем учитывать лишь члены ряда, большие,
        чем δ = 10**-4.
        Выполнив вычисления с четырьмя значащими цифрами,
        получим sin (0.5236) = ?
        результат соответствует принятой точности?
    2 вариант:
        x = 52.36,  sin (52.36) = ? При δ = 10**-8
        результат соответствует принятой точности?
    """

    def __init__(self) -> None:
        self.x = [0.5236, 52.36]
        self.d = [10**-4, 10**-8]
        self.sigdig = [4, 8]

    def __taylor_sin(self, x: float, d: float, sigdig: int) -> float:
        """
        TODO: Could be made a generator
        """
        if x < 0:
            return self.__taylor_sin(-x, d, sigdig)
        elif x >= 2 * pi:
            return self.__taylor_sin(x - 2 * round(pi, sigdig), d, sigdig)
        terms: list[float] = []
        sin_x = 0.0
        i = 0
        while True:
            term: float = x ** (2 * i + 1) / fact(2 * i + 1)
            if term <= d:
                break
            terms.append((-1) ** (i % 2) * term)
            sin_x += (-1) ** (i % 2) * term
            i += 1
        return terms, round(sin_x, sigdig)

    def solve(self) -> None:
        results = defaultdict(list)
        for x_i, d_i, sigdig_i in zip(self.x, self.d, self.sigdig):
            approx_terms, approx = self.__taylor_sin(x_i, d_i, sigdig_i)
            real = round(sin(x_i), sigdig_i)
            err = abs(real - approx)
            rel_err = round(err / real, sigdig_i) * 100

            results["x"].append(x_i)
            results["Taylor sin(x)"].append(approx)
            results["Taylor terms"].append(approx_terms)
            results["sin(x)"].append(real)
            results["Absolute error"].append(err)
            results["Relative error (%)"].append(rel_err)
        print(json.dumps(results, indent=4))
        # print(DataFrame(results)) # Non-preserving of significant digits

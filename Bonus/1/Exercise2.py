from math import pi, sin, factorial as fact


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
        self.sigdig = 4
        self.pi = round(pi, self.sigdig)

    def __taylor_sin(self, x: float, d: float) -> float:
        if x < 0:
            return self.__taylor_sin(-x, d)
        elif x >= 2 * pi:
            return self.__taylor_sin(x - 2 * self.pi, d)
        sin_x = 0.0
        i = 0
        while True:
            term: float = x ** (2 * i + 1) / fact(2 * i + 1)
            if term <= d:
                break
            sin_x += round((-1) ** (i % 2) * term, self.sigdig)
            i += 1
        return round(sin_x, self.sigdig)

    def solve(self) -> None:
        for x_i, d_i in zip(self.x, self.d):
            print(self.__taylor_sin(x_i, d_i))

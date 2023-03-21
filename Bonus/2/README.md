# Bonus HW 2

## Task

Сделать сходящуюся и расходящуюся интерполяцию.

Do a converging and diverging interpolation.

## Approach

y = ln(x^2+2) and y = round(sin(x)+2) were the functions chosen.

Equally spaced points can produce diverging Lagrange interpolating polynomials (see [Runge's phenomenon](https://en.wikipedia.org/wiki/Runge%27s_phenomenon)). The second function chosen will diverge for large values of n. To contrast this divergence, a better approach is shown: using [Chebyshev nodes](https://en.wikipedia.org/wiki/Chebyshev_nodes).

Also, the relative difference is graphed in order to evidence the effectiveness of using Chebyshev nodes and a way to mitigate error when interpolating with Lagrange's polynomial.

The interval and n values used, along with the functions are defined in the code as follows (function names are defined using LaTeX):

    a = -1.5
    b = 1.5
    n_values: list = [5, 10, 15, 25]
    functions: dict = {
        "$\\ln(x^2+2)$": lambda x: np.log(x**2 + 2),
        "round($\\sin(x)+2$)": lambda x: np.round(np.sin(x) + 2),
    }

## Results (see `output`)

Converging interpolation (y = ln(x^2+2)):

![Converging interpolation](output/ln(x%5E2+2).png)

Diverging interpolation (y = round(sin(x)+2)):

![Diverging interpolation](output/round(sin(x)+2).png)

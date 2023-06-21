import numpy as np

u_0 = np.array(
    [
        [0],
        [0],
    ]
)


def F(u):
    x = u[0][0]
    y = u[1][0]
    return np.array(
        [
            [x - y**3 - 1],
            [(x + 3) * (y - 1) - 5],
        ]
    )


def invJacobian(u):
    x = u[0][0]
    y = u[1][0]
    det = x + 3 - (-3 * y**2) * (y - 1)
    return (
        1
        / det
        * np.array(
            [
                [x + 3, 3 * y**2],
                [1 - y, 1],
            ]
        )
    )


def u_n(u):
    invJ = invJacobian(u)
    f = F(u)
    return u + np.array(
        [
            [invJ[0][0] * f[0][0] + invJ[0][1] * f[1][0]],
            [invJ[1][0] * f[0][0] + invJ[1][1] * f[1][0]],
        ]
    )


def arr2tex(i, arr):
    return f"\\[ u_{{{i}}} = \\left(\\begin{{matrix}} {arr[0][0]} \\\\ {arr[1][0]} \\end{{matrix}}\\right) \\]"


curr_u = u_0
i = 0
while F(curr_u)[0][0] != 0 and F(curr_u)[1][0] != 0 and i < 100:
    print(arr2tex(i, curr_u))
    curr_u = u_n(curr_u)
    i += 1

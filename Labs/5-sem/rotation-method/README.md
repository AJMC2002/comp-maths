# [Лаб No7] Метод вращения

Using [Jacobi's algorithm](https://en.wikipedia.org/wiki/Jacobi_eigenvalue_algorithm) one can get the eigenvalues of a symmetric matrix, by rotating it with [Givens rotations](https://en.wikipedia.org/wiki/Givens_rotation)

## Results

Test taken from <https://en.wikipedia.org/wiki/Jacobi_eigenvalue_algorithm>

```haskell
let a :: Matrix Double =
      fromLists
        [ [4, -30, 60, -35],
          [-30, 300, -675, 420],
          [60, -675, 1620, -1050],
          [-35, 420, -1050, 700]
        ]
```

It's eigenvalues should be:

- e_1 = 2585.25381092892231
- e_2 = 37.1014913651276582
- e_3 = 1.4780548447781369
- e_4 = 0.1666428611718905

The values obtained were:

```haskell
Eigenvalues [2585.253810928922,37.10149136512769,1.4780548447781732,0.16664286117190028]
Done in 16 steps
```

# [Лаб No5] Метод Якоби

Using [Jacobi's algorithm](https://en.wikipedia.org/wiki/Jacobi_method) one can solve a stricty diagonally dominant system of linear equations (element on the diagonal of the row > sum of elements on the rest of the row)

It uses the following iteration for each element of the vector of the iteration:

  x<sub>i</sub><sup>(k+1)</sup> = 1 / a<sub>ii</sub> ( b<sub>i</sub> - sum<sub>i!=j</sub> a<sub>ij</sub> x<sub>i</sub> )

## Results

From the following test:

```rust
const EPSILON: FloatingType = 0.0001;

fn main() {
    jacobi_method(
        &matrix![10.,2.,1.; 1.,10.,2.; 1.,1.,10.], // A matrix
        &vector![10., 12., 8.], // b vector
        &vector![0., 0., 0.], // x0 vector
        EPSILON, // precision criteria
    );
}
```

This result was obtained:

```rust
x: 
  ┌             ┐
  │ 0.737171375 │
  │   1.0010458 │
  │ 0.626176975 │
  └             ┘

 diff to 0: 0.00002759378326833206 | iter no.: 10
```

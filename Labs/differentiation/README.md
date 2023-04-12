# _3rd Lab_: Numeric differentiation w/ Lagrange's polynomial

> ## Variant
>
> 19
>
> ## Function
>
> y = x^2+log(x) (log in base 10).
>
> ## Interval
>
> [a, b] = [0.4, 0.9]
>
> ## Interpolation points
>
> {x\*, x**, x\***, x\*\*\*\*} = {0.53, 0.43, 0.86, 0.67}
>
> ## K, N, M
>
> 2, 3, 3

## Goal

Calculate the Kth derivative of the Nth Lagrange's interpolating polynomial
at the Mth node in order to approximate to the real value. The goal is to fall
between the min and max error possible.

## Usage

Run `cargo run` in terminal to get the results thrown.

## Faster Calculations

The math was done on paper to derivate everything.

Using nested iterators in Rust it was possible to save up on writing for loops.

## Results

Results were pretty good tbh :)

```text
f^(2)(x3) = 1.4638339729589485
L^(2)(x3) = 1.7562282922068704
R_{3,2}(x3) = -0.2923943192479219
Bounds = RBounds {
                    min: -1.4055469997390937,   
                    max: 0.41607855831093676
                }
Is R in bounds? true
```

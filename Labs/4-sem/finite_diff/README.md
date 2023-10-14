# _2nd Lab_: Interpolation w/ Help of Formulas w/ Finite Differences

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

## Goal

Apply methods of interpolation like Newton and Gauss' formulas and evaluate the error these carry using the minimum and maximum value of the error term of interpolation for these methods.

## Usage

Run `cargo run` in terminal to get the results thrown.

If needed, a view of the table of finite differences is stored in `table.txt`.

## Faster Calculations

Some of the ideas used in the 1st lab about the nth derivative of the function studied are being used here.

## Results

```text
x = 0.43 <- Using Newton's first formula
f(x) = -0.18163 and L(x) = -0.18163
Δ = 3.76545e-10 is between 2.66379e-12 and 1.00896e-7?
Yes

x = 0.86 <- Using Newton's second formula
f(x) = 0.67410 and L(x) = 0.67410
Δ = 1.92572e-11 is between 2.59035e-13 and 9.81142e-9?
Yes

x = 0.67 <- Using Gauss' first formula at x0 = a+6*h
f(x) = 0.27497 and L(x) = 0.27497
Δ = 2.40502e-12 is between 4.02800e-11 and 1.52568e-6?
No
```

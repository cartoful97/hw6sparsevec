
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hw6sparsevec

<!-- badges: start -->

[![R-CMD-check](https://github.com/cartoful97/hw6sparsevec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cartoful97/hw6sparsevec/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of hw6sparsevec is to represent a sparse vector with many 0s in
a more efficient form than simply as a dense vector, by storing only the
positions and corresponding values of nonzero elements.

## Installation

You can install the development version of hw6sparsevec by cloning the
GitHub repository to your computer, then running
`devtools::install("hw6sparsevec")` in the R console.

## Class

The class has 3 attributes: a numeric vector containing the nonzero
values of the sparse vector, an integer vector containing the positions
of the sparse vector, and the integer length of the complete sparse
vector, including 0s. You can initialize a sparse numeric object as
follows:

``` r
library(hw6sparsevec)
#> 
#> Attaching package: 'hw6sparsevec'
#> The following object is masked from 'package:base':
#> 
#>     norm

x <- new("sparse_numeric",
         value = c(1, 2, 1),
         pos = c(1L, 2L, 5L),
         length = 5L)
x
#> nonzero positions:  1 2 5 
#> nonzero values:  1 2 1 
#> total length:  5
```

Or, you can convert a numeric vector to a sparse numeric object:

``` r
x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
x
#> nonzero positions:  4 5 
#> nonzero values:  1 2 
#> total length:  5
```

## Functions

The functions `sparse_add()`, `sparse_mult()`, and `sparse_sub()`
perform their respective operations on two sparse numeric vector
objects.

``` r
x <- as(c(-2, 8, 3, 0, 1), "sparse_numeric")
y <- as(c(2, 0, 7, 1, -9), "sparse_numeric")
print('addition')
#> [1] "addition"
sparse_add(x, y)
#> nonzero positions:  2 3 4 5 
#> nonzero values:  8 10 1 -8 
#> total length:  5
print('subtraction')
#> [1] "subtraction"
sparse_sub(x, y)
#> nonzero positions:  1 2 3 4 5 
#> nonzero values:  -4 8 -4 -1 10 
#> total length:  5
print('multiplication')
#> [1] "multiplication"
sparse_mult(x, y)
#> nonzero positions:  1 3 5 
#> nonzero values:  -4 21 -9 
#> total length:  5
```

Instead of using the functions directly, the numerical symbols of +, -,
and \* can be used as well.

``` r
print('addition')
#> [1] "addition"
x + y
#> nonzero positions:  2 3 4 5 
#> nonzero values:  8 10 1 -8 
#> total length:  5
print('subtraction')
#> [1] "subtraction"
x - y
#> nonzero positions:  1 2 3 4 5 
#> nonzero values:  -4 8 -4 -1 10 
#> total length:  5
print('multiplication')
#> [1] "multiplication"
x * y
#> nonzero positions:  1 3 5 
#> nonzero values:  -4 21 -9 
#> total length:  5
```

The function `sparse_crossprod()` calculates the cross product of two
sparse numeric vectors (the sum of the vectors multiplied together).

``` r
sparse_crossprod(x, y)
#> [1] 8
```

The function `unsparse()` removes all zeros from a sparse numeric and
returns the result as a numeric vector.

``` r
x
#> nonzero positions:  1 2 3 5 
#> nonzero values:  -2 8 3 1 
#> total length:  5
unsparse(x)
#> [1] -2  8  3  1
```

The `plot` function plots the values of elements with overlapping
positions of nonzero elements between two sparse numerics.

``` r
plot(x, y)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

The function `mean()` finds the mean of the elements of a sparse numeric
vector.

``` r
x
#> nonzero positions:  1 2 3 5 
#> nonzero values:  -2 8 3 1 
#> total length:  5
mean(x)
#> [1] 2
```

The function `norm()` finds the squared norm of the vector (the square
root of the sum of the squared elements).

``` r
x
#> nonzero positions:  1 2 3 5 
#> nonzero values:  -2 8 3 1 
#> total length:  5
norm(x)
#> [1] 8.831761
```

The function `standardize()` standardizes the elements of the sparse
vector and returns the result as a dense vector, as all the standardized
0s are unlikely to remain 0.

``` r
x
#> nonzero positions:  1 2 3 5 
#> nonzero values:  -2 8 3 1 
#> total length:  5
standardize(x)
#> [1] -1.0504515  1.5756772  0.2626129 -0.5252257 -0.2626129
```

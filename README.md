
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IANOVA

<!-- badges: start -->
<!-- badges: end -->

IANOVA is a package built for simulating and running IANOVA hypothesis
tests. The current functionality is relatively limited, but does allow
users to build their own simulations and run full hypothesis tests.

## Installation

You can install the development version of IANOVA from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zac-d-rios/IANOVA")
```

## Example

Example scripts used to generate the figures found in the thesis are
available in the “scripts” folder. These scripts are by no means
comprehensive, but provide the framework for using the functions in this
package. generates correlated data for certain distributions, and
importantly returns data in the format that other functions will expect.
Below is an example than generates three groups of 30 intervals, where
both the center and radii are normally distributed. The center and
radius are correlated, and params1 represents mean parameters, while
params2 represent variance parameters. The output of the first group of
intervals is then displayed.

``` r
library(IANOVA)
set.seed(90210)
data <- sim_data(group_sizes = c(30, 30, 30), dist = "normal", corr = 0.8, 
                 params1 = c(0, 10), params2 = c(1, 2))
data[[1]]
#>     id          V1        V2
#>  1:  1 -1.37392332  8.706789
#>  2:  2 -0.99687304  7.658921
#>  3:  3 -0.74139526 10.069017
#>  4:  4  0.01375188 10.246117
#>  5:  5  0.61412112 10.551656
#>  6:  6 -0.24497679 10.434357
#>  7:  7  0.62343890 11.680089
#>  8:  8 -0.27315611 10.467678
#>  9:  9  0.46225128 11.508887
#> 10: 10 -0.17192647  8.248633
#> 11: 11 -0.19338731  8.405630
#> 12: 12 -0.41869021  9.330988
#> 13: 13 -1.22073679  7.437850
#> 14: 14 -1.48405236  7.220901
#> 15: 15  0.71764002 11.237066
#> 16: 16 -0.30884449  9.401629
#> 17: 17  0.07025332  9.754651
#> 18: 18  1.37786648 11.108307
#> 19: 19  0.19984169 12.470692
#> 20: 20  0.67191623 11.236854
#> 21: 21 -0.95110116  8.129690
#> 22: 22 -0.50580761  8.395030
#> 23: 23 -1.08371027  8.615895
#> 24: 24 -0.71017228 10.095478
#> 25: 25  0.58040277 11.372653
#> 26: 26 -0.53707012  8.540172
#> 27: 27  0.08376344  9.573079
#> 28: 28  0.84013620 11.093305
#> 29: 29 -0.43060005  9.224848
#> 30: 30  0.07080873 10.825447
#>     id          V1        V2
```

We also can use functions and to get the different pieces of our
statistic, and divide their outputs to get the full statistic.

``` r
full_statistic <- observe_num(data)/observe_den(data)
full_statistic
#> [1] 0.1392561
```

This value can then be compared to the theoretical limiting
distribution, via two other functions in the package. In general, the
arguments for the functions are the same. Trials indicates the number of
simulations you’d like to use to generate the numerator, and g is the
number of groups, rather than their individual sizes.

``` r
num_theory <- sim_theory_num(g = 3, dist = "normal", corr = 0.8, 
                              params1 = c(0, 10), params2 = c(1, 2),
                              trials = 100000, omega = 0.8)

den_theory <- sim_theory_den(dist = "normal", params1 = c(0, 10),
                             params2 = c(1, 2), omega = 0.8)

limiting_distribution <- num_theory/den_theory
```

You can then make a decision by comparing your test statistic with the
limiting distribution, shown below.

``` r
full_statistic > quantile(limiting_distribution, 0.95)
#>   95% 
#> FALSE
```

In this case, our test statistic did not exceed the critical value of
our limiting distribution, so we would fail to reject the null. There
isn’t sufficient evidence to suggest our intervals are different across
groups, which is in line with how we simulated.

## General Procedure

In general, IANOVA hypothesis tests will be run using the following
steps:

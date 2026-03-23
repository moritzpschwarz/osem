# Super Exogeneity (Parameter Invariance) Test

This function tests for super exogeneity (parameter invariance) of the
model. Parameter invariance is a key component of Super Exogeneity,
which was first introduced in Engle, Hendry, and Richard (1983;
Econometrica). This test runs an indicator saturation model for each
independent variable that is present in the `initial.model` object -
these individual models are called the marginal models. If any outliers
or step-shifts are detected using the `isat` function from the `gets`
package in the marginal models, then these indicators are added to the
initial model, called the conditional model. Any pre-existing indicators
from the initial model are removed, as otherwise the power of the test
would be reduced. The conditional model is then used to run a linear
regression and to obtain an F-Stat statistic to determine whether the
shocks detected in the marginal models affect the conditional model.

## Usage

``` r
super.exogeneity(initial.model, saturation.tpval = 0.01, quiet = FALSE)
```

## Arguments

- initial.model:

  An object of class `isat` (i.e. from the `gets` package) that contains
  the initial model

- saturation.tpval:

  The target p-value of the saturation methods (e.g. SIS and IIS, see
  the 'isat' function in the 'gets' package). Default is 0.01.

- quiet:

  Logical. Should messages about the forecast procedure be suppressed?

## Value

An h-test object with the results of the super exogeneity test

## Details

A p-value below the desired level of statistical significance (e.g.
0.05) would indicate that the model is not super exogenous (i.e. the
parameters are variant).

## References

Engle, R. F., Hendry, D. F., & Richard, J. F. (1983). Exogeneity.
Econometrica: Journal of the Econometric Society, 73-85.

Engle, R. F., & Hendry, D. F. (1993). Testing superexogeneity and
invariance in regression models. Journal of Econometrics, 56(1-2),
119-139.

Hendry, D. F., & Santos, C., 'An Automatic Test of Super Exogeneity', in
Bollerslev, T., Russell, J., & Watson, M. (Eds.). (2010). Volatility and
time series econometrics: essays in honor of Robert Engle. OUP oxford.

Castle, J. L., Hendry, D. F., & Martinez, A. B. (2017). Evaluating
forecasts, narratives and policy using a test of invariance.
Econometrics, 5(3), 39.

## Examples

``` r
#load Hoover and Perez (1999) data:
data(hpdata, package = "gets")

##run isat with step impulse saturation on two lags and a constant 1 percent significance level:
is.model <- gets::isat(
  y = hpdata$GCQ,
  mxreg = hpdata[,"GYDQ", drop = FALSE],
  ar = 1:2,
  sis = TRUE,
  t.pval = 0.01
)
#> 
#> SIS block 1 of 5:
#> 29 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 
#> SIS block 2 of 5:
#> 29 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 
#> SIS block 3 of 5:
#> 28 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 
#> SIS block 4 of 5:
#> 28 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 
#> SIS block 5 of 5:
#> 26 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 
#> GETS of union of retained SIS variables... 
#> 2 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 
#> GETS of union of ALL retained variables...

super.exogeneity(is.model)
#> 
#>  Super Exogeneity (Parameter Invariance) Test
#> 
#> data:  Conditional Model for: y
#> F-Stat = 68.779, df1 = 4, df2 = 2, p-value = 0.01438
#> alternative hypothesis: Parameter Variance
#> 
```

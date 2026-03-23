# OSEM Model Table

Show the final regression equations estimated by the OSEM Model

## Usage

``` r
model_table(model, output = "default", title = "OSEM Model Results")
```

## Arguments

- model:

  An OSEM model object or class `osem`, created by the `run_model`
  function.

- output:

  Character. Either "default", "html", "data.frame", or "latex". Default
  is "default" (which is the default in the
  [`modelsummary`](https://modelsummary.com/man/modelsummary.html)
  function.

- title:

  Character. The title of the table. Default is "OSEM Model Results".

## Value

A table summarizing the regression results of the OSEM model (either an
html or a text (latex) table).

## Examples

``` r
spec <- dplyr::tibble(
  type = c(
    "d",
    "d",
    "n"
  ),
  dependent = c(
    "StatDiscrep",
    "TOTS",
    "Import"
  ),
  independent = c(
    "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    "GValueAdd + Import",
    "FinConsExpHH + GCapitalForm"
  )
)
# \donttest{
a <- run_model(specification = spec,
              input = sample_input,
              primary_source = "local",
              constrain.to.minimum.sample = FALSE)
#> 
#> --- Estimation begins ---
#> Estimating Import = FinConsExpHH  +  GCapitalForm 
#> Constructing TOTS = GValueAdd  +  Import 
#> Constructing StatDiscrep = TOTS  -  FinConsExpHH  -  FinConsExpGov  -  GCapitalForm  -  Export 

model_table(a)
#> 
#> +-----------------+----------+
#> |                 | Import   |
#> +=================+==========+
#> | mconst          | -1.322   |
#> +-----------------+----------+
#> |                 | (0.966)  |
#> +-----------------+----------+
#> | trend           | 0.004*** |
#> +-----------------+----------+
#> |                 | (0.000)  |
#> +-----------------+----------+
#> | ln.FinConsExpHH | 0.663*** |
#> +-----------------+----------+
#> |                 | (0.091)  |
#> +-----------------+----------+
#> | ln.GCapitalForm | 0.492*** |
#> +-----------------+----------+
#> |                 | (0.042)  |
#> +-----------------+----------+
#> | Num.Obs.        | 50       |
#> +-----------------+----------+
#> | AIC             | -305.3   |
#> +-----------------+----------+
#> | BIC             | -290.0   |
#> +-----------------+----------+
#> | Log.Lik.        | 160.629  |
#> +=================+==========+
#> | + p < 0.1, * p < 0.05, **  |
#> | p < 0.01, *** p < 0.001    |
#> +=================+==========+
#> | Quarterly Dummies,         |
#> | Impulse (IIS) and Step     |
#> | Indicators (SIS) are not   |
#> | shown individually but     |
#> | were activated for all     |
#> | models.                    |
#> +=================+==========+
#> Table: OSEM Model Results 
# }
```

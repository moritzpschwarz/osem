# Visualize dependence between modules

Visualize dependence between modules

## Usage

``` r
network(model, layout = "kk")
```

## Arguments

- model:

  A model of class 'osem'

- layout:

  Character. The layout of the network graph as per the `ggraph`
  package. Default is "kk", try "auto", "fr" (Fruchterman-Reingold), or
  "dh" (Davidson-Harel).

## Value

Returns a network graph illustrating the dependence between the
different modules.

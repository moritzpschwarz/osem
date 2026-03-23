# Creates and runs the OSEM Shiny App

Opens a window for the user to interact with the app. The user can
upload an
[osem](http://www.moritzschwarz.org/osem/reference/new_osem.md) object
returned by
[`run_model`](http://www.moritzschwarz.org/osem/reference/run_model.md)
and produce graphical and tabular output.

## Usage

``` r
run_shiny(model = NULL)
```

## Arguments

- model:

  The model object that is passed by the 'present_model()' function.

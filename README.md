
<!-- README.md is generated from README.Rmd. Please edit that file -->

# osem - Open Source Empirical Macro Model

<!-- badges: start -->

[![R-CMD-check](https://github.com/moritzpschwarz/osem/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/moritzpschwarz/osem/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/moritzpschwarz/osem/branch/main/graph/badge.svg)](https://app.codecov.io/gh/moritzpschwarz/osem?branch=main)
<!-- badges: end -->

The goal of the {osem} Package is to implement and operationalise the
Open Source Empirical Macro (OSEM) Model, developed by Moritz Schwarz,
Jonas Kurle, Felix Pretis, and Andrew Martinez. This is an adaptation of
the [Norwegian Aggregate Model](https://normetrics.no/nam/), developed
by Gunnar Bardsen and Ragnar Nymoen.

## Installation

You can install the development version of {osem} from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("moritzpschwarz/osem")
```

## Basic Workflow

This is an example which shows you how to run the model:

First we load the package:

``` r
library(osem)
```

### Specify the model

The we calibrate the model specification and save this in a tibble. Here
the column names and the structure of the specification table must
follow the basic structure below.

``` r
spec <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n",
    "n",
    "d",
    "n",
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "Import",
    "FinConsExpHH",
    "GCapitalForm",
    "Emissions",
    "GDP",
    "GValueAddGov", # as in NAM, technical relationship
    "GValueAddManuf", # more complicated in NAM, see 2.3.3 and 6.3.1
    "GValueAddConstr" ,
    "GValueAddWholesaletrade"
  ),
  independent = c(
    "FinConsExpHH + GCapitalForm",
    "",
    "FinConsExpGov + FinConsExpHH",
    "GDP + Export + GValueAddIndus",
    "GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr + GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance + GValueAddRealest + GValueAddResearch + GValueAddArts",
    "FinConsExpGov", # as in NAM, technical relationship
    "Export + LabCostManuf", # NAM uses 'export market indicator' not exports - unclear what this is, NAM uses unit labour cost in NOR manufacturing relative to the foreign price level - here is just total labour cost
    "LabCostConstr + BuildingPermits", # in NAM some form of YFP2J = 0.3JBOL + 0.2JF P N + 0.3JO + 0.3JOIL. Unclear what this is. Using Building Permits instead
    "Export + LabCostService"
  ),
  lag = "",
  cvar = "")
```

To summarise this, we can print out the specification table:

<table class="table lightable-paper" style="color: black; margin-left: auto; margin-right: auto; color: black; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
type
</th>
<th style="text-align:left;">
dependent
</th>
<th style="text-align:left;">
independent
</th>
<th style="text-align:left;">
lag
</th>
<th style="text-align:left;">
cvar
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
Import
</td>
<td style="text-align:left;">
FinConsExpHH + GCapitalForm
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
FinConsExpHH
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
GCapitalForm
</td>
<td style="text-align:left;">
FinConsExpGov + FinConsExpHH
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
Emissions
</td>
<td style="text-align:left;">
GDP + Export + GValueAddIndus
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
d
</td>
<td style="text-align:left;">
GDP
</td>
<td style="text-align:left;">
GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr +
GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance +
GValueAddRealest + GValueAddResearch + GValueAddArts
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
GValueAddGov
</td>
<td style="text-align:left;">
FinConsExpGov
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
GValueAddManuf
</td>
<td style="text-align:left;">
Export + LabCostManuf
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
GValueAddConstr
</td>
<td style="text-align:left;">
LabCostConstr + BuildingPermits
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
GValueAddWholesaletrade
</td>
<td style="text-align:left;">
Export + LabCostService
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
</tbody>
</table>

In order to run this model, we also need a dictionary that translates
our model variables to EUROSTAT codes so that the download process can
be automated. You can either pass a new dictionary to the model
function, or you can use the built in dictionary `osem::dict` (here the
first few rows):

<table class="table lightable-paper" style="color: black; margin-left: auto; margin-right: auto; color: black; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
model_varname
</th>
<th style="text-align:left;">
full_name
</th>
<th style="text-align:left;">
variable_code
</th>
<th style="text-align:left;">
database
</th>
<th style="text-align:left;">
dataset_id
</th>
<th style="text-align:left;">
var_col
</th>
<th style="text-align:left;">
freq
</th>
<th style="text-align:left;">
geo
</th>
<th style="text-align:left;">
unit
</th>
<th style="text-align:left;">
s_adj
</th>
<th style="text-align:left;">
nace_r2
</th>
<th style="text-align:left;">
cpa2_1
</th>
<th style="text-align:left;">
sector
</th>
<th style="text-align:left;">
direct
</th>
<th style="text-align:left;">
age
</th>
<th style="text-align:left;">
partner
</th>
<th style="text-align:left;">
finpos
</th>
<th style="text-align:left;">
p_adj
</th>
<th style="text-align:left;">
meat
</th>
<th style="text-align:left;">
citizen
</th>
<th style="text-align:left;">
wstatus
</th>
<th style="text-align:left;">
tra_oper
</th>
<th style="text-align:left;">
ipcc_sector
</th>
<th style="text-align:left;">
GEO
</th>
<th style="text-align:left;">
Seasonal adjustment
</th>
<th style="text-align:left;">
North American Industry Classification System (NAICS)
</th>
<th style="text-align:left;">
North American Product Classification System (NAPCS)
</th>
<th style="text-align:left;">
Prices
</th>
<th style="text-align:left;">
Type of fuel
</th>
<th style="text-align:left;">
Products and product groups
</th>
<th style="text-align:left;">
ref_area
</th>
<th style="text-align:left;">
commodity
</th>
<th style="text-align:left;">
unit_measure
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Supply
</td>
<td style="text-align:left;">
Total Supply
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
Demand
</td>
<td style="text-align:left;">
Total Demand
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
GDPOutput
</td>
<td style="text-align:left;">
GDP Output Approach
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
GDPExpenditure
</td>
<td style="text-align:left;">
GDP Expenditure Approach
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
CO2Industry
</td>
<td style="text-align:left;">
Co2 Emissions Industry in Tonnes
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
TOTS
</td>
<td style="text-align:left;">
Total Supply
</td>
<td style="text-align:left;">
TOTS
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
</table>
<!-- The last step in specifying the model is to create a filter list that determines the country that we want to estimate this for and also which unit we need for each variable. This, for the moment, is quite tedious, but this will be improved in the future: -->
<!-- ```{r} -->
<!-- fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR") -->
<!-- fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR") -->
<!-- fc <- list(geo = "AT", unit = "THS_T") -->
<!-- fd <- list(geo = "AT", s_adj = "SCA") -->
<!-- fe <- list(geo = "AT", s_adj = "SCA", unit = "I15") -->
<!-- ff <- list(geo = "AT", s_adj = "SCA", unit = "I16") -->
<!-- filter_list <- list( -->
<!-- "P7" = fa, -->
<!-- "YA0" = fb, -->
<!-- "P31_S14_S15" = fa, -->
<!-- "P5G" = fa, -->
<!-- "B1G" = fa, -->
<!-- "P3_S13" = fa, -->
<!-- "P6" = fa, -->
<!-- "GHG" = fc, -->
<!-- "B1GQ" = fa, -->
<!-- "PSQM" = fe, -->
<!-- "LM-LCI-TOT" = ff -->
<!-- ) -->
<!-- ``` -->

### Running the model

Now we are ready to run the model with the `run_model()` function:

``` r
model_result <- run_model(
  specification = spec,
  save_to_disk = "inst/extdata/InputData.xlsx",
  primary_source = "download",
  trend = TRUE,
  saturation.tpval = 0.01,
  plot = FALSE
)
#> Lade nötigen Namensraum: tidygraph
#> Lade nötigen Namensraum: ggraph
#> indexed 0B in  0s, 0B/sindexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table sts_cobp_q cached at C:\Users\jonas\AppData\Local\Temp\RtmpUnjZbF/eurostat/359a70cefc8dc229abccf821f6ab46a1.rds
#> Dataset query already saved in cache_list.json...
#> Reading cache file C:\Users\jonas\AppData\Local\Temp\RtmpUnjZbF/eurostat/1a1e18ed86b9f5c9518ed4a594836ef0.rds
#> Table  namq_10_gdp  read from cache file:  C:\Users\jonas\AppData\Local\Temp\RtmpUnjZbF/eurostat/1a1e18ed86b9f5c9518ed4a594836ef0.rds
#> indexed 0B in  0s, 0B/sindexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table env_ac_aigg_q cached at C:\Users\jonas\AppData\Local\Temp\RtmpUnjZbF/eurostat/e57785d3b87ff75e07d888c79049c6c1.rds
#> indexed 0B in  0s, 0B/sindexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table ei_lmlc_q cached at C:\Users\jonas\AppData\Local\Temp\RtmpUnjZbF/eurostat/024d13ac9c0969c94ae43c9a3c2a1a1d.rds
#> Dataset query already saved in cache_list.json...
#> Reading cache file C:\Users\jonas\AppData\Local\Temp\RtmpUnjZbF/eurostat/f477c9ee01a86dadfa13c08f2dae0171.rds
#> Table  namq_10_a10  read from cache file:  C:\Users\jonas\AppData\Local\Temp\RtmpUnjZbF/eurostat/f477c9ee01a86dadfa13c08f2dae0171.rds
#> Warning in load_or_download_variables(specification = module_order, dictionary
#> = dictionary, : Unbalanced panel, will lose more than 20\% of data when making
#> balanced
#> 
#> --- Estimation begins ---
#> Estimating FinConsExpHH =
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> No Outliers or Step-Shifts detected in the marginal equations to test for Super Exogeneity in ln.FinConsExpHH.
#> Hence not possible to run the test.
#> Estimating GValueAddGov = FinConsExpGov
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
#> Estimating GCapitalForm = FinConsExpGov  +  FinConsExpHH
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
#> Estimating GValueAddConstr = LabCostConstr  +  BuildingPermits
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> No Outliers or Step-Shifts detected in the marginal equations to test for Super Exogeneity in ln.GValueAddConstr.
#> Hence not possible to run the test.
#> Estimating GValueAddManuf = Export  +  LabCostManuf
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
#> Estimating GValueAddWholesaletrade = Export  +  LabCostService
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
#> Estimating Import = FinConsExpHH  +  GCapitalForm
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
#> Constructing GDP = GValueAddGov  +  GValueAddAgri  +  GValueAddIndus  +  GValueAddConstr  +  GValueAddWholesaletrade  +  GValueAddInfocom  +  GValueAddFinance  +  GValueAddRealest  +  GValueAddResearch  +  GValueAddArts 
#> Estimating Emissions = GDP  +  Export  +  GValueAddIndus
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = zoo::zoo(yvar, order.by = clean_data$time), : The
#> 'include.gum' argument is ignored (temporarily deprecated in isat)
#> Warning in isat.default(y = initial.model$aux$mX[, dep_var_exog_test], mxreg =
#> xvars_ready_df, : The 'include.gum' argument is ignored (temporarily deprecated
#> in isat)
```

``` r
model_result
#> OSEM Model Output
#> -----------------------
#> 
#> Estimation Options:
#> Sample: 2010-01-01 to 2023-07-01
#> Max AR Considered: 4
#> Estimation Option: ardl
#> 
#> Relationships considered: 
#> # A tibble: 9 × 3
#>   Model `Dep. Var.`             `Ind. Var`                                      
#> 1     1 FinConsExpHH            ""                                              
#> 2     2 GValueAddGov            "FinConsExpGov"                                 
#> 3     3 GCapitalForm            "FinConsExpGov  +  FinConsExpHH"                
#> 4     4 GValueAddConstr         "LabCostConstr  +  BuildingPermits"             
#> 5     5 GValueAddManuf          "Export  +  LabCostManuf"                       
#> 6     6 GValueAddWholesaletrade "Export  +  LabCostService"                     
#> 7     7 Import                  "FinConsExpHH  +  GCapitalForm"                 
#> 8     8 GDP                     "GValueAddGov  +  GValueAddAgri  +  GValueAddIn…
#> 9     9 Emissions               "GDP  +  Export  +  GValueAddIndus"             
#> 
#> 
#> Relationships estimated in the order:  1,2,3,4,5,6,7,8,9
#> 
#> Diagnostics:
#>  # A tibble: 8 × 8
#>   `Dependent Variable`    AR       ARCH   `Super Exogeneity`   IIS   SIS     n
#>   <chr>                   <chr>    <chr>  <chr>              <int> <int> <int>
#> 1 FinConsExpHH            0.675    0.920  ""                     9     0    54
#> 2 GValueAddGov            0.088*   0.752  "0.027**"             10     3    55
#> 3 GCapitalForm            0.599    0.446  ""                     2     1    53
#> 4 GValueAddConstr         0.008*** 0.524  ""                     7     3    55
#> 5 GValueAddManuf          0.004*** 0.092* "0.014**"              0     7    55
#> 6 GValueAddWholesaletrade 0.616    0.588  "0.240"                0     5    52
#> 7 Import                  0.740    0.700  ""                     5     0    53
#> 8 Emissions               0.042**  0.364  "0.050**"              0     2    55
#> # ℹ 1 more variable: `Share of Indicators` <dbl>
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

The first time that we run this, all data will be downloaded and saved
in the folder `data/use/InputData.xlsx`.

The next time that we run the same model, we can save some time and just
load the data from our earlier run:

``` r
model_result <- run_model(
  specification = spec,
  primary_source = "local",
  inputdata_directory = "inst/extdata",
  trend = TRUE,
  saturation.tpval = 0.01
)
```

### Forecasting the model

Now that we have run the model, we can forecast the model (here using an
AR process for the exogenous values and for 10 time periods):

``` r
model_forecast <- forecast_model(model_result, n.ahead = 10, exog_fill_method = "AR", plot = FALSE)
#> No exogenous values provided. Model will forecast the exogenous values with an AR4 process (incl. Q dummies, IIS and SIS w 't.pval = 0.001').
#> Alternative is exog_fill_method = 'last'.
```

Once we are done, we can plot the forecast:

``` r
plot(model_forecast, order.as.run = TRUE)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

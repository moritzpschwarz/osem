
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
  ))
```

To summarise this, we can print out the specification table:

<table class="table lightable-paper" style="margin-left: auto; margin-right: auto; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;">

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

</tr>

</tbody>

</table>

In order to run this model, we also need a dictionary that translates
our model variables to EUROSTAT codes so that the download process can
be automated. You can either pass a new dictionary to the model
function, or you can use the built in dictionary `osem::dict` (here the
first few rows):

<table class="table lightable-paper" style="margin-left: auto; margin-right: auto; font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;">

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
#> indexed 0B in  0s, 0B/sindexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table sts_cobp_q cached at C:\Users\morit\AppData\Local\Temp\RtmpsZ2vkm/eurostat/3740f861961c59cd9c1235bc6ab63ce3.rds
#> indexed 0B in  0s, 0B/sindexed 14.29MB in  0s, 71.06MB/sindexed 14.42MB in  0s, 71.16MB/sindexed 14.55MB in  0s, 71.23MB/sindexed 14.68MB in  0s, 71.27MB/sindexed 14.81MB in  0s, 71.36MB/sindexed 14.94MB in  0s, 71.32MB/sindexed 15.07MB in  0s, 71.46MB/sindexed 15.20MB in  0s, 71.57MB/sindexed 15.34MB in  0s, 71.65MB/sindexed 15.47MB in  0s, 71.79MB/sindexed 15.60MB in  0s, 71.89MB/sindexed 15.73MB in  0s, 71.98MB/sindexed 15.86MB in  0s, 72.10MB/sindexed 15.99MB in  0s, 72.10MB/sindexed 16.12MB in  0s, 72.08MB/sindexed 16.25MB in  0s, 72.16MB/sindexed 16.38MB in  0s, 72.24MB/sindexed 16.51MB in  0s, 72.34MB/sindexed 16.65MB in  0s, 72.43MB/sindexed 16.78MB in  0s, 72.54MB/sindexed 16.91MB in  0s, 72.60MB/sindexed 17.04MB in  0s, 72.66MB/sindexed 17.17MB in  0s, 72.76MB/sindexed 17.30MB in  0s, 70.38MB/sindexed 17.43MB in  0s, 70.43MB/sindexed 17.56MB in  0s, 70.53MB/sindexed 17.69MB in  0s, 70.65MB/sindexed 17.83MB in  0s, 70.75MB/sindexed 17.96MB in  0s, 70.86MB/sindexed 18.09MB in  0s, 70.94MB/sindexed 18.22MB in  0s, 70.93MB/sindexed 18.35MB in  0s, 70.93MB/sindexed 18.48MB in  0s, 70.97MB/sindexed 18.61MB in  0s, 71.01MB/sindexed 18.74MB in  0s, 71.09MB/sindexed 18.87MB in  0s, 71.13MB/sindexed 19.01MB in  0s, 71.08MB/sindexed 19.14MB in  0s, 71.19MB/sindexed 19.27MB in  0s, 71.31MB/sindexed 19.40MB in  0s, 71.38MB/sindexed 19.53MB in  0s, 71.44MB/sindexed 19.66MB in  0s, 71.53MB/sindexed 19.79MB in  0s, 71.61MB/sindexed 19.92MB in  0s, 71.73MB/sindexed 20.05MB in  0s, 71.86MB/sindexed 20.18MB in  0s, 71.97MB/sindexed 20.32MB in  0s, 72.02MB/sindexed 20.45MB in  0s, 72.12MB/sindexed 20.58MB in  0s, 72.11MB/sindexed 20.71MB in  0s, 72.22MB/sindexed 20.84MB in  0s, 72.28MB/sindexed 20.97MB in  0s, 72.37MB/sindexed 21.10MB in  0s, 72.47MB/sindexed 21.23MB in  0s, 72.57MB/sindexed 21.36MB in  0s, 72.68MB/sindexed 21.50MB in  0s, 72.81MB/sindexed 21.63MB in  0s, 72.89MB/sindexed 21.76MB in  0s, 72.93MB/sindexed 21.89MB in  0s, 73.01MB/sindexed 22.02MB in  0s, 73.10MB/sindexed 22.15MB in  0s, 73.18MB/sindexed 22.28MB in  0s, 73.27MB/sindexed 22.41MB in  0s, 73.36MB/sindexed 22.54MB in  0s, 73.45MB/sindexed 22.68MB in  0s, 73.55MB/sindexed 22.81MB in  0s, 73.62MB/sindexed 22.94MB in  0s, 73.71MB/sindexed 23.07MB in  0s, 73.68MB/sindexed 23.20MB in  0s, 73.74MB/sindexed 23.33MB in  0s, 73.83MB/sindexed 23.46MB in  0s, 73.93MB/sindexed 23.59MB in  0s, 74.01MB/sindexed 23.72MB in  0s, 74.09MB/sindexed 23.85MB in  0s, 74.13MB/sindexed 23.99MB in  0s, 74.22MB/sindexed 24.12MB in  0s, 74.31MB/sindexed 24.25MB in  0s, 74.38MB/sindexed 24.38MB in  0s, 74.45MB/sindexed 24.51MB in  0s, 74.53MB/sindexed 24.64MB in  0s, 74.59MB/sindexed 24.77MB in  0s, 74.69MB/sindexed 24.90MB in  0s, 74.70MB/sindexed 25.03MB in  0s, 74.76MB/sindexed 25.17MB in  0s, 74.83MB/sindexed 25.30MB in  0s, 74.80MB/sindexed 25.43MB in  0s, 74.80MB/sindexed 25.56MB in  0s, 74.85MB/sindexed 25.69MB in  0s, 74.90MB/sindexed 25.82MB in  0s, 74.96MB/sindexed 25.95MB in  0s, 75.02MB/sindexed 26.08MB in  0s, 75.08MB/sindexed 26.21MB in  0s, 75.14MB/sindexed 26.35MB in  0s, 75.12MB/sindexed 26.48MB in  0s, 75.09MB/sindexed 26.61MB in  0s, 75.06MB/sindexed 26.74MB in  0s, 75.10MB/sindexed 26.87MB in  0s, 75.13MB/sindexed 27.00MB in  0s, 75.20MB/sindexed 27.13MB in  0s, 75.25MB/sindexed 27.26MB in  0s, 75.27MB/sindexed 27.39MB in  0s, 75.32MB/sindexed 27.52MB in  0s, 73.09MB/sindexed 27.66MB in  0s, 73.15MB/sindexed 27.79MB in  0s, 73.22MB/sindexed 27.92MB in  0s, 73.28MB/sindexed 28.05MB in  0s, 73.35MB/sindexed 28.18MB in  0s, 73.41MB/sindexed 28.31MB in  0s, 73.45MB/sindexed 28.44MB in  0s, 73.52MB/sindexed 28.57MB in  0s, 73.60MB/sindexed 28.70MB in  0s, 73.68MB/sindexed 28.84MB in  0s, 73.72MB/sindexed 28.97MB in  0s, 73.76MB/sindexed 29.10MB in  0s, 73.81MB/sindexed 29.23MB in  0s, 73.84MB/sindexed 29.36MB in  0s, 73.89MB/sindexed 29.49MB in  0s, 73.95MB/sindexed 29.62MB in  0s, 73.99MB/sindexed 29.75MB in  0s, 74.06MB/sindexed 29.88MB in  0s, 74.12MB/sindexed 30.02MB in  0s, 74.14MB/sindexed 30.15MB in  0s, 74.19MB/sindexed 30.28MB in  0s, 74.24MB/sindexed 30.41MB in  0s, 74.29MB/sindexed 30.54MB in  0s, 74.33MB/sindexed 30.67MB in  0s, 74.35MB/sindexed 30.80MB in  0s, 74.35MB/sindexed 30.93MB in  0s, 74.39MB/sindexed 31.06MB in  0s, 74.44MB/sindexed 31.19MB in  0s, 74.51MB/sindexed 31.33MB in  0s, 74.58MB/sindexed 31.46MB in  0s, 74.62MB/sindexed 31.59MB in  0s, 74.65MB/sindexed 31.72MB in  0s, 74.68MB/sindexed 31.85MB in  0s, 74.73MB/sindexed 31.98MB in  0s, 74.79MB/sindexed 32.11MB in  0s, 74.84MB/sindexed 32.24MB in  0s, 74.92MB/sindexed 32.37MB in  0s, 74.93MB/sindexed 32.51MB in  0s, 74.97MB/sindexed 32.64MB in  0s, 75.02MB/sindexed 32.77MB in  0s, 75.05MB/sindexed 32.90MB in  0s, 75.06MB/sindexed 33.03MB in  0s, 75.11MB/sindexed 33.16MB in  0s, 75.17MB/sindexed 33.29MB in  0s, 75.21MB/sindexed 33.42MB in  0s, 75.26MB/sindexed 33.55MB in  0s, 75.25MB/sindexed 33.69MB in  0s, 75.21MB/sindexed 33.82MB in  0s, 75.18MB/sindexed 33.95MB in  0s, 75.16MB/sindexed 34.08MB in  0s, 75.14MB/sindexed 34.21MB in  0s, 75.12MB/sindexed 34.34MB in  0s, 75.09MB/sindexed 34.47MB in  0s, 75.08MB/sindexed 34.60MB in  0s, 75.07MB/sindexed 34.73MB in  0s, 75.02MB/sindexed 34.86MB in  0s, 74.99MB/sindexed 35.00MB in  0s, 74.98MB/sindexed 35.13MB in  0s, 74.94MB/sindexed 35.26MB in  0s, 74.91MB/sindexed 35.39MB in  0s, 74.88MB/sindexed 35.52MB in  0s, 74.86MB/sindexed 35.65MB in  0s, 74.85MB/sindexed 35.78MB in  0s, 74.84MB/sindexed 35.91MB in  0s, 74.83MB/sindexed 36.04MB in  0s, 74.82MB/sindexed 36.18MB in  0s, 74.79MB/sindexed 36.31MB in  0s, 74.77MB/sindexed 36.44MB in  0s, 74.77MB/sindexed 36.57MB in  0s, 74.75MB/sindexed 36.70MB in  0s, 74.71MB/sindexed 36.83MB in  0s, 74.71MB/sindexed 36.96MB in  0s, 74.72MB/sindexed 37.09MB in  0s, 74.70MB/sindexed 37.22MB in  0s, 74.73MB/sindexed 37.36MB in  0s, 74.75MB/sindexed 37.49MB in  1s, 74.76MB/sindexed 37.62MB in  1s, 74.77MB/sindexed 37.75MB in  1s, 74.74MB/sindexed 37.88MB in  1s, 74.75MB/sindexed 38.01MB in  1s, 74.74MB/sindexed 38.14MB in  1s, 73.72MB/sindexed 38.27MB in  1s, 73.71MB/sindexed 38.40MB in  1s, 73.72MB/sindexed 38.53MB in  1s, 73.73MB/sindexed 38.67MB in  1s, 73.76MB/sindexed 38.80MB in  1s, 73.79MB/sindexed 38.93MB in  1s, 73.76MB/sindexed 39.06MB in  1s, 73.73MB/sindexed 39.19MB in  1s, 73.68MB/sindexed 39.32MB in  1s, 73.66MB/sindexed 39.45MB in  1s, 73.62MB/sindexed 39.58MB in  1s, 73.59MB/sindexed 39.71MB in  1s, 73.55MB/sindexed 39.85MB in  1s, 73.54MB/sindexed 39.98MB in  1s, 73.53MB/sindexed 40.11MB in  1s, 73.50MB/sindexed 40.24MB in  1s, 73.51MB/sindexed 40.37MB in  1s, 73.54MB/sindexed 40.50MB in  1s, 73.58MB/sindexed 40.63MB in  1s, 73.63MB/sindexed 40.76MB in  1s, 73.64MB/sindexed 40.89MB in  1s, 73.66MB/sindexed 41.03MB in  1s, 73.70MB/sindexed 41.16MB in  1s, 73.73MB/sindexed 41.29MB in  1s, 73.75MB/sindexed 41.42MB in  1s, 73.76MB/sindexed 41.55MB in  1s, 73.81MB/sindexed 41.68MB in  1s, 73.83MB/sindexed 41.81MB in  1s, 73.88MB/sindexed 41.94MB in  1s, 73.91MB/sindexed 42.07MB in  1s, 73.93MB/sindexed 42.20MB in  1s, 73.95MB/sindexed 42.34MB in  1s, 73.95MB/sindexed 42.47MB in  1s, 73.97MB/sindexed 42.60MB in  1s, 73.97MB/sindexed 42.73MB in  1s, 73.97MB/sindexed 42.86MB in  1s, 73.97MB/sindexed 42.99MB in  1s, 73.96MB/sindexed 43.12MB in  1s, 73.99MB/sindexed 43.25MB in  1s, 74.01MB/sindexed 43.38MB in  1s, 74.00MB/sindexed 43.52MB in  1s, 73.89MB/sindexed 43.65MB in  1s, 73.85MB/sindexed 43.78MB in  1s, 73.80MB/sindexed 43.91MB in  1s, 73.79MB/sindexed 44.04MB in  1s, 73.81MB/sindexed 44.17MB in  1s, 73.83MB/sindexed 44.30MB in  1s, 73.85MB/sindexed 44.43MB in  1s, 73.87MB/sindexed 44.56MB in  1s, 73.86MB/sindexed 44.70MB in  1s, 73.78MB/sindexed 44.83MB in  1s, 73.80MB/sindexed 44.96MB in  1s, 73.82MB/sindexed 45.09MB in  1s, 73.84MB/sindexed 45.22MB in  1s, 73.85MB/sindexed 45.35MB in  1s, 73.86MB/sindexed 45.48MB in  1s, 73.89MB/sindexed 45.61MB in  1s, 73.89MB/sindexed 45.74MB in  1s, 73.90MB/sindexed 45.87MB in  1s, 73.92MB/sindexed 46.01MB in  1s, 73.95MB/sindexed 46.14MB in  1s, 73.98MB/sindexed 46.27MB in  1s, 74.00MB/sindexed 46.40MB in  1s, 74.04MB/sindexed 46.53MB in  1s, 74.06MB/sindexed 46.66MB in  1s, 74.07MB/sindexed 46.79MB in  1s, 74.11MB/sindexed 46.92MB in  1s, 74.14MB/sindexed 47.05MB in  1s, 74.16MB/sindexed 47.19MB in  1s, 74.16MB/sindexed 47.32MB in  1s, 74.15MB/sindexed 47.45MB in  1s, 74.16MB/sindexed 47.58MB in  1s, 74.16MB/sindexed 47.71MB in  1s, 74.18MB/sindexed 47.84MB in  1s, 74.21MB/sindexed 47.97MB in  1s, 74.23MB/sindexed 48.10MB in  1s, 74.25MB/sindexed 48.23MB in  1s, 74.30MB/sindexed 48.37MB in  1s, 74.32MB/sindexed 48.50MB in  1s, 74.36MB/sindexed 48.63MB in  1s, 74.36MB/sindexed 48.76MB in  1s, 74.36MB/sindexed 48.89MB in  1s, 74.38MB/sindexed 49.02MB in  1s, 74.39MB/sindexed 49.15MB in  1s, 74.40MB/sindexed 49.28MB in  1s, 74.40MB/sindexed 49.41MB in  1s, 74.39MB/sindexed 49.54MB in  1s, 74.36MB/sindexed 49.68MB in  1s, 74.32MB/sindexed 49.81MB in  1s, 74.25MB/sindexed 49.94MB in  1s, 74.11MB/sindexed 50.07MB in  1s, 74.08MB/sindexed 50.20MB in  1s, 74.08MB/sindexed 50.33MB in  1s, 74.07MB/sindexed 50.46MB in  1s, 74.07MB/sindexed 50.59MB in  1s, 74.05MB/sindexed 50.72MB in  1s, 74.03MB/sindexed 50.86MB in  1s, 74.01MB/sindexed 50.99MB in  1s, 73.97MB/sindexed 51.12MB in  1s, 71.58MB/sindexed 51.25MB in  1s, 71.55MB/sindexed 51.38MB in  1s, 71.54MB/sindexed 51.51MB in  1s, 71.52MB/sindexed 51.64MB in  1s, 71.50MB/sindexed 51.77MB in  1s, 71.48MB/sindexed 51.90MB in  1s, 71.48MB/sindexed 52.04MB in  1s, 71.46MB/sindexed 52.17MB in  1s, 71.44MB/sindexed 52.30MB in  1s, 71.42MB/sindexed 52.43MB in  1s, 71.41MB/sindexed 52.56MB in  1s, 71.43MB/sindexed 52.69MB in  1s, 71.45MB/sindexed 52.82MB in  1s, 71.47MB/sindexed 52.95MB in  1s, 71.49MB/sindexed 53.08MB in  1s, 71.52MB/sindexed 53.21MB in  1s, 71.55MB/sindexed 53.35MB in  1s, 71.58MB/sindexed 53.48MB in  1s, 71.61MB/sindexed 53.61MB in  1s, 71.63MB/sindexed 53.74MB in  1s, 71.66MB/sindexed 53.87MB in  1s, 71.65MB/sindexed 54.00MB in  1s, 71.63MB/sindexed 54.13MB in  1s, 71.63MB/sindexed 54.26MB in  1s, 71.65MB/sindexed 54.39MB in  1s, 71.63MB/sindexed 54.53MB in  1s, 71.68MB/sindexed 54.66MB in  1s, 71.71MB/sindexed 54.79MB in  1s, 71.76MB/sindexed 54.92MB in  1s, 71.77MB/sindexed 55.05MB in  1s, 71.78MB/sindexed 55.18MB in  1s, 71.82MB/sindexed 55.31MB in  1s, 71.85MB/sindexed 55.44MB in  1s, 71.88MB/sindexed 55.57MB in  1s, 71.92MB/sindexed 55.71MB in  1s, 71.97MB/sindexed 55.84MB in  1s, 72.01MB/sindexed 55.97MB in  1s, 72.05MB/sindexed 56.10MB in  1s, 72.08MB/sindexed 56.23MB in  1s, 72.12MB/sindexed 56.36MB in  1s, 72.16MB/sindexed 56.49MB in  1s, 72.20MB/sindexed 56.62MB in  1s, 72.23MB/sindexed 56.75MB in  1s, 72.27MB/sindexed 56.88MB in  1s, 72.17MB/sindexed 57.02MB in  1s, 72.13MB/sindexed 57.15MB in  1s, 72.13MB/sindexed 57.28MB in  1s, 72.13MB/sindexed 57.41MB in  1s, 72.12MB/sindexed 57.54MB in  1s, 72.14MB/sindexed 57.67MB in  1s, 72.15MB/sindexed 57.80MB in  1s, 72.12MB/sindexed 57.93MB in  1s, 72.11MB/sindexed 58.06MB in  1s, 72.06MB/sindexed 58.20MB in  1s, 72.08MB/sindexed 58.33MB in  1s, 72.08MB/sindexed 58.46MB in  1s, 72.13MB/sindexed 58.59MB in  1s, 72.15MB/sindexed 58.72MB in  1s, 72.16MB/sindexed 58.85MB in  1s, 71.41MB/sindexed 58.98MB in  1s, 71.32MB/sindexed 59.11MB in  1s, 71.26MB/sindexed 59.24MB in  1s, 71.25MB/sindexed 59.38MB in  1s, 71.20MB/sindexed 59.51MB in  1s, 71.22MB/sindexed 59.64MB in  1s, 71.23MB/sindexed 59.77MB in  1s, 71.22MB/sindexed 59.90MB in  1s, 71.22MB/sindexed 60.03MB in  1s, 71.23MB/sindexed 60.16MB in  1s, 71.25MB/sindexed 60.29MB in  1s, 71.27MB/sindexed 60.42MB in  1s, 71.32MB/sindexed 60.55MB in  1s, 71.32MB/sindexed 60.69MB in  1s, 71.34MB/sindexed 60.82MB in  1s, 71.36MB/sindexed 60.95MB in  1s, 71.36MB/sindexed 61.08MB in  1s, 71.37MB/sindexed 61.21MB in  1s, 71.31MB/sindexed 61.34MB in  1s, 71.28MB/sindexed 61.47MB in  1s, 71.26MB/sindexed 61.60MB in  1s, 71.28MB/sindexed 61.73MB in  1s, 71.24MB/sindexed 61.87MB in  1s, 71.27MB/sindexed 62.00MB in  1s, 71.30MB/sindexed 62.13MB in  1s, 71.30MB/sindexed 62.26MB in  1s, 71.30MB/sindexed 62.39MB in  1s, 71.32MB/sindexed 62.52MB in  1s, 71.34MB/sindexed 62.65MB in  1s, 71.36MB/sindexed 62.78MB in  1s, 71.38MB/sindexed 62.91MB in  1s, 71.38MB/sindexed 63.05MB in  1s, 71.39MB/sindexed 63.18MB in  1s, 71.41MB/sindexed 63.31MB in  1s, 71.39MB/sindexed 63.44MB in  1s, 71.35MB/sindexed 63.57MB in  1s, 71.38MB/sindexed 63.70MB in  1s, 71.40MB/sindexed 63.83MB in  1s, 71.41MB/sindexed 63.96MB in  1s, 71.43MB/sindexed 64.09MB in  1s, 71.43MB/sindexed 64.22MB in  1s, 71.46MB/sindexed 64.36MB in  1s, 71.48MB/sindexed 64.49MB in  1s, 71.44MB/sindexed 64.62MB in  1s, 71.44MB/sindexed 64.75MB in  1s, 71.39MB/sindexed 64.88MB in  1s, 71.34MB/sindexed 65.01MB in  1s, 71.30MB/sindexed 65.14MB in  1s, 71.24MB/sindexed 65.27MB in  1s, 71.19MB/sindexed 65.40MB in  1s, 71.09MB/sindexed 65.54MB in  1s, 71.02MB/sindexed 65.67MB in  1s, 71.00MB/sindexed 65.80MB in  1s, 71.02MB/sindexed 65.93MB in  1s, 71.04MB/sindexed 66.06MB in  1s, 71.05MB/sindexed 66.19MB in  1s, 71.05MB/sindexed 66.32MB in  1s, 71.07MB/sindexed 66.45MB in  1s, 71.10MB/sindexed 66.58MB in  1s, 71.11MB/sindexed 66.72MB in  1s, 71.08MB/sindexed 66.85MB in  1s, 71.09MB/sindexed 66.98MB in  1s, 71.08MB/sindexed 67.11MB in  1s, 71.10MB/sindexed 67.24MB in  1s, 71.12MB/sindexed 67.37MB in  1s, 71.14MB/sindexed 67.50MB in  1s, 71.17MB/sindexed 67.63MB in  1s, 71.17MB/sindexed 67.76MB in  1s, 71.20MB/sindexed 67.89MB in  1s, 71.21MB/sindexed 68.03MB in  1s, 71.23MB/sindexed 68.16MB in  1s, 71.21MB/sindexed 68.29MB in  1s, 71.23MB/sindexed 68.42MB in  1s, 71.24MB/sindexed 68.55MB in  1s, 71.24MB/sindexed 68.68MB in  1s, 71.25MB/sindexed 68.81MB in  1s, 71.26MB/sindexed 68.94MB in  1s, 71.29MB/sindexed 69.07MB in  1s, 71.30MB/sindexed 69.21MB in  1s, 71.32MB/sindexed 69.34MB in  1s, 71.32MB/sindexed 69.47MB in  1s, 71.34MB/sindexed 69.60MB in  1s, 71.36MB/sindexed 69.73MB in  1s, 71.37MB/sindexed 69.86MB in  1s, 71.39MB/sindexed 69.99MB in  1s, 71.40MB/sindexed 70.12MB in  1s, 71.41MB/sindexed 70.25MB in  1s, 71.42MB/sindexed 70.39MB in  1s, 71.43MB/sindexed 70.52MB in  1s, 71.44MB/sindexed 70.65MB in  1s, 71.41MB/sindexed 70.78MB in  1s, 71.42MB/sindexed 70.91MB in  1s, 71.43MB/sindexed 71.04MB in  1s, 71.44MB/sindexed 71.17MB in  1s, 71.45MB/sindexed 71.30MB in  1s, 71.44MB/sindexed 71.43MB in  1s, 71.44MB/sindexed 71.56MB in  1s, 71.44MB/sindexed 71.70MB in  1s, 71.45MB/sindexed 71.83MB in  1s, 71.43MB/sindexed 71.96MB in  1s, 71.42MB/sindexed 72.09MB in  1s, 71.43MB/sindexed 72.22MB in  1s, 71.44MB/sindexed 72.35MB in  1s, 71.44MB/sindexed 72.48MB in  1s, 71.43MB/sindexed 72.61MB in  1s, 71.39MB/sindexed 72.74MB in  1s, 71.35MB/sindexed 72.88MB in  1s, 71.30MB/sindexed 73.01MB in  1s, 71.26MB/sindexed 73.14MB in  1s, 71.23MB/sindexed 73.27MB in  1s, 71.21MB/sindexed 73.40MB in  1s, 71.20MB/sindexed 73.53MB in  1s, 71.18MB/sindexed 73.66MB in  1s, 71.16MB/sindexed 73.79MB in  1s, 71.14MB/sindexed 73.92MB in  1s, 71.12MB/sindexed 74.06MB in  1s, 71.10MB/sindexed 74.19MB in  1s, 71.10MB/sindexed 74.32MB in  1s, 71.10MB/sindexed 74.45MB in  1s, 71.10MB/sindexed 74.58MB in  1s, 71.10MB/sindexed 74.71MB in  1s, 71.09MB/sindexed 74.84MB in  1s, 71.08MB/sindexed 74.97MB in  1s, 71.06MB/sindexed 75.10MB in  1s, 71.02MB/sindexed 75.23MB in  1s, 71.00MB/sindexed 75.37MB in  1s, 70.99MB/sindexed 75.50MB in  1s, 70.97MB/sindexed 75.63MB in  1s, 70.97MB/sindexed 75.76MB in  1s, 70.95MB/sindexed 75.89MB in  1s, 70.95MB/sindexed 76.02MB in  1s, 70.93MB/sindexed 76.15MB in  1s, 70.90MB/sindexed 76.28MB in  1s, 70.89MB/sindexed 76.41MB in  1s, 70.89MB/sindexed 76.55MB in  1s, 70.89MB/sindexed 76.68MB in  1s, 70.86MB/sindexed 76.81MB in  1s, 70.79MB/sindexed 76.94MB in  1s, 70.77MB/sindexed 77.07MB in  1s, 70.73MB/sindexed 77.20MB in  1s, 70.72MB/sindexed 77.33MB in  1s, 70.70MB/sindexed 77.46MB in  1s, 70.69MB/sindexed 77.59MB in  1s, 70.68MB/sindexed 77.73MB in  1s, 70.69MB/sindexed 77.86MB in  1s, 70.71MB/sindexed 77.99MB in  1s, 70.70MB/sindexed 78.12MB in  1s, 70.68MB/sindexed 78.25MB in  1s, 70.65MB/sindexed 78.38MB in  1s, 70.63MB/sindexed 78.51MB in  1s, 70.61MB/sindexed 78.64MB in  1s, 70.51MB/sindexed 78.77MB in  1s, 70.47MB/sindexed 78.90MB in  1s, 70.41MB/sindexed 79.04MB in  1s, 70.32MB/sindexed 79.17MB in  1s, 70.30MB/sindexed 79.30MB in  1s, 70.31MB/sindexed 79.43MB in  1s, 69.82MB/sindexed 79.56MB in  1s, 69.82MB/sindexed 79.69MB in  1s, 69.83MB/sindexed 79.82MB in  1s, 69.84MB/sindexed 79.95MB in  1s, 69.85MB/sindexed 80.08MB in  1s, 69.86MB/sindexed 80.22MB in  1s, 69.88MB/sindexed 80.35MB in  1s, 69.90MB/sindexed 80.48MB in  1s, 69.91MB/sindexed 80.61MB in  1s, 69.94MB/sindexed 80.74MB in  1s, 69.94MB/sindexed 80.87MB in  1s, 69.95MB/sindexed 81.00MB in  1s, 69.95MB/sindexed 81.13MB in  1s, 69.96MB/sindexed 81.26MB in  1s, 69.96MB/sindexed 81.40MB in  1s, 69.96MB/sindexed 81.53MB in  1s, 69.99MB/sindexed 81.65MB in  1s, 70.00MB/s                                                                              indexed 2.15GB in  1s, 2.15GB/s                                                                              
#> Table namq_10_gdp cached at C:\Users\morit\AppData\Local\Temp\RtmpsZ2vkm/eurostat/3c62f858b84e23f9db4052b0ff27f317.rds
#> indexed 0B in  0s, 0B/sindexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table env_ac_aigg_q cached at C:\Users\morit\AppData\Local\Temp\RtmpsZ2vkm/eurostat/04f733c4c8add870d5f85b191ad25ac8.rds
#> indexed 0B in  0s, 0B/sindexed 10.88MB in  0s, 54.07MB/sindexed 11.01MB in  0s, 53.96MB/sindexed 11.14MB in  0s, 54.03MB/sindexed 11.27MB in  0s, 54.17MB/sindexed 11.40MB in  0s, 54.16MB/sindexed 11.53MB in  0s, 54.25MB/sindexed 11.67MB in  0s, 54.51MB/sindexed 11.80MB in  0s, 54.62MB/sindexed 11.93MB in  0s, 53.22MB/sindexed 12.06MB in  0s, 53.30MB/sindexed 12.19MB in  0s, 53.37MB/sindexed 12.32MB in  0s, 53.49MB/sindexed 12.45MB in  0s, 53.73MB/sindexed 12.58MB in  0s, 53.89MB/sindexed 12.71MB in  0s, 54.04MB/sindexed 12.84MB in  0s, 54.12MB/sindexed 12.98MB in  0s, 54.25MB/sindexed 13.11MB in  0s, 54.36MB/sindexed 13.24MB in  0s, 54.52MB/sindexed 13.37MB in  0s, 54.54MB/sindexed 13.50MB in  0s, 54.63MB/sindexed 13.63MB in  0s, 54.78MB/sindexed 13.76MB in  0s, 54.85MB/sindexed 13.89MB in  0s, 54.90MB/sindexed 14.02MB in  0s, 55.03MB/sindexed 14.16MB in  0s, 55.13MB/sindexed 14.29MB in  0s, 55.22MB/sindexed 14.42MB in  0s, 55.32MB/sindexed 14.55MB in  0s, 55.44MB/sindexed 14.68MB in  0s, 55.52MB/sindexed 14.81MB in  0s, 55.60MB/sindexed 14.94MB in  0s, 55.72MB/sindexed 15.07MB in  0s, 55.82MB/sindexed 15.19MB in  0s, 55.89MB/s                                                                              indexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table ei_lmlc_q cached at C:\Users\morit\AppData\Local\Temp\RtmpsZ2vkm/eurostat/42ec0997a83e18e05c3b5bf5419b68e9.rds
#> indexed 0B in  0s, 0B/sindexed 13.24MB in  0s, 66.18MB/sindexed 13.37MB in  0s, 66.30MB/sindexed 13.50MB in  0s, 64.46MB/sindexed 13.63MB in  0s, 64.52MB/sindexed 13.76MB in  0s, 64.67MB/sindexed 13.89MB in  0s, 64.82MB/sindexed 14.02MB in  0s, 64.97MB/sindexed 14.16MB in  0s, 65.07MB/sindexed 14.29MB in  0s, 65.17MB/sindexed 14.42MB in  0s, 65.27MB/sindexed 14.55MB in  0s, 65.40MB/sindexed 14.68MB in  0s, 65.53MB/sindexed 14.81MB in  0s, 65.65MB/sindexed 14.94MB in  0s, 65.79MB/sindexed 15.07MB in  0s, 65.87MB/sindexed 15.20MB in  0s, 65.98MB/sindexed 15.34MB in  0s, 66.15MB/sindexed 15.47MB in  0s, 66.27MB/sindexed 15.60MB in  0s, 66.36MB/sindexed 15.73MB in  0s, 66.40MB/sindexed 15.86MB in  0s, 66.10MB/sindexed 15.99MB in  0s, 66.04MB/sindexed 16.12MB in  0s, 65.76MB/sindexed 16.25MB in  0s, 65.82MB/sindexed 16.38MB in  0s, 65.87MB/sindexed 16.51MB in  0s, 65.95MB/sindexed 16.65MB in  0s, 66.03MB/sindexed 16.78MB in  0s, 66.06MB/sindexed 16.91MB in  0s, 66.16MB/sindexed 17.04MB in  0s, 66.23MB/sindexed 17.17MB in  0s, 66.34MB/sindexed 17.30MB in  0s, 66.44MB/sindexed 17.43MB in  0s, 66.48MB/sindexed 17.56MB in  0s, 66.55MB/sindexed 17.69MB in  0s, 66.63MB/sindexed 17.83MB in  0s, 66.66MB/sindexed 17.96MB in  0s, 66.73MB/sindexed 18.09MB in  0s, 66.89MB/sindexed 18.22MB in  0s, 66.97MB/sindexed 18.35MB in  0s, 67.14MB/sindexed 18.48MB in  0s, 67.30MB/sindexed 18.61MB in  0s, 67.36MB/sindexed 18.74MB in  0s, 67.37MB/sindexed 18.87MB in  0s, 67.50MB/sindexed 19.01MB in  0s, 67.55MB/sindexed 19.14MB in  0s, 67.61MB/sindexed 19.27MB in  0s, 67.77MB/sindexed 19.40MB in  0s, 67.83MB/sindexed 19.53MB in  0s, 67.81MB/sindexed 19.66MB in  0s, 67.80MB/sindexed 19.79MB in  0s, 67.78MB/sindexed 19.92MB in  0s, 67.63MB/sindexed 20.05MB in  0s, 67.69MB/sindexed 20.18MB in  0s, 67.66MB/sindexed 20.32MB in  0s, 67.67MB/sindexed 20.45MB in  0s, 67.73MB/sindexed 20.58MB in  0s, 67.89MB/sindexed 20.71MB in  0s, 67.91MB/sindexed 20.84MB in  0s, 68.01MB/sindexed 20.97MB in  0s, 68.04MB/sindexed 21.10MB in  0s, 68.01MB/sindexed 21.23MB in  0s, 67.97MB/sindexed 21.36MB in  0s, 67.97MB/sindexed 21.50MB in  0s, 67.94MB/sindexed 21.63MB in  0s, 67.90MB/sindexed 21.76MB in  0s, 67.90MB/sindexed 21.89MB in  0s, 67.92MB/sindexed 22.02MB in  0s, 67.92MB/sindexed 22.15MB in  0s, 67.60MB/sindexed 22.28MB in  0s, 67.59MB/sindexed 22.41MB in  0s, 67.62MB/sindexed 22.54MB in  0s, 67.64MB/sindexed 22.68MB in  0s, 67.67MB/sindexed 22.81MB in  0s, 67.68MB/sindexed 22.94MB in  0s, 67.70MB/sindexed 23.07MB in  0s, 67.73MB/sindexed 23.20MB in  0s, 67.68MB/sindexed 23.33MB in  0s, 67.71MB/sindexed 23.46MB in  0s, 67.78MB/sindexed 23.59MB in  0s, 67.84MB/sindexed 23.72MB in  0s, 67.89MB/sindexed 23.85MB in  0s, 67.95MB/sindexed 23.99MB in  0s, 67.98MB/sindexed 24.12MB in  0s, 68.03MB/sindexed 24.25MB in  0s, 68.07MB/sindexed 24.38MB in  0s, 68.11MB/sindexed 24.51MB in  0s, 68.10MB/sindexed 24.64MB in  0s, 68.14MB/sindexed 24.77MB in  0s, 68.16MB/sindexed 24.90MB in  0s, 68.19MB/sindexed 25.03MB in  0s, 68.18MB/sindexed 25.17MB in  0s, 68.21MB/sindexed 25.30MB in  0s, 66.26MB/sindexed 25.43MB in  0s, 66.29MB/sindexed 25.56MB in  0s, 66.35MB/sindexed 25.69MB in  0s, 66.39MB/sindexed 25.82MB in  0s, 66.38MB/sindexed 25.95MB in  0s, 66.13MB/sindexed 26.08MB in  0s, 66.07MB/sindexed 26.21MB in  0s, 66.07MB/sindexed 26.35MB in  0s, 66.08MB/sindexed 26.48MB in  0s, 66.08MB/sindexed 26.61MB in  0s, 66.13MB/sindexed 26.74MB in  0s, 66.15MB/sindexed 26.87MB in  0s, 66.26MB/sindexed 27.00MB in  0s, 66.29MB/sindexed 27.13MB in  0s, 66.36MB/sindexed 27.26MB in  0s, 66.44MB/sindexed 27.39MB in  0s, 66.49MB/sindexed 27.52MB in  0s, 66.53MB/sindexed 27.66MB in  0s, 66.59MB/sindexed 27.79MB in  0s, 66.64MB/sindexed 27.92MB in  0s, 66.69MB/sindexed 28.05MB in  0s, 66.74MB/sindexed 28.18MB in  0s, 66.80MB/sindexed 28.31MB in  0s, 66.83MB/sindexed 28.44MB in  0s, 66.85MB/sindexed 28.57MB in  0s, 66.84MB/sindexed 28.70MB in  0s, 66.88MB/sindexed 28.84MB in  0s, 66.91MB/sindexed 28.97MB in  0s, 66.95MB/sindexed 29.10MB in  0s, 66.99MB/sindexed 29.23MB in  0s, 67.03MB/sindexed 29.36MB in  0s, 66.94MB/sindexed 29.49MB in  0s, 66.99MB/sindexed 29.62MB in  0s, 67.00MB/sindexed 29.75MB in  0s, 66.93MB/sindexed 29.88MB in  0s, 66.96MB/sindexed 30.02MB in  0s, 66.98MB/sindexed 30.15MB in  0s, 67.02MB/sindexed 30.28MB in  0s, 67.08MB/sindexed 30.41MB in  0s, 67.13MB/sindexed 30.54MB in  0s, 67.18MB/sindexed 30.67MB in  0s, 67.25MB/sindexed 30.80MB in  0s, 67.30MB/sindexed 30.93MB in  0s, 67.33MB/sindexed 31.06MB in  0s, 67.35MB/sindexed 31.19MB in  0s, 67.41MB/sindexed 31.33MB in  0s, 67.43MB/sindexed 31.46MB in  0s, 67.45MB/sindexed 31.59MB in  0s, 67.46MB/sindexed 31.72MB in  0s, 67.49MB/sindexed 31.85MB in  0s, 67.51MB/sindexed 31.98MB in  0s, 67.50MB/sindexed 32.11MB in  0s, 67.40MB/sindexed 32.24MB in  0s, 67.44MB/sindexed 32.37MB in  0s, 67.48MB/sindexed 32.51MB in  0s, 67.52MB/sindexed 32.64MB in  0s, 67.50MB/sindexed 32.77MB in  0s, 67.48MB/sindexed 32.90MB in  0s, 67.47MB/sindexed 33.03MB in  0s, 67.50MB/sindexed 33.16MB in  0s, 67.46MB/sindexed 33.29MB in  0s, 67.43MB/sindexed 33.42MB in  0s, 67.39MB/sindexed 33.55MB in  0s, 67.39MB/sindexed 33.69MB in  1s, 67.37MB/sindexed 33.82MB in  1s, 67.36MB/sindexed 33.95MB in  1s, 67.35MB/sindexed 34.08MB in  1s, 67.33MB/sindexed 34.21MB in  1s, 67.30MB/sindexed 34.34MB in  1s, 67.26MB/sindexed 34.47MB in  1s, 67.27MB/sindexed 34.60MB in  1s, 67.26MB/sindexed 34.73MB in  1s, 67.26MB/sindexed 34.86MB in  1s, 67.26MB/sindexed 35.00MB in  1s, 67.27MB/sindexed 35.13MB in  1s, 67.27MB/sindexed 35.26MB in  1s, 67.34MB/sindexed 35.39MB in  1s, 67.35MB/sindexed 35.52MB in  1s, 67.35MB/sindexed 35.65MB in  1s, 67.31MB/sindexed 35.78MB in  1s, 67.28MB/sindexed 35.91MB in  1s, 67.27MB/sindexed 36.04MB in  1s, 67.26MB/sindexed 36.18MB in  1s, 67.23MB/sindexed 36.31MB in  1s, 67.21MB/sindexed 36.44MB in  1s, 67.12MB/sindexed 36.57MB in  1s, 67.08MB/sindexed 36.70MB in  1s, 67.07MB/sindexed 36.83MB in  1s, 67.08MB/sindexed 36.96MB in  1s, 67.08MB/sindexed 37.09MB in  1s, 67.08MB/sindexed 37.22MB in  1s, 67.05MB/sindexed 37.36MB in  1s, 67.05MB/sindexed 37.49MB in  1s, 66.94MB/sindexed 37.62MB in  1s, 66.73MB/sindexed 37.75MB in  1s, 66.70MB/sindexed 37.88MB in  1s, 66.68MB/sindexed 38.01MB in  1s, 66.66MB/sindexed 38.14MB in  1s, 66.65MB/sindexed 38.27MB in  1s, 66.63MB/sindexed 38.40MB in  1s, 66.51MB/sindexed 38.53MB in  1s, 66.51MB/sindexed 38.67MB in  1s, 66.51MB/sindexed 38.80MB in  1s, 66.53MB/sindexed 38.93MB in  1s, 66.52MB/sindexed 39.06MB in  1s, 66.53MB/sindexed 39.19MB in  1s, 66.54MB/sindexed 39.32MB in  1s, 66.52MB/sindexed 39.45MB in  1s, 66.53MB/sindexed 39.58MB in  1s, 66.51MB/sindexed 39.71MB in  1s, 66.49MB/sindexed 39.85MB in  1s, 66.50MB/sindexed 39.98MB in  1s, 66.52MB/sindexed 40.11MB in  1s, 66.56MB/sindexed 40.24MB in  1s, 66.52MB/sindexed 40.37MB in  1s, 66.52MB/sindexed 40.50MB in  1s, 66.57MB/sindexed 40.63MB in  1s, 66.50MB/sindexed 40.76MB in  1s, 66.42MB/sindexed 40.89MB in  1s, 66.35MB/sindexed 41.03MB in  1s, 66.37MB/sindexed 41.16MB in  1s, 66.42MB/sindexed 41.29MB in  1s, 66.47MB/sindexed 41.42MB in  1s, 66.41MB/sindexed 41.55MB in  1s, 66.43MB/sindexed 41.68MB in  1s, 66.26MB/sindexed 41.81MB in  1s, 66.12MB/sindexed 41.94MB in  1s, 66.15MB/sindexed 42.07MB in  1s, 66.21MB/sindexed 42.20MB in  1s, 66.26MB/sindexed 42.34MB in  1s, 66.28MB/sindexed 42.47MB in  1s, 66.27MB/sindexed 42.60MB in  1s, 66.21MB/sindexed 42.73MB in  1s, 66.16MB/sindexed 42.86MB in  1s, 66.20MB/sindexed 42.99MB in  1s, 66.25MB/sindexed 43.12MB in  1s, 66.30MB/sindexed 43.25MB in  1s, 66.36MB/sindexed 43.38MB in  1s, 66.39MB/sindexed 43.52MB in  1s, 66.39MB/sindexed 43.65MB in  1s, 66.44MB/sindexed 43.78MB in  1s, 66.49MB/sindexed 43.91MB in  1s, 66.53MB/sindexed 44.04MB in  1s, 66.52MB/sindexed 44.17MB in  1s, 66.56MB/sindexed 44.30MB in  1s, 66.61MB/sindexed 44.43MB in  1s, 66.66MB/sindexed 44.56MB in  1s, 66.72MB/sindexed 44.70MB in  1s, 66.76MB/sindexed 44.83MB in  1s, 66.79MB/sindexed 44.96MB in  1s, 66.83MB/sindexed 45.09MB in  1s, 66.89MB/sindexed 45.22MB in  1s, 66.93MB/sindexed 45.35MB in  1s, 66.91MB/sindexed 45.48MB in  1s, 66.94MB/sindexed 45.61MB in  1s, 67.01MB/sindexed 45.74MB in  1s, 67.07MB/sindexed 45.87MB in  1s, 67.14MB/sindexed 46.01MB in  1s, 67.21MB/sindexed 46.14MB in  1s, 67.26MB/sindexed 46.27MB in  1s, 67.33MB/sindexed 46.40MB in  1s, 67.38MB/sindexed 46.53MB in  1s, 67.45MB/sindexed 46.66MB in  1s, 67.53MB/sindexed 46.79MB in  1s, 67.56MB/sindexed 46.92MB in  1s, 67.56MB/sindexed 47.05MB in  1s, 67.55MB/sindexed 47.19MB in  1s, 67.57MB/sindexed 47.32MB in  1s, 67.60MB/sindexed 47.45MB in  1s, 67.62MB/sindexed 47.58MB in  1s, 67.64MB/sindexed 47.71MB in  1s, 67.67MB/sindexed 47.84MB in  1s, 67.71MB/sindexed 47.97MB in  1s, 67.74MB/sindexed 48.10MB in  1s, 67.75MB/sindexed 48.23MB in  1s, 67.76MB/sindexed 48.37MB in  1s, 67.78MB/sindexed 48.50MB in  1s, 65.47MB/sindexed 48.63MB in  1s, 65.49MB/sindexed 48.76MB in  1s, 65.50MB/sindexed 48.89MB in  1s, 65.48MB/sindexed 49.02MB in  1s, 65.52MB/sindexed 49.15MB in  1s, 65.56MB/sindexed 49.28MB in  1s, 65.60MB/sindexed 49.41MB in  1s, 65.58MB/sindexed 49.54MB in  1s, 65.63MB/sindexed 49.68MB in  1s, 65.69MB/sindexed 49.81MB in  1s, 65.71MB/sindexed 49.94MB in  1s, 65.74MB/sindexed 50.07MB in  1s, 65.75MB/sindexed 50.20MB in  1s, 65.77MB/sindexed 50.33MB in  1s, 65.78MB/sindexed 50.46MB in  1s, 65.81MB/sindexed 50.59MB in  1s, 65.84MB/sindexed 50.72MB in  1s, 65.85MB/sindexed 50.86MB in  1s, 65.87MB/sindexed 50.99MB in  1s, 65.90MB/sindexed 51.12MB in  1s, 65.91MB/sindexed 51.25MB in  1s, 65.89MB/sindexed 51.38MB in  1s, 65.85MB/sindexed 51.51MB in  1s, 65.82MB/sindexed 51.64MB in  1s, 65.82MB/sindexed 51.77MB in  1s, 65.83MB/sindexed 51.90MB in  1s, 65.77MB/sindexed 52.04MB in  1s, 65.78MB/sindexed 52.17MB in  1s, 65.76MB/sindexed 52.30MB in  1s, 65.73MB/sindexed 52.43MB in  1s, 65.72MB/sindexed 52.56MB in  1s, 65.71MB/sindexed 52.69MB in  1s, 65.71MB/sindexed 52.82MB in  1s, 65.69MB/sindexed 52.95MB in  1s, 65.69MB/sindexed 53.08MB in  1s, 65.67MB/sindexed 53.21MB in  1s, 65.61MB/sindexed 53.35MB in  1s, 65.60MB/sindexed 53.48MB in  1s, 65.58MB/sindexed 53.61MB in  1s, 65.60MB/sindexed 53.74MB in  1s, 65.61MB/sindexed 53.87MB in  1s, 65.60MB/sindexed 54.00MB in  1s, 65.59MB/sindexed 54.13MB in  1s, 65.60MB/sindexed 54.26MB in  1s, 65.62MB/sindexed 54.39MB in  1s, 65.65MB/sindexed 54.53MB in  1s, 65.68MB/sindexed 54.66MB in  1s, 65.71MB/sindexed 54.79MB in  1s, 65.74MB/sindexed 54.92MB in  1s, 65.77MB/sindexed 55.05MB in  1s, 65.79MB/sindexed 55.18MB in  1s, 65.81MB/sindexed 55.31MB in  1s, 65.81MB/sindexed 55.44MB in  1s, 65.84MB/sindexed 55.57MB in  1s, 65.89MB/sindexed 55.71MB in  1s, 65.90MB/sindexed 55.84MB in  1s, 65.93MB/sindexed 55.88MB in  1s, 65.86MB/s                                                                              indexed 2.15GB in  1s, 2.15GB/s                                                                              
#> Table namq_10_a10 cached at C:\Users\morit\AppData\Local\Temp\RtmpsZ2vkm/eurostat/ac6993d663727d3f13b0841f600cfe6d.rds
#> Warning in load_or_download_variables(specification = module_order, dictionary
#> = dictionary, : Unbalanced panel, will lose more than 20\% of data when making
#> balanced
#> 
#> --- Estimation begins ---
#> Estimating GValueAddGov = FinConsExpGov 
#> Estimating GValueAddManuf = Export + LabCostManuf 
#> Estimating GValueAddConstr = LabCostConstr + BuildingPermits 
#> No Outliers or Step-Shifts detected in the marginal equations to test for Super Exogeneity in ln.GValueAddConstr.
#> Hence not possible to run the test.
#> Estimating GValueAddWholesaletrade = Export + LabCostService 
#> Estimating FinConsExpHH =  
#> No Outliers or Step-Shifts detected in the marginal equations to test for Super Exogeneity in ln.FinConsExpHH.
#> Hence not possible to run the test.
#> Constructing GDP = GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr + GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance + GValueAddRealest + GValueAddResearch + GValueAddArts 
#> Estimating GCapitalForm = FinConsExpGov + FinConsExpHH 
#> Estimating Emissions = GDP + Export + GValueAddIndus 
#> Estimating Import = FinConsExpHH + GCapitalForm
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
#> 1     1 Import                  "FinConsExpHH + GCapitalForm"                   
#> 2     2 FinConsExpHH            ""                                              
#> 3     3 GCapitalForm            "FinConsExpGov + FinConsExpHH"                  
#> 4     4 Emissions               "GDP + Export + GValueAddIndus"                 
#> 5     5 GDP                     "GValueAddGov + GValueAddAgri + GValueAddIndus …
#> 6     6 GValueAddGov            "FinConsExpGov"                                 
#> 7     7 GValueAddManuf          "Export + LabCostManuf"                         
#> 8     8 GValueAddConstr         "LabCostConstr + BuildingPermits"               
#> 9     9 GValueAddWholesaletrade "Export + LabCostService"                       
#> 
#> 
#> Relationships estimated in the order:  6,7,8,9,2,5,3,4,1
#> 
#> Diagnostics:
#>  # A tibble: 8 × 8
#>   `Dependent Variable`    AR        ARCH    `Super Exogeneity`   IIS   SIS     n
#>   <chr>                   <chr>     <chr>   <chr>              <int> <int> <int>
#> 1 GValueAddGov            0.096*    0.557   "0.357"               10     2    55
#> 2 GValueAddManuf          <0.001*** 0.016** "0.059*"               0     4    55
#> 3 GValueAddConstr         0.008***  0.524   ""                     7     3    55
#> 4 GValueAddWholesaletrade 0.616     0.588   "0.240"                0     5    52
#> 5 FinConsExpHH            0.675     0.920   ""                     9     0    54
#> 6 GCapitalForm            0.691     0.247   ""                     1     1    53
#> 7 Emissions               0.042**   0.364   "0.050**"              0     2    55
#> 8 Import                  0.740     0.700   "<0.001***"            5     0    53
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
  input = "inst/extdata/InputData.xlsx",
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

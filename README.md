
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aggregate.model

<!-- badges: start -->

[![R-CMD-check](https://github.com/moritzpschwarz/aggregate.model/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/moritzpschwarz/aggregate.model/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of the aggregate.model Package is to implement and
operationalise the Aggregate Model, developed by Moritz Schwarz, Jonas
Kurle, Felix Pretis, and Andrew Martinez. This is an adaptation of the
[Norwegian Aggregate Model](https://normetrics.no/nam/), developed by
Gunnar Bardsen and Ragnar Nymoen.

## Installation

You can install the development version of aggregate.model from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("moritzpschwarz/aggregate.model")
```

## Basic Workflow

This is an example which shows you how to run the model:

First we load the package:

``` r
library(aggregate.model)
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
function, or you can use the built in dictionary
`aggregate.model::dict`:

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
database
</th>
<th style="text-align:left;">
variable_code
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
ipcc_sector
</th>
<th style="text-align:left;">
cpa2_1
</th>
<th style="text-align:left;">
siec
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
TOTS
</td>
<td style="text-align:left;">
Total Supply
</td>
<td style="text-align:left;">
NA
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
</tr>
<tr>
<td style="text-align:left;">
GDP
</td>
<td style="text-align:left;">
Gross domestic product at market prices
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1GQ
</td>
<td style="text-align:left;">
namq_10_gdp
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
NA
</td>
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
GValueAdd
</td>
<td style="text-align:left;">
Value added, gross
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
TOTAL
</td>
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
Export
</td>
<td style="text-align:left;">
Exports of goods and services
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
P6
</td>
<td style="text-align:left;">
namq_10_gdp
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
NA
</td>
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
Import
</td>
<td style="text-align:left;">
Imports of goods and services
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
P7
</td>
<td style="text-align:left;">
namq_10_gdp
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
NA
</td>
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
GCapitalForm
</td>
<td style="text-align:left;">
Gross capital formation
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
P5G
</td>
<td style="text-align:left;">
namq_10_gdp
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
NA
</td>
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
FinConsExp
</td>
<td style="text-align:left;">
Final consumption expenditure
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
P3
</td>
<td style="text-align:left;">
namq_10_gdp
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
NA
</td>
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
FinConsExpGov
</td>
<td style="text-align:left;">
Final consumption expenditure of general government
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
P3_S13
</td>
<td style="text-align:left;">
namq_10_gdp
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
NA
</td>
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
FinConsExpHH
</td>
<td style="text-align:left;">
Household and NPISH final consumption expenditure
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
P31_S14_S15
</td>
<td style="text-align:left;">
namq_10_gdp
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
NA
</td>
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
StatDiscrep
</td>
<td style="text-align:left;">
Statistical discrepancy (expenditure approach)
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
YA0
</td>
<td style="text-align:left;">
namq_10_gdp
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CP_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
NA
</td>
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
Emissions
</td>
<td style="text-align:left;">
Greenhouse Gas Emissions (All NACE and HH)
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
GHG
</td>
<td style="text-align:left;">
env_ac_aigg_q
</td>
<td style="text-align:left;">
airpol
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
THS_T
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
TOTAL_HH
</td>
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
LabCostManuf
</td>
<td style="text-align:left;">
Manufacturing Labour cost index - Total Labour Cost
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
LM-LCI-TOT
</td>
<td style="text-align:left;">
ei_lmlc_q
</td>
<td style="text-align:left;">
indic
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I20
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
C
</td>
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
DomDemand
</td>
<td style="text-align:left;">
Domestic Demand
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
DomDemand
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
GValueAddManuf
</td>
<td style="text-align:left;">
Value added, gross Manufacturing
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
C
</td>
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
GValueAddGov
</td>
<td style="text-align:left;">
Value added, gross Government
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
O-Q
</td>
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
LabCostConstr
</td>
<td style="text-align:left;">
Construction Labour cost index - Total Labour Cost
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
LM-LCI-TOT
</td>
<td style="text-align:left;">
ei_lmlc_q
</td>
<td style="text-align:left;">
indic
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I20
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
F
</td>
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
BuildingPermits
</td>
<td style="text-align:left;">
Building permits - m^2 useful floorspace - Buildings
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
PSQM
</td>
<td style="text-align:left;">
sts_cobp_q
</td>
<td style="text-align:left;">
indic_bt
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I15
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
F_CC1
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CPA_F41001_41002
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
LabCostService
</td>
<td style="text-align:left;">
Service Labour cost index - Total Labour Cost
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
LM-LCI-TOT
</td>
<td style="text-align:left;">
ei_lmlc_q
</td>
<td style="text-align:left;">
indic
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I20
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
G-N
</td>
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
GValueAddConstr
</td>
<td style="text-align:left;">
Value added, gross Construction
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
F
</td>
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
GValueAddAgri
</td>
<td style="text-align:left;">
Value added, gross Agriculture
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
A
</td>
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
GValueAddIndus
</td>
<td style="text-align:left;">
Value added, gross Industry
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
B-E
</td>
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
GValueAddInfocom
</td>
<td style="text-align:left;">
Value added, gross Information and Communication
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
J
</td>
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
GValueAddFinance
</td>
<td style="text-align:left;">
Value added, gross Financial Services
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
K
</td>
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
GValueAddRealest
</td>
<td style="text-align:left;">
Value added, gross Real Estate
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
L
</td>
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
GValueAddResearch
</td>
<td style="text-align:left;">
Value added, gross Scientific and Professional Services
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
M_N
</td>
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
GValueAddArts
</td>
<td style="text-align:left;">
Value added, gross Arts and Entertainment
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
R-U
</td>
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
GValueAddWholesaletrade
</td>
<td style="text-align:left;">
Value added, gross Wholesale and retail trade and Tourism
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
B1G
</td>
<td style="text-align:left;">
namq_10_a10
</td>
<td style="text-align:left;">
na_item
</td>
<td style="text-align:left;">
q
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
CLV05_MEUR
</td>
<td style="text-align:left;">
SCA
</td>
<td style="text-align:left;">
G-I
</td>
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
HICP
</td>
<td style="text-align:left;">
Harmonised Index of Consumer Prices, all items, index 100 = 2015
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
CP00
</td>
<td style="text-align:left;">
prc_hicp_midx
</td>
<td style="text-align:left;">
coicop
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I15
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
HICP_Energy
</td>
<td style="text-align:left;">
Harmonised Index of Consumer Prices, Energy, index 100 = 2015
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
NRG
</td>
<td style="text-align:left;">
prc_hicp_midx
</td>
<td style="text-align:left;">
coicop
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I15
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
HICP_Electricity
</td>
<td style="text-align:left;">
Harmonised Index of Consumer Prices, Electricity, index 100 = 2015
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
CP0451
</td>
<td style="text-align:left;">
prc_hicp_midx
</td>
<td style="text-align:left;">
coicop
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I15
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
HICP_Gas
</td>
<td style="text-align:left;">
Harmonised Index of Consumer Prices, Gas, index 100 = 2015
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
CP0452
</td>
<td style="text-align:left;">
prc_hicp_midx
</td>
<td style="text-align:left;">
coicop
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I15
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
HICP_Liquid_Fuels
</td>
<td style="text-align:left;">
Harmonised Index of Consumer Prices, Liquid Fuels, index 100 = 2015
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
CP0453
</td>
<td style="text-align:left;">
prc_hicp_midx
</td>
<td style="text-align:left;">
coicop
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I15
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
HICP_Solid_Fuels
</td>
<td style="text-align:left;">
Harmonised Index of Consumer Prices, Solid Fuels, index 100 = 2015
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
CP0454
</td>
<td style="text-align:left;">
prc_hicp_midx
</td>
<td style="text-align:left;">
coicop
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I15
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
HICP_Heat
</td>
<td style="text-align:left;">
Harmonised Index of Consumer Prices, Heat Energy, index 100 = 2015
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
CP0455
</td>
<td style="text-align:left;">
prc_hicp_midx
</td>
<td style="text-align:left;">
coicop
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
I15
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
HDD
</td>
<td style="text-align:left;">
Heating Degree Days
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
HDD
</td>
<td style="text-align:left;">
nrg_chdd_m
</td>
<td style="text-align:left;">
indic_nrg
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
NR
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
CDD
</td>
<td style="text-align:left;">
Cooling Degree Days
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
CDD
</td>
<td style="text-align:left;">
nrg_chdd_m
</td>
<td style="text-align:left;">
indic_nrg
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
NR
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
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
EmiCH4Livestock
</td>
<td style="text-align:left;">
Methane Emissions from Livestock
</td>
<td style="text-align:left;">
edgar
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/v70_FT2021_CH4_m_2000_2021.zip>
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
3.A
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
EmiCO2Industry
</td>
<td style="text-align:left;">
Carbon Emissions from Industrial Processes and Product Use
</td>
<td style="text-align:left;">
edgar
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/v70_FT2021_CO2_m_2000_2021.zip>
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
2
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
EmiCO2Combustion
</td>
<td style="text-align:left;">
Carbon Emissions from Fuel Combustion Activities
</td>
<td style="text-align:left;">
edgar
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/v70_FT2021_CO2_m_2000_2021.zip>
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
1.A
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
EmiN2OTotal
</td>
<td style="text-align:left;">
Nitrous Oxide Emissions Total
</td>
<td style="text-align:left;">
edgar
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/v70_FT2021_N2O_m_2000_2021.zip>
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
m
</td>
<td style="text-align:left;">
AT
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
TOTAL
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
  saturation.tpval = 0.01
)
#> Dataset query already saved in cache_list.json...
#> Reading cache file C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/d63eb01c350ec7f1bc8d8a8748cbaf1c.rds
#> Table  namq_10_gdp  read from cache file:  C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/d63eb01c350ec7f1bc8d8a8748cbaf1c.rds
#> Dataset query already saved in cache_list.json...
#> Reading cache file C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/70233871602d11a16e54c9ad5a1c57c1.rds
#> Table  env_ac_aigg_q  read from cache file:  C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/70233871602d11a16e54c9ad5a1c57c1.rds
#> Dataset query already saved in cache_list.json...
#> Reading cache file C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/4081fb052bac8868b67123e3066ec6a0.rds
#> Table  ei_lmlc_q  read from cache file:  C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/4081fb052bac8868b67123e3066ec6a0.rds
#> Dataset query already saved in cache_list.json...
#> Reading cache file C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/0ce900d1db16519a6a4802bf61a2de80.rds
#> Table  namq_10_a10  read from cache file:  C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/0ce900d1db16519a6a4802bf61a2de80.rds
#> Dataset query already saved in cache_list.json...
#> Reading cache file C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/8c5d516c94180cbe8c02ed2720aaf9b7.rds
#> Table  sts_cobp_q  read from cache file:  C:\Users\morit\AppData\Local\Temp\RtmpAHxT8m/eurostat/8c5d516c94180cbe8c02ed2720aaf9b7.rds
#> Warning in load_or_download_variables(specification = module_order, dictionary
#> = dictionary, : Unbalanced panel, will lose more than 20\% of data when making
#> balanced
#> 
#> --- Estimation begins ---
#> Estimating GValueAddGov = FinConsExpGov 
#> Estimating GValueAddManuf = Export + LabCostManuf 
#> Estimating GValueAddConstr = LabCostConstr + BuildingPermits 
#> Estimating GValueAddWholesaletrade = Export + LabCostService 
#> Estimating FinConsExpHH =  
#> Constructing GDP = GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr + GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance + GValueAddRealest + GValueAddResearch + GValueAddArts 
#> Estimating GCapitalForm = FinConsExpGov + FinConsExpHH 
#> Estimating Emissions = GDP + Export + GValueAddIndus 
#> Estimating Import = FinConsExpHH + GCapitalForm
```

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
model_forecast <- forecast_model(model_result, n.ahead = 10, exog_fill_method = "AR", plot.forecast = FALSE)
#> No exogenous values provided. Model will forecast the exogenous values with an AR4 process (incl. Q dummies, IIS and SIS w 't.pval = 0.001').
#> Alternative is exog_fill_method = 'last'.
```

Once we are done, we can plot the forecast:

``` r
plot(model_forecast, order.as.run = TRUE)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

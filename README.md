
<!-- README.md is generated from README.Rmd. Please edit that file -->

# osem - Open Source Empirical Macro Model

<!-- badges: start -->

[![R-CMD-check](https://github.com/moritzpschwarz/osem/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/moritzpschwarz/osem/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of the {osem} Package is to implement and operationalise the
Open Source Empirical Macro (OSEM) Model, developed by Moritz Schwarz,
Jonas Kurle, Felix Pretis, and Andrew Martinez. This is an adaptation of
the [Norwegian Aggregate Model](https://normetrics.no/nam/), developed
by Gunnar Bardsen and Ragnar Nymoen.

## Installation

You can install the development version of osem from
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
function, or you can use the built in dictionary `osem::dict`:

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
Building permits - m^2 useful floor area
</td>
<td style="text-align:left;">
eurostat
</td>
<td style="text-align:left;">
BPRM_SQM
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
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/EDGAR_CH4_m_1970_2022.zip>
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
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022.zip>
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
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022.zip>
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
<https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/EDGAR_N2O_m_1970_2022.zip>
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
#> indexed 0B in  0s, 0B/sindexed 20.05MB in  0s, 99.64MB/sindexed 20.18MB in  0s, 99.68MB/sindexed 20.32MB in  0s, 99.79MB/sindexed 20.45MB in  0s, 99.88MB/sindexed 20.58MB in  0s, 99.99MB/sindexed 20.71MB in  0s, 100.11MB/sindexed 20.84MB in  0s, 100.16MB/sindexed 20.97MB in  0s, 100.07MB/sindexed 21.10MB in  0s, 100.15MB/sindexed 21.23MB in  0s, 100.11MB/s                                                                              indexed 21.36MB in  0s, 99.87MB/sindexed 21.50MB in  0s, 99.64MB/sindexed 21.63MB in  0s, 99.45MB/sindexed 21.76MB in  0s, 99.41MB/sindexed 21.89MB in  0s, 99.51MB/sindexed 22.02MB in  0s, 99.61MB/sindexed 22.15MB in  0s, 99.71MB/sindexed 22.28MB in  0s, 99.82MB/sindexed 22.41MB in  0s, 99.92MB/sindexed 22.54MB in  0s, 99.92MB/sindexed 22.68MB in  0s, 99.84MB/sindexed 22.81MB in  0s, 99.69MB/sindexed 22.94MB in  0s, 99.74MB/sindexed 23.07MB in  0s, 99.84MB/sindexed 23.20MB in  0s, 99.95MB/sindexed 23.33MB in  0s, 100.06MB/sindexed 23.46MB in  0s, 100.18MB/sindexed 23.59MB in  0s, 100.30MB/sindexed 23.72MB in  0s, 100.42MB/sindexed 23.85MB in  0s, 100.53MB/sindexed 23.99MB in  0s, 100.66MB/sindexed 24.12MB in  0s, 100.66MB/sindexed 24.25MB in  0s, 100.64MB/sindexed 24.38MB in  0s, 100.58MB/sindexed 24.51MB in  0s, 100.61MB/sindexed 24.64MB in  0s, 100.51MB/sindexed 24.77MB in  0s, 100.39MB/sindexed 24.90MB in  0s, 100.30MB/sindexed 25.03MB in  0s, 100.11MB/sindexed 25.17MB in  0s, 100.03MB/sindexed 25.30MB in  0s, 100.05MB/s                                                                              indexed 25.43MB in  0s, 99.98MB/sindexed 25.56MB in  0s, 100.01MB/s                                                                              indexed 25.69MB in  0s, 99.97MB/sindexed 25.82MB in  0s, 99.77MB/sindexed 25.95MB in  0s, 99.74MB/sindexed 26.08MB in  0s, 99.54MB/sindexed 26.21MB in  0s, 99.45MB/sindexed 26.35MB in  0s, 99.34MB/sindexed 26.48MB in  0s, 94.98MB/sindexed 26.61MB in  0s, 94.91MB/sindexed 26.74MB in  0s, 94.94MB/sindexed 26.87MB in  0s, 94.94MB/sindexed 27.00MB in  0s, 94.89MB/sindexed 27.13MB in  0s, 94.94MB/sindexed 27.26MB in  0s, 94.98MB/sindexed 27.39MB in  0s, 94.88MB/sindexed 27.52MB in  0s, 94.96MB/sindexed 27.66MB in  0s, 94.88MB/sindexed 27.79MB in  0s, 94.85MB/sindexed 27.92MB in  0s, 94.75MB/sindexed 28.05MB in  0s, 94.70MB/sindexed 28.18MB in  0s, 94.57MB/sindexed 28.31MB in  0s, 94.45MB/sindexed 28.44MB in  0s, 94.37MB/sindexed 28.57MB in  0s, 94.42MB/sindexed 28.70MB in  0s, 94.45MB/sindexed 28.84MB in  0s, 94.49MB/sindexed 28.97MB in  0s, 94.52MB/sindexed 29.10MB in  0s, 94.53MB/sindexed 29.23MB in  0s, 94.42MB/sindexed 29.36MB in  0s, 94.32MB/sindexed 29.49MB in  0s, 94.25MB/sindexed 29.62MB in  0s, 94.28MB/sindexed 29.75MB in  0s, 94.33MB/sindexed 29.88MB in  0s, 94.38MB/sindexed 30.02MB in  0s, 94.43MB/sindexed 30.15MB in  0s, 94.41MB/sindexed 30.28MB in  0s, 94.45MB/sindexed 30.41MB in  0s, 94.50MB/sindexed 30.54MB in  0s, 94.55MB/sindexed 30.67MB in  0s, 94.63MB/sindexed 30.80MB in  0s, 94.69MB/sindexed 30.93MB in  0s, 94.63MB/sindexed 31.06MB in  0s, 94.54MB/sindexed 31.19MB in  0s, 94.39MB/sindexed 31.33MB in  0s, 94.38MB/sindexed 31.46MB in  0s, 94.43MB/sindexed 31.59MB in  0s, 94.48MB/sindexed 31.72MB in  0s, 94.55MB/sindexed 31.85MB in  0s, 94.58MB/sindexed 31.98MB in  0s, 94.64MB/sindexed 32.11MB in  0s, 94.64MB/sindexed 32.24MB in  0s, 94.68MB/sindexed 32.37MB in  0s, 94.21MB/sindexed 32.51MB in  0s, 94.08MB/sindexed 32.64MB in  0s, 93.98MB/sindexed 32.77MB in  0s, 93.97MB/sindexed 32.90MB in  0s, 93.98MB/sindexed 33.03MB in  0s, 94.01MB/sindexed 33.16MB in  0s, 94.04MB/sindexed 33.29MB in  0s, 94.10MB/sindexed 33.42MB in  0s, 94.16MB/sindexed 33.55MB in  0s, 94.22MB/sindexed 33.69MB in  0s, 94.16MB/sindexed 33.82MB in  0s, 93.96MB/sindexed 33.95MB in  0s, 93.77MB/sindexed 34.08MB in  0s, 93.80MB/sindexed 34.21MB in  0s, 93.83MB/sindexed 34.34MB in  0s, 93.66MB/sindexed 34.47MB in  0s, 93.60MB/sindexed 34.60MB in  0s, 93.66MB/sindexed 34.73MB in  0s, 93.70MB/sindexed 34.86MB in  0s, 93.74MB/sindexed 35.00MB in  0s, 93.76MB/sindexed 35.13MB in  0s, 93.69MB/sindexed 35.26MB in  0s, 93.56MB/sindexed 35.39MB in  0s, 93.52MB/sindexed 35.52MB in  0s, 93.51MB/sindexed 35.65MB in  0s, 93.56MB/sindexed 35.78MB in  0s, 93.60MB/sindexed 35.91MB in  0s, 93.56MB/sindexed 36.04MB in  0s, 93.58MB/sindexed 36.18MB in  0s, 93.63MB/sindexed 36.31MB in  0s, 93.67MB/sindexed 36.44MB in  0s, 93.57MB/sindexed 36.57MB in  0s, 93.51MB/sindexed 36.70MB in  0s, 93.55MB/sindexed 36.83MB in  0s, 93.58MB/sindexed 36.96MB in  0s, 93.66MB/sindexed 37.09MB in  0s, 93.60MB/sindexed 37.22MB in  0s, 93.63MB/sindexed 37.36MB in  0s, 93.68MB/sindexed 37.49MB in  0s, 93.72MB/sindexed 37.62MB in  0s, 93.77MB/sindexed 37.75MB in  0s, 93.83MB/sindexed 37.88MB in  0s, 93.87MB/sindexed 38.01MB in  0s, 93.90MB/sindexed 38.14MB in  0s, 93.95MB/sindexed 38.27MB in  0s, 93.88MB/sindexed 38.40MB in  0s, 93.81MB/sindexed 38.53MB in  0s, 93.76MB/sindexed 38.67MB in  0s, 93.77MB/sindexed 38.80MB in  0s, 93.80MB/sindexed 38.93MB in  0s, 93.90MB/sindexed 39.06MB in  0s, 93.97MB/sindexed 39.19MB in  0s, 93.95MB/sindexed 39.32MB in  0s, 93.87MB/sindexed 39.45MB in  0s, 93.76MB/sindexed 39.58MB in  0s, 93.69MB/sindexed 39.71MB in  0s, 93.68MB/sindexed 39.85MB in  0s, 93.68MB/sindexed 39.98MB in  0s, 93.56MB/sindexed 40.11MB in  0s, 93.44MB/sindexed 40.24MB in  0s, 93.47MB/sindexed 40.37MB in  0s, 93.49MB/sindexed 40.50MB in  0s, 93.53MB/sindexed 40.63MB in  0s, 93.52MB/sindexed 40.76MB in  0s, 93.55MB/sindexed 40.89MB in  0s, 93.59MB/sindexed 41.03MB in  0s, 93.63MB/sindexed 41.16MB in  0s, 93.66MB/sindexed 41.29MB in  0s, 93.63MB/sindexed 41.42MB in  0s, 93.63MB/sindexed 41.55MB in  0s, 93.59MB/sindexed 41.68MB in  0s, 93.61MB/sindexed 41.81MB in  0s, 93.62MB/sindexed 41.94MB in  0s, 93.36MB/sindexed 42.07MB in  0s, 93.29MB/sindexed 42.20MB in  0s, 93.27MB/sindexed 42.34MB in  0s, 93.31MB/sindexed 42.47MB in  0s, 93.35MB/sindexed 42.60MB in  0s, 93.40MB/sindexed 42.73MB in  0s, 93.45MB/sindexed 42.86MB in  0s, 93.45MB/sindexed 42.99MB in  0s, 93.47MB/sindexed 43.12MB in  0s, 93.52MB/sindexed 43.25MB in  0s, 93.48MB/sindexed 43.38MB in  0s, 93.38MB/sindexed 43.52MB in  0s, 93.32MB/sindexed 43.65MB in  0s, 93.27MB/sindexed 43.78MB in  0s, 93.28MB/sindexed 43.91MB in  0s, 93.32MB/sindexed 44.04MB in  0s, 93.36MB/sindexed 44.17MB in  0s, 93.41MB/sindexed 44.30MB in  0s, 93.45MB/sindexed 44.43MB in  0s, 93.49MB/sindexed 44.56MB in  0s, 93.52MB/sindexed 44.70MB in  0s, 91.04MB/sindexed 44.83MB in  0s, 91.07MB/sindexed 44.96MB in  0s, 91.03MB/sindexed 45.09MB in  0s, 91.07MB/sindexed 45.22MB in  0s, 91.12MB/sindexed 45.35MB in  0s, 91.17MB/sindexed 45.48MB in  0s, 91.18MB/sindexed 45.61MB in  1s, 91.22MB/sindexed 45.74MB in  1s, 91.27MB/sindexed 45.87MB in  1s, 91.32MB/sindexed 46.01MB in  1s, 91.35MB/sindexed 46.14MB in  1s, 91.39MB/sindexed 46.27MB in  1s, 91.30MB/sindexed 46.40MB in  1s, 91.23MB/sindexed 46.53MB in  1s, 91.14MB/sindexed 46.66MB in  1s, 91.06MB/sindexed 46.79MB in  1s, 91.01MB/sindexed 46.92MB in  1s, 90.92MB/sindexed 47.05MB in  1s, 90.84MB/sindexed 47.19MB in  1s, 90.82MB/sindexed 47.32MB in  1s, 90.76MB/sindexed 47.45MB in  1s, 90.76MB/sindexed 47.58MB in  1s, 90.78MB/sindexed 47.71MB in  1s, 90.81MB/sindexed 47.84MB in  1s, 90.85MB/sindexed 47.97MB in  1s, 90.85MB/sindexed 48.10MB in  1s, 90.71MB/sindexed 48.23MB in  1s, 90.68MB/sindexed 48.37MB in  1s, 90.64MB/sindexed 48.50MB in  1s, 90.63MB/sindexed 48.63MB in  1s, 90.66MB/sindexed 48.76MB in  1s, 90.70MB/sindexed 48.89MB in  1s, 90.69MB/sindexed 49.02MB in  1s, 90.73MB/sindexed 49.15MB in  1s, 90.78MB/sindexed 49.28MB in  1s, 90.83MB/sindexed 49.41MB in  1s, 90.88MB/sindexed 49.54MB in  1s, 90.92MB/sindexed 49.68MB in  1s, 90.97MB/sindexed 49.81MB in  1s, 91.01MB/sindexed 49.94MB in  1s, 91.05MB/sindexed 50.07MB in  1s, 91.04MB/sindexed 50.20MB in  1s, 91.04MB/sindexed 50.33MB in  1s, 91.07MB/sindexed 50.46MB in  1s, 91.10MB/sindexed 50.59MB in  1s, 91.14MB/sindexed 50.72MB in  1s, 91.18MB/sindexed 50.86MB in  1s, 91.16MB/sindexed 50.99MB in  1s, 87.13MB/sindexed 51.12MB in  1s, 87.15MB/sindexed 51.25MB in  1s, 87.19MB/sindexed 51.38MB in  1s, 87.20MB/sindexed 51.51MB in  1s, 87.24MB/sindexed 51.64MB in  1s, 87.24MB/sindexed 51.77MB in  1s, 87.16MB/sindexed 51.90MB in  1s, 87.15MB/sindexed 52.04MB in  1s, 87.18MB/sindexed 52.17MB in  1s, 87.21MB/sindexed 52.30MB in  1s, 87.14MB/sindexed 52.43MB in  1s, 87.15MB/sindexed 52.56MB in  1s, 87.20MB/sindexed 52.69MB in  1s, 87.25MB/sindexed 52.82MB in  1s, 87.29MB/sindexed 52.95MB in  1s, 87.34MB/sindexed 53.08MB in  1s, 87.39MB/sindexed 53.21MB in  1s, 87.38MB/sindexed 53.35MB in  1s, 87.42MB/sindexed 53.48MB in  1s, 87.48MB/sindexed 53.61MB in  1s, 87.41MB/sindexed 53.74MB in  1s, 87.44MB/sindexed 53.87MB in  1s, 87.40MB/sindexed 54.00MB in  1s, 87.36MB/sindexed 54.13MB in  1s, 87.36MB/sindexed 54.26MB in  1s, 87.40MB/sindexed 54.39MB in  1s, 87.45MB/sindexed 54.53MB in  1s, 87.48MB/sindexed 54.66MB in  1s, 87.53MB/sindexed 54.79MB in  1s, 87.50MB/sindexed 54.92MB in  1s, 87.46MB/sindexed 55.05MB in  1s, 87.45MB/sindexed 55.18MB in  1s, 87.42MB/sindexed 55.31MB in  1s, 87.43MB/sindexed 55.44MB in  1s, 87.46MB/sindexed 55.57MB in  1s, 87.51MB/sindexed 55.71MB in  1s, 87.55MB/sindexed 55.84MB in  1s, 87.60MB/sindexed 55.97MB in  1s, 87.62MB/sindexed 56.10MB in  1s, 87.64MB/sindexed 56.23MB in  1s, 87.69MB/sindexed 56.36MB in  1s, 87.68MB/sindexed 56.49MB in  1s, 87.66MB/sindexed 56.62MB in  1s, 87.60MB/sindexed 56.75MB in  1s, 87.57MB/sindexed 56.88MB in  1s, 87.43MB/sindexed 57.02MB in  1s, 87.37MB/sindexed 57.15MB in  1s, 87.29MB/sindexed 57.28MB in  1s, 87.26MB/sindexed 57.41MB in  1s, 87.24MB/sindexed 57.54MB in  1s, 87.15MB/sindexed 57.67MB in  1s, 87.04MB/sindexed 57.80MB in  1s, 86.98MB/sindexed 57.93MB in  1s, 86.86MB/sindexed 58.06MB in  1s, 86.82MB/sindexed 58.20MB in  1s, 86.82MB/sindexed 58.33MB in  1s, 86.85MB/sindexed 58.46MB in  1s, 86.85MB/sindexed 58.59MB in  1s, 86.89MB/sindexed 58.72MB in  1s, 86.88MB/sindexed 58.85MB in  1s, 86.90MB/sindexed 58.98MB in  1s, 86.82MB/sindexed 59.11MB in  1s, 86.74MB/sindexed 59.24MB in  1s, 86.69MB/sindexed 59.38MB in  1s, 86.69MB/sindexed 59.51MB in  1s, 86.70MB/sindexed 59.64MB in  1s, 86.70MB/sindexed 59.77MB in  1s, 86.64MB/sindexed 59.90MB in  1s, 86.60MB/sindexed 60.03MB in  1s, 86.61MB/sindexed 60.16MB in  1s, 86.61MB/sindexed 60.29MB in  1s, 86.60MB/sindexed 60.42MB in  1s, 86.63MB/sindexed 60.55MB in  1s, 86.61MB/sindexed 60.69MB in  1s, 86.57MB/sindexed 60.82MB in  1s, 86.58MB/sindexed 60.95MB in  1s, 86.60MB/sindexed 61.08MB in  1s, 86.56MB/sindexed 61.21MB in  1s, 86.58MB/sindexed 61.34MB in  1s, 86.59MB/sindexed 61.47MB in  1s, 86.62MB/sindexed 61.60MB in  1s, 86.65MB/sindexed 61.73MB in  1s, 86.56MB/sindexed 61.87MB in  1s, 86.56MB/sindexed 62.00MB in  1s, 86.59MB/sindexed 62.13MB in  1s, 86.63MB/sindexed 62.26MB in  1s, 86.66MB/sindexed 62.39MB in  1s, 86.63MB/sindexed 62.52MB in  1s, 86.64MB/sindexed 62.65MB in  1s, 86.66MB/sindexed 62.78MB in  1s, 86.69MB/sindexed 62.91MB in  1s, 86.71MB/sindexed 63.05MB in  1s, 86.64MB/sindexed 63.18MB in  1s, 86.60MB/sindexed 63.31MB in  1s, 86.56MB/sindexed 63.44MB in  1s, 86.53MB/sindexed 63.57MB in  1s, 86.55MB/sindexed 63.70MB in  1s, 86.58MB/sindexed 63.83MB in  1s, 86.61MB/sindexed 63.96MB in  1s, 86.65MB/sindexed 64.09MB in  1s, 86.67MB/sindexed 64.22MB in  1s, 86.70MB/sindexed 64.36MB in  1s, 86.73MB/sindexed 64.49MB in  1s, 86.67MB/sindexed 64.62MB in  1s, 86.63MB/sindexed 64.75MB in  1s, 86.66MB/sindexed 64.88MB in  1s, 86.69MB/sindexed 65.01MB in  1s, 86.66MB/sindexed 65.14MB in  1s, 86.63MB/sindexed 65.27MB in  1s, 86.65MB/sindexed 65.35MB in  1s, 86.62MB/s                                                                              indexed 2.15GB in  1s, 2.15GB/s                                                                              
#> Table namq_10_gdp cached at C:\Users\morit\AppData\Local\Temp\Rtmpo1GImx/eurostat/e76122ae8ad09435f09b9fba897779aa.rds
#> indexed 0B in  0s, 0B/sindexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table env_ac_aigg_q cached at C:\Users\morit\AppData\Local\Temp\Rtmpo1GImx/eurostat/f9f0df28d832dc70fee1519c934adc15.rds
#> indexed 0B in  0s, 0B/sindexed 6.42MB in  0s, 16.27MB/s                                                                              indexed 6.55MB in  1s, 8.19MB/sindexed 6.68MB in  1s, 8.32MB/sindexed 6.82MB in  1s, 8.42MB/sindexed 6.95MB in  1s, 8.54MB/sindexed 7.08MB in  1s, 8.67MB/sindexed 7.21MB in  1s, 8.79MB/sindexed 7.34MB in  1s, 8.92MB/sindexed 7.47MB in  1s, 9.04MB/sindexed 7.60MB in  1s, 9.17MB/sindexed 7.73MB in  1s, 9.29MB/sindexed 7.86MB in  1s, 9.41MB/sindexed 8.00MB in  1s, 9.53MB/sindexed 8.13MB in  1s, 9.65MB/sindexed 8.26MB in  1s, 9.77MB/sindexed 8.39MB in  1s, 9.89MB/sindexed 8.52MB in  1s, 10.01MB/sindexed 8.65MB in  1s, 10.13MB/sindexed 8.78MB in  1s, 10.24MB/sindexed 8.91MB in  1s, 10.36MB/sindexed 9.04MB in  1s, 10.47MB/sindexed 9.17MB in  1s, 10.59MB/sindexed 9.31MB in  1s, 10.69MB/sindexed 9.44MB in  1s, 10.79MB/sindexed 9.57MB in  1s, 10.90MB/sindexed 9.70MB in  1s, 11.01MB/sindexed 9.83MB in  1s, 11.12MB/sindexed 9.96MB in  1s, 11.22MB/sindexed 10.09MB in  1s, 11.31MB/sindexed 10.22MB in  1s, 11.42MB/sindexed 10.35MB in  1s, 11.52MB/sindexed 10.49MB in  1s, 11.60MB/sindexed 10.62MB in  1s, 11.68MB/sindexed 10.75MB in  1s, 11.77MB/sindexed 10.88MB in  1s, 11.85MB/sindexed 11.01MB in  1s, 11.81MB/sindexed 11.14MB in  1s, 11.89MB/sindexed 11.27MB in  1s, 11.98MB/sindexed 11.40MB in  1s, 12.07MB/sindexed 11.53MB in  1s, 12.16MB/sindexed 11.67MB in  1s, 12.26MB/sindexed 11.80MB in  1s, 12.17MB/sindexed 11.93MB in  1s, 12.26MB/sindexed 12.06MB in  1s, 12.35MB/sindexed 12.19MB in  1s, 12.44MB/sindexed 12.32MB in  1s, 12.52MB/sindexed 12.45MB in  1s, 12.58MB/sindexed 12.58MB in  1s, 12.67MB/sindexed 12.71MB in  1s, 12.76MB/s                                                                              indexed 12.84MB in  2s, 7.82MB/sindexed 12.98MB in  2s, 7.87MB/sindexed 13.11MB in  2s, 7.93MB/sindexed 13.24MB in  2s, 7.78MB/sindexed 13.37MB in  2s, 7.80MB/sindexed 13.50MB in  2s, 7.84MB/sindexed 13.63MB in  2s, 7.88MB/sindexed 13.76MB in  2s, 7.87MB/sindexed 13.89MB in  2s, 7.93MB/sindexed 14.02MB in  2s, 7.98MB/sindexed 14.16MB in  2s, 8.03MB/sindexed 14.29MB in  2s, 8.08MB/sindexed 14.42MB in  2s, 8.14MB/sindexed 14.55MB in  2s, 8.20MB/sindexed 14.68MB in  2s, 8.25MB/sindexed 14.81MB in  2s, 8.31MB/sindexed 14.94MB in  2s, 8.37MB/sindexed 14.97MB in  2s, 8.35MB/s                                                                              indexed 2.15GB in  2s, 2.15GB/s                                                                              
#> Table ei_lmlc_q cached at C:\Users\morit\AppData\Local\Temp\Rtmpo1GImx/eurostat/258fb00990b016df7382eafb05dc1ba3.rds
#> indexed 0B in  0s, 0B/sindexed 25.03MB in  0s, 124.50MB/sindexed 25.17MB in  0s, 124.51MB/sindexed 25.30MB in  0s, 124.50MB/sindexed 25.43MB in  0s, 124.38MB/sindexed 25.56MB in  0s, 124.23MB/sindexed 25.69MB in  0s, 124.33MB/sindexed 25.82MB in  0s, 124.48MB/sindexed 25.95MB in  0s, 124.65MB/sindexed 26.08MB in  0s, 124.81MB/sindexed 26.21MB in  0s, 124.95MB/sindexed 26.35MB in  0s, 125.13MB/sindexed 26.48MB in  0s, 125.31MB/sindexed 26.61MB in  0s, 125.34MB/sindexed 26.74MB in  0s, 125.47MB/sindexed 26.87MB in  0s, 125.63MB/sindexed 27.00MB in  0s, 125.79MB/sindexed 27.13MB in  0s, 125.95MB/sindexed 27.26MB in  0s, 126.09MB/sindexed 27.39MB in  0s, 126.25MB/sindexed 27.52MB in  0s, 126.40MB/sindexed 27.66MB in  0s, 126.46MB/sindexed 27.79MB in  0s, 126.46MB/sindexed 27.92MB in  0s, 126.49MB/sindexed 28.05MB in  0s, 126.56MB/sindexed 28.18MB in  0s, 126.61MB/sindexed 28.31MB in  0s, 126.64MB/sindexed 28.44MB in  0s, 126.65MB/sindexed 28.57MB in  0s, 126.74MB/sindexed 28.70MB in  0s, 126.71MB/sindexed 28.84MB in  0s, 126.70MB/sindexed 28.97MB in  0s, 126.69MB/sindexed 29.10MB in  0s, 126.64MB/sindexed 29.23MB in  0s, 126.62MB/sindexed 29.36MB in  0s, 126.72MB/sindexed 29.49MB in  0s, 126.79MB/sindexed 29.62MB in  0s, 126.73MB/sindexed 29.75MB in  0s, 126.82MB/sindexed 29.88MB in  0s, 126.81MB/sindexed 30.02MB in  0s, 126.80MB/sindexed 30.15MB in  0s, 126.66MB/sindexed 30.28MB in  0s, 126.52MB/sindexed 30.41MB in  0s, 126.62MB/sindexed 30.54MB in  0s, 126.61MB/sindexed 30.67MB in  0s, 126.65MB/sindexed 30.80MB in  0s, 126.64MB/sindexed 30.93MB in  0s, 126.66MB/sindexed 31.06MB in  0s, 126.68MB/sindexed 31.19MB in  0s, 126.69MB/sindexed 31.33MB in  0s, 126.79MB/sindexed 31.46MB in  0s, 126.83MB/sindexed 31.59MB in  0s, 126.84MB/sindexed 31.72MB in  0s, 126.93MB/sindexed 31.85MB in  0s, 127.04MB/sindexed 31.98MB in  0s, 127.14MB/sindexed 32.11MB in  0s, 127.16MB/sindexed 32.24MB in  0s, 127.18MB/sindexed 32.37MB in  0s, 127.10MB/sindexed 32.51MB in  0s, 127.05MB/sindexed 32.64MB in  0s, 126.91MB/sindexed 32.77MB in  0s, 127.00MB/sindexed 32.90MB in  0s, 127.09MB/sindexed 33.03MB in  0s, 127.19MB/sindexed 33.16MB in  0s, 127.20MB/sindexed 33.29MB in  0s, 127.34MB/sindexed 33.42MB in  0s, 127.23MB/sindexed 33.55MB in  0s, 127.26MB/sindexed 33.69MB in  0s, 127.22MB/sindexed 33.82MB in  0s, 127.12MB/sindexed 33.95MB in  0s, 127.15MB/sindexed 34.08MB in  0s, 127.01MB/sindexed 34.21MB in  0s, 127.13MB/sindexed 34.34MB in  0s, 127.11MB/sindexed 34.47MB in  0s, 127.00MB/sindexed 34.60MB in  0s, 126.86MB/sindexed 34.73MB in  0s, 126.84MB/sindexed 34.86MB in  0s, 126.94MB/sindexed 35.00MB in  0s, 126.94MB/sindexed 35.13MB in  0s, 127.06MB/sindexed 35.26MB in  0s, 127.19MB/sindexed 35.39MB in  0s, 127.30MB/sindexed 35.52MB in  0s, 127.43MB/sindexed 35.65MB in  0s, 127.54MB/sindexed 35.78MB in  0s, 127.67MB/sindexed 35.91MB in  0s, 127.64MB/sindexed 36.04MB in  0s, 127.76MB/sindexed 36.18MB in  0s, 127.87MB/sindexed 36.31MB in  0s, 127.99MB/sindexed 36.44MB in  0s, 128.12MB/sindexed 36.57MB in  0s, 128.24MB/sindexed 36.70MB in  0s, 128.37MB/sindexed 36.83MB in  0s, 128.42MB/sindexed 36.96MB in  0s, 128.49MB/sindexed 37.09MB in  0s, 128.57MB/sindexed 37.22MB in  0s, 128.65MB/sindexed 37.36MB in  0s, 128.76MB/sindexed 37.49MB in  0s, 128.88MB/sindexed 37.62MB in  0s, 128.78MB/sindexed 37.75MB in  0s, 128.82MB/sindexed 37.88MB in  0s, 128.86MB/sindexed 38.01MB in  0s, 128.96MB/sindexed 38.14MB in  0s, 129.08MB/sindexed 38.27MB in  0s, 129.19MB/sindexed 38.40MB in  0s, 129.20MB/sindexed 38.53MB in  0s, 129.30MB/sindexed 38.67MB in  0s, 129.39MB/sindexed 38.80MB in  0s, 129.41MB/sindexed 38.93MB in  0s, 129.30MB/sindexed 39.06MB in  0s, 129.22MB/sindexed 39.19MB in  0s, 129.30MB/sindexed 39.32MB in  0s, 129.29MB/sindexed 39.45MB in  0s, 129.39MB/sindexed 39.58MB in  0s, 129.49MB/sindexed 39.71MB in  0s, 129.60MB/sindexed 39.85MB in  0s, 129.70MB/sindexed 39.98MB in  0s, 129.81MB/sindexed 40.11MB in  0s, 129.92MB/sindexed 40.24MB in  0s, 130.03MB/sindexed 40.37MB in  0s, 130.13MB/sindexed 40.50MB in  0s, 130.25MB/sindexed 40.63MB in  0s, 130.26MB/sindexed 40.76MB in  0s, 130.35MB/sindexed 40.89MB in  0s, 130.44MB/sindexed 41.03MB in  0s, 130.48MB/sindexed 41.16MB in  0s, 130.56MB/sindexed 41.29MB in  0s, 130.66MB/sindexed 41.42MB in  0s, 130.76MB/sindexed 41.55MB in  0s, 130.67MB/sindexed 41.68MB in  0s, 130.73MB/sindexed 41.81MB in  0s, 130.82MB/sindexed 41.94MB in  0s, 130.92MB/sindexed 42.07MB in  0s, 131.01MB/sindexed 42.20MB in  0s, 131.11MB/sindexed 42.34MB in  0s, 131.20MB/sindexed 42.47MB in  0s, 131.30MB/sindexed 42.60MB in  0s, 131.39MB/sindexed 42.73MB in  0s, 131.40MB/sindexed 42.86MB in  0s, 131.50MB/sindexed 42.99MB in  0s, 131.61MB/sindexed 43.12MB in  0s, 131.71MB/sindexed 43.25MB in  0s, 131.75MB/sindexed 43.38MB in  0s, 131.66MB/sindexed 43.52MB in  0s, 131.52MB/sindexed 43.65MB in  0s, 131.38MB/sindexed 43.78MB in  0s, 131.31MB/sindexed 43.91MB in  0s, 131.35MB/sindexed 44.04MB in  0s, 131.33MB/sindexed 44.17MB in  0s, 131.28MB/sindexed 44.30MB in  0s, 131.13MB/sindexed 44.43MB in  0s, 131.00MB/sindexed 44.56MB in  0s, 131.02MB/sindexed 44.70MB in  0s, 130.99MB/sindexed 44.83MB in  0s, 131.04MB/sindexed 44.96MB in  0s, 131.01MB/sindexed 45.09MB in  0s, 131.06MB/sindexed 45.22MB in  0s, 130.97MB/sindexed 45.35MB in  0s, 130.98MB/sindexed 45.48MB in  0s, 131.01MB/sindexed 45.61MB in  0s, 130.93MB/sindexed 45.74MB in  0s, 130.96MB/sindexed 45.87MB in  0s, 131.00MB/sindexed 46.01MB in  0s, 131.04MB/sindexed 46.14MB in  0s, 131.11MB/sindexed 46.27MB in  0s, 131.19MB/sindexed 46.40MB in  0s, 131.28MB/sindexed 46.53MB in  0s, 131.36MB/sindexed 46.66MB in  0s, 131.44MB/sindexed 46.79MB in  0s, 131.46MB/sindexed 46.92MB in  0s, 131.53MB/sindexed 47.05MB in  0s, 131.60MB/sindexed 47.19MB in  0s, 125.45MB/sindexed 47.32MB in  0s, 125.50MB/sindexed 47.45MB in  0s, 125.59MB/sindexed 47.58MB in  0s, 125.66MB/sindexed 47.71MB in  0s, 125.74MB/sindexed 47.82MB in  0s, 125.76MB/s                                                                              indexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table namq_10_a10 cached at C:\Users\morit\AppData\Local\Temp\Rtmpo1GImx/eurostat/37ff1e8a43300ebf543f2324f2b54596.rds
#> indexed 0B in  0s, 0B/sindexed 2.15GB in  0s, 2.15GB/s                                                                              
#> Table sts_cobp_q cached at C:\Users\morit\AppData\Local\Temp\Rtmpo1GImx/eurostat/9e930697659f7bcb8ef80083c6d3b693.rds
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

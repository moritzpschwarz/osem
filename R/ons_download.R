install.packages("onsr")
library(onsr)

# check available datasets
datasets <- ons_datasets()
# can extract id from there, e.g. "gdp-to-four-decimal-places"
data <- ons_get(id = "gdp-to-four-decimal-places")

# Note: ONS' API is still in its beta stage
ons_ids()
length(ons_ids())
# only 60 data sets at the moment but online see 337
# https://api.beta.ons.gov.uk/v1/datasets?limit=1000

require(easyr)
require(glue)
require(magrittr)
require(progress)
require(reshape2)
require(glmnet) # for LASSO.
require(lubridate)
begin()

# read data.
runfolder('scripts')

dt %>%
  correlatedfeatures_address(
    target = 'total_claim_amount'
  ) %>%
  fitmodel(
    ignorecols = c('vehicle_claim', 'property_claim', 'injury_claim')
  ) %>%
  summary()

dt %>%
  fitmodel(
    target = 'total_claim_amount',
    ignorecols = c('vehicle_claim', 'property_claim', 'injury_claim')
  ) %>%
  summary()



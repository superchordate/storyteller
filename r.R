require(easyr)
require(glue)
require(magrittr)
require(progress)
require(reshape2)
require(glmnet) # for LASSO.
require(nnet) # for multinomimal regression
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

plot_correlation(
  dt,  
  c('incident_date_year', 'age')
)


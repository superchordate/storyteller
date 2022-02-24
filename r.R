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
    target = 'policy_annual_premium',
    verbose = FALSE
  ) %>%
  fitmodel(target = 'policy_annual_premium') %>%
  summary()

dt %>%
  fitmodel(target = 'policy_annual_premium') %>%
  summary()



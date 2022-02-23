require(easyr)
require(glue)
require(magrittr)
require(progress)
require(reshape2)
begin()

# read data.
runfolder('scripts')

# run steps:
dt %<>% 
  clean(run_autotype = FALSE) %>%
  dropnoisecols() %>%
  dropoutliers() %>%
  correlatedfeatures_find() #%>%
  #correlatedfeatures_address()

summary(dt)

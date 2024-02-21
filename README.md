R functions (package TBD) to quicky find stories in data using data mining models. 

These steps are working (ish):

* clean: Clean the data. Right now this just means replacing Inf with NA. Will expand this over time.
* dropnoisecols: Remove columns that cannot easily be analyzed or do not contribute information. 
* groupother: Group small groups.
* dropoutliers: Remove outliers.
* find_correlated_features: compare features to identify those that are correlated.
* Method to easily plot correlated features.
* Pick a target and identify which features drive it and the strength and direction of effects.

# About Me

I'm an independent contractor helping companies build custom cloud apps and leverage data science, visual analytics, and AI. I offer low introductory rates, free consultation and estimates, and no minimums, so contact me today and let's chat about how I can help!

https://www.bryce-chamberlain.com/

# Example

```r

# packages required for storyteller.
require(glue)
require(magrittr)
require(progress)
require(reshape2)
require(ggplot2)

# easyr project setup.
require(easyr)
begin()

# read in some data. google or kaggle to find a dataset you are interested in. 
dt = read.any('myfile.ext')

# run the steps (functions in fun/ folder).
dt %<>% 
  clean(run_autotype = FALSE) %>% # read.any already runs autotype by default.
  dropnoisecols() %>%
  groupother() %>%
  dropoutliers() %>%
  correlatedfeatures_find()

# summarize patterns found in your data.
summary(dt)

# plot correlation between two variables.
plot_correlation(
  dt,  
  c('incident_date_year', 'age')
)

# fit a model against a target and identify key drivers.
dt %>%
  correlatedfeatures_address(
    target = 'total_claim_amount'
  ) %>%
  fitmodel(
    ignorecols = c('vehicle_claim', 'property_claim', 'injury_claim')
  ) %>%
  summary()

```

You can also visit https://www.kaggle.com/code/brycechamberlain/data-explore-automl/ for a notebook example. 


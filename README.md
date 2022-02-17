R functions (package TBD) to quicky find stories in data using data mining models. 

These steps are working (ish):

* clean: Clean the data. Right now this just means replacing Inf with NA. Will expand this over time.
* dropnoisecols: Remove columns that cannot easily be analyzed or do not contribute information. 
* groupother: Group small groups.
* dropoutliers: Remove outliers.
* find_correlated_features: compare features to identify those that are correlated.

These are still outsanding:

* Method to easily plot correlated features.
* Pick a target and identify which features drive it and the strength and direction of effects.

Example:

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
  find_correlated_features()

summary(dt)
```

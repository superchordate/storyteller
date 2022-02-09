R functions (package TBD) to quicky find stories in data using data mining models. 

Example:

```r
dt %<>% 
  clean(run_autotype = FALSE) %>%
  dropnoisecols() %>%
  dropoutliers() %>%
  find_correlated_features()

summary(dt)
```

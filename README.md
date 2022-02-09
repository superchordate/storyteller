R functions (package TBD) to quicky find stories in data using data mining models. 

Example:

```r
dt %<>% 
  clean(run_autotype = FALSE) %>%
  dropnoisecols() %>%
  dropoutliers()

summary(dt)
```

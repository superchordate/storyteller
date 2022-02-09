require(easyr)
require(glue)
require(magrittr)
begin()

# https://www.kaggle.com/mur418/2020-fantasy-football/data?select=rb_stats_and_projections.csv
if(TRUE || !file.exists('out/rb_stats_and_projections.csv.RDS')){ 
  
  dt = bind_rows(lapply(
    list.files('in', pattern = 'projections.csv$', full.names = TRUE),
    function(file) read.any(file, folder = 'in', all_chars = TRUE) %>% select(-`NA`)
  )) %>%
    atype()
  
  saveRDS(dt, 'out/rb_stats_and_projections.csv.RDS')
  
}

dt = readRDS('out/rb_stats_and_projections.csv.RDS') %>%
  filter(`2020 FPTS` > 0)

dt[dt == "No outlook available."] <- NA

# pre-check
dt %<>% 
  clean(run_autotype = FALSE) %>%
  dropnoisecols() %>%
  dropoutliers() %>%
  find_correlated_features()

summary(dt)

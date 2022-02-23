require(easyr)
require(glue)
require(magrittr)
require(progress)
require(reshape2)
begin()

# https://www.kaggle.com/mur418/2020-fantasy-football/data?select=rb_stats_and_projections.csv
if(FALSE || !file.exists('out/stats_and_projections.RDS')){ 
  
  dofiles = list.files('in', pattern = 'projections.csv$', full.names = TRUE)
  dofiles = dofiles[!grepl('wr_stats', dofiles)] # this file uses different column names. drop for now.
  
  dt = bind_rows(lapply(
    dofiles,
    function(file) read.any(
        file, 
        folder = 'in', 
        all_chars = TRUE,
        first_column_name = 'TEAM NAME',
        field_name_map = c(
          'Player OUTLOOK' = '2020 OUTLOOK',
          'PLAYER OUTLOOK' = '2020 OUTLOOK'
        )
      ) %>% 
      select(-`NA`)
  )) %>%
    atype()
  
  saveRDS(dt, 'out/stats_and_projections.RDS')
  
}

dt = readRDS('out/stats_and_projections.RDS') %>%
  filter(`2020 FPTS` > 0)

# manual data fixes for this specific dataset.

  # replace nas with 0s in numeric columns.
  for(col in names(dt)) if(is.numeric(dt[[col]])) dt[[col]][ is.na(dt[[col]]) ] <- 0

  # NA teams = no team.
  dt$`TEAM NAME`[ is.na(dt$`TEAM NAME`) ] <- 'noteam'
  
  # No outlook available = NA
  dt[dt == "No outlook available."] <- NA

# pre-check
dt %<>% 
  clean(run_autotype = FALSE) %>%
  dropnoisecols() %>%
  dropoutliers() %>%
  correlatedfeatures_find() #%>%
  #correlatedfeatures_address()

summary(dt)

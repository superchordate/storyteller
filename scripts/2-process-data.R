if(!cache.ok(2)){
    
    # run initial steps:
    dt %<>% 
        clean(run_autotype = FALSE) %>%
        dropoutliers() %>%
        convert_date_features() %>%
        dropnoisecols() %>%
        correlatedfeatures_find(corr_cutoff = 0.6)

    save.cache(dt)

}
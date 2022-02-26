if(!cache.ok(2)){
    
    # run initial steps:
    dt %<>% 
        clean(run_autotype = FALSE) %>%
        dropoutliers() %>%
        convert_date_features() %>%
        dropnoisecols() %>%
        correlatedfeatures_find()

    save.cache(dt)

}
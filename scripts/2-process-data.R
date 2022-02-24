if(!cache.ok(2)){
    
    # run initial steps:
    dt %<>% 
        clean(run_autotype = FALSE) %>%
        dropnoisecols() %>%
        convert_date_features() %>%
        dropoutliers() %>%
        correlatedfeatures_find()

    save.cache(dt)

}
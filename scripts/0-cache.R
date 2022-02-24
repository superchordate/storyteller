cache.init(at.path = 'cache', caches = list( 
    list(
        name = 'read-data',
        depends.on = c('scripts/1-read-data.R', 'in/')
    ),
    list(
        name = 'process-data',
        depends.on = c(
            'scripts/2-process-data.R',
            'fun/convertdatefeatures.R',
            'fun/correlatedfeatures-find.R',
            'fun/dropnoisecols.R',
            'fun/dropoutliers.R',
            'fun/groupother.R'
        )
    )
))


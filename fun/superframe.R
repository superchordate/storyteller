# class and constructor.
as.superframe = function(x, run_autotype = run_autotype){ 
    
    if(class(x)[1] == 'superframe') return(x)

    # validation. 
    badnames = grep('=', names(x), value = TRUE)
    if(length(badnames) > 0) stop(glue('Please remove special characters from column names: [{cc(badnames, sep = ", ")}]'))

    x = data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    if(run_autotype) x = easyr::atype(x)
    x = easyr::char2fac(x, na_level = NA)

    return(structure(
        list(
            data = x,
            raw_dim = dim(x),
            initrows = 1:nrow(x),
            classes = sapply(x, function(i) class(i)[1]),
            dropped_rows = data.frame(),
            dropped_cols = list(),
            grouped_cols = list(),
            correlated_features = list(),
            text_cols = c(),
            othername = 'small_groups',
            target = NULL
        ), 
        class = "superframe"
    ))
}
superframe = as.superframe

# summary function.
summary.superframe = function(x) list(
    raw_dim = x$raw_dim,
    dim = dim(x$data),
    #classes = x$classes,
    text_cols = x$text_cols,
    `dropped columns` = as.character(sapply(x$dropped_cols, function(i){
        glue::glue('{i$col} ({i$reason} {i$shortinfo})')
    })),
    `dropped` = glue::glue('[{nrow(x$dropped_rows)}] rows due to [{cc(unique(x$dropped_rows$reason, sep = ", "))}]'),
    `correlated features` = if(length(x$correlated_features) == 0){
        'no-correlated-features'
    } else {
        data.frame(
            col1 = sapply(x$correlated_features, function(x) x$cols[1]),
            col2 = sapply(x$correlated_features, function(x) x$cols[2]),
            test = sapply(x$correlated_features, function(x) x$test),
            value = sapply(x$correlated_features, function(x) x$value)
        ) %>% 
        arrange(col1, col2)
     },
    `model results` = x$results    
)

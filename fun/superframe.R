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

drop.superframe = function(x, cols, reason){

    # ensure that x is a superframe.
    x = as.superframe(x)

    # verify that all columns are in x$data.
    badcols = setdiff(cols, colnames(x$data))
    if(length(badcols) > 0) stop(glue('Columns [{cc(badcols, sep = ", ")}] not found in data.'))

    # add the columns to the dropped list.
    for(col in cols){
        x$dropped_cols[[col]] = list(
            col = col, 
            reason = reason, 
            info = glue('Column [{col}] dropped manually: {reason}.'),
            shortinfo = glue('Manually dropped: {reason}.')
        )
    }

    # drop from data. 
    x$data = x$data[, setdiff(colnames(x$data), cols)]
    x$data_nocorrelatedfeatures = x$data_nocorrelatedfeatures[, setdiff(colnames(x$data_nocorrelatedfeatures), cols)]
    x$text_cols = setdiff(x$text_cols, cols)
    x$classes = x$classes[which(names(x$classes) %ni% cols)]

    # drop from correlated features. 
    x$correlated_features = Filter(
        function(i) !any(i$cols %in% cols),
        x$correlated_features
    )

    return(x)
}

colnames.superframe = function(x) colnames(x$data)
names.superframe = function(x) colnames(x$data)
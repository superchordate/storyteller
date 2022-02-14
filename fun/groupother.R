groupother = function(x, verbose = TRUE, run_autotype = TRUE, pct_catg_cutoff = 0.05){

    x = as.superframe(x, run_autotype = run_autotype)
    if(verbose) print('Removing columns that are not useful.')

    for(col in names(x$data)) if(class(x$data[[col]])[1] == 'factor'){

        valcounts = table(x$data[[col]][ !is.na(x$data[[col]]) ])
        toosmall = which(valcounts < nrow(x$data) * pct_catg_cutoff)

        if(length(toosmall) > 0){
            x$grouped_cols[[col]] <- x$data[[col]] # capture raw grouped column before modifying it. 
            levels(x$data[[col]]) = c(levels(x$data[[col]]), 'small_groups') # add the grouped label.
            for(i in toosmall) x$data[[col]][ x$data[[col]] == names(valcounts)[i] ] <- 'small_groups' # replace the values.
            if(verbose) print(glue('Grouping [{length(toosmall)}] values into "small_groups" for column [{col}].'))
        }

    }

    return(x)

}
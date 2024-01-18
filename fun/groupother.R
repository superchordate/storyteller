groupother = function(
    x, verbose = TRUE, run_autotype = TRUE, pct_catg_cutoff = 0.05, 
    othername = 'small_groups', excludefrom_groupother = c()
){

    x = as.superframe(x, run_autotype = run_autotype)
    x$othername = othername

    if(verbose) print(as.character(glue('Grouping to small categories to \'{othername}\'.')))

    for(col in setdiff(names(x$data), excludefrom_groupother)) if(class(x$data[[col]])[1] == 'factor'){

        valcounts = table(x$data[[col]][ !is.na(x$data[[col]]) ])
        toosmall = which(valcounts < nrow(x$data) * pct_catg_cutoff)

        if(length(toosmall) >= 2){ # must be at least 2 to create a combined group.
            x$grouped_cols[[col]] <- x$data[[col]] # capture raw grouped column before modifying it. 
            levels(x$data[[col]]) = c(levels(x$data[[col]]), othername) # add the grouped label.
            for(i in toosmall) x$data[[col]][ x$data[[col]] == names(valcounts)[i] ] <- othername # replace the values.
            if(verbose) print(glue('Grouping [{length(toosmall)}] values into "small_groups" for column [{col}].'))
        }

    }

    return(x)

}
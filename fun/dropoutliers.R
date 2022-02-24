dropoutliers = function(x, verbose = TRUE, checkabs = TRUE, run_autotype = TRUE, percentile_limit = 0.99){

    x = as.superframe(x, run_autotype = run_autotype)
    if(verbose) print('Checking for outliers.')
    found = FALSE

    # check each numeric column and drop rows with outliers.
    for(numcol in names(x$data)[x$classes %in% c('integer', 'numeric')]){

        # search for outliers.
        
        vals = setdiff(x$data[, numcol], 0) # with many 0s, including them can result in 0-valued quantiles.
        if(checkabs) vals = abs(vals)
        qs = quantile(vals, probs = c(0.25, 0.75, percentile_limit), na.rm = TRUE)

        cutoff = max(qs[2] + (qs[2] - qs[1]), qs[3]) # don't allow cutoff below the 95th percentile. 
        checkvals = if(checkabs){ abs(x$data[, numcol]) } else { x$data[, numcol]  }
        drop = which((!is.na(checkvals)) & (checkvals > cutoff))

        # if they exist:
        if(length(drop) > 0){
            
            # add to "dropped" dataset.
            x$dropped_rows = bind_rows(
                x$dropped_rows,
                dplyr::mutate(x$data[drop, ], 
                    reason = 'outliers',
                    raw_row = x$initrows[drop], 
                    outlier_column = numcol
                )
            ) %>% dplyr::relocate(raw_row, reason, outlier_column)

            # remove the rows.
            x$initrows = x$initrows[-drop]
            x$data = x$data[-drop, ]
            
            if(verbose) print(glue::glue('\t dropped [{length(drop)}] rows with outlier at [{numcol}] > {round(cutoff, 4)}'))
            found = TRUE

        }

        rm(qs, cutoff, checkvals, drop)

    }

    if(!found & verbose) print('\t No outliers found.')

    # return data and dropped rows.
    return(x)

}
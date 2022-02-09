dropoutliers = function(x, verbose = TRUE, checkabs = TRUE, run_autotype = TRUE){

    x = as.superframe(x, run_autotype = run_autotype)
    if(verbose) print('Checking for outliers.')
    found = FALSE

    # check each numeric column and drop rows with outliers.
    for(numcol in names(x$data)[which(x$classes %in% c('integer', 'numeric'))]){

        # search for outliers.
        qs = quantile(x$data[, numcol], probs = c(0.25, 0.75), na.rm = TRUE)
        cutoff = qs[2] + diff(qs)
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
            )

            # remove the rows.
            x$initrows = x$initrows[-drop]
            x$data = x$data[-drop, ]
            
            if(verbose) print(glue::glue('\t dropped [{length(drop)}] rows with outlier at [{numcol}]'))
            found = TRUE

        }

        if(!found & verbose) print('\t No outliers found. ')

        rm(qs, cutoff, checkvals, drop)

    }

    # return data and dropped rows.
    return(x)

}
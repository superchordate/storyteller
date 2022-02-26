convert_date_features = function(x,  verbose = TRUE){
    
    x = as.superframe(x, run_autotype = run_autotype)
    if(verbose) print('Adding features.')

    addcol = function(colname, values){
        x$data[[colname]] <<- values
        x$classes <<- c(x$classes, class(values)[1])
        names(x$classes)[length(x$classes)] <<- colname
    }

    # add date parts. 
    dates = names(x$data)[sapply(x$data, function(x) class(x)[1] %in% c('Date', 'POSIXct', 'POSIXt'))]
    for(date in dates){
        weekdays = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
        addcol(paste0(date, '_year'), year(x$data[[date]]))
        addcol(paste0(date, '_month'), month(x$data[[date]]))
        addcol(paste0(date, '_dayofmonth'), day(x$data[[date]]))
        addcol(paste0(date, '_dayofweek'), as.numeric(sapply(
            weekdays(x$data[[date]], abbreviate = TRUE), 
            function(x) which(weekdays == x)))
        )
    }

    # add date differences. 
    if(length(dates) > 1){
        todo = expand.grid(dates, dates, stringsAsFactors = FALSE) %>% filter(Var1 != Var2)
        for(irow in split(todo, 1:nrow(todo))){
            firstdate = ifelse(
                mean(x$data[[irow$Var1]] < x$data[[irow$Var2]], na.rm = TRUE) > 0.5,
                irow$Var1,
                irow$Var2
            )
            seconddate = setdiff(as.character(irow[1,]), firstdate)
            addcol(
                paste0(firstdate, '_to_', seconddate), 
                x$data[[seconddate]] - x$data[[firstdate]]
            )
        }
    }

    # now remove date columns.
    for(date in dates){
        x$dropped_cols[[date]] <- list(
            col = date, 
            reason = 'date',
            info = 'Converted to date parts and differences.',
            shortinfo = 'Converted to date parts and differences.'
        )
        x$data[[date]] <- NULL
        x$classes = x$classes[names(x$classes) != date]
    }

    return(x)

}
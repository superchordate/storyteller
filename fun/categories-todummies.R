categories_to_dummies = function(
    x, verbose = TRUE, skip = c()
){

    x = as.superframe(x, run_autotype = run_autotype)
    if(verbose) print('Converting columns to dummies.')
    found = FALSE

    x$data = convert_to_dummies(x = x$data, skip = skip)

    # return data and dropped rows.
    return(x)

}

convert_to_dummies = function(x, factors, istrain, skip = c()){

    if(is.null(factors)) factors = setdiff(names(x)[sapply(x, is.factor)], skip)

    # check for single-valued.
    badcols = factors[sapply(factors, function(i) sum(!duplicated(x[[i]])) == 1)]
    if(istrain) if(length(badcols) > 0) warning(glue('
        storyteller:convert_to_dummies: consider dropping single-valued columns: [{cc(badcols, sep = ", ")}].
    '))

    # replace with better dummies:
    dummies = x[, factors, drop = FALSE] %>% droplevels()
    for(factorcol in factors){
        # create 1s and 0s. 
        for(level in levels(dummies[[factorcol]])){
            dummies[[cc(factorcol, ' = ', level)]] = (dummies[[factorcol]] == level) * 1
        }
        # drop the raw column.
        dummies = dummies[, setdiff(names(dummies), factorcol), drop = FALSE]
    }

    # remove meta values from dummies, they are not useful.
    dummies = dummies[, setdiff(names(dummies), grep('Other|Missing|small_groups', names(dummies), value = TRUE))]

    x = data.frame(
        x[, !colnames(x) %in% factors],
        dummies,
        check.names = FALSE
    )

    return(x)

}


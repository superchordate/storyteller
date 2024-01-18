clean = function(x, verbose = TRUE, run_autotype = TRUE){

    x = as.superframe(x, run_autotype = run_autotype)

    # replace Inf, -Inf with NA.
    for(icol in names(x$data)) if(class(x$data[[icol]])[1] %in% c('integer', 'numeric')){
        x$data[[icol]][ x$data[[icol]] %in% c(-Inf, Inf) ] <- NA
    }

    return(x)

}
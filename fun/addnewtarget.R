# allow flexible addition/replacement of a target.
addnewtarget = function(x, colname, data, verbose = FALSE){

    if(class(x) != 'superframe') stop('addcol can only take a superframe as an argument.')

    # add to data and update classes.
    x$data[[colname]] = data
    x$classes = sapply(x, function(i) class(i)[1])

    return(x)

}


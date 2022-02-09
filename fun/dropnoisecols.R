dropnoisecols = function(
    x, verbose = TRUE, checkabs = TRUE, run_autotype = TRUE,
    napct_cutoff = 0.25, pctunique_range = c(0.05, 0.8), truepct_cutoff = 0.1
){

    x = as.superframe(x, run_autotype = run_autotype)
    if(verbose) print('Removing columns that are not useful.')
    
    dodrop = list()

    for(col in names(x$data)){

        uniquevals = setdiff(unique(x$data[[col]]), NA)
        iclass = class(x$data[[col]])[1]
        pctunique = round(length(uniquevals) / sum(!is.na(x$data[[col]])), 2)
        napct = round(mean(is.na(x$data[[col]])), 2)
        if(class(x$data[[col]])[1] == 'logical') truepct = round(sum(x$data[[col]]) / nrow(x$data), 2)
        
        # single-valued
        if(iclass %in% c('factor', 'ordinal') && pctunique < pctunique_range[1]){
            dodrop[[length(dodrop) + 1]] <- list(
                col = col, 
                reason = 'pctunique < pctunique_range',
                info = glue::glue('[{pctunique}] vs cutoff [{pctunique_range[2]}]. Values: [{paste(uniquevals, sep = ", ")}].'),
                shortinfo = pctunique
            )

        # too many NAs.
        } else if(napct > napct_cutoff){
            dodrop[[length(dodrop) + 1]] <- list(
                col = col, 
                reason = 'napct',
                info = glue::glue('[{napct}] vs cutoff [{napct_cutoff}]'),
                shortinfo = napct
            )

        # too many values (id-like)
        } else if(iclass %in% c('factor', 'ordinal') && (pctunique > pctunique_range[2])){
            dodrop[[length(dodrop) + 1]] <- list(
                col = col, 
                reason = 'pctunique > pctunique_range',
                info = glue::glue('[{pctunique}] vs cutoff [{pctunique_range[2]}]. Sample: [{paste(easyr::spl(x$data[[col]], 5), sep = ", ")}].'),
                shortinfo = pctunique
            )

        # not enough TRUE values. 
        } else if(iclass == 'logical' && (truepct > truepct_cutoff)){
            dodrop[[length(dodrop) + 1]] <- list(
                col = col, 
                reason = 'truepct',
                info = glue::glue('[{truepct}] vs cutoff [{truepct_cutoff}]'),
                shortinfo = truepct
            )

        }
        
        rm(iclass, pctunique, napct)
        
    }

    # drop columns and add info to object.
    for(idodrop in dodrop){
        
        # move data to the dropped column list.
        idodrop$data = x$data[[idodrop$col]]
        x$data[[idodrop$col]] <- NULL
        x$classes = x$classes[which(names(x$classes) != idodrop$col)]

        # add to object dropped columns.
        x$dropped_cols[[idodrop$col]] <- idodrop

        if(verbose) print(glue('
            Dropped column [{idodrop$col}] for reason [{idodrop$reason}].
        '))

        rm(idodrop)

    }

    return(x)

}

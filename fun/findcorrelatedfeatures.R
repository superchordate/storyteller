# https://medium.com/@outside2SDs/an-overview-of-correlation-measures-between-categorical-and-continuous-variables-4c7f85610365
find_correlated_features = function(x, verbose = FALSE, run_autotype = TRUE){

    x = as.superframe(x, run_autotype = run_autotype)
    if(verbose) print('Searching for high correlation.')

    colcombos = filter(
        expand.grid(names(x$data), names(x$data), stringsAsFactors = FALSE),
        Var1 != Var2
    )

    for(colcombos_idx in 1:nrow(colcombos)){

        colcombo = as.character(colcombos[colcombos_idx, ])

        types = sapply(colcombo, function(i){
            iclass = class(x$data[[i]])[1]
            if(iclass %in% c(
                'integer', 'numeric', 'Date', 'POSIXct', 'POSIXt', 'logical', 'ordinal'
            )) return('numlike')
            return(iclass)
        })
        
        # all numeric-like
        if(all(types == 'numlike')){
            
            notna = which(!is.na(x$data[[colcombo[1]]]) & !is.na(x$data[[colcombo[2]]]))
            if(length(notna) > 2){
                icor = cor(
                    as.numeric(x$data[notna, colcombo[1]]), 
                    as.numeric(x$data[notna, colcombo[2]])
                )
                if(icor > 0.3) x$correlated_features[[length(x$correlated_features) + 1]] <- list(
                    cols = colcombo, 
                    types = colcombo, 
                    test = 'corr',
                    value = icor
                )
            }
            
        # all factors.
        } else if(all(types == 'factor')){
            
            
            stop('Case not yet handled. Err 745.')
            
            #easyr::crun('dplyr::count(x$data, ', colcombo[1], ', ', colcombo[2], ')') %>% head()            
            #msumm = summary(
            #    nnet::multinom(as.formula(paste0(colcombo[1], ' ~ ', colcombo[2])), data = x$data)
            #)            

            #z <- msumm$coefficients/msumm$standard.errors
            # 2-tailed Wald z tests to test significance of coefficients
            #p <- (1 - pnorm(abs(z), 0, 1)) * 2
            #p
            
            
        # factor and number.
        } else if(
            (types[1] == 'factor' && types[2] == 'numlike') ||
            (types[1] == 'numlike' && types[2] == 'factor')
        ){
            
            faccol = colcombo[types == 'factor']
            numcol = colcombo[types == 'numlike']
            
            mresult = data.frame(
                summary(lm(as.formula(paste0(numcol, ' ~ ', faccol)), data = x$data))$coefficients
            )

            if(any(mresult$Pr...t.. < 0.1)) x$correlated_features[[length(x$correlated_features) + 1]] <- list(
                cols = colcombo, 
                types = colcombo, 
                test = 'lm-pvalue',
                value = mresult$Pr...t..
            )
            
        }    

    }

    return(x)

}
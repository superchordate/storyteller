# https://medium.com/@outside2SDs/an-overview-of-correlation-measures-between-categorical-and-continuous-variables-4c7f85610365
find_correlated_features = function(x, verbose = FALSE, run_autotype = TRUE){

    x = as.superframe(x, run_autotype = run_autotype)
    x$correlated_features = list()

    checkcols = setdiff(names(x$data), x$text_cols)
    colcombos = filter(
        expand.grid(checkcols, checkcols, stringsAsFactors = FALSE),
        Var1 != Var2
    )
    
    if(verbose) print(glue('Searching for high correlation among {nrow(colcombos)} feature combinations. May take some time.'))
    
    pb <- progress_bar$new(total = nrow(colcombos))
    for(colcombos_idx in 1:nrow(colcombos)){

        if(verbose) pb$tick()
        colcombo = as.character(colcombos[colcombos_idx, ])

        types = as.character(sapply(colcombo, function(i){
            iclass = class(x$data[[i]])[1]
            if(iclass %in% c(
                'integer', 'numeric', 'Date', 'POSIXct', 'POSIXt', 'logical', 'ordinal'
            )) return('numlike')
            return(iclass)
        }))
        
        # all numeric-like
        if(all(types == 'numlike')){
            
            notna = which(!is.na(x$data[[colcombo[1]]]) & !is.na(x$data[[colcombo[2]]]))
            if(length(notna) > 2){
                icor = cor(
                    as.numeric(x$data[notna, colcombo[1]]), 
                    as.numeric(x$data[notna, colcombo[2]])
                )
                if(abs(icor) > 0.5) x$correlated_features[[length(x$correlated_features) + 1]] <- list(
                    cols = colcombo, 
                    types = types, 
                    test = 'corr',
                    value = round(icor, 4)
                )
            }
            
        # all factors.
        } else if(all(types == 'factor')){
            
            
            # choose one feature to check chi sq over values, and one to 
            loopover = names(sapply(colcombo, function(icol) sum(!duplicated(x$data[[icol]]))))
            notna = which(!is.na(x$data[[colcombo[1]]]) & !is.na(x$data[[colcombo[2]]]))
            idt = x$data[notna, loopover]
            names(idt) = c('loopover', 'checkvals')
            sigfound = FALSE
            for(ival in levels(idt$loopover)){
                
                ival_dt = idt %>%
                    dplyr::mutate(match = loopover == ival) %>%
                    dplyr::group_by(checkvals, match) %>%
                    dplyr::summarize(n = n(), .groups = 'drop') %>%
                    reshape2::dcast(checkvals ~ match, value.var = 'n')
                    
                ival_dt = ival_dt[, setdiff(names(ival_dt), c('NA', 'checkvals'))]
                ival_dt = as.matrix(ival_dt)
                
                ival_dt[is.na(ival_dt)] <- 0
                testval = suppressWarnings(chisq.test(ival_dt)$p.value)
                
                if(!is.na(testval) && testval < 0.1){
                    sigfound = TRUE
                    break
                }
                
            }
            
            if(sigfound) x$correlated_features[[length(x$correlated_features) + 1]] <- list(
                cols = colcombo, 
                types = types, 
                test = 'chisq-loop',
                value = round(testval, 4)
            )
            
            
        # factor and number.
        } else if(
            (types[1] == 'factor' && types[2] == 'numlike') ||
            (types[1] == 'numlike' && types[2] == 'factor')
        ){
            
            faccol = colcombo[types == 'factor']
            numcol = colcombo[types == 'numlike']
            notna = which(!is.na(x$data[[colcombo[1]]]) & !is.na(x$data[[colcombo[2]]]))

            inum = as.numeric(x$data[[numcol]][notna])
            ifac = x$data[[faccol]][notna]

            mresult = data.frame(summary(lm(inum ~ ifac))$coefficients)
            rm(inum, ifac)

            if(any(mresult$Pr...t.. < 0.1)) x$correlated_features[[length(x$correlated_features) + 1]] <- list(
                cols = colcombo, 
                types = types, 
                test = 'lm-pvalue',
                value = round(min(mresult$Pr...t..), 4)
            )
            
        }    

    }

    return(x)

}
# https://medium.com/@outside2SDs/an-overview-of-correlation-measures-between-categorical-and-continuous-variables-4c7f85610365
require(progress)
require(reshape2)
correlatedfeatures_find = function(x, verbose = TRUE, run_autotype = TRUE, corr_cutoff){

    x = as.superframe(x, run_autotype = run_autotype)
    x$correlated_features = list()
    x$correlation = list()

    colcombos = getcolcombos(x$data, exclude = x$text_cols)
    
    if(verbose) print(glue('Searching for high correlation among {nrow(colcombos)} feature combinations. May take some time.'))
    
    pb <- progress_bar$new(total = nrow(colcombos))
    for(colcombos_idx in 1:nrow(colcombos)){

        if(verbose) pb$tick()
        colcombo = as.character(colcombos[colcombos_idx, c('col1', 'col2')])
        colcomboid = paste0(sort(colcombo), collapse = '-')

        types = as.character(sapply(colcombo, function(i){
            iclass = class(x$data[[i]])[1]
            if(iclass %in% c(
                'integer', 'numeric', 'Date', 'POSIXct', 'POSIXt', 'logical', 'ordinal'
            )) return('numlike')
            return(iclass)
        }))

        #if(colcomboid == 'age-policy_state') browser()
        
        # all numeric-like
        if(all(types == 'numlike')){
            
            notna = which(!is.na(x$data[[colcombo[1]]]) & !is.na(x$data[[colcombo[2]]]))
            if(length(notna) > 2){
                
                # special case where inum is single-value.
                singevalued = sapply(colcombo, function(icol) all(x$data[[icol]] == x$data[[icol]][1]))
                singevalued = singevalued[singevalued]
                
                if(length(singevalued) > 0){
                    
                    x$correlated_features[[colcomboid]] <- list(
                        cols = colcombo, 
                        types = types, 
                        test = 'single-value',
                        value = round(x$data[[names(singevalued)]], 4),
                        info = glue('non-na values are all [{round(x$data[[names(singevalued)]], 4)}].')
                    )
                    x$correlation %<>% bind_rows(data.frame(id = colcomboid, type = 'both-numeric', corr = NA, note = 'single-value'))
                    
                } else {
                    
                    icor = cor(
                        as.numeric(x$data[notna, colcombo[1]]), 
                        as.numeric(x$data[notna, colcombo[2]])
                    )
                    
                    if(abs(icor) > corr_cutoff) x$correlated_features[[colcomboid]] <- list(
                        cols = colcombo, 
                        types = types, 
                        test = 'corr',
                        value = round(icor, 4),
                        info = glue('correlation (closer to 1 better) value of {fmat(round(icor, 4), "%")}')
                    )
                    x$correlation %<>% bind_rows(data.frame(id = colcomboid, type = 'both-numeric', corr = icor, corr_abs = abs(icor)))
                    rm(icor)
                    
                }
            }
            rm(notna)
            
        # all factors.
        } else if(all(types == 'factor')){
            
            # get counts in a format we can send to chisq.test.
            idt = x$data[, colcombo]
            names(idt) = c('col1', 'col2')
            idt$ct = 1
            idt %<>% 
                dcast(col1 ~ col2, value.var = 'ct', fun.aggregate = sum) %>%
                select(-col1)
            idt[is.na(idt)] <- 0            

            # run chi squared.
            test_pval = tryCatch(
                { suppressWarnings(chisq.test(idt))$p.value },
                error = function(e){
                    stop(glue('
                        Error correlatedfeatures-find.R chisq.test columns [{cc(colcombo, sep = ", ")}]: {as.character(e)}
                        This sometimes happens if you call dropnoisecols before groupother. 
                        Please use similar to: clean() %>% groupother() %>% dropnoisecols() %>% dropoutliers() %>% correlatedfeatures_find(corr_cutoff = 0.6).
                    '))
                }
            )
            
            if(test_pval < 0.05) x$correlated_features[[colcomboid]] <- list(
                cols = colcombo, 
                types = types, 
                test = 'chi-squared',
                value = round(testval, 4),
                info = glue('chi-squared p-value (smaller better): {fmat(round(testval, 4), "%")}')
            )
            x$correlation %<>% bind_rows(data.frame(id = colcomboid, type = 'both-factors', test_pval = test_pval))
            
            rm(idt, test_pval)
            
            
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
            
            # special case where inum is single-value.
            if(all(inum == inum[1])){
                
                x$correlated_features[[colcomboid]] <- list(
                    cols = colcombo, 
                    types = types, 
                    test = 'single-value',
                    value = round(inum[1], 4),
                    info = glue('non-na values are all [{round(inum[1], 4)}].')
                )
                
            } else {

                m = lm(inum ~ ifac)
                coefs = data.frame(summary(m)$coefficients, check.names = FALSE)
                test_rsquared = summary(m)$r.squared
                
                #if(any(coefs$Pr...t..[-1] < 0.1)){ # -1 to drop (Intercept)
                if(test_rsquared > corr_cutoff){
                    x$correlated_features[[colcomboid]] <- list(
                        cols = colcombo, 
                        types = types, 
                        #test = 'lm-pvalue',
                        #value = round(min(mresult$Pr...t..), 4),
                        #info = glue('regression p-value (smaller better):{fmat(round(min(mresult$Pr...t..), 4), "%")}')
                        test = 'lm-rsquared',
                        value = round(test_rsquared, 4),
                        info = glue('regression r squared:{fmat(round(test_rsquared, 4), "%")}')
                    )
                }
                x$correlation %<>% bind_rows(data.frame(id = colcomboid, type = 'factor-number', test_rsquared = test_rsquared))
                
                rm(m, coefs)
                
            }
            
            rm(inum, ifac, faccol, numcol, notna)
            
        }
        
        rm(colcombo, types)

    }
    
    x$correlation %<>% arrange(desc(corr_abs))

    if(verbose) print(glue('{fmat(length(x$correlated_features)/nrow(colcombos), "%")} of feature combinations were correlated.'))

    return(x)

}
require(ggplot2)
plot_correlation = function(x, columns){

    colcomboid = paste0(sort(columns), collapse = '-')
    icorrelated_features = x$correlated_features[[colcomboid]]

    # two factors = heatmap:
    if(all(icorrelated_features$types == 'factor')){

        idt = x$data %>% 
            group_by_at(icorrelated_features$cols) %>% 
            summarize(ct = n(), .groups = 'drop')

        names(idt) = c('x', 'y', 'ct')

        idt %<>% filter(!is.na(x) & !is.na(y))

        idt %<>% 
            group_by(y) %>% 
            mutate(`% of Row` = ct/sum(ct)) %>%
            ungroup() %>%
            mutate(pctoftotal = ct/sum(ct))

        iplot = ggplot(idt, aes(x, y, fill = `% of Row`)) + 
            geom_tile() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
            geom_text(aes(label = cc(fmat(`% of Row`, "%"), fmat(ct, ",", digits = 0), sep = ' ')), size = 2.5) + 
            scale_fill_gradient(low = 'white', high = 'dodgerblue1') + # http://sape.inf.usi.ch/quick-reference/ggplot2/colour
            ggtitle(
                label = glue('Distribution of {cc(icorrelated_features$cols, sep = " by ")}'),
                subtitle = icorrelated_features$info
            ) + 
            xlab(icorrelated_features$cols[1]) + 
            ylab(icorrelated_features$cols[2])

    # 1 factor and 1 numlike = boxplot.
    } else if(all(sort(icorrelated_features$types) == c('factor', 'numlike'))){
        
        faccol = icorrelated_features$cols[icorrelated_features$types == 'factor']
        numcol = icorrelated_features$cols[icorrelated_features$types == 'numlike']
        notna = which(!is.na(x$data[[faccol]]) & !is.na(x$data[[numcol]]))
        
        inum = as.numeric(x$data[[numcol]][notna])
        ifac = x$data[[faccol]][notna]
        
        iplot = ggplot(data.frame(num = inum, fac = ifac), aes(x = fac, y = num)) + 
            geom_boxplot() + 
            ggtitle(glue('Distribution of {numcol} by {faccol}')) + 
            xlab(faccol) + 
            ylab(numcol)

    # 2 numlike: scatter.
    } else if(all(icorrelated_features$types == 'numlike')){
        
        col1 = icorrelated_features$cols[1]
        col2 = icorrelated_features$cols[2]
        
        iplot = ggplot(data.frame(x = x$data[[col1]], y = x$data[[col2]]), aes(x = x, y = y)) + 
            geom_point() + 
            ggtitle(glue('{col2} by {col1}')) + 
            xlab(col1) + 
            ylab(col2)

    } else {
        stop(glue('Case not yet handled: [{easyr::cc(icorrelated_features$types, sep = ", ")}]. Error E524.'))
    }
    
    return(iplot)

}

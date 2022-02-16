require(ggplot2)
plot_correlation = function(x, i){

    icorrelated_features = x$correlated_features[[i]]

    # two factors = heatmap:
    if(all(icorrelated_features$types == 'factor')){

        idt = x$data %>% 
            group_by_at(x$correlated_features[[1]]$cols) %>% 
            summarize(ct = n(), .groups = 'drop')

        names(idt) = c('x', 'y', 'ct')

        idt %<>% filter(!is.na(x) & !is.na(y))

        idt %<>% group_by(y) %>% 
            mutate(z = ct/sum(ct))

        iplot = ggplot(idt, aes(x, y, fill = z)) + 
            geom_tile() + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
            geom_text(aes(label = cc(fmat(z, "%"), fmat(ct, ",", digits = 0), sep = ' ')), size = 2.5) + 
            ggtitle(glue('Distribution of {cc(x$correlated_features[[1]]$cols, sep = " by ")}')) + 
            xlab(x$correlated_features[[1]]$cols[1]) + 
            ylab(x$correlated_features[[1]]$cols[2])

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

    } else {
        stop(glue('Case not yet handled: [{easyr::cc(icorrelated_features$types, sep = ", ")}]. Error E524.'))
    }
    
    return(iplot)

}

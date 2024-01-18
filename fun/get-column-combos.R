getcolcombos = function(x, exclude = c()){
    
    colcombos = data.frame(t(combn(setdiff(names(x), exclude), 2)))
    
    names(colcombos) = c('col1', 'col2')
    colcombos %<>% filter(col1 != col2)

    # remove combinations that involve dummies from the same source column.
    colcombos$col1_source = gsub('[_=][^_=]+', '', colcombos$col1) 
    colcombos$col2_source = gsub('[_=][^_=]+', '', colcombos$col2) 
    colcombos %<>% filter(col1_source != col2_source)

    return(colcombos)

}


correlatedfeatures_address = function(x, verbose = TRUE, run_autotype = TRUE, target){

    x = as.superframe(x, run_autotype = run_autotype)
    x$target = target
    
    addresscols = list()
    for(i in x$correlated_features) if(target %ni% i$cols) addresscols[[paste0(i$cols, collapse = '')]] <- i$cols
    
    dodrop = list()
    for(i in names(addresscols)){

      cols = addresscols[[i]]
      if(is.null(cols)) next
      
      r2s = sapply(cols, function(i) summary(lm(as.formula(paste0('`', target, '` ~ `', i, '`')), data = x$data))$r.squared)
      dropcol = names(r2s)[r2s != max(r2s)]      
      
      dodrop[[length(dodrop) + 1]] <- list(
          col = dropcol, 
          reason = 'correlated',
          info = glue('Column [{dropcol}] dropped and [{setdiff(cols, dropcol)}] kept.'),
          shortinfo = glue('Column [{dropcol}] dropped and [{setdiff(cols, dropcol)}] kept.')
      )
      
      for(j in names(addresscols)) if(!is.null(addresscols[[j]]) && dropcol %in% addresscols[[j]]) addresscols[[j]] <- NULL
      
      rm(i, j, cols, r2s, dropcol)
      
    }

    # drop columns and add info to object.
    # must match fun\dropnoisecols.R
    for(idodrop in dodrop){
        
        # move data to the dropped column list.
        idodrop$data = x$data[[idodrop$col]]
        x$data[[idodrop$col]] <- NULL
        x$classes = x$classes[which(names(x$classes) != idodrop$col)]

        # add to object dropped columns.
        x$dropped_cols[[idodrop$col]] <- idodrop

        if(verbose) print(idodrop$info)

        rm(idodrop)

    }

    return(x)

}

todummies = function(
    x, match.to = NULL, make.names = TRUE, ignore.cols = c(), other.name = 'Other'
){
  
  for(icol in setdiff(colnames(x), ignore.cols)){
      if(is.character(x[[icol]]) || is.factor(x[[icol]])){
    
    # Convert to character then factor to ensure no extra levels.
    x[[icol]] = as.character(x[[icol]])
    x[[icol]] = as.factor(x[[icol]])
    
    # Get unique values sorted by occurence. 
    # The least occured should be the 'other' that doesn't get its own columns.
    ivals = table(x[[icol]]) %>% sort(decreasing = TRUE)
    
    # Pick the 'Other' column that won't show up as a columns, and remove it from vals.
    # If it isn't there, remove the last (smallest) group.
    if(other.name %in% names(ivals)){
      ivals = ivals[names( ivals ) != other.name]
    } else {
      ivals = ivals[-length(ivals )]
    }
    
    for(ival in names(ivals)) x[[cc( icol, '=', ival)]] = (x[[icol]] == ival) * 1
    
    x = x[, setdiff(colnames(x), icol)]
    
    if(make.names){
      this.names = which(grepl(icol, colnames(x), fixed = TRUE ))
      colnames(x)[this.names] = make.names(colnames(x)[ this.names])
    }
    
  }}
  
  return(x)
    
}
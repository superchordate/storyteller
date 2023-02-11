correlatedfeatures_address = function(x, verbose = TRUE, run_autotype = TRUE, target){

    x = as.superframe(x, run_autotype = run_autotype)
    x$target = target
    
    if(target %ni% names(x$data)) stop(glue('
      Target [{target}] is not in the dataset. 
      It may have been dropped during pre-processing. Use verbose and review the console output to check.
      storytellr::correlatedfeatures_address Error 751.
    '))
    
    addresscols = list()
    for(i in x$correlated_features) if(target %ni% i$cols) addresscols[[paste0(i$cols, collapse = '')]] <- i$cols
    
    dodrop = list()
    pb <- progress_bar$new(total = length(addresscols))
    for(i in names(addresscols)){

      if(verbose) pb$tick()

      cols = addresscols[[i]]
      if(is.null(cols)) next
      
      cols_strength = sapply(cols, function(i) calculate_strength(x$data, target, i))
      
      # sometimes the cols_strength is the same. 
      #  in this case, drop the second.
      dropcol = if(all(cols_strength == max(cols_strength))){
        names(cols_strength)[2]
      } else {
        names(cols_strength)[cols_strength != max(cols_strength)]
      }
      
      dodrop[[length(dodrop) + 1]] <- list(
          col = dropcol, 
          reason = 'correlated',
          info = glue('Column [{dropcol}] dropped and [{setdiff(cols, dropcol)}] kept.'),
          shortinfo = glue('Column [{dropcol}] dropped and [{setdiff(cols, dropcol)}] kept.')
      )
      
      for(j in names(addresscols)) if(!is.null(addresscols[[j]]) && dropcol %in% addresscols[[j]]) addresscols[[j]] <- NULL
      
      rm(i, j, cols, cols_strength, dropcol)
      
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

calculate_strength = function(data, target, calculating_column){

  # is binomial or numeric? use lasso regression. 
  isbinom = length(unique(data[[target]])) == 2
  if(is.numeric(data[[target]]) || isbinom){

    if(isbinom && is.factor(data[[target]])) stop(glue('
      Target column [{target}] could seems binomial but is not a logical or integer.
      Please convert the column to logical prior to sending it to storytellr.
      storytellr::correlatedfeatures_address Error 712.
    '))

    if(isbinom && is.logical(data[[target]])) data[[target]] = data[[target]] * 1

    testval = summary(lm(
      as.formula(paste0('`', target, '` ~ `', calculating_column, '`')), 
      data = data
    ))$r.squared

  # otherwise use a loop to create models for each value
  # similar to multinomial.
  } else {

    testval = mean(sapply(levels(data[[target]]), function(ival){
      data$y = (data[[target]] == ival) * 1
      summary(lm(
        as.formula(paste0('y ~ `', calculating_column, '`')), 
        data = data
      ))$r.squared
    }), na.rm = TRUE)
    
  }

  if(is.na(testval)) stop(glue('
    NA test value at storyteller::calculate_strength.
    target = [{target}]
    calculating_column = [{calculating_column}]
    Error 103.
  '))

  return(testval)

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
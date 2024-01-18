require(glmnet)
fitmodel = function(x, verbose = TRUE, run_autotype = TRUE, target = x$target, ignorecols = c(), addinteractions = FALSE){

    if(length(unique(x$data[[x$target]])) == 1) stop(glue('
      Target is single-valued. Cannot build a model.
      Error storyteller-fitmodel E1108.
    '))

    x = as.superframe(x, run_autotype = run_autotype)
    x$target = target
    results = list()
    
    # drop text columns and convert to dummies.
    dorows = which(!is.na(x$data_nocorrelatedfeatures[[target]]))
    y = x$data_nocorrelatedfeatures[[target]][dorows]
    X = x$data_nocorrelatedfeatures[dorows, setdiff(names(x$data_nocorrelatedfeatures), c(target, x$text_cols, ignorecols))]
    X %<>% todummies(other.name = x$othername)

    # add interaction terms.
    if(addinteractions){ 
      
      if(verbose > 0) print('Adding interactions.')
    
      interactions = getcolcombos(X)
      
      # add the columns. 
      for(i in 1:nrow(interactions)){
        ivals = suppressWarnings(X[[interactions$col1[i]]] * X[[interactions$col2[i]]])
        if(!any(is.na(ivals))) X[[glue('{interactions$col1[i]} _x_ {interactions$col2[i]}')]] = ivals
      }

    }


    # save data with dummies.
    x$data_withdummies = X %>% mutate(y = y)
    names(x$data_withdummies)[ncol(x$data_withdummies)] <- target

    # convert to data matrix.
    Xdm = data.matrix(X)

    # run the appropriate model. 
    # is binomial or numeric? use lasso regression. 
    isbinom = length(unique(y)) == 2
    if(is.numeric(y) || isbinom){

      # set up the target if binomial.
      if(isbinom) if(is.logical(y)){
        y = y * 1
      } else {
        y = (y == y[1]) * 1
      }
      
      # remove single-valued columns.
      ls = sapply(1:ncol(Xdm), function(x) length(unique(Xdm[,x])))
      Xdm = Xdm[, ls > 1]
      
      # https://www.statology.org/lasso-regression-in-r/
      m = tryCatch({

        glmnet(Xdm, y, alpha = 1, lambda = cv.glmnet(Xdm, y, alpha = 1)$lambda.min)

      }, error = function(e){
        
        warning(paste(
          'Error using cv.glmnet:',
          as.character(e),
          'This is probably fine, but might be worth addressing if you get bad results.',
          'Warning storyteller::fitmodel W1113.',
          collapse = '\n'
        ))

        glmnet(Xdm, y, alpha = 1)

      })

      cm = coef(m)
      scm = summary(cm)
      features = data.frame(feature = rownames(cm)[scm$i], coef = scm$x) %>%
        filter(feature != '(Intercept)')
      
      yX = X[, unique(features$feature), drop = FALSE]
      yX$y = y
      results[[target]] <- lm(y ~ ., data = yX)

    # otherwise use a loop to create models for each value
    # similar to multinomial.
    } else {

      for(yval in levels(y)){
      
        # https://www.statology.org/lasso-regression-in-r/
        iy = (y == yval) * 1
        if(sum(iy) == 0) stop(glue('
          Target level [{yval}] has no instances. Please correct the factor.
          Error storyteller-fitmodel E1214.
        '))
        #if(yval == 'Cluster 11') browser()
        m = glmnet(Xdm, iy, alpha = 1, lambda = cv.glmnet(Xdm, iy, alpha = 1)$lambda.min)
        #m = glmnet(Xdm, iy, alpha = 1)
        cm = coef(m)
        scm = summary(cm)
        features = data.frame(feature = rownames(cm)[scm$i], coef = scm$x) %>%
          filter(feature != '(Intercept)')
        
        yX = X[, features$feature, drop = FALSE]
        yX$y = iy
        results[[yval]] <- lm(y ~ ., data = yX)

      }

    }

    # return coefficients and p-values. 
    x$results <- lapply(results, function(ilm){
      
      idt = summary(ilm)$coefficients
      idt = as.data.frame(idt)[, c('Pr(>|t|)', 'Estimate')]
      names(idt)[1:2] = c('pval', 'estimate')
      idt$estimate %<>% round(4)
      idt$feature = rownames(idt)
      idt %<>% select(feature, pval, estimate) %>% filter(feature != '(Intercept)')
      
      # clean data.
      idt$feature = gsub('`', '', idt$feature)
      idt$pval = round(idt$pval, 6)
      idt %<>% arrange(pval)
      rownames(idt) = NULL
      
      return(list(
        model = ilm,
        adjrsquared = summary(ilm)$adj.r.squared,
        coefs = idt,
        yX = yX
      ))
    })
    
    names(x$results) = names(results)

    return(x)

}


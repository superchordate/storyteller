fitmodel = function(x, verbose = TRUE, run_autotype = TRUE, target = x$target, ignorecols = c()){

    x = as.superframe(x, run_autotype = run_autotype)
    x$target = target
    results = list()
    
    # drop text columns and convert to dummies.
    dorows = which(!is.na(x$data[[target]]))
    y = x$data[[target]][dorows]
    X = x$data[dorows, setdiff(names(x$data), c(target, x$text_cols, ignorecols))]
    X %<>% todummies(other.name = x$othername)
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
      
      # https://www.statology.org/lasso-regression-in-r/
      m = glmnet(Xdm, y, alpha = 1, lambda = cv.glmnet(Xdm, y, alpha = 1)$lambda.min)
      cm = coef(m)
      scm = summary(cm)
      features = data.frame(feature = rownames(cm)[scm$i], coef = scm$x) %>%
        filter(feature != '(Intercept)')
      
      yX = X[, features$feature, drop = FALSE]
      yX$y = y
      results[[target]] <- lm(y ~ ., data = yX)

    # otherwise use a loop to create models for each value
    # similar to multinomial.
    } else {

      for(yval in levels(y)){
      
        # https://www.statology.org/lasso-regression-in-r/
        iy = (y == yval) * 1
        m = glmnet(Xdm, iy, alpha = 1, lambda = cv.glmnet(Xdm, iy, alpha = 1)$lambda.min)
        cm = coef(m)
        scm = summary(cm)
        features = data.frame(feature = rownames(cm)[scm$i], coef = scm$x) %>%
          filter(feature != '(Intercept)')
        
        yX = X[, features$feature, drop = FALSE]
        yX$y = iy
        results[[yval]] <- lm(y ~ ., data = yX)

      }

    }

    for(valname in names(results)){
      print(valname)
      print(summary(results[[valname]]))
    }

}


fitmodel = function(x, verbose = TRUE, run_autotype = TRUE, target = x$target, ignorecols = c()){

    x = as.superframe(x, run_autotype = run_autotype)
    x$target = target
    
    # drop text columns and run LASSO.
    y = x$data[[target]]
    X = x$data[, setdiff(names(x$data), c(target, x$text_cols, ignorecols))]
    X %<>% todummies(other.name = x$othername)
    Xdm = data.matrix(X)
    
    # https://www.statology.org/lasso-regression-in-r/
    m = glmnet(Xdm, y, alpha = 1, lambda = cv.glmnet(Xdm, y, alpha = 1)$lambda.min)
    cm = coef(m)
    scm = summary(cm)
    features = data.frame(feature = rownames(cm)[scm$i], coef = scm$x) %>%
      filter(feature != '(Intercept)')
    
    yX = X[, features$feature, drop = FALSE]
    yX$y = y
    m = lm(y ~ ., data = yX)
    
    return(m)

}


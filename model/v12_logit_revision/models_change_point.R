models_change_point = function(Lags, 
                               regress.window.full.start, 
                               regress.window.full.end, 
                               change.points, 
                               input.matrix,
                               output.vector,
                               Aggregationmin) {

  change.points.iter = c(regress.window.full.start, change.points, regress.window.full.end)

  store.logLik.list = list()
  store.lag.lasso.res.list = list()
  store.lag.lasso.a0.res.list = list()
  for (iter.change in 1:(length(change.points.iter)-1)) {
    # set variables for parameter estimation as well as evaluation via Log Likelihood
    #regress.window.start = regress.window.full.start + change.points.iter[iter.change]
    regress.window.start = change.points.iter[iter.change]+1
    regress.window.end = change.points.iter[iter.change + 1]
    regress.window = regress.window.start:regress.window.end
    
    est.window = matrix(NA, nrow = Lags, ncol = length(regress.window))
    est.window2 = matrix(NA, nrow = Lags, ncol = length(regress.window))
    
    
    for (i in 1:Lags) {
      est.window[i,] = regress.window - i
    }
    
    for (i in 1:Lags) { #this is to index AC_24, taking AC 24,28,72 hours etc before current modeling time
      est.window2[i,] = regress.window - (24*(60/Aggregationmin)*i) #minus -1 because AC are time lagged in the input_matrix owing to previous construction
    }
    
    output.data = output.vector[regress.window]
    
    #create input matrix with lagging some columns (but not the dummy columnts)
    nvar = ncol(input.matrix)
    ndum = 24+1
    nvarlag = nvar - ndum
    input.data.lagged = input.matrix[,1:nvarlag]
    input.data.dummies = input.matrix[regress.window,(nvarlag+1):nvar]
    input.data =c()
    for (k in 1:Lags) {
      input.data = cbind(input.data,
                         cbind(input.data.lagged[est.window[k,],], AC24=input.data.lagged[est.window2[k,],'AC'])
      )
    }
    input.data = cbind(input.data,input.data.dummies)
    
    store.logLik = matrix(NA, nrow = 1, ncol = 1)
    store.logit.lasso = matrix(0, nrow = 1, ncol = ncol(input.data))
    store.logit.lasso.a0 = c()
    
    # we only consider the variables which display values different from 0
    # This formulation ensures the AC are not omitted and the
    # followers with only 0 are excluded
    # We will add later 0 as parameter for the excluded variables to ensure
    # we can analyse the data properly between changepoints
    # This formulation changes nothing, a variable with no changes would be 
    # estimated as 0 anyway
    names.all.coef = colnames(input.data)
    included.coef = which(colSums(input.data) != 0)
    input.data = input.data[,included.coef]
    
    logit.lasso = glm(output.data ~ input.data,
                         family = 'binomial')
    predict.logit.lasso.probs = predict(logit.lasso, newx = input.data, type = 'response')
    predict.logit.lasso.probs.minus = 1 - predict.logit.lasso.probs
    # log(0) = -Inf, next lines rectify this problem
    # add very small number to prevent -Inf when log(0)
    temp = ifelse(predict.logit.lasso.probs == 0, 1, 0)
    temp = temp * 1e-323
    predict.logit.lasso.probs = predict.logit.lasso.probs + temp
    temp = ifelse(predict.logit.lasso.probs.minus == 0, 1, 0)
    temp = temp * 1e-323
    predict.logit.lasso.probs.minus = predict.logit.lasso.probs.minus + temp
    store.logLik = (t(output.data) %*% log(predict.logit.lasso.probs) + t(1 - output.data) %*% log(predict.logit.lasso.probs.minus))
    logit.lasso.coef = coef(logit.lasso)
    #logit.lasso.coef = na.replace(logit.lasso.coef, 0) #gives an error so use:
    logit.lasso.coef[is.na(logit.lasso.coef)]<-0
      #store.logit.lasso[1,1:(ncol(input.matrix)*Lags)] = logit.lasso.coef[-1]
    # store.logit.lasso is by default all 0. So here we add the estimated
    # parameters to the positions corresponding to the input.data
    # Hence the variables excluded from the estimation are 'estimated' as 0
      store.logit.lasso[1,included.coef] = logit.lasso.coef[-1]
      store.logit.lasso.a0 = logit.lasso.coef[1]
  #    colnames(store.logit.lasso) = names(coef(logit.lasso)[-1])
      colnames(store.logit.lasso) = names.all.coef

    store.lag.lasso.res.list[[iter.change]] = store.logit.lasso
    store.lag.lasso.a0.res.list[[iter.change]] = store.logit.lasso.a0
    store.logLik.list[[iter.change]] = store.logLik
  }
  return(list(store.logLik.list, store.lag.lasso.res.list, store.lag.lasso.a0.res.list))
}

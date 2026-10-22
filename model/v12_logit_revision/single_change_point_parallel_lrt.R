single_change_point_lrt = function(Lags, 
                               regress.window.full.start, 
                               regress.window.full.end, 
                               change.point.sequence, 
                               input.matrix,
                               output.vector,
                               Aggregationmin) {
  
  counter = 0
  nCP = length(change.point.sequence)
  store.logLik.list = vector(mode = "list", length = nCP)
  
  for (change.point in change.point.sequence) {
    counter = counter + 1
    store.logLik.c = c()
    for (iter.change in 0:1) {
      # set variables for parameter estimation as well as evaluation via Log Likelihood
      regress.window.start = regress.window.full.start + (change.point - regress.window.full.start + 1) * iter.change
      regress.window.end = change.point + (regress.window.full.end - change.point) * iter.change
      regress.window = regress.window.start:regress.window.end
      
      est.window = matrix(NA, nrow = Lags, ncol = length(regress.window))
      est.window2 = matrix(NA, nrow = Lags, ncol = length(regress.window))
      
      ## Only derive the model for all chosen lags. In terms of prediction, 
      ## this should change nothing. Choice of lags is only a matter of 
      ## concern when it comes to the model selection which happens after
      ## the change point candidate derivation 
      for (i in 1:Lags) {
        est.window[i,] = regress.window - i
      }
      
      for (i in 1:Lags) { #this is to index AC_24, taking AC 24,28,72 hours etc before current modeling time
        est.window2[i,] = regress.window - (24*(60/Aggregationmin)*i ) 
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

      
      # we only consider the variables which display values different from 0
      # This formulation ensures the AC are not omitted and the
      # followers with only 0 are excluded
      input.data = input.data[,which(colSums(input.data) != 0)]
        
      logit.lasso = glm(output.data ~ input.data, family = 'binomial')
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
      
      store.logLik = matrix(NA, nrow = 1, ncol = 1)
      store.logLik = (t(output.data) %*% log(predict.logit.lasso.probs) + t(1 - output.data) %*% log(predict.logit.lasso.probs.minus))
      store.logLik.c[iter.change + 1] = store.logLik
    }
    store.logLik.list[[counter]] = store.logLik.c
  }
  
  logLiks = c()
  for (i in 1:nCP) {
    logLiks[i] = sum(unlist(store.logLik.list[[i]]))
  }
  return(list(change.point.sequence[which.max(logLiks)]))
}

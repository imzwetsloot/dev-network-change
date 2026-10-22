VuongTest = function(input.data, store.logit.glm.object, store.names.selected.coef) {
  #### vuong test, quick coded
#  library(sandwich)
#  library(CompQuadForm)
#  library(MASS)
  dimmat =(dim(input.data)[2] + 1)
#  if(length(dimmat)==0){dimmat=1}
  #  BA2
  BA2 = matrix(0, nrow = dimmat, ncol = dimmat)
  colnames(BA2) = c("(Intercept)",colnames(input.data))
  rownames(BA2) = c("(Intercept)",colnames(input.data)) 
  
  current.object = store.logit.glm.object[[length(store.logit.glm.object)]]
  
  scaling <- summary(current.object)$sigma
  if (is.null(scaling)) {
    scaling <- 1
  } else {
    scaling <- scaling^2
  }
  b = (1/scaling) * estfun(current.object)
  vc <- dim(b)[1] * vcov(current.object)
  #' implemented in case NA for parameter estimates occurred. Happens when 
  #' multicollinearity present. Next line rectifies issue but would be better 
  #' to remove parameter from dataset all together
#  if (any(is.na(vc))) {vc = matrix(vc[!is.na(vc)], nrow = dim(b)[2], ncol = dim(b)[2])}
  Ainv2 <- try(solve(vc), silent = TRUE)
  if (class(Ainv2)[1] == "try-error") {Ainv2 = ginv(vc)}
  #A1 = chol2inv(chol(A))
  B2 = crossprod(b) / dim(b)[1]
  
  current.matrix = B2 %*% Ainv2
  
  if(length(store.names.selected.coef)==0){
    colnames(current.matrix) = rownames(current.matrix) = "(Intercept)"
  }else {
    colnames(current.matrix) = rownames(current.matrix) = c("(Intercept)", store.names.selected.coef[[length(store.names.selected.coef)]])
  }
    #  colnames(current.matrix) = rownames(current.matrix) = c("(Intercept)",colnames(store.logit.glm.object[[length(store.logit.glm.object)]]$model$`input.data[, which.paras]`))[!is.na(store.logit.glm.object[[length(store.logit.glm.object)]]$coefficients)]
  BA2[rownames(current.matrix), colnames(current.matrix)] = current.matrix
  
  # BA1
  Ainv = list()
  B = list()
  BA1 = matrix(0, nrow = dimmat, ncol = dimmat)
  colnames(BA1) = c("(Intercept)",colnames(input.data))
  rownames(BA1) = c("(Intercept)",colnames(input.data)) 
  for (i in 1:(length(store.logit.glm.object) - 1)) {
    current.object = store.logit.glm.object[[i]]
    
    scaling <- summary(current.object)$sigma
    if (is.null(scaling)) {
      scaling <- 1
    } else {
      scaling <- scaling^2
    }
    b = (1/scaling) * estfun(current.object)
    vc <- dim(b)[1] * vcov(current.object)
    #' implemented in case NA for parameter estimates occurred. Happens when 
    #' multicollinearity present. Next line rectifies issue but would be better 
    #' to remove parameter from dataset all together
#    if (any(is.na(vc))) {vc = matrix(vc[!is.na(vc)], nrow = dim(b)[2], ncol = dim(b)[2])}
    Ainv[[i]] <- try(solve(vc), silent = TRUE)
    if (class(Ainv[[i]])[1] == "try-error") {Ainv[[i]] = ginv(vc)}
    #A1 = chol2inv(chol(A))
    B[[i]] = crossprod(b) / dim(b)[1]
    
    current.matrix = B[[i]] %*% Ainv[[i]]
    if(length(store.names.selected.coef)==0){
      colnames(current.matrix) = rownames(current.matrix) = "(Intercept)"
    }else{
      colnames(current.matrix) = rownames(current.matrix) = c("(Intercept)", store.names.selected.coef[[i]])
    }
#    colnames(current.matrix) = rownames(current.matrix) = c("(Intercept)",colnames(store.logit.glm.object[[i]]$model$`input.data[, which.paras]`))[!is.na(store.logit.glm.object[[i]]$coefficients)]
    BA1[rownames(current.matrix), colnames(current.matrix)] = BA1[rownames(current.matrix), colnames(current.matrix)] + current.matrix
  }
  BA1 = -BA1
  
  # BA12 and BA21
  B12 = list()
  BA12 = matrix(0, nrow = dimmat, ncol = dimmat)
  colnames(BA12) = c("(Intercept)",colnames(input.data))
  rownames(BA12) = c("(Intercept)",colnames(input.data)) 
  
  B21 = list()
  BA21 = matrix(0, nrow = dimmat, ncol = dimmat)
  colnames(BA21) = c("(Intercept)",colnames(input.data))
  rownames(BA21) = c("(Intercept)",colnames(input.data)) 
  
  current.object2 = store.logit.glm.object[[length(store.logit.glm.object)]]
  scaling <- summary(current.object2)$sigma
  if (is.null(scaling)) {
    scaling <- 1
  } else {
    scaling <- scaling^2
  }
  b2 = (1/scaling) * estfun(current.object2)
  
  start.val = 0
  for (i in 1:(length(store.logit.glm.object) - 1)) {
    current.object1 = store.logit.glm.object[[i]]
    
    scaling <- summary(current.object1)$sigma
    if (is.null(scaling)) {
      scaling <- 1
    } else {
      scaling <- scaling^2
    }
    b1 = (1/scaling) * estfun(current.object1)
    
    B12[[i]] = crossprod(b1,b2[(start.val + 1):(dim(b1)[1] + start.val),]) / dim(b1)[1]
    
    current.matrix = B12[[i]] %*% Ainv2
    if(length(store.names.selected.coef)==0){
      colnames(current.matrix) = "(Intercept)"
      rownames(current.matrix) = "(Intercept)"
    }else {
      colnames(current.matrix) = c("(Intercept)", store.names.selected.coef[[length(store.names.selected.coef)]])
      rownames(current.matrix) = c("(Intercept)", store.names.selected.coef[[i]])
    }
#    colnames(current.matrix) = c("(Intercept)",colnames(store.logit.glm.object[[length(store.logit.glm.object)]]$model$`input.data[, which.paras]`))[!is.na(store.logit.glm.object[[length(store.logit.glm.object)]]$coefficients)]
#    rownames(current.matrix) = c("(Intercept)",colnames(store.logit.glm.object[[i]]$model$`input.data[, which.paras]`))[!is.na(store.logit.glm.object[[i]]$coefficients)]
    BA12[rownames(current.matrix), colnames(current.matrix)] = BA12[rownames(current.matrix), colnames(current.matrix)] + current.matrix
    
    B21[[i]] = crossprod(b2[(start.val + 1):(dim(b1)[1] + start.val),], b1) / dim(b1)[1]
    
    start.val = dim(b1)[1]
    
    current.matrix = B21[[i]] %*% Ainv[[i]]
    if(length(store.names.selected.coef)==0){
      rownames(current.matrix) = "(Intercept)"
      colnames(current.matrix) = "(Intercept)"
    } else {
      rownames(current.matrix) = c("(Intercept)", store.names.selected.coef[[length(store.names.selected.coef)]])
      colnames(current.matrix) = c("(Intercept)", store.names.selected.coef[[i]])
    }
#    rownames(current.matrix) = c("(Intercept)",colnames(store.logit.glm.object[[length(store.logit.glm.object)]]$model$`input.data[, which.paras]`))[!is.na(store.logit.glm.object[[length(store.logit.glm.object)]]$coefficients)]
#    colnames(current.matrix) = c("(Intercept)",colnames(store.logit.glm.object[[i]]$model$`input.data[, which.paras]`))[!is.na(store.logit.glm.object[[i]]$coefficients)]
    BA21[rownames(current.matrix), colnames(current.matrix)] = BA21[rownames(current.matrix), colnames(current.matrix)] + current.matrix
    
  }
  BA12 = -BA12
  BA21 = BA21
  ####
  W = rbind(cbind(BA1, BA12), cbind(BA21, BA2))
  Weigen = eigen(W, only.values = TRUE)$values
  
  L1 = 0
  for (i in 1:(length(store.logit.glm.object) - 1)) {
    L1 = L1 + sum(logLik(store.logit.glm.object[[i]]))
  }
  L2 = sum(logLik(store.logit.glm.object[[length(store.logit.glm.object)]]))
  
  paras1 = 0
  for (i in 1:(length(store.logit.glm.object) - 1)) {
    if(length(store.names.selected.coef)==0){
      paras1 = paras1 + 1
    }else{
      paras1 = paras1 + (length(store.names.selected.coef[[i]]) + 1)
    }
#    paras1 = paras1 + (dim(store.logit.glm.object[[i]]$model$`input.data[, which.paras]`)[2] + 1)
#    paras1 = paras1 + (length(colnames(store.logit.glm.object[[i]]$model$`input.data[, which.paras]`)[!is.na(store.logit.glm.object[[i]]$coefficients)]) + 1)
  }
 
  if(length(store.names.selected.coef)==0){
    paras2 = 1 
  }else {
    paras2 = (length(store.names.selected.coef[[length(store.names.selected.coef)]]) + 1)
  }
#  paras2 = (dim(store.logit.glm.object[[length(store.logit.glm.object)]]$model$`input.data[, which.paras]`)[2] + 1)
#  paras2 = (length(colnames(store.logit.glm.object[[length(store.logit.glm.object)]]$model$`input.data[, which.paras]`)[!is.na(store.logit.glm.object[[length(store.logit.glm.object)]]$coefficients)]) + 1)
  
  test.val = 2 * (L1 - L2)
  L1betterL2_likelihood = imhof(test.val, -Weigen)[[1]]
  
  test.val = 2 * (L1 - L2 - (paras1 - paras2) )
  L1betterL2_AIC = imhof(test.val, -Weigen)[[1]]
  
  nobs =length(store.logit.glm.object[[length(store.logit.glm.object)]]$residuals)
  test.val = 2 * (L1 - L2 - (paras1 - paras2) / 2 * log(nobs))
  L1betterL2_BIC = imhof(test.val, -Weigen)[[1]]

  L1betterL2 = c(L1betterL2_likelihood, L1betterL2_AIC, L1betterL2_BIC)
  names(L1betterL2) = c("likelihood", "AIC", "BIC")
  return(L1betterL2)
}

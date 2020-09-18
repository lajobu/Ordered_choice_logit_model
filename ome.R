
ome = function(model, x) {
  
  ff = function(model, value) {
    if (model$method=="logistic") {
      value = dlogis(value)
    } else if (model$method=="probit") {
      value = dnorm(value)
    }
    return(value)
  }
  
  fcdf = function(model, value) {
    if (model$method=="logistic") {
      value = plogis(value)
    } else if (model$method=="probit") {
      value = pnorm(value)
    }
    return(value)
  }
  
  # model$method
  y = as.matrix(sapply(model$model[,1], as.numeric))
  list_of_alternatives = sort(unique(y))
  J = length(list_of_alternatives)
  beta_hat = model$coefficients
  cutoffs = model$zeta
  
  names = "alternative1"
  for(i in 2:J) {
    names = c(names, paste("alternative", list_of_alternatives[i], sep=""))
  }
  
  # the very first alternative
  meff = t(-ff(model, cutoffs[1]-x%*%beta_hat)%*%beta_hat)
  # middle alternatives
  for(i in 2:(J-1)) {
    me = t(-ff(model, cutoffs[i]-x%*%beta_hat)%*%beta_hat-ff(model, cutoffs[i-1]-x%*%beta_hat)%*%beta_hat)
    meff = cbind(meff, me)
  }
  # the last alternative
  me = t(ff(model, cutoffs[J-1]-x%*%beta_hat)%*%beta_hat)
  meff = cbind(meff, me)
  #colnames(meff) = names
  #rownames(meff) = attr(model$coefficients, "names")
  #show(round(meff,9))
  
  # dummy variables marginal effects
  # independent variables
  indeps = model$model[,-1]
  # dummy variables indices
  dvi = rep(0, times=ncol(indeps))
  for(i in 1:ncol(indeps)) {
    if(is.numeric(indeps[,i])==1) {
      if(length(unique(indeps[,i]))==2L) {
        # the variable is dummy
        dvi[i] = 1
      }
    }
  } 
  dvi = which(dvi==1)
  if(length(dvi)>0){ 
    x0 = x
    x0[dvi] = 0
    x1 = x0
  }
  # Marginal effects of the dummy variables
  if(length(dvi)>0) {
    for(i in 1:length(dvi)) {
      for(j in 1:J) {
        x1[dvi[i]] = 1
        if(j==1){
          meff[dvi[i], j] = fcdf(model, cutoffs[j]-x1%*%model$coefficients)-
            fcdf(model, cutoffs[j]-x0%*%model$coefficients)
        } else if(j==J){
          meff[dvi[i], j] = (1-fcdf(model, cutoffs[J-1]-x1%*%model$coefficients))-
            (1-fcdf(model, cutoffs[J-1]-x0%*%model$coefficients))
        } else {
          meff[dvi[i], j] = fcdf(model, cutoffs[j]-x1%*%model$coefficients)-
            fcdf(model, cutoffs[j-1]-x1%*%model$coefficients)-
            fcdf(model, cutoffs[j]-x0%*%model$coefficients)+
            fcdf(model, cutoffs[j-1]-x0%*%model$coefficients)
        }
        x1[dvi[i]] = 0
      }
    }  
  }
  
  #colnames(meff) = names
  rownames(meff) = attr(model$coefficients, "names")
  if(length(dvi)==0){ 
    x0 = x
  }
  meff = cbind(meff, x0)  
  colnames(meff) = c(names, "at X=")
  if(length(dvi)>0) {
    rn = rownames(meff)
    for(i in 1:length(dvi)) {
      rn[dvi[i]] <- paste(rn[dvi[i]], "!", sep="")
    }
    rownames(meff) = rn
  }
  # print results
  show(round(meff,9))
  if(length(dvi)>0){
    cat("(!) indicates marginal effect was calculated for discrete change of dummy variable from 0 to 1")
  }
  
}
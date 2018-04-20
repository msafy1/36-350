generate_data = function(n, p){
  covariates = matrix(rnorm(n*p), nrow=n, ncol=p)
  responses = rnorm(n)
  return(list(covariates, responses))
}

model_select = function(covariates, responses, cutoff) {
  mod = lm(responses~covariates)
  sub.cov = covariates[covariates<cutoff]
  mod2 = lm(responses~mod)
  return(summary(mod2)$coefficients[,4])
}

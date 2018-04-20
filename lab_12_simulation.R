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

run_simulation = function(n_trials, n, p, cutoff){
  data = generate_data(n, p)
  mod = model_select(data[[0]]~data[[1]])
  sum = summary(mod)
  hist(sum[,4])
}

#p-values seem to be uniformly distributed.

run_simulation(n=100, p=10)
run_simulation(n=1000, p=10)
run_simulation(n=10000, p=10)
run_simulation(n=100, p=20)
run_simulation(n=1000, p=20)
run_simulation(n=10000, p=20)
run_simulation(n=100, p=50)
run_simulation(n=1000, p=50)
run_simulation(n=10000, p=50)
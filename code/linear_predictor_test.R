library(INLA)

set.seed(2021)
v = vector(length = 20)
v[1:2] = c(0,0)
for(i in 3:20) {v[i] = -v[i-2]+2*v[i-1]+rnorm(1, sd = 0.2)}

v=v[-10:-1]

w = rnorm(10, sd = 0.5)

m = mean(v)

plot(v)
plot(v+w)

eta = exp(v+w)
eta = rep(eta, each = 10)

precision = 1
u = rgamma(100, shape = eta^2*precision, scale = 1/(precision*eta))

plot(u)

dat = data.frame(u = u, index = rep(1:10, each = 10), index2 = rep(1:10, each = 10))

hyper = list(prec = list(prior = 'pc.prec', params = c(0.5,0.1)))

res = inla(formula = u ~ f(index, model = 'rw2', constr = T, hyper = hyper) +
                         f(index2, model = 'iid', constr = T, hyper = hyper),
           data = dat,
           family = 'gamma',
           control.predictor = list(compute = T),
           verbose = T)
plot(res)

i = (0:9)*10 +1
linear_predictors = res$summary.linear.predictor$mean[i]
linear_predictor_variances = res$summary.linear.predictor$sd[i]^2

estimated_linear_predictors = res$summary.fixed$mean[1] + res$summary.random$index$mean
estimated_lin_pred_variances = res$summary.fixed$sd[1]^2 + res$summary.random$index$sd^2 + 
  res$summary.random$index2$sd^2

plot(linear_predictors)
lines(estimated_linear_predictors)

plot(linear_predictor_variances)
lines(estimated_lin_pred_variances)

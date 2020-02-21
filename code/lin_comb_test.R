library(INLA)

n = 300
t = seq(1:n)
eta = 2 + sin(t/30) + rnorm(n)
mu = exp(eta)
prec.par = 1.2
a = prec.par
b =  mu / (prec.par)
y = rgamma(n, shape = a, scale = b)

formula = y ~ f(t, model = "rw2") + f(u, model = "iid")
lin = inla.make.lincomb( "(Intercept)" = 1, t = c(1), u = c(rep(NA,n),1))

data = data.frame(y = c(y,NA), t = inla.group(c(1:n,1)), u =1:(n+1))

res = inla(formula, family =  "gamma", data= data,
           control.predictor = list(compute = T, link = 1),
           lincomb = lin)

## computed with postprocessing of results
m = res$summary.fixed[1,1] + res$summary.random$t$mean[1]
s = sqrt(res$summary.fixed[1,2]^2 +
           res$summary.random$t$sd[1]^2 +
           1/res$summary.hyperpar[3,1])

## computed from inla in the linear predictor
l1 = res$summary.linear.predictor[301,1:2]

## from the linear combination
l2 = res$summary.lincomb.derived[2:3]

c(m,s)
l1
l2

## Задание №1 ----
B = function(delta,n,B0){
  B = c(B0)
  k_delta = c(delta)
  for(k in 2:n){
    e = rnorm(1,0,sqrt(delta))
    B = c(B,B[k-1]+e)
    k_delta = c(k_delta,k*delta)
  }
  return(data.frame(k_delta,B))
}
S = function(delta,n,S0,sigma,a,B0){
  Brownian=B(delta,n,B0)
  S = c(S0)
  for(k in 2:n){
    ex = (a-(sigma**2)/2)*k*delta+sigma*Brownian$B[k]
    S = c(S,S0*exp(ex))
  }
  return (data.frame(k_d = Brownian$k_delta,S))
}
S0 = 100
a = 0.5
sigma = 0.8
delta = 0.0001
n = 1000
B0 = 0
s = S(delta,n,S0,sigma,a,B0)
plot(s$k_d,s$S,type = "l",col="red")


## Задание №2 ----
X = c()
for(k in 1:n-1){
  X = c(X,log(s$S[k+1])-log(s$S[k]))
}
mu_est = (1/n)*sum(X)
sigma2_est = (1/n)*sum((X-mu_est)**2)
sigma_est = sqrt(sigma2_est/delta)
sigma_est**2
a_est = (mu_est/delta) + (sigma_est**2)/2;a_est


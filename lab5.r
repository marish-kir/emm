## Замоделировать процесс (8) для ∆ = 0.0001 ( т.е. средне-квадратическое )
Brownian = function(n,B0,d){
  Brownian = numeric(n)
  k = numeric(n)
  Brownian[1] = B0
  k[1] = d
  for(i in 2:n){
    e = rnorm(1,0,sqrt(d))
    Brownian[i] = Brownian[i-1]+e
    k[i] = i*d
  }
  return(data.frame(Brownian,k))
}
n = 1000
d = 0.0001
B0 = 0
B=Brownian(n,B0,d)
plot(B$k,B$Brownian,type="l",col="blue")

## Построить ансамбль реализаций процесса, замоделированного на предыдущем шаге, 
## и вывести все реализации процесса на один график. Т.е.
## построить 200 реализаций процесса (8) на одном графике.

for(i in 1:199){
  B=Brownian(n,B0,d)
  lines(B$k,B$Brownian,type = "l",col="blue")
}
## Ограничить ансамбль реализаций, построенный на предыдущем шаге по правилу трех сигм.
lines(B$k,3*sqrt(B$k),col = "red",type = "l", lwd = 2)
lines(B$k,-3*sqrt(B$k),col = "red",type = "l",lwd = 2)

## Реализовать процесс (9) при следующих входных значениях параметров:
##𝑆0 = 1, 𝑎 = 0.5, 𝜎 = 0.9,√∆ = 0.01, 𝑘 = 0, 1, . . . , 103.
S = function(n,d,a,sigma,S0,B0){
  S = numeric(n)
  B=Brownian(n,B0,d)
  k = B$k
  S[1] = S0
  for(i in 2:n){
    S[i] = S0*exp((a-(sigma**2)/2)*i*d+sigma*B$Brownian[i])
  }
  return (data.frame(S,k))
}
n = 1000
S0 = 1
B0 = 0
a = 0.5
d = 0.0001
sigma = 0.9
s = S(n,d,a,sigma,S0,B0)
plot(s$k,s$S,type = "l",col="blue")

## Построить 200 реализаций процесса (10) на одном графике.
for(i in 1:199){
  s = S(n,d,a,sigma,S0,B0)
  lines(s$k,s$S,type = "l",col="blue")
}


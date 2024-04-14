install.packages("tseries")
library("tseries")
library("stats")

## 1. Построить график стационарного процесса {ℎ𝑛} и график волатильности
##{𝜎𝑛} процесса 𝐺𝐴𝑅𝐶𝐻(1,0), из 𝑛 = 1000 наблюдений.

garch_1_0 <- function(n,a0,a1){
  s = numeric(n)
  h = numeric(n)
  e=rnorm(1,0,1)
  s[1] = runif(1, min = 0, max = 1)
  h[1] = e*sqrt(s[1])
  for(i in 2:n){
    s[i] = a0+a1*h[i-1]*h[i-1]
    e=rnorm(1,0,1)
    h[i] = e*sqrt(s[i])
  }
  return (data.frame(s,h))
}
a0 = 0.3
a1 = 0.4
n = 1000
reg = garch_1_0(n,a0,a1)
title = sprintf("График волатильности. Параметры a0 = %f , a1 = %f  ",a0,a1)
plot(reg$s,main = title,type="l",col="blue")
title = sprintf("График процесса. Параметры a0 = %f , a1 = %f  ",a0,a1)
plot(reg$h,main = title,type="l",col="blue")

## 2. Оценить параметры 𝑎0 и 𝑎1 с помощью метода наименьших квадратов
## (МНК), путем преобразования процесса 𝐴𝑅𝐶𝐻(1) ≡ 𝐺𝐴𝑅𝐶𝐻(1, 0) к
## процессу авторегрессии первого порядка.
reg = reg$h
ch = numeric(n-1)
zn = numeric(n)
sum2 = 0
a1_estimate = 0
a0_estimate = 0
for (i in 2 : n) {
  ch[i-1] = (reg[i]**2)*((reg[i-1]**2) - 1)
  zn[i-1] = (reg[i-1]**4)-(reg[i-1]**2)
}
a1_estimate = sum(ch)/sum(zn)
sum = numeric(n-1)
for (i in 2 : n) {
  sum[i-1]= reg[i]**2 - a1_estimate*reg[i-1]**2
}
a0_estimate = sum(sum)/n
print("Оценка а0:")
a0_estimate
print("Оценка а1:")
a1_estimate

## 3. Оценить параметры 𝑎0, 𝑎1 при помощи функции 𝑔𝑎𝑟𝑐ℎ() пакета 𝑡𝑠𝑒𝑟𝑖𝑒𝑠 по
## 3выборке {ℎ𝑛}.

garch(reg,order = c(1,0))

## 4. Построить график стационарного процесса 𝐺𝐴𝑅𝐶𝐻(3,0), из 𝑛 = 1100
## наблюдений.

garch_3_0 <- function(n,a0,a1,a2,a3){
  s = numeric(n)
  h = numeric(n)
  s[1] = runif(1, min = 0, max = 1)
  e=rnorm(1,0,1)
  h[1] = e*sqrt(s[1])
  s[2] = a0+a1*h[1]*h[1]
  e=rnorm(1,0,1)
  h[2] = e*sqrt(s[2])
  s[3] = a0+a1*h[2]*h[2]+a2*h[1]*h[1]
  e=rnorm(1,0,1)
  h[3] = e*sqrt(s[3])
  for(i in 4:n){
    s[i] = a0+a1*h[i-1]*h[i-1]+a2*h[i-2]*h[i-2]+a3*h[i-3]*h[i-3]
    e=rnorm(1,0,1)
    h[i] = e*sqrt(s[i])
    
  }
  return (h)
}
a0 = 0.2
a1 = 0.55
a2 = 0.119
a3 = 0.67
reg = garch_3_0(1100,a0,a1,a2,a3)
title = sprintf("График процесса. Параметры a0 = %f , a1 = %f, a2 = %f , a3 = %f  ",a0,a1,a2,a3)
plot(reg,main = title,type="l",col="blue")
garch(reg,order=c(3,0),start=c(a0,a1,a2,a3))$coef

## Задание №5 ----
a0 = 0.1
a1 = 0.1
b1 = 0.1
garch_1_1 <- function(n,a0,a1,b1){
  s = numeric(n)
  h = numeric(n)
  e=rnorm(1,0,1)
  s[1] = runif(1, min = 0, max = 1)
  h[1] = e*sqrt(s[1])
  for(i in 2:n){
    s[i] = a0+a1*h[i-1]*h[i-1]+b1*s[i-1]*s[i-1]
    e=rnorm(1,0,1)
    h[i] = e*sqrt(s[i])
  }
  return (data.frame(s,h))
}

reg = garch_1_1(1000,a0,a1,b1)
title = sprintf("График волатильности. Параметры a0 = %f , a1 = %f, b1 = %f ",a0,a1,b1)
plot(reg$s,main = title,type="l",col="blue")
title = sprintf("График процесса. Параметры a0 = %f , a1 = %f, b1 = %f  ",a0,a1,b1)
plot(reg$h,main = title,type="l",col="blue")
garch(reg$h,order = c(1,1), start = c(a0,a1,b1))




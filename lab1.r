Ar <- function(n, theta) {
  reg = numeric(n)
  reg[1] = runif(1, min = 0, max = 1)
  
  for(i in 2:n) {
    e = rnorm(1, 0, 1)
    reg[i] = reg[i - 1] * theta + e
  }
  
  return(reg)
}

## 1. Построить график процесса 𝐴𝑅(1), из 𝑛 наблюдений, для различных значений параметра θ
theta = 0.55
reg = Ar(2000,theta)
title = sprintf("theta = %f ",theta)
plot(reg,main = title ,type = c("l"), col="blue")

theta = 1
reg = Ar(2000,theta)
title = sprintf("theta = %f ",theta)
plot(reg,main = title ,type = c("l"), col="blue")

theta = -1
reg = Ar(500,theta)
title = sprintf("theta = %f ",theta)
plot(reg,main = title ,type = c("l"), col="blue")

theta = 2
reg = Ar(100,theta)
title = sprintf("theta = %f ",theta)
plot(reg,main = title ,type = c("l"), col="blue")


## 2. Оценить параметр |𝜃| ≤ 1 по методу наименьших квадратов (МНК).
n = 10000
theta = 0.55
reg = Ar(n,theta)
squaring_elem = numeric(n-1)
sum_of_previous = numeric(n-1)
for(i in 2:n){
    squaring_elem[i] = reg[i-1]*reg[i-1]
    sum_of_previous[i] = reg[i]*reg[i-1]  
}
theta_estimate_mnk = sum(sum_of_previous)/sum(squaring_elem)
sprintf("theta = %f, MNK - оценка theta = %f",theta,theta_estimate_mnk)

## 3. Найти оценку максимального правдоподобия (МП) параметра 𝜃 процесса 𝐴𝑅(1).
n = 10000
MP = function(theta) {
  MP_elems = numeric(n-1)
  for (i in 2 : n) {
    MP_elems[i] = (reg[i] - theta * reg[i-1])**2
  }
  return (sum(MP_elems))
}
theta = 0.55
reg = Ar(n,theta)
interval = c(-10,10)
tetta_estimete_mp <- optimize(f = MP, lower = min(interval), upper = max(interval),
                      maximum = FALSE)$minimum
sprintf("theta = %f, MNK - оценка theta = %f, MP - оценка theta =  %f",theta,theta_estimate_mnk,tetta_estimete_mp )


## 4. Рассчитать МНК-оценки для объема выборки 𝑘 = 10, 11, . . . , 𝑛. 
## Посмотреть на динамику оценки, в зависимости от объема выборки, и сделать выводы.
##      (a) Замоделировать процесс 𝐴𝑅(1) объема выборки 𝑛 = 1000;
n = 1000
theta = 0.55
reg = Ar(n,theta)
##      (b) Расчитать МНК-оценку по 10 наблюдениям реализованного процесса;
squaring_elem = numeric(9)
sum_of_previous = numeric(9)
for(i in 2:10){
  squaring_elem[i] = reg[i-1]*reg[i-1]
  sum_of_previous[i] = reg[i]*reg[i-1]  
}
theta_estimate_mnk_10 = sum(sum_of_previous)/sum(squaring_elem)
sprintf("theta = %f, MNK - оценка theta = %f",theta,theta_estimate_mnk_10)
##      (c) Расчитать МНК-оценку по 11 наблюдениям и т.д. для всех 𝑘 = 10,11, . . . ,𝑛;
observations = numeric(n-10)
observations[1] = theta_estimate_mnk_10
for(i in 11:n){
  squaring_elem = numeric(i-1)
  sum_of_previous = numeric(i-1)
  for(j in 2:i){
    squaring_elem[j] = reg[j-1]*reg[j-1]
    sum_of_previous[j] = reg[j]*reg[j-1]  
  }
  theta_estimate_mnk = sum(sum_of_previous)/sum(squaring_elem)
  observations[i-9] = theta_estimate_mnk
}
##      (d) Построить последовательность полученных оценок на графике.
plot(observations,type = c("l"), col="blue")

## 5. Построить график устойчивого процесса 𝐴𝑅(2), из 𝑛 наблюдений.
Ar2 <- function(n, theta1, theta2) {
  reg = numeric(n)
  reg[1] = runif(1, min = 0, max = 1)
  reg[2] = runif(1, min = 0, max = 1)
  
  for(i in 3:n) {
    e = rnorm(1, 0, 1)
    reg[i] = reg[i - 2] * theta1 + reg[i - 1] * theta2 + e
  }
  return (reg)
}
n = 1000
theta1 = 0.51
theta2 = 0.27
reg = Ar2(n, theta1, theta2)
## Для того, чтобы процесс 𝐴𝑅(2) был стационарным необходимо, чтобы корни уравнения
## удовлетворяли условию |𝜆𝑖| < 1, 𝑖 = 1, 2.
lamda1 = (theta1+sqrt(theta1*theta1 + 4*theta2))*0.5
lamda2 = (theta1-sqrt(theta1*theta1 + 4*theta2))*0.5

sprintf("Корни уравнения : lamda1 = %f, lamda2 = %f",lamda1,lamda2)
title = sprintf("theta = %f ",theta)
plot(reg,main = title ,type = c("l"), col="blue")

## 6. Вычислить значение параметра 𝐴𝑅(2), используя функцию 𝑎𝑟𝑖𝑚𝑎 пакета 𝑠𝑡𝑎𝑡
arima(reg, order = c(2, 0, 0), include.mean = FALSE)


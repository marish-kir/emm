library(stats)
library(tseries)
## 1. Реализовать 𝐴𝑅(2)𝐴𝑅𝐶𝐻(3) процесс из 𝑛 = 2100 наблюдений с значениями параметров 
##𝜃 = (−0.3, 0.4)′, 𝐴 = (1, 0.2, 0.1, 0.2)′ и построить его график.
ar_2_arch_3 <- function(n,a0,a1,a2,a3,theta1,theta2){
  s = numeric(n)
  X = numeric(n)
  s[1] = a0
  e = rnorm(1, 0, 1)
  X[1] = sqrt(s[1]) * e
  s[2] = a0 + a1*X[1]*X[1]
  e = rnorm(1, 0, 1)
  X[2] = X[1]*theta1 + sqrt(s[2]) * e
  s[3] = a0 + a1*X[2]*X[2] + a2*X[1]*X[1]
  e = rnorm(1, 0, 1)
  X[3] = X[2]*theta1 + X[1]*theta2 + sqrt(s[3]) * e
  for(i in 4:n){
    e=rnorm(1,0,1)
    s[i] = a0 + a1*X[i-1]*X[i-1] + a2*X[i-2]*X[i-2] + a3*X[i-3]*X[i-3]
    X[i] = X[i-1]*theta1 + X[i-2]*theta2 + sqrt(s[i]) * e
  }
  return (X)
}
n = 2100
a0 = 1
a1 = 0.2
a2 = 0.1
a3 = 0.2
theta1 = -0.3
theta2 = 0.4
reg = ar_2_arch_3(n,a0,a1,a2,a3,theta1,theta2)
plot(reg,type = c("l"), col="blue")
## 2. Разделить, полученную на первом шаге последовательность {𝑥𝑛}, 
## в отношении 20 : 1 на обучающую и тестовую выборки соответственно.
learn = reg[1:2000]
test = reg[2001:2100]


##3. На основе обучающей выборки получить оценки параметров 𝜃 = (𝜃1, 𝜃2)′
## и 𝐴 = (𝑎0, 𝑎1, 𝑎2, 𝑎3)′

##(a) Из условия минимизации по 𝜃 суммы квадратов невязок получаем МНК-оценку ˆ𝜃
theta_estimate = c(arima(learn, order = c(2, 0, 0), include.mean = F)$coef)
t1 = theta_estimate[1]
t2 = theta_estimate[2]
##(b) Используя полученную оценку ˆ𝜃, находим оценку 𝐴ˆ = (ˆ𝑎0, . . . , 𝑎ˆ3)′
##которая минимизирует сумму квадратов ошибок
h = numeric(1999)
h[1] = c(learn[1])
h[2] = learn[2] - t1 * learn[1]
for(i in 3:2000){
  h[i] = learn[i] - (t1*learn[i-1] + t2*learn[i-2])
}
A_estimate = garch(h, c(0, 3))
sprintf("𝜃1 = %f ",t1)
sprintf("𝜃2 = %f ",t2)
sprintf("a0 = %f ",A_estimate$coef[1])
sprintf("a1 = %f ",A_estimate$coef[2])
sprintf("a2 = %f ",A_estimate$coef[3])
sprintf("a3 = %f ",A_estimate$coef[4])

##4. Построить последовательность прогнозов на один шаг на тестовой выборке. 
## Наложить последовательность прогнозов на последовательность наблюдений процесса. 
theta_estimate = c(arima(test, order = c(2, 0, 0), include.mean = F)$coef)
t1 = theta_estimate[1]
t2 = theta_estimate[2]
prediction = numeric(100)
prediction[1] = test[1]
prediction[2] = test[2]
for(i in 3:100){
  prediction[i] = t1*test[i-1] + t2*test[i-2]
}
plot(test,type = c("l"), col="cyan", lwd = 3)
  lines(prediction,type = c("p"), col="black" )
#Оценка параметров a0,a1,a2,a3
h = numeric(99)
h[1] = c(test[1])
h[2] = test[2] - t1 * test[1]
for(i in 3:100){
  h[i] = test[i] - (t1*test[i-1] + t2*test[i-2])
}
s = numeric(99)
A = garch(h, c(0, 3))$coef  
s[1] = sqrt(A[1])
s[2] = sqrt(A[1] + A[2]*prediction[1]**2)
s[3] = sqrt(A[1] + A[2]*prediction[2]**2 + A[3]*prediction[1]**2)
for(i in 4:n){
  s[i] = sqrt(A[1] + A[2]*prediction[i-1]**2 + A[3]*prediction[i-2]**2 + A[4]*prediction[i-3]**2)
}
top = numeric(100)
bottom = numeric(100)
for(i in 1:100){
  top[i] = prediction[i]+s[i]
  bottom[i] = prediction[i]-s[i]
}
  lines(top,type = c("l"), col="red", lty = 2,xlim = c(min(top), max(top)))
  lines(bottom,type = c("l"), col="red", lty = 2, xlim = c(min(bottom), max(bottom)))

##5. Скачать с сайта https://www.finam.ru/ любые дневные котировки 
##финансовых активов или значения индексов (минимум за 3 года).
#Экспорт данных Сбербанк

##6. Импортировать скачанные данные в 𝑅, используя функцию 𝑟𝑒𝑎𝑑.𝑡𝑎𝑏𝑙𝑒();
table=read.csv2("C:/Users/radko/OneDrive/Рабочий стол/учёба/4 курс/ЭММ/SBER1.csv", sep= ";")
X = as.numeric(sub(",", ".", table$X76555100, fixed = TRUE))

##7. Построить график динамики актива;
plot(table$X227.88,type="l",col="blue")
plot(table$X224.6.1,type="l",col="blue")
plot(table$X76555100,type="l",col="blue")


##8. Привести данные к стационарному виду, используя одно из преобразований
n = length(X)
z = numeric(n-1)
z[1] = 0
for(i in 2:n){
  z[i] = (X[i]-X[i-1])/X[i-1]
}

##9. Построить график доходностей {𝑧𝑘} финансового актива;
plot(z,type="l",col="blue")

##10. Повторить шаги 2-4 для последовательности {𝑧𝑛} при предположении,
## что процесс {𝑧𝑛} описывается моделью 𝐴𝑅(2)𝐴𝑅𝐶𝐻(3).

## a. Разделить, полученную на первом шаге последовательность {𝑥𝑛}, 
## в отношении 20 : 1 на обучающую и тестовую выборки соответственно.
length(z)
length(z)/21
length(z) - 55
learn = z[1:1107]
test = z[1108:1162]
n = length(z)

## b. На основе обучающей выборки получить оценки параметров 𝜃 = (𝜃1, 𝜃2)′
## и 𝐴 = (𝑎0, 𝑎1, 𝑎2, 𝑎3)′
theta_estimate = c(arima(learn, order = c(2, 0, 0), include.mean = F)$coef)
t1 = theta_estimate[1]
t2 = theta_estimate[2]
h = numeric(1106)
h[1] = c(learn[1])
h[2] = learn[2] - t1 * learn[1]
for(i in 3:1107){
  h[i] = learn[i] - (t1*learn[i-1] + t2*learn[i-2])
}
A_estimate = garch(h, c(0, 3))
sprintf("𝜃1 = %f ",t1)
sprintf("𝜃2 = %f ",t2)
sprintf("a0 = %f ",A_estimate$coef[1])
sprintf("a1 = %f ",A_estimate$coef[2])
sprintf("a2 = %f ",A_estimate$coef[3])
sprintf("a3 = %f ",A_estimate$coef[4])

## c. Построить последовательность прогнозов на один шаг на тестовой выборке. 
## Наложить последовательность прогнозов на последовательность наблюдений процесса. 
theta_estimate = c(arima(test, order = c(2, 0, 0), include.mean = F)$coef)
t1 = theta_estimate[1]
t2 = theta_estimate[2]
prediction = numeric(54)
prediction[1] = test[1]
prediction[2] = test[2]
for(i in 3:55){
  prediction[i] = t1*test[i-1] + t2*test[i-2]
}
plot(test,type = c("l"), col="cyan", lwd = 3)
lines(prediction,type = c("p"), col="black" )
#Оценка параметров a0,a1,a2,a3
h = numeric(54)
h[1] = c(test[1])
h[2] = test[2] - t1 * test[1]
for(i in 3:55){
  h[i] = test[i] - (t1*test[i-1] + t2*test[i-2])
}
s = numeric(54)
A = garch(h, c(0, 3))$coef  
s[1] = sqrt(A[1])
s[2] = sqrt(A[1] + A[2]*prediction[1]**2)
s[3] = sqrt(A[1] + A[2]*prediction[2]**2 + A[3]*prediction[1]**2)
for(i in 4:n){
  s[i] = sqrt(A[1] + A[2]*prediction[i-1]**2 + A[3]*prediction[i-2]**2 + A[4]*prediction[i-3]**2)
}
top = numeric(55)
bottom = numeric(55)
for(i in 1:55){
  top[i] = prediction[i]+s[i]
  bottom[i] = prediction[i]-s[i]
}
lines(top,type = c("l"), col="red", lty = 2,xlim = c(min(top), max(top)))
lines(bottom,type = c("l"), col="red", lty = 2, xlim = c(min(bottom), max(bottom)))


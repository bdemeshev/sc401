require(ggplot2)

step <- 10^(-3) # шаг для броуновского движени
b <- 10 # до какого момента времени мы создаём броуновское движение

# количество точек, в которых считается
# броуновское движение
# здесь обязательно НАДО округлять (!)
nn <- round(b/step+1,0)

deltaW <- rnorm(n=nn,mean=0,sd=sqrt(step))
deltaW[1] <- 0
W <- cumsum(deltaW)
  
t <- seq(from=0,to=b,by=step)

head(t)
head(deltaW)
head(W)


qplot(t,W,geom="line")

Wfun <- function(t) {
  n <- round(t/step,0) + 1
  return(W[n])  
}



time <- seq(from=0.1,to=10,by=0.01)
y <- time*Wfun(1/time)

qplot(time,y,geom="line")



### sem 2

# параметры точности
step <- 10^(-5) # шаг для броуновского движени
n.sim <- 1000 # размер случайной выборки для 
# стохастического интеграла
b <- 1 # правая граница интервала

# количество точек, в которых считается
# броуновское движение
nn <- round(b/step+1,0)

# создаем вектор из n.sim нулей
IIto <- rep(0,n.sim)

for (j in 1:n.sim) {
  deltaW <- rnorm(n=nn,mean=0,sd=sqrt(step))
  deltaW[1] <- 0
  W <- cumsum(deltaW)
  
  # Очень важно для интеграла Ито:
  # Брать приращение правее момента, где оценивается
  # интегрируемое выражение
  # интересующий нас стохастический интеграл
  IIto[j] <- sum(W[-nn]*deltaW[-1])
}

qplot(IIto)
qplot(IIto,geom="density")
mean(IIto)
var(IIto)
sum(IIto>0)/n.sim




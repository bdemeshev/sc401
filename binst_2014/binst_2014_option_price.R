require(quantmod)
require(erer)

# скачиваем цены акций компании APPLE
prices <- getSymbols(
  Symbols="AAPL",
  from="2012-01-01",
  to="2013-02-02",
  auto.assign=FALSE) # auto.assign --- это маленькое шаманство

head(prices) # что там в этих данных
# plot(prices)


df <- data.frame(prices, t=time(prices))
head(df)

qplot(t,AAPL.Close,data=df)
qplot(t,AAPL.Close,data=df,
      geom="line")
qplot(t,AAPL.Close,data=df,
      geom="line")+stat_smooth()

# оцениваем волатильность руками:
S <- df$AAPL.Close
summary(S)
str(S)
dS <- diff(S)
ret <- dS/S[-272] # вектор доходностей
mu <- 260*mean(ret)
si2 <- 260*var(ret)

# волатильность, рассчитываемая за последние 100 дней
prices$vol <- volatility(prices, N=260, n=100)

# график изменения волатильности
df <- data.frame(prices, t=time(prices))
qplot(t,vol,data=df,geom="line")


# немного про нормальную функцию распределения
t <- seq(-5,5,by=0.05)
qplot(t,pnorm(t)) # её график

# её значения в некоторых точках
pnorm(4)
pnorm(-5)
pnorm(0)

# параметры модели
si <- sqrt(si2) # для краткости
r <- 0.001
K <- 500
T <- 1
S0 <- as.numeric(last(prices$AAPL.Close))

# применяем формулу Блэка-Шоулса
d<- (log(K)-log(S0)-
       (r-si^2/2)*T)/si/sqrt(T)
X0 <- S0*pnorm(si*sqrt(T)-d)-K*pnorm(-d)
X0  

# симуляционный подход
n.sim <- 10^5 # количество симуляций
opt.price <- rep(0,n.sim) # вектор куда потом мы занесем цену опциона для каждой симуляции

dt <- 1/260 # шаг = 1 торговый день, 1/260 года
n.points <- round(T/dt+1,0) # число дней от 0 до времени T
t <- seq(0,T,by=dt) # вектор моментов времени

for (i in 1:n.sim) {
# одна симуляция будущей траектории цен

# n.points
dW <- rnorm(n.points,0,sd=sqrt(dt)) # генерим приращения броуновского движения
dW[1] <- 0
W <- cumsum(dW) # считаем броуновское движение
# qplot(t,W)

S <- S0*exp((r-si^2/2)*t+si*W) # вектор цен акции
# qplot(t,S)


# case 1. Классический опцион колл (право купить акцию), со страйк ценой K=500
# opt.price[i] <- exp(-r*T)*max(S[n.points]-K,0)

# case 2. Опцион платит 10 долларов в момент T, если max цены акции > 500
#opt.price[i] <- exp(-r*T)*10*(max(S)>500)

# case 3. 
# Опцион платит 10 долларов в момент времени (tau),
# когда цена акции впервые достигнет 500

# if(max(S)>500) {
#   tau <- t[min(which(S>500))]
#   opt.price[i] <- exp(-r*tau)*10
# }
# else opt.price[i] <- 0

# case 4. "Han's option"
# опцион платит 1 доллар каждый день,
# пока цена акции превышает 500

if(max(S)>500) {
   taus <- t[S>500]
   opt.price[i] <- sum(exp(-r*taus)*1)
}
else opt.price[i] <- 0

}

mean(opt.price)


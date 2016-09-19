

T <- 2 # правый край времени
n <- 1000 # разбиваем время на n кусочков

dt <- T/n # длина одного интервальчика времени
dt

dW <- rnorm(n, mean = 0, sd = sqrt(dt))
dW # случайный вектор независимых нормальных величин

W <- cumsum(dW) # накопленная сумма

1:10 # числа подряд от 1 до 10
t <- (1:n) * dt
t # вектор: (dt, 2dt, 3dt, ..., ndt)

# параметры модели:
S0 <- 100
mu <- 0.1
sigma <- 0.25
r <- 0.02

library("ggplot2")

qplot(x = t, y = W)
qplot(x = t, y = W, geom = "line")

S <- S0 * exp( (r - sigma^2/2) * t + sigma * W)

qplot(x = t, y = S, geom = "line")

max_S <- max(S)
max_S



n_sim <- 50000

for (i in 1:n_sim) {
  cat("Привет, Люба, это число ", i, ":)  \n")
}


all_max_S <- rep(NA, n_sim)

for (i in 1:n_sim) {
  dW <- rnorm(n, mean = 0, sd = sqrt(dt))
  W <- cumsum(dW) # накопленная сумма
  S <- S0 * exp( (r - sigma^2/2) * t + sigma * W)
  all_max_S[i] <- max(S)
}

all_max_S

X2 <- ifelse(all_max_S > 150, 100, 0)
X2

X0 <- exp(-r * T) * mean(X2)
X0

# оценка mu и sigma по реальным данным
library("quantmod")
# library("finam")
# Tools - Install packages to install package

getSymbols("GOOG", from = "2015-01-01",
           to = "2016-02-01", source = "google")

G <- GOOG$GOOG.Adjusted
G
y <- diff(log(G))
model <- lm(y~1)
summary(model)



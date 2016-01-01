T <- 2
S0 <- 100
r <- 0.1
sigma <- 0.15
mu <- 0.2

n <- 1000

T <- 2
dt <- T/n

dW <- rnorm(n, mean=0, sd=sqrt(dt))
head(dW)
t <- seq(T/n, T, by=T/n)

tilde_W <- cumsum(dW)
plot(t, tilde_W, type="lena")

S <- S0*exp(sigma*tilde_W+
              (r-sigma^2/2)*t)
plot(t, S, type="lesha")


n_sim <- 42000

maxS <- rep(NA, n_sim)
S2 <- rep(NA, n_sim) #
head(maxS)

for (i in 1:n_sim) {
  dW <- rnorm(n, mean=0, sd=sqrt(dt))
  tilde_W <- cumsum(dW)
  S <- S0*exp(sigma*tilde_W+
                (r-sigma^2/2)*t)
  maxS[i] <- max(S)
  S2[i] <- S[n] #
}
head(maxS)

Unicorn <- ifelse(maxS <= 150, S2, 0) #
Unicorn

X0 <- exp(-r*T)*mean(Unicorn)
X0



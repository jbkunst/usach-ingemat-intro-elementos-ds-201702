
N <- 50
PHI <- 1
y <- rep(NA, N)


# y_t = phi y_t-1 + e_t
y[1] <- 0

for(i in 2:N) {
  message("voy en el ", i)
  y[i] <- PHI * y[i-1] + rnorm(1)
}

y
plot(y, type = "l")

# Si uno repite el cdodigo mas de una vez, entonces hay que hacerlo function

sim_ar_1 <- function(PHI = 1, N = 50) {
  
  y <- rep(NA, N)
  # y_t = phi y_t-1 + e_t
  y[1] <- 0
  for(i in 2:N) {
    message("voy en el ", i)
    y[i] <- PHI * y[i-1] + rnorm(1)
  }
  plot(y, type = "l")
  
  y
  
}

sim_ar_1(15, 5)


y <- sim_ar_1(0.99999999999999999999999999999999999999999999999999, 500)
mean(y)

y <- sim_ar_1(0, 500)
mean(y)


#  que paza oe zi?
sim_ar_1(+0.9, 100)
sim_ar_1(-0.9, 100)


y <- arima.sim(n = 100, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
plot(y)


e <- rnorm(100)

set.seed(123)
y <- arima.sim(n = 100, list(ar = c(0.9, -0.001)), innov = e)
plot(y)

set.seed(123)
y <- arima.sim(n = 100, list(ar = c(0.9)), innov = e)
plot(y)

y <- sim_ar_1(-0.9, 1000)
y

dplyr::lag(y)

cor(y, dplyr::lag(y))

plot(y[-1], dplyr::lag(y)[-1])
cor(y[-1], dplyr::lag(y)[-1])


y <- sim_ar_1(-0.9, 100)
y

plot(y[-c(1,2)], dplyr::lag(y, n = 2)[-c(1,2)])
cor(y[-c(1,2)], dplyr::lag(y, n = 2)[-c(1,2)])

plot(y[-c(1,2,3)], dplyr::lag(y, n = 3)[-c(1,2,3)])
cor(y[-c(1,2,3)], dplyr::lag(y, n = 3)[-c(1,2,3)])


acf(y)

# a lo entrete 
plot(AirPassengers)
class(AirPassengers)

plot(AirPassengers)
plot(log(AirPassengers))

#  para la otra
# MASS::boxcox(as.numeric(AirPassengers))
x <- (AirPassengers)
plot(x)

x <- log(AirPassengers)
t <- 1:length(x)
df <- data.frame(
  x = as.numeric(x), t, t2 = t*t
)
head(df)
modlm <- lm(x ~ t + t2, data = df)
           
library(tidyverse)

df <- df %>% 
  mutate(xest = as.numeric(predict(modlm)))
str(df)

ggplot(df) +
  geom_line(aes(t, x)) +
  geom_line(aes(t, xest))

df <- df %>% 
  mutate(xres = x - xest)

ggplot(df) +
  geom_line(aes(t, xres))

acf(df$xres)

#  cual modelo arma
library(forecast)

mod <- auto.arima(df$xres, max.p = 12, max.q = 12)
mod

N <- 12

f <- forecast(mod, h = N)
plot(f)
autoplot(f)
xresest <- as.numeric(f$mean)

dfp <- data.frame(
  t = 144  + 1:12,
  xresest
)
dfp <- dfp %>% 
  mutate(t2 = t*t)

dfp <- dfp %>% 
  mutate(xpredtend = predict(modlm, newdata = dfp))

str(df)

dfp <- dfp %>% 
  mutate(xestfinal = xpredtend + xresest)

ggplot(df) +
  geom_line(aes(t, x)) +
  geom_line(aes(t, xest)) +
  geom_line(aes(t, xpredtend), data = dfp, color = "darkred", size = 3) + 
  geom_line(aes(t, xestfinal), data = dfp, color = "blue")


df2 <- df %>% 
  mutate_at(vars(starts_with("x")), exp)

dfp2 <- dfp %>% 
  mutate_at(vars(starts_with("x")), exp)

ggplot(df2) +
  geom_line(aes(t, x)) +
  geom_line(aes(t, xest)) +
  geom_line(aes(t, xpredtend), data = dfp2, color = "darkred", size = 3) + 
  geom_line(aes(t, xestfinal), data = dfp2, color = "blue")


mods <- forecast(AirPassengers)
mods
ets
autoplot(forecast(AirPassengers))

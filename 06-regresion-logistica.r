# packages ----------------------------------------------------------------
rm(lst = ls())
library(tidyverse)

theme_set(theme_minimal())

# data --------------------------------------------------------------------
n <- 1000

# supongamos x1: cantidad de globulos rojos, x2: nivel felicidad
df <- data_frame(
  x1 = rnorm(n),
  x2 = 2 + 3 * rnorm(n)
)

ggplot(df) + 
  geom_point(aes(x1, x2))

# supongamos que tb medimos si esta enfermo o no
df <- df %>% 
  mutate(
    y = x1 > (x2 - 2)/3 + rnorm(n),
    y = as.numeric(y)
    )

ggplot(df) + 
  geom_point(aes(x1, x2, color = y))

#  como lo hariamos en regresion lineal?
ggplot(df) + 
  geom_point(aes(x1, y, color = y))

# como solucionar lo superpuesto, visualmente
ggplot(df) + 
  geom_point(aes(x1, y, color = y), alpha = 0.15)

ggplot(df) + 
  geom_jitter(aes(x1, y, color = y), alpha = 0.25, height = 0.0565)

last_plot() +
  geom_smooth(aes(x1, y), method = "lm")

last_plot() +
  geom_smooth(aes(x1, y), method = "loess", color = "red")

#  buscar una funcion que tenga recorrido entre 0 y 1
dfs <- data_frame(
  s = seq(-5, 5, length.out = 101) 
)

ggplot(dfs) + 
  geom_line(aes(s, sin(s)))

ggplot(dfs) + 
  geom_line(aes(s, abs(sin(s))))

ggplot(dfs) + 
  geom_line(aes(s,  1 / (1 + exp(s))))

ggplot(dfs) + 
  geom_line(aes(s,  1 / (1 + exp(-s))))

ggplot(dfs) + 
  geom_line(aes(s,  1 / (1 + exp(-(3 - 4*s)))))

s2 <- seq(-10, 10, length.out = 6) %>% 
  c(0) %>% 
  sort()
s2

dfsim <- expand.grid(x = s, b0 = s2, b1 = s2) %>% 
  tbl_df() %>% 
  mutate(y = 1 / (1 + exp(-(b0 + b1 * x))))
dfsim

ggplot(dfsim) +
  geom_line(aes(x, y)) +
  facet_grid(b0 ~ b1) + 
  theme(axis.text = element_blank())

last_plot() +
  geom_point(data = df, aes(x1, y, color = y))

m1 <- glm(y ~ x1, data = df, family = binomial(link = "logit"))
c <- coefficients(m1)

dfs
ggplot() +
  geom_jitter(aes(x1, y, color = y), data = df, height = 0.0456, alpha = 0.1)  +
  geom_line(aes(s, 1 / (1 + exp(-(c[1] + c[2]*s)))), data = dfs)

# y x2?
ggplot(df) + 
  geom_point(aes(x2, y, color = y))

m2 <- glm(y ~ x1 + x2, data = df, family = binomial(link = "logit"))
m2
c <- coefficients(m2)
c

library(plotly)
plot_ly() %>% 
  add_markers(data = df, x = ~x1, y = ~x2, color = ~y)

dfsurface <- expand.grid(
  x1 = seq(min(df$x1), max(df$x1), length.out = 50),
  x2 = seq(min(df$x2), max(df$x2), length.out = 50)
)  %>% 
  tbl_df()
  
dfsurface <- dfsurface %>% 
  mutate(
    # p = 1 / (1 + exp(-(c[1] + c[2]*x1 +  c[3]*x2))),
    # pl = c[1] + c[2]*x1 +  c[3]*x2,
    # p1 = predict(m2, newdata = .),
    p = predict(m2, newdata = ., type = "response"),
    p1 = predict(m1, newdata = ., type = "response")
  )

ggplot(dfsurface) +
  geom_point(aes(x1, x2, color = p))

ggplot(dfsurface) +
  geom_point(aes(x1, x2, color = p1))

dfsurfacematrixp <- dfsurface %>%
  select(-p1) %>% 
  spread(x1, p) %>% 
  select(-x2) %>% 
  as.matrix()

plot_ly() %>% 
  add_markers(data = df, x = ~x1, y = ~x2, z = ~y, color = ~y) %>% 
  add_surface(
    x = sort(unique(dfsurface$x1)),
    y = sort(unique(dfsurface$x2)),
    z = dfsurfacematrixp
    )

dfsurfacematrixp1 <- dfsurface %>%
  select(-p) %>% 
  spread(x1, p1) %>% 
  select(-x2) %>% 
  as.matrix()

plot_ly() %>% 
  add_markers(data = df, x = ~x1, y = ~x2, z = ~y, color = ~y) %>% 
  add_surface(
    x = sort(unique(dfsurface$x1)),
    y = sort(unique(dfsurface$x2)),
    z = dfsurfacematrixp1
  )

mean(m1$residuals)
mean(m2$residuals)


# evaluar -----------------------------------------------------------------
dfr <- df

dfr <- dfr %>% 
  mutate(
    p1 = predict(m1, type = "response"),
    p2 = predict(m2, type = "response")
    )

ggplot(dfr) +
  geom_density(aes(p1))

ggplot(dfr) +
  geom_density(aes(p1, group = y, fill = y), alpha = .75)


ggplot(dfr) +
  geom_density(aes(p2, group = y, fill = y), alpha = .75)

# KS Kolmogorov Smirnoff Ice Daniels
x <- dfr$p1

plot(density(x))

plot(ecdf(x))

plot(ecdf(1:10))
plot(ecdf(c(1:10, 3)))
plot(ecdf(c(1:10, 3, 3)))

n <- 666+2

plot(density(rnorm(n)))
fsane <- ecdf(rnorm(n))

plot(fsane)

plot(ecdf(runif(n)))
plot(ecdf(rexp(n)), add  = TRUE)


plot(ecdf(rexp(n)))
plot(ecdf(runif(n)), add  = TRUE)


# las probabilidades del modelo 1 cuando el paciente es enfermo
pe <- dfr$p1[dfr$y == 0]
ps <- dfr$p1[dfr$y == 1]
ks.test(pe, ps)

plot(ecdf(pe))
plot(ecdf(ps), add = TRUE)


plot(density(pe))
plot(density(ps), add = TRUE)



pe <- dfr$p2[dfr$y == 0]
ps <- dfr$p2[dfr$y == 1]

plot(ecdf(pe))
plot(ecdf(ps), add = TRUE)


plot(density(pe))
plot(density(ps), add = TRUE)

ks.test(pe, ps)

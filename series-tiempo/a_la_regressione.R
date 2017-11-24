# packages ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(lubridate)
library(viridis)

# data --------------------------------------------------------------------
# separar train/entrenamiento/modelo y test/validacion
t <- AirPassengers
plot(t)

ntest <- 24

ttrain <- head(t, length(t) - ntest)
ttrain
plot(ttrain)

ttest <- tail(t, ntest)
ttest
plot(ttest)

# mod facil/forecast ------------------------------------------------------
library(forecast)

fcts <- forecast(ttrain, h = ntest)
names(fcts)
fcts$model

pred <- fcts$mean
pred
#  comparar
ttest

# mod a lo regression -----------------------------------------------------
df <- data_frame(
  y = as.vector(ttrain),
  year = rep(1949:1958, each = 12),
  month = rep(month.abb, times = length(1949:1958)),
  monthn = rep(1:12, times = length(1949:1958)),
  dt = ymd(paste0(year, "-", monthn, "-", 1)),
  datetime = as.numeric(dt)
)

df

modr1 <- lm(y ~ year, data = df)

df <- df %>% 
  mutate(
    modr1 = predict(modr1)
  )
df

ggplot(df) +
  geom_line(aes(dt, y)) +
  geom_line(aes(dt, modr1)) 


ggplot(df) +
  geom_line(aes(dt, y - modr1))


modr2 <- lm(y ~ year + month, data = df)

df <- df %>% 
  mutate(
    modr2 = predict(modr2)
  )
df

ggplot(df) +
  geom_line(aes(dt, y)) +
  geom_smooth(aes(dt, y), method = "lm") +
  geom_line(aes(dt, modr2)) 

ggplot(df) +
  geom_line(aes(dt, y - modr2))


dftest <- data_frame(
  y = as.vector(ttest),
  year = rep(1959:1960, each = 12),
  month = rep(month.abb, times = length(1959:1960)),
)

dftest <- dftest %>% 
  mutate(
    modf = pred,
    modr2 = predict(modr2, newdata = dftest),
    monthn = rep(1:12, times = length(1959:1960)),
    dt = ymd(paste0(year, "-", monthn, "-", 1))
  )

ggplot(dftest, aes(dt)) +
  geom_line(aes(y = y)) +
  geom_line(aes(y = modf)) +
  geom_line(aes(y = modr2)) 

dftest %>% 
  select(dt, y, modf, modr2) %>% 
  gather(tipo, valor, -dt) %>% 
  ggplot() +
  geom_line(aes(dt, valor, group = tipo, color = tipo), size = 1) +
  scale_color_viridis(discrete = TRUE)

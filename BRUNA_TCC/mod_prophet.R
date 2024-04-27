# Install the library
#install.packages("prophet")
install.packages("TSstudio")
install.packages("imputeTS")

# Load the library
library(prophet)
library(tidymodels)
library(tidyverse)
library(imputeTS)
library(feasts)
library(TSstudio)

# Letura da base de dados

d_obit <- read.csv("Obitos_maternos_2014_2021.csv", encoding = "UTF-8")  #Leitura da base de dados SIM
d_obit$data_obito <- as.Date(d_obit$data_obito, tryFormats = "%d/%m/%Y")

d_obit <- d_obit %>% 
  select(data_obito) %>% 
  group_by(data_obito) %>% 
  summarise(y = n()) %>% 
  rename(ds = data_obito) %>%
  ungroup() %>% 
  arrange(ds)

# sinasc (2014-2021)
d_sinasc <- read.csv2("Nascimentos_2014_2021.csv", encoding = "UTF-8")

d_sinasc <- d_sinasc |> 
  mutate(data_nasc = as.Date(data_nasc, tryFormats = "%d/%m/%Y")) |> 
  rename(nascidos = obitos) |> 
  select(2, 4, 7) |>
  group_by(data_nasc) |> 
  summarise(nasc = sum(nascidos)) |> 
  ungroup()

# sim + sinasc

d_sim_sinasc <- d_obit |> 
  left_join(
    d_sinasc, 
    by = c("ds"="data_nasc")) |>
    mutate(rmm = (y*100000)/nasc) |>
    select(1,4) |>
    rename(y = rmm)

## Vendo se há datas faltantes

# datas_dia <- seq(from=as.Date("2014-01-01"), to=as.Date("2021-12-31"), by="day") %>%
#   as.data.frame() %>% 
#   rename(ds = ".")
# 
# df <- left_join(datas_dia, d_obit)
# sum(is.na(df$y)) # 25 missing values
# which(is.na(df$y))

# Imputando valores para os missing value

#summary(df_fit$y)
#summary(df_prev$y)

## Interpolação linear
# Linear Interpolation
#df_int <- na_interpolation(df, option = "linear")
#df_int <- d_obit

# Separando a base em treino e teste

d_train <- d_sim_sinasc %>%
  filter(year(ds)<2019)

d_test <- d_sim_sinasc %>%
  filter(year(ds) < 2020)

d_prev <- d_sim_sinasc %>%
  filter(year(ds) >= 2020)

# Ajustando modelo
Fit_1 <- prophet(d_train, daily.seasonality = TRUE, yearly.seasonality=TRUE)

# Obtendo dados para previsão
Future1 <- make_future_dataframe(Fit_1, periods = 365)
tail(Future1)

Forecast1 <- predict(Fit_1, Future1)
tail(Forecast1[c("ds", "yhat", "yhat_lower", "yhat_upper")])

tail(d_test)

Forecast1$ds <- as.Date(Forecast1$ds)
fpred <- tsibble::as_tsibble(Forecast1, index = ds)
d_test <- tsibble::as_tsibble(d_test, index = ds)

# Get the residuals

fpred <- d_test %>%
  left_join(fpred, by = "ds")
fpred$resid <- fpred$y - fpred$yhat

fpred <- tsibble::as_tsibble(fpred, index = ds)
fpred %>%
  features(y, unitroot_kpss)  # Teste de raiz única rejeitou H0 e a série é não estacionária

ggplot(fpred, aes(ds, resid))+ 
  geom_line() + 
  geom_smooth() + 
  ggtitle("Resíduos Prophet",subtitle = paste0("RMSE: ", round(sqrt(mean(fpred$resid^2)), 2)))

fpred %>%
  features(resid, unitroot_kpss) # Teste de raiz única nos resíduos não rejeitou H0 e a série de resíduos é estacionária, embora um p-valor baixo

dyplot.prophet(Fit_1, Forecast1)

fpred %>% 
  ggplot(aes(ds,y)) +
  geom_line(color = "blue") +
  geom_line(aes(y=yhat),color="red") +
  geom_line(aes(y=yhat_lower),color="red",linetype = 2, alpha = 0.5)+
  geom_line(aes(y=yhat_upper),color="red",linetype = 2, alpha = 0.5) +
  labs(title = "Óbitos Maternos",
       x = "Data",
       y = "Óbitos")

dyplot.prophet(Fit_1, Forecast1)


# Plot dos componentes tendência, sazonalidade semanal e anual 
prophet_plot_components(Fit_1, Forecast1)
teste <- fpred %>% 
  filter(ds >= 2019 & ds <2020) %>% 
  select(yhat,y)

accuracy(teste$yhat,teste$y)

## Acrescentar os anos pandemicos




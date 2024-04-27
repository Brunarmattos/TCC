# Modelo Poisson -- Mesma base do xgboost

#dados
library(tidyverse)
library(tidyr)

#Modelo
library(tidymodels)
library(MASS)
library(AER)
library(statmod)

# dados -------------------------------------------------------------------

# sim (2014-2021)
d_sim <- read.csv("Obitos_maternos_2014_2021.csv", encoding = "UTF-8")  

# sinasc (2014-2021)
d_sinasc <- read.csv2("Nascimentos_2014_2021.csv", encoding = "UTF-8")


# manipulacao -------------------------------------------------------------

# sim

d_sim <- d_sim |> 
  filter(raca_cor != "Ignorado") |> 
  filter(estado_civil != "Ignorado") |> 
  filter(tipo_de_morte_materna != "Não especificada") |> 
  mutate(
    data_obito = as.Date(data_obito, tryFormats = "%d/%m/%Y"),
    tipo_de_morte_materna = factor(tipo_de_morte_materna),
    estado_civil = factor(estado_civil),
    raca_cor = factor(raca_cor),
    escolaridade = factor(escolaridade)
  ) |> 
  dplyr::select(data_obito,raca_cor,estado_civil,escolaridade,ocor_sigla_uf,idade,tipo_de_morte_materna)

# sinasc

d_sinasc <- d_sinasc |> 
  mutate(data_nasc = as.Date(data_nasc, tryFormats = "%d/%m/%Y")) |> 
  rename(nascidos = obitos) |> 
  dplyr::select(data_nasc, nasc_sigla_uf,nascidos)


# uniao de bases: sim + sinasc --------------------------------------------

d_sim_agr <- d_sim |> 
  group_by(
    year(data_obito), month(data_obito), ocor_sigla_uf, tipo_de_morte_materna 
    #raca_cor, estado_civil, escolaridade
  ) |> 
  summarise(
    obitos = n()
    #med_idades = mean(idade)
  ) |> 
  ungroup()

d_sinasc_agr <- d_sinasc |> 
  group_by(year(data_nasc), month(data_nasc), nasc_sigla_uf) |> 
  summarise(nasc = sum(nascidos)) |> 
  ungroup()

# sim + sinasc

d_sim_sinasc <- d_sim_agr |> 
  left_join(
    d_sinasc_agr, 
    by = c("year(data_obito)"="year(data_nasc)", 
           "month(data_obito)"="month(data_nasc)", 
           "ocor_sigla_uf"="nasc_sigla_uf")
  ) |> 
  rename(Ano_obito = `year(data_obito)`, Mes_obito = `month(data_obito)`)


# xgb: selecao de bases ---------------------------------------------------

# dados para ajuste
d_ajus <- d_sim_sinasc |> 
  filter(Ano_obito <= 2019) |> 
  drop_na() #2.485 observacoes

# dados real
d_real <- d_sim_sinasc |> 
  filter(Ano_obito > 2019) |> 
  drop_na() #941 observacoes


# xgb: treino e teste -----------------------------------------------------

set.seed(123)

d_split <- d_ajus |>
  initial_split(.75)

# treino
d_trein <- training(d_split)

# teste
d_teste <- testing(d_split)

# Modelo Poisson ---------------------------------------

mod_poi <- glm(obitos ~ Mes_obito + ocor_sigla_uf + tipo_de_morte_materna, data = d_trein, family = "poisson")
summary(mod_poi)

par(mfrow = c(2, 2))
plot(mod_poi)

# Diagnóstico do modelo
envelope <- function(modelo) {
  dados <- na.omit(modelo$data)
  nsim <- 100
  n <- modelo$df.null + 1
  r1 <- sort(rstandard(modelo, type = "deviance"))
  m1 <- matrix(0, nrow = n, ncol = nsim)
  a2 <- simulate(modelo, nsim = nsim)
  
  for (i in 1:nsim) {
    dados$y <- a2[, i]
    aj <- update(modelo, y ~ ., data = dados)
    m1[, i] <- sort(rstandard(aj, type = "deviance"))
  }
  
  li <- apply(m1, 1, quantile, 0.025)
  m <- apply(m1, 1, quantile, 0.5)
  ls <- apply(m1, 1, quantile, 0.975)
  
  quantis <- qnorm((1:n - 0.5)/n)
  
  plot(rep(quantis, 2), c(li, ls), type = "n",
       xlab = "Percentil da N(0,1)",
       ylab = "Resíduos")
  title("Gráfico Normal de Probabilidades")
  lines(quantis, li, type = "l")
  lines(quantis, m, type = "l", lty = 2)
  lines(quantis, ls, type = "l")
  points(quantis, r1, pch = 16, cex = 0.75)
}


par(mfrow = c(1, 1))
envelope(mod_poi)


# Teste de Sobredispersão

summary(mod_poi)$deviance/summary(mod_poi)$df.residual # Indicativo de Subdispersão

AER::dispersiontest(mod_poi)

mean(d_ajus$obitos)
hist(d_ajus$obitos)


dados_pois <- rpois(n=nrow(d_ajus),lambda = mean(d_ajus$obitos))
hist(dados_pois)

# Resíduos quantilicos ----------------

par(mfrow=c(1,2))
res <- statmod::qresiduals(mod_poi)

plot(res, ylab = "Resíduos Quantílico")

qqnorm(res, pch = 19, cex=1.2)
qqline(res, col = 2)

shapiro.test(res) # Residuo não segue distribuição normal

par(mfrow=c(1,1))
hnp::hnp(mod_poi, print.on = T)


# Predicoes --------------------
pred <- predict(mod_poi, d_teste) 

predicoes <- data.frame(truth = d_teste$obitos, 
                        estimate = pred)
avaliacao <- data.frame(RMSE = rmse(predicoes, truth, estimate)$.estimate,
                        MAPE = mape(predicoes, truth, estimate)$.estimate,
                        MAE = mae(predicoes, truth, estimate)$.estimate,
                        RSQ = rsq(predicoes, truth, estimate)$.estimate
)

avaliacao<-t(avaliacao)
avaliacao


# Excesso de mortalidade --------------------------------

esperado <- predict(mod_poi, d_real)

excesso.poi <- data.frame(observado = d_real$obitos, 
                          esperado = esperado
)

excesso.poi["excesso"] = excesso.poi$observado - excesso.poi$esperado
excesso.poi["p-score"] = round(excesso.poi$excesso/excesso.poi$esperado, 2)


# Testes -------------------

## Teste para o Excesso

t.test(excesso.poi$excesso, mu=0) # IC[2.870692 ; 3.478268]

## Teste para o Excesso

t.test(excesso.poi$`p-score`, mu=0) # IC[4.48906; 11.14280]



#  Modelo GAM: Obitos ~ Mes + UF + Tipo de Morte

#dados
library(tidyverse)
library(tidyr)
#ml
library(mgcv)
library(ModelMetrics)
library(MLmetrics)

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
  select(3:6, 12, 13, 15)

# sinasc

d_sinasc <- d_sinasc |> 
  mutate(data_nasc = as.Date(data_nasc, tryFormats = "%d/%m/%Y")) |> 
  rename(nascidos = obitos) |> 
  select(2, 4, 7)


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


saveRDS(d_sim_sinasc, "sim_sinasc_agr_14-21.rds")


# xgb: selecao de bases ---------------------------------------------------

# dados para ajuste
d_ajus <- d_sim_sinasc |> 
  filter(Ano_obito <= 2019) |> 
  drop_na() #2.137 observacoes

# dados real
d_real <- d_sim_sinasc |> 
  filter(Ano_obito > 2019) |> 
  drop_na() #941 observacoes

# xgb: treino e teste -----------------------------------------------------

set.seed(123)

d_split <- d_ajus |>
  rsample::initial_split(.75)

# treino
d_trein <- rsample::training(d_split)

# teste
d_teste <- rsample::testing(d_split)

# Observando as relações entre as variáveis

ggplot(data = d_ajus)+
  geom_point(aes(x=nasc, y= obitos))

ggplot(data = d_ajus)+
  geom_point(aes(x=Mes_obito, y= obitos))


# Modelo 1 ------------------------------

gam1 <- gam(obitos ~ s(Mes_obito) + s(nasc) + tipo_de_morte_materna, family = quasipoisson, method = 'REML', data = d_trein) 
summary(gam1)

ggplot(data.frame(Fitted = fitted(gam1), 
                  Resid = resid(gam1)),
       aes(Fitted, Resid)) + geom_point()

par(mfrow = c(1, 2))
plot(gam1)

par(mfrow = c(2, 2))
gam.check(gam1)

# Modelo 2 ------------------------------ Melhor!

gam2 <- gam(obitos ~ s(Mes_obito) + s(nasc) + ocor_sigla_uf + tipo_de_morte_materna, family = quasipoisson, method = 'REML', data = d_trein) 
summary(gam2)


ggplot(data.frame(Fitted = fitted(gam2), 
                  Resid = resid(gam2)),
       aes(Fitted, Resid)) + geom_point()

par(mfrow = c(1, 2))
plot(gam2)

par(mfrow = c(2, 2))
gam.check(gam2)

# Modelo 3 ------------------------------

gam3 <- gam(obitos ~ s(Mes_obito) + s(nasc) + ocor_sigla_uf, family = quasipoisson, method = 'REML', data = d_trein) 
summary(gam3)

ggplot(data.frame(Fitted = fitted(gam3), 
                  Resid = resid(gam3)),
       aes(Fitted, Resid)) + geom_point()

par(mfrow = c(1, 2))
plot(gam3)

par(mfrow = c(2, 2))
gam.check(gam3)



# Predicoes -------------------- Modelo 2 -----------------------
pred <- predict(gam2, d_teste) 

predicoes <- data.frame(truth = d_teste$obitos, 
                        estimate = pred)
avaliacao <- data.frame(RMSE = rmse(predicoes$truth, predicoes$estimate),
                        MAPE = MAPE(predicoes$estimate,predicoes$truth),
                        MAE = mae(predicoes$truth, predicoes$estimate),
                        RSQ = yardstick::rsq(predicoes, truth,estimate)$.estimate
)

avaliacao<-t(avaliacao)
avaliacao

gam2$deviance # 1130.146
gam2$df.null-27 -2.232 - 3.011 # 1829.757

# Excesso de mortalidade --------------------------------

esperado <- predict(gam2, d_real)

excesso.gam <- data.frame(observado = d_real$obitos, 
                        esperado = esperado
                        )

excesso.gam["excesso"] = excesso.gam$observado - excesso.gam$esperado
excesso.gam["p-score"] = round(excesso.gam$excesso/excesso.gam$esperado, 2)

excesso.gam

(excesso.gam)

# Testes -------------------

## Teste para o Excesso

t.test(excesso.gam$excesso, mu=0) # I.C [2.941265, 3.552889] mu = 3.247077 

## Teste para o Excesso

t.test(excesso.gam$`p-score`, mu=0) # IC [-2.936594, 17.524946]

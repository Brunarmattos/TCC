## Pacotes ###

library(tidyverse)
library(tidymodels)
library(xgboost)


## LEITURA DA BASE DE DADOS --------------------------
dados_obitos <- read.csv("Obitos_maternos_2014_2021.csv", encoding = "UTF-8")  #Leitura da base de dados SIM
dados_nasc <- read.csv2("Nascimentos_2014_2021.csv",encoding = "UTF-8") # Leitura da base do dados SINASC
dados_nasc <- rename(dados_nasc, nascidos = obitos) # ACERTANDO NOME DA COLUNA NÚMERO DE NASCIDOS

## Ajeitando as covariáveis da base

dados_obitos$data_obito <- as.Date(dados_obitos$data_obito, tryFormats = "%d/%m/%Y")
dados_nasc$data_nasc <- as.Date(dados_nasc$data_nasc, tryFormats = "%d/%m/%Y")

dados_obitos <- dados_obitos %>%
  filter(raca_cor != "Ignorado")
dados_obitos <- dados_obitos %>%
  filter(estado_civil != "Ignorado")
dados_obitos <- dados_obitos %>%
  filter(tipo_de_morte_materna != "Não especificada")


dados_obitos$tipo_de_morte_materna <- factor(dados_obitos$tipo_de_morte_materna)
dados_obitos$estado_civil <- factor(dados_obitos$estado_civil)
dados_obitos$raca_cor<-factor(dados_obitos$raca_cor)
dados_obitos$escolaridade <- factor(dados_obitos$escolaridade)


## ABAIXO Mudar ao ivés de colocar o indice da variável coloar o nome dela
dados_obitos <- dados_obitos[,c(3,4,5,6,12,13,15)]# SELEÇÃO DAS VARIÁVEIS NA BASE SIM
dados_nasc<- dados_nasc[,c(2,4,7)] # SELEÇÃO DAS VARIÁVEIS NA BASE SINASC

## AGREGANDO AS BASES DE NASCIDOS E ÓBITOS PELO DIA E ESTADO
dados_nasc_agr <-  dados_nasc %>%
  group_by(year(data_nasc),month(data_nasc),nasc_sigla_uf) %>%
  summarise(nasc=sum(nascidos)) %>% 
  ungroup()# 8(anos) x 12(meses) x 27(UF's) = 2592 combinações

dados_obitos_agr <-  dados_obitos %>%
  group_by(year(data_obito),month(data_obito),ocor_sigla_uf,tipo_de_morte_materna,raca_cor, estado_civil,escolaridade) %>%
  summarise(obitos = n(),
            med_idades = mean(idade)) %>%
  ungroup()

dados <- left_join(dados_obitos_agr,dados_nasc_agr, by = c("year(data_obito)" = "year(data_nasc)", "month(data_obito)"= "month(data_nasc)", "ocor_sigla_uf"="nasc_sigla_uf"))
dados <- rename(dados, Ano_obito = `year(data_obito)`)
dados <- rename(dados, Mes_obito = `month(data_obito)`)

Norte <- c("AM", "RR", "AP", "PA", "TO", "RO", "AC")
Nordeste <- c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA")
Centro_Oeste <- c("MT", "MS", "GO","DF")
Sudeste <- c("SP", "RJ", "ES", "MG")
Sul <- c("PR", "RS", "SC")

dados <- dados %>%
  mutate(regiao = case_when(ocor_sigla_uf %in% Norte ~ "Norte",
                            ocor_sigla_uf %in% Nordeste ~ "Nordeste",
                            ocor_sigla_uf %in% Centro_Oeste ~ "Centro Oeste",
                            ocor_sigla_uf %in% Sudeste ~ "Sudeste",
                            ocor_sigla_uf %in% Sul ~ "Sul"))
dados$regiao <- factor(dados$regiao)

# Os dados utlizados para construção dos modelos (tanto base de treino quanto de validação 
# constarão apenas o período pré pandemico)

dados_mod <- dados %>%
  filter(Ano_obito <= 2019)
dados_mod <- dados_mod %>%
  mutate(linha = row_number())
dados_mod <- na.omit(dados_mod)
dados_predicao <- dados %>%
  filter(Ano_obito > 2019)
dados_predicao <- na.omit(dados_predicao)

#-----------------------------------------------------

# Separando a base de validação
dados_valid <- dados_mod %>%
  slice_sample(prop=0.25)

dados_mod <- dados_mod %>%
  anti_join(dados_valid, by="linha")

dados_valid <- dados_valid %>%
  select(-c(ocor_sigla_uf,linha,nasc,Ano_obito))

dados_mod <- dados_mod %>%
  select(-c(ocor_sigla_uf,linha,nasc,Ano_obito))

# base treino e teste 
set.seed(123)
dados_mod_initial_split <- dados_mod %>% initial_split(3/4)

dados_mod_train <- training(dados_mod_initial_split)
dados_mod_test <- testing(dados_mod_initial_split)


# xgb: validacao cruzada --------------------------------------------------

set.seed(456)

xgb_folds <- vfold_cv(dados_valid, v = 10, repeats = 3)


# xgb: especificacao do modelo --------------------------------------------

xgb_espec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     
  sample_size = tune(), mtry = tune(),        
  learn_rate = tune(),                        
) |> 
  set_engine("xgboost", objective = "reg:squarederror") |>  
  set_mode("regression")

#xgb_espec

# xgb: grid search --------------------------------------------------------

xgb_grid <- grid_max_entropy(
  tree_depth(), min_n(),
  loss_reduction(), sample_size = sample_prop(),
  finalize(mtry(), dados_valid),
  learn_rate(), size = 20
)

# xgb: workflow -----------------------------------------------------------

xgb_wf <- workflow() |> 
  add_formula(obitos ~ .) |> 
  add_model(xgb_espec)


# xgb: tune ---------------------------------------------------------------

doParallel::registerDoParallel()

set.seed(789)

xgb_tun <- tune_grid(
  xgb_wf,
  resamples = xgb_folds,
  grid = xgb_grid,
  metrics = metric_set(rmse, rsq, mae, mape),
  control = control_grid(save_pred = TRUE)
)

# salva tune
saveRDS(xgb_tun, "xgb_tun.rds")

#xgb_tun <- readRDS("xgb_tun.rds")

# melhores hiperparametros

xgb_params <- xgb_tun |> select_best("rmse")

xgb_final <- xgb_espec |> finalize_model(xgb_params)


# xgb: ajuste/predicao ----------------------------------------------------

xgb_pred <- xgb_final |> 
  # ajuste
  fit(formula = obitos ~ ., data = dados_mod_train) |> 
  # predicao
  predict(new_data = dados_mod_test) |> 
  rename(pred = .pred) |> 
  mutate(verd = dados_mod_test$obitos)


# xgb: performance --------------------------------------------------------

xgb_perf <- xgb_pred |> 
  metrics(truth = verd, estimate = pred)

ggplot(xgb_pred, aes(x = verd, y = pred)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", colour = "red", se = FALSE) +
  labs(x = "y_observado", y = "y_estimado") +
  theme_bw()



## Breve descritiva

# summary(dados_mod$obitos)
# summary(dados_predicao$obitos)
# 
# dados_mod %>%
#   ggplot(aes(x = obitos)) +
#   geom_histogram(color = "white")
# 
# dados_predicao %>%
#   ggplot(aes(x = obitos)) +
#   geom_histogram(color = "white")
# 
# dados_mod %>%
#   ggplot(aes(x = raca_cor, y=obitos)) +
#   geom_boxplot()
# 
# dados_mod %>%
#   ggplot(aes(x = escolaridade, y=obitos)) +
#   geom_boxplot()
# 
# dados_mod %>%
#   ggplot(aes(x = estado_civil, y=obitos)) +
#   geom_boxplot()
# 
# dados_mod %>%
#   ggplot(aes(x = tipo_de_morte_materna, y=obitos)) +
#   geom_boxplot()
# 
# dados_mod %>%
#   ggplot(aes(x = regiao, y=obitos)) +
#   geom_boxplot()
# 
# dados_mod %>%
#   ggplot(aes(x = med_idades, y = obitos)) +
#   geom_point(alpha=3) +
#   geom_smooth(formula = y ~ poly(x,2), se = FALSE, col="red")
# 
# # Vendo gráficamente a possível existência entre variáveis
# ## Categorica vs. Numérico
# 
# dados_mod %>%
#   ggplot(aes(x = med_idades, y = obitos)) +
#   geom_point(alpha=3)+
#   facet_wrap(~regiao)+
#   geom_smooth(method = lm, formula = y ~ poly(x,2), se = FALSE, col="red")
# 
# # Visão temporal do nº de óbitos 
# dados_obitos %>%
#   filter(year(data_obito) <= 2019) %>%
#   group_by(data_obito) %>%
#   summarise(mortes = n()) %>%
#   ggplot() + geom_line(aes(data_obito, mortes))

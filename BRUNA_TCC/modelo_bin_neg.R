library(corrplot)
library(tidyverse)

#Leitura da base de dados SIM
dados_obitos <- read.csv("Obitos_maternos_2014_2021.csv", encoding = "UTF-8")  
dados_nasc <- read.csv2("Nascimentos_2014_2021.csv",encoding = "UTF-8") # Leitura da base do dados SINASC
dados_nasc <- rename(dados_nasc, nascidos = obitos)
dados_nasc<- dados_nasc[,-6]

dados_obitos <- dados_obitos %>%
  mutate(
    mes_obito = month(dados_obitos$data_obito)
  )

dados_obitos$data_obito <- as.Date(dados_obitos$data_obito, tryFormats = "%d/%m/%Y")
dados_nasc$data_nasc <- as.Date(dados_nasc$data_nasc, tryFormats = "%d/%m/%Y")

dados_obitos2 <- dados_nasc %>% 
  right_join(dados_obitos, by = c("data_nasc"="data_obito","nasc_munnome"="ocor_munnome","nasc_sigla_uf"="ocor_sigla_uf"),relationship = "many-to-many")

## Criando a variável de estações do ano

season <- function(dates) {
  get_season <- function(data) {
    mes <- as.POSIXlt(data)$mon + 1
    dia <- as.POSIXlt(data)$mday
    
    if ((mes == 12 && dia >= 21) || (mes == 3 && dia < 20)) {
      return("Verão")
    } else if ((mes == 3 && dia >= 20) || (mes == 6 && dia < 20)) {
      return("Outono")
    } else if ((mes == 6 && dia >= 20) || (mes == 9 && dia < 22)) {
      return("Inverno")
    } else {
      return("Primavera")
    }
  }
  
  estacoes <- sapply(dates, get_season)
  return(estacoes)
}

dados_obitos <- dados_obitos %>%
  mutate(est_ano = case_when(
    (data_obito >= "2014-03-21" & data_obito < "2014-06-21") ~ "Outono",
    (data_obito >= "2015-03-21" & data_obito < "2015-06-21") ~ "Outono",
    (data_obito >= "2016-03-21" & data_obito < "2016-06-21") ~ "Outono",
    (data_obito >= "2017-03-21" & data_obito < "2017-06-21") ~ "Outono",
    (data_obito >= "2018-03-21" & data_obito < "2018-06-21") ~ "Outono",
    (data_obito >= "2019-03-21" & data_obito < "2019-06-21") ~ "Outono",
    (data_obito >= "2020-03-21" & data_obito < "2020-06-21") ~ "Outono",
    (data_obito >= "2021-03-21" & data_obito < "2021-06-21") ~ "Outono",
    (data_obito >= "2014-06-21" & data_obito < "2014-09-23") ~ "Inverno",
    (data_obito >= "2015-06-21" & data_obito < "2015-09-23") ~ "Inverno",
    (data_obito >= "2016-06-21" & data_obito < "2016-09-23") ~ "Inverno",
    (data_obito >= "2017-06-21" & data_obito < "2017-09-23") ~ "Inverno",
    (data_obito >= "2018-06-21" & data_obito < "2018-09-23") ~ "Inverno",
    (data_obito >= "2019-06-21" & data_obito < "2019-09-23") ~ "Inverno",
    (data_obito >= "2020-06-21" & data_obito < "2020-09-23") ~ "Inverno",
    (data_obito >= "2021-06-21" & data_obito < "2021-09-23") ~ "Inverno",
    (data_obito >= "2014-09-23" & data_obito < "2014-12-21") ~ "Primavera",
    (data_obito >= "2015-09-23" & data_obito < "2015-12-21") ~ "Primavera",
    (data_obito >= "2016-09-23" & data_obito < "2016-12-21") ~ "Primavera",
    (data_obito >= "2017-09-23" & data_obito < "2017-12-21") ~ "Primavera",
    (data_obito >= "2018-09-23" & data_obito < "2018-12-21") ~ "Primavera",
    (data_obito >= "2019-09-23" & data_obito < "2019-12-21") ~ "Primavera",
    (data_obito >= "2020-09-23" & data_obito < "2020-12-21") ~ "Primavera",
    (data_obito >= "2021-09-23" & data_obito < "2021-12-21") ~ "Primavera",
    (data_obito >= "2014-12-21" & data_obito < "2014-03-21")~ "Verão",
    (data_obito >= "2015-12-21" & data_obito < "2015-03-21")~ "Verão",
    (data_obito >= "2016-12-21" & data_obito < "2016-03-21")~ "Verão",
    (data_obito >= "2017-12-21" & data_obito < "2017-03-21")~ "Verão",
    (data_obito >= "2018-12-21" & data_obito < "2018-03-21")~ "Verão",
    (data_obito >= "2019-12-21" & data_obito < "2019-03-21")~ "Verão",
    (data_obito >= "2020-12-21" & data_obito < "2020-03-21")~ "Verão",
    (data_obito >= "2021-12-21" & data_obito < "2021-03-21") ~ "Verão"
  )
  )
n<-nrow(dados_obitos)
for (i in 1:n) {
  if(is.na(dados_obitos$est_ano[i])){
    dados_obitos$est_ano[i] = "Verão"
  } 
}

# Criando a variável de regiões do Brasil
dados_obitos<- dados_obitos %>%
  mutate(regiao = case_when((ocor_sigla_uf == "AC")| 
                              (ocor_sigla_uf == "AP")|
                              (ocor_sigla_uf == "AM")|
                              (ocor_sigla_uf == "PA")|
                              (ocor_sigla_uf == "RO")|
                              (ocor_sigla_uf == "RR")|
                              (ocor_sigla_uf == "TO")~"Norte",
                              (ocor_sigla_uf == "AL")|
                              (ocor_sigla_uf == "BA")|
                              (ocor_sigla_uf == "CE")|
                              (ocor_sigla_uf == "MA")|
                              (ocor_sigla_uf == "PB")|
                              (ocor_sigla_uf == "PI")|
                              (ocor_sigla_uf == "PE")|
                              (ocor_sigla_uf == "RN")|
                              (ocor_sigla_uf == "SE")~"Nordeste",
                              (ocor_sigla_uf == "DF")|
                              (ocor_sigla_uf == "GO")|
                              (ocor_sigla_uf == "MT")|
                              (ocor_sigla_uf == "MS")~"Centro oeste",
                              (ocor_sigla_uf == "ES")|
                              (ocor_sigla_uf == "MG")|
                              (ocor_sigla_uf == "RJ")|
                              (ocor_sigla_uf == "SP")~"Suldeste",
                            (ocor_sigla_uf == "PR")|
                              (ocor_sigla_uf == "SC")|
                              (ocor_sigla_uf == "RS")~"Sul"
                            ))


## Análises da significância das estações do ano e regiões versus ano de óbito

ctable(dados_obitos$est_ano,dados_obitos$ano_obito,chisq = TRUE, prop = "c") # p valor = 0, sem warning - Outono, estação com maior proporçao de mortes em todos os anos
ctable(dados_obitos$regiao,dados_obitos$ano_obito,chisq = TRUE,prop = "c")  # p valor = 0, sem warning - região suldeste e nordeste com maior frequencia de mortes em todos os anos

dados_nasc$data_nasc <- as.Date(dados_nasc$data_nasc, tryFormats = "%d/%m/%Y")

dados_nasc <- dados_nasc %>%
  mutate(mes_nasc = month(dados_nasc$data_nasc))
    
dados_obitos$ano_obito<-as.factor(dados_obitos$ano_obito)
dados_obitos$mes_obito<-as.factor(dados_obitos$mes_obito)

dados_obitos <- dados_obitos %>%
  mutate(cor = case_when(raca_cor == "Parda" ~ "Parda",
                         raca_cor == "Preta" ~ "Preta",
                         raca_cor == "Branca" ~ "Branca",
                         (raca_cor == "Amarela" | raca_cor == "Indígena") ~ "Outros",
                         raca_cor == "Ignorado" ~ "Ignorado"))



## Transformando variáveis categorias em variáveis quanti (Através da contagem de observações nas categorias)
## Transformando as categorias mais frequentes em variáveis quanti

dados_obitos_new <- dados_obitos ## Criando nova base para não afetar a original

## Quali (Cor) 
dados_obitos_new <- dados_obitos_new %>%
  mutate(Cor_Preto = case_when(cor == "Preta" ~ 1,
                               TRUE ~ 0))
dados_obitos_new <- dados_obitos_new %>%
  mutate(Cor_Branca = case_when(cor == "Branca" ~ 1,
                                TRUE ~ 0))
dados_obitos_new <- dados_obitos_new %>%
  mutate(Cor_Parda = case_when(cor == "Parda" ~ 1,
                               TRUE ~ 0))
dados_obitos_new <- dados_obitos_new %>%
  mutate(Cor_Outros = case_when((cor == "Outros"|cor == "Ignorado") ~ 1,
                                TRUE ~ 0))

## Quali (Tipo de morte)
dados_obitos_new <- dados_obitos_new %>%
  mutate(Morte_direta = case_when(tipo_de_morte_materna == "Direta" ~ 1,
                                  TRUE ~ 0))
dados_obitos_new <- dados_obitos_new %>%
  mutate(Morte_indireta = case_when((tipo_de_morte_materna == "Indireta"|tipo_de_morte_materna == "Não especificada") ~ 1,
                                   TRUE ~ 0))

## Quali (Escolaridade)
dados_obitos_new <- dados_obitos_new %>%
  mutate(Esc_medio = case_when(escolaridade == "Médio" ~ 1,
                               TRUE ~ 0))
dados_obitos_new <- dados_obitos_new %>%
 mutate(Esc_fundI = case_when(escolaridade == "Fundamental I" ~ 1,
                             TRUE ~ 0))
dados_obitos_new <- dados_obitos_new %>%
 mutate(Esc_fundII = case_when(escolaridade == "Fundamental II" ~ 1,
                              TRUE ~ 0))
dados_obitos_new <- dados_obitos_new %>%
 mutate(Esc_outros = case_when((escolaridade == "Ignorado"|escolaridade == "Sem escolaridade"|escolaridade == "Superior completo"|escolaridade =="Superior incompleto") ~ 1,
                                TRUE ~ 0))

## Selecionando as variáveis de interesse para o modelo

dados_obitos_mod <- dados_obitos_new[,c(13,23,24,26:35)]

## Criando a base de dados agrupando por mês e estado, assim criaremos a variável resposta
## N_mortes
myData2 <-
  aggregate(cbind(idade,Cor_Preto,Cor_Branca,Cor_Parda,Cor_Outros,Morte_direta,Morte_indireta,Esc_medio,Esc_fundI,Esc_fundII,Esc_outros) ~ dados_obitos_mod$est_ano + dados_obitos_mod$regiao,
    data = dados_obitos_mod,
            FUN =  function(x){c(
      med = mean(x,na.rm = TRUE),
      sum = sum(x,na.rm = TRUE),
      n = length(x))
    }
  )

# Selecionando as variáveis para o modelo
myData2<-as.matrix(myData2)
dados_obitos_mod2 <- myData2[,c(1,2,3,5,seq(7,35,3))]
dados_obitos_mod2 <- as.data.frame(dados_obitos_mod2)

## Reestrurturando as variáveis da base (renomeando, arredondando, etc)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(Est_ano = `dados_obitos_mod$est_ano`)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(Regiao = `dados_obitos_mod$regiao`)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(Y = idade.n)

for (j in 4:14) {
  dados_obitos_mod2[,j]<-dados_obitos_mod2[,j] %>%
    as.numeric()%>%
    round(digits = 0) 
}

dados_obitos_mod2[,3]<- dados_obitos_mod2[,3] %>%
  as.numeric()%>%
  round(digits = 2) 


dados_obitos_mod2$Est_ano<-factor(dados_obitos_mod2$Est_ano)
dados_obitos_mod2$Regiao<-factor(dados_obitos_mod2$Regiao)


## Renomeando a variável resposta n_mortes
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X1 = Cor_Preto.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X2 = Cor_Branca.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X3 = Cor_Parda.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X4 = Cor_Outros.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X5 = Morte_direta.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X6 = Morte_indireta.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X7 = Morte_outros.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X8 = Est_civil_Solteira.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X9 = Est_civil_Casada.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X10 = Est_civil_outros.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X11 = Esc_medio.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X12 = Esc_fundI.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X13 = Esc_fundII.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X14 = Esc_outros.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X15 = Periodo_gravidez.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X16 = Periodo_puerperio.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X17 = Periodo_outros.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X18 = Cid_XV.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X19 = Cid_outros.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X20 = Com_assistencia.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X21 = Assistencia_outros.sum)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(X22 = idade.med)

## Fazendo uma análise as variáveis do modelo
df<-dados_obitos_mod2[,-c(1,2)]
df<- df %>%
  select(Y,everything())

corrplot(cor(df), method = "number",type = 'lower')

mtxc<-cor(df)
mtxc <- mtxc %>%
  round(digits = 2)

freq(dados_obitos_mod2$Morte_outros.sum)

dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(Ano = `dados_obitos_new$ano_obito`)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(Mes = `dados_obitos_new$mes_obito`)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(UF = `dados_obitos_new$ocor_sigla_uf`)

## Modelagem
library(MASS)
base_nasc$ano_nasc<-factor(base_nasc$ano_nasc)
base_nasc$mes_nasc<-factor(base_nasc$mes_nasc)

dados_obitos_mod2 <- base_nasc %>% 
  inner_join(dados_obitos_mod2, by = c("ano_nasc"="Ano","mes_nasc"="Mes","nasc_sigla_uf"="UF"))

dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(Ano = ano_nasc)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(Mes = mes_nasc)
dados_obitos_mod2 <- dados_obitos_mod2 %>% 
  rename(UF = nasc_sigla_uf)

dados_obitos_mod2$Ano<-as.numeric(as.character(dados_obitos_mod2$Ano))

DO14_19 <- dados_obitos_mod2 %>%
  filter(Ano > 2013 & Ano < 2020)
dados_treino <- DO14_19 %>%
  filter(Ano < 2019)
dados_teste <-DO14_19 %>%
  filter(Ano  == 2019) 

dados_treino$Ano<-factor(dados_treino$Ano)
dados_treino$UF<-factor(dados_treino$UF)


fit1<-glm.nb(Y~.,data=dados_treino)

summary(fit1)

stepAIC(fit1)

glm.nb(formula = Y ~ UF + Cor_Preto.sum + Cor_Branca.sum + Cor_Parda.sum + 
         Cor_Outros.sum, data = dados_treino, init.theta = 179273.0494, 
       link = log) %>%
  summary()

fit2<-glm(Y~.,data=dados_treino, family = "poisson")
summary(fit2)
stepAIC(fit2)
#-----Seleçao de variáveis

fit0<-glm(Y~ Mes + UF,family = "poisson", data = dados_treino)
summary(fit0)
fit3<-glm(Y~ Mes + UF + idade.med,family = "poisson", data = dados_treino)
summary(fit3) 
lmtest::lrtest(fit0,fit3) ## Variável idade media não se mostrou significativa

fit4<-glm(Y~Mes + UF + Cor_Preto.sum,family = "poisson", data = dados_treino)
summary(fit4) 
lmtest::lrtest(fit0,fit4) ## Variável cor_preto SE MOSTROU SIGNIFICATIVA

fit5<-glm(Y~Mes + UF + Cor_Branca.sum,family = "poisson", data = dados_treino)
summary(fit5) 
lmtest::lrtest(fit0,fit5) ## Variável cor_branca SE MOSTROU SIGNIFICATIVA

fit6<-glm(Y~ Mes + UF + Cor_Parda.sum,family = "poisson", data = dados_treino)
summary(fit6) 
lmtest::lrtest(fit0,fit6) ## Variável cor_parda SE MOSTROU SIGNIFICATIVA

fit7<-glm(Y~ Mes + UF + Cor_Outros.sum,family = "poisson", data = dados_treino)
summary(fit7) 
lmtest::lrtest(fit0,fit7) ## Variável cor_outros SE MOSTROU SIGNIFICATIVA

fit8<-glm(Y~ Mes + UF + Morte_direta.sum,family = "poisson", data = dados_treino)
summary(fit8) 
lmtest::lrtest(fit0,fit8) ## Variável morte direta SE MOSTROU SIGNIFICATIVA

fit9<-glm(Y~ Mes + UF + Ano,family = "poisson", data = dados_treino)
summary(fit9) 
lmtest::lrtest(fit0,fit9) ## Variável ANO NÃO SE MOSTROU SIGNIFICATIVA

fit10<-glm(Y~ Mes + UF + Cor_Preto.sum + Cor_Branca.sum + Cor_Parda.sum + Cor_Outros.sum + Morte_direta.sum ,family = "poisson", data = dados_treino)
summary(fit10) 

fit11<-glm(Y~ Mes + UF + Cor_Preto.sum + Cor_Branca.sum + Cor_Parda.sum + Cor_Outros.sum ,family = "poisson", data = dados_treino)
summary(fit11) 
lmtest::lrtest(fit11,fit10) ## Variável ANO NÃO SE MOSTROU SIGNIFICATIVA

fit12<-glm(Y~ Mes + UF + Cor_Preto.sum + Cor_Branca.sum + Cor_Parda.sum + Cor_Outros.sum + idade.med ,family = "poisson", data = dados_treino)
summary(fit12) 
lmtest::lrtest(fit12,fit11) ## Variável ANO NÃO SE MOSTROU SIGNIFICATIVA


# MODELO FINAL FIT11

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

par(mfrow = c(2, 2))
plot(fit8)

par(mfrow = c(1, 1))
envelope(fit8)

modbn <- glm.nb(Y~ Mes + UF + Cor_Preto.sum + Cor_Branca.sum + Cor_Parda.sum + Cor_Outros.sum + offset(UF),
                data = dados_treino, init.theta = 179273.0494, 
                link = identity)

envelope_bn <- function(modelo) {
  dados <- na.omit(dados_treino)
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



par(mfrow = c(2, 2))
plot(modbn)

par(mfrow = c(1, 1))
envelope_bn(modbn)

fit.model<-modbn

base_nasc <-
  aggregate(cbind(obitos) ~ ano_nasc + mes_nasc + nasc_sigla_uf,
            data = dados_nasc,
            FUN =  function(x){
                  n = sum(x)
            }
  )
MDBN<-glm.nb(Y~ Mes + UF + Morte_direta.sum + offset(obitos),data = dados_treino, init.theta = 179273.0494, 
             link = log)

summary(MDBN)

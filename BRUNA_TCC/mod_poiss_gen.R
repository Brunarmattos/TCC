# Carrega o devtools.
library(devtools)
library(doBy)
library(lattice)
library(latticeExtra)
library(bbmle)
library(corrplot)
library(plyr)
library(car)
library(multcomp)

# Instalando a partir do zip (Windows).
#devtools::install_github(repo = "leg-ufpr/MRDCr", ref="devel")
# Carrega o pacote.
library(MRDCr)

# Lista os objetos do pacote.
ls("package:MRDCr")

# Abre a documentação do pacote.
help(package = "MRDCr", help_type = "html")

# Citação do pacote.
citation("MRDCr")

# Caminho para o diretório com slides do MRDCr.
sld <- system.file("slides", package = "MRDCr")
dir(sld)

# Caminho para os slides.
paste0(sld, "/slides-mrdcr.pdf")

# Gerando uma amostra aleatória da Poisson, para teste.

# Offset = 2, lambda = 3.
y <- rpois(100, lambda = 2 * 3)

L <- list(y = y,
          offset = rep(2, length(y)),
          X = cbind(rep(1, length(y))))

start <- c(alpha = 0, lambda = 1)
parnames(llpgnz) <- names(start)

# Como \alpha foi fixado em 1, essa ll corresponde à Poisson.
n0 <- mle2(minuslogl = llpgnz,
           start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Para conferir.
c(coef(n0)["lambda"],
  coef(glm(y ~ offset(log(L$offset)), family = poisson)))


# -------------------------- Teste em Dados Reais ------------------------------------------------
# Carregando e explorando os dados.
data(soja, package = "MRDCr")
str(soja)

soja <- soja[-74, ]

xyplot(nvag ~ K | umid, data = soja, layout = c(NA, 1),
       type = c("p", "smooth"),
       ylab = "Número de vagens por parcela",
       xlab = expression("Dose de potássio aplicada"~(mg ~ dm^{3})),
       strip = strip.custom(strip.names = TRUE, var.name = "Umidade"))

soja <- transform(soja, K = factor(K))

#-----------------------------------------------------------------------
# Modelo Poisson.

m0 <- glm(nvag ~ bloc + umid * K, data = soja, family = poisson)
m1 <- update(m0, family = quasipoisson)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0); layout(1)
summary(m1)

#-----------------------------------------------------------------------
# Modelo Poisson Generalizado.

L <- with(soja,
          list(y = nvag, offset = 1, X = model.matrix(m0)))

# Usa as estimativas do Poisson como valores iniciais para a PGen.
start <- c(alpha = 0, coef(m0))
parnames(llpgnz) <- names(start)

# Com alpha fixo em 0 corresponde à Poisson.
m2 <- mle2(llpgnz, start = start, data = L,
           fixed = list(alpha = 0), vecpar = TRUE)

# Mesma medida de ajuste e estimativas.
c(logLik(m2), logLik(m0))
cbind(coef(m2)[-1], coef(m0))

# Poisson Generalizada.
m3 <- mle2(llpgnz, start = start, data = L, vecpar = TRUE)

cbind("PoissonGLM" = c(NA, coef(m0)),
      "PoissonML" = coef(m2),
      "PGeneraliz" = coef(m3))

plot(profile(m3, which = "alpha"))
abline(v = 0, lty = 2)

#-----------------------------------------------------------------------
# Predição com bandas de confiança.

X <- LSmatrix(m0, effect = c("umid", "K"))

pred <- attr(X, "grid")
pred <- transform(pred,
                  K = as.integer(K),
                  umid = factor(umid))
pred <- list(pois = pred, pgen = pred)

# Preditos pela Poisson.
# aux <- predict(m0, newdata = pred$pois, se.fit = TRUE)
# aux <- exp(aux$fit + outer(aux$se.fit, qn, FUN = "*"))
# pred$pois <- cbind(pred$pois, aux)
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$pois <- cbind(pred$pois, exp(aux))
str(pred$pois)


#-------- Aplicando aos dados de morte de gestantes e puérperas na COVID-19 ------------------
# Modelo Poisson
m4<-glm(obitos ~ med_idades + I(med_idades^2) + regiao + 
          tipo_de_morte_materna + Est_civil + cor + med_idades:tipo_de_morte_materna + 
          med_idades:Est_civil + I(med_idades^2):tipo_de_morte_materna + 
          regiao:cor + tipo_de_morte_materna:cor + Est_civil:cor + 
          offset(log(nasc)), data=dados_treino, family = "poisson") 

summary(m4)
par(mfrow = c(2, 2))
plot(m4); layout(1)

anova(m4, test = "F")

m4.2 <- update(m4, family = quasipoisson)
anova(m4.2, test = "F")
coef(m4.2)

-1/max(dados_teste$obitos)

M <- with(dados_treino,
          list(y = obitos, offset = nasc, X = model.matrix(m4)))

# Usa as estimativas do Poisson como valores iniciais para a PGen.
start <- c(alpha = -0.1, coef(m4))
parnames(llpgnz) <- names(start)

# Com alpha fixo em 0 corresponde à Poisson.
m5 <- mle2(llpgnz, start = start, data = M,fixed = list(alpha = 0), vecpar = TRUE)

# Mesma medida de ajuste e estimativas.
c(logLik(m5), logLik(m4))
cbind(coef(m5)[-1], coef(m4))

# Poisson Generalizada.
m6 <- mle2(llpgnz, start = start, data = M, vecpar = TRUE)

cbind("PoissonGLM" = c(NA, coef(m4)),
      "PoissonML" = coef(m5),
      "PGeneraliz" = coef(m6))

plot(profile(m6, which = "alpha")) ## Erro.. não está convergindo?
abline(v = 0, lty = 2)

# Teste para nulinidade do parâmetro de dispersão (H_0: alpha == 0).
anova(m6, m5)


V <- cov2cor(vcov(m6))
corrplot::corrplot.mixed(V, lower = "number", upper = "ellipse",
               diag = "l", tl.pos = "lt", tl.col = "black",
               tl.cex = 0.8)
dev.off()
#-----------------------------------------------------------------------
# Testes de hipótese.

# Teste de Wald para a interação.
a <- c(0, attr(model.matrix(m4), "assign"))
ai <- a == max(a)
L <- t(replicate(sum(ai), rbind(coef(m6) * 0), simplify = "matrix"))
L[, ai] <- diag(sum(ai))

# Cálculo da estatística Chi-quadrado.
# t(L %*% coef(m3)) %*%
#     solve(L %*% vcov(m3) %*% t(L)) %*%
#     (L %*% coef(m3))
crossprod(L %*% coef(m6),
          solve(L %*% vcov(m6) %*% t(L),
                L %*% coef(m6)))
# Teste de Wald para interação (poderia ser LRT, claro).
# É necessário passar um objeto glm mesmo fornecendo o restante a parte.
linearHypothesis(model = m4,
                 hypothesis.matrix = L,
                 vcov. = vcov(m6),
                 coef. = coef(m6))
#-----------------------------------------------------------------------
# Predição com bandas de confiança.

pred <- unique(subset(dados_treino, select = c("tipo_de_morte_materna","cor","Est_civil","med_idades","regiao")))

X <- model.matrix(formula(m6)[-2],
                  data = cbind(nvag = 1, bloc = dados$bloc[1], pred))

i <- grep(x = colnames(X), pattern = "^bloc")
X[, i] <- X[, i] * 0 + 1/(length(i) + 1)
head(X)

pred <- list(pois = pred, quasi = pred, pgen = pred)

# Preditos pela Poisson.
aux <- confint(glht(m0, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$pois <- cbind(pred$pois, exp(aux))

# Preditos pela Quasi-Poisson.
aux <- confint(glht(m1, linfct = X),
               calpha = univariate_calpha())$confint
colnames(aux)[1] <- "fit"
pred$quasi <- cbind(pred$quasi, exp(aux))

# Preditos pela Poisson Generalizada.
aux <- predict(m3, newdata = X,
               interval = "confidence", type = "response")
pred$pgen <- cbind(pred$pgen, aux[, c(2, 1, 3)])

# Junta o resultado dos 3 modelos.
pred <- ldply(pred, .id = "modelo")
pred <- arrange(pred, umid, K, modelo)
pred$K <- as.numeric(as.character(pred$K))

key <- list(type = "o", divide = 1,
            lines = list(pch = 1:nlevels(pred$modelo),
                         lty = 1, col = 1),
            text = list(c("Poisson", "Quasi-Poisson",
                          "Poisson Generelizada")))

xyplot(fit ~ K | umid, data = pred,
       layout = c(NA, 1), as.table = TRUE,
       xlim = extendrange(range(pred$K), f = 0.2),
       key = key, pch = pred$modelo,
       xlab = expression("Dose de potássio"~(mg~dm^{-3})),
       ylab = "Número de grãos por parcela",
       ly = pred$lwr, uy = pred$upr, cty = "bars", length = 0,
       prepanel = prepanel.cbH,
       desloc = 8 * scale(as.integer(pred$modelo), scale = FALSE),
       panel = panel.cbH)


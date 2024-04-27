library(likert)
library(tidyverse)
library(readxl)

dados <- read_excel("todas_as_respostas_sem_dados_sensiveis.xlsx")

head(dados[,14:19])


dados$`Impressão ou cópia (xerox) de documentos` <- factor(dados$`Impressão ou cópia (xerox) de documentos`, levels = c("Não é oferecido pela Edufes","Não tenho certeza","É oferecido pela Edufes"))
dados$`Download gratuito de livros com conteúdo técnico-científico produzido na Ufes` <- factor(dados$`Download gratuito de livros com conteúdo técnico-científico produzido na Ufes`, levels = c("Não é oferecido pela Edufes","Não tenho certeza","É oferecido pela Edufes"))
dados$`Editoração de livros, incluindo: avaliação de originais; preparação e revisão de textos; e diagramação` <- factor(dados$`Editoração de livros, incluindo: avaliação de originais; preparação e revisão de textos; e diagramação`, levels = c("Não é oferecido pela Edufes","Não tenho certeza","É oferecido pela Edufes"))
dados$`Venda de livros do Núcleo de Línguas da Ufes (cursos de idiomas)` <- factor(dados$`Venda de livros do Núcleo de Línguas da Ufes (cursos de idiomas)`, levels = c("Não é oferecido pela Edufes","Não tenho certeza","É oferecido pela Edufes"))
dados$`Impressão de materiais de divulgação da Ufes (cartão de visita, folheto, marcador de páginas)` <- factor(dados$`Impressão de materiais de divulgação da Ufes (cartão de visita, folheto, marcador de páginas)`, levels = c("Não é oferecido pela Edufes","Não tenho certeza","É oferecido pela Edufes"))
dados$`Empréstimo de livros para estudantes e professores da Ufes`<-factor(dados$`Empréstimo de livros para estudantes e professores da Ufes`, levels = c("Não é oferecido pela Edufes","Não tenho certeza","É oferecido pela Edufes"))

lik1<-likert(as.data.frame(dados[,14:19]))

plot(lik1, text.size=3, col=c("#e73737","#ffc21f","#7ccb00"), centered = FALSE)

dados <- dados %>%
  rename("Você já leu um livro publicado pela Edufes? (CON03)" = CON03)
dados$`Você já leu um livro publicado pela Edufes? (CON03)`  <-factor(dados$`Você já leu um livro publicado pela Edufes? (CON03)`,levels = c("Não","Não tenho certeza", "Sim"))

dados <- dados %>%
  rename("Você já comprou um livro publicado pela Edufes? (CON04)" = CON04) 
dados$`Você já comprou um livro publicado pela Edufes? (CON04)`  <-factor(dados$`Você já comprou um livro publicado pela Edufes? (CON04)`,levels = c("Não","Não tenho certeza", "Sim"))

dados <- dados %>%
  rename("Você já submeteu um livro à Edufes? (CON05)" = CON05)
dados$`Você já submeteu um livro à Edufes? (CON05)`  <-factor(dados$`Você já submeteu um livro à Edufes? (CON05)`,levels = c("Não","Não tenho certeza", "Sim"))

dados <- dados %>%
  rename("Você já esteve na livraria da Ufes? (CON06)" = CON06)
dados$`Você já esteve na livraria da Ufes? (CON06)`  <-factor(dados$`Você já esteve na livraria da Ufes? (CON06)`,levels = c("Não","Não tenho certeza", "Sim"))

dados <- dados %>%
  rename("Você já esteve na Edufes? (CON07)" = CON07)
dados$`Você já esteve na Edufes? (CON07)`  <-factor(dados$`Você já esteve na Edufes? (CON07)`,levels = c("Não","Não tenho certeza", "Sim"))

lik_CON <- likert(as.data.frame(dados[,8:12]))
plot(lik_CON, text.size=3, col=c("#e73737","#ffc21f","#7ccb00"), centered = FALSE)

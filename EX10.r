
############################# LETRA A #############################
## Instalar pacotes do ISLR para pegar as bibliotecas.
install.packages("ISLR")

## Biblioteca ISLR aonde tem o dataset Weekly para comparação.
library(ISLR)

## Numerais de correlação
summary(Weekly)

## Gráficos de correlação, fica aparente que relação volume x ano possuem um gráfico diferente
pairs(Weekly)

## Comparar no dataset weekly a relação entre as variáveis
cor(Weekly[, -9])

## Volume e ano aparentam ter uma relação entre si, pois na linha do "cor(Weekly[,-9]), Volume e 
## Year possuem um valor bem mais alto que o restante.

############################# LETRA B #############################
attach(Weekly)

## Usando a função "Generalized Linear Models", feito a regressão.
exb.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, 
              family = binomial)
summary(exb.fit)

## Com Pr(>|z|) = 3%, prova que Lag 2 tem uma significancia estatística na regressão

############################# LETRA C #############################

exb.probs = predict(exb.fit, type = "response")
exb.pred = rep("Down", length(exb.probs))
exb.pred[exb.probs > 0.5] = "Up"
table(exb.pred, Direction)

## A porcentagem das predições: (54+557)/(54+557+48+430) = 56.1%.
## Quando o mercado vai pra "cima" a regressão está certa maior parte do tempo: 557/(557+48) = 92.1%.

############################# LETRA D #############################

## Ajustando o modelo usando o treino de 90 a 2008 com Lag2 de preditor
train = (Year < 2009)
exd = Weekly[!train, ]
exb.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
exb.probs = predict(exb.fit, exd, type = "response")
exb.pred = rep("Down", length(exb.probs))
exb.pred[exb.probs > 0.5] = "Up"
direcao = Direction[!train]
table(exb.pred, direcao)

## Verificando a fração geral de predições corretas com o treino
mean(exb.pred == direcao)

############################# LETRA E #############################

## Calculando com a Linear Discriminant Analysis (LDA)
library(MASS)
exe.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
exe.pred = predict(exe.fit, exd)
table(exe.pred$class, direcao)

## Verificando a fração geral de predições corretas da LDA
mean(exe.pred$class == direcao)

############################# LETRA F #############################

## Calculando com a Quadratic Discriminant Analysis (QDA)
exf.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
exf.class = predict(exf.fit, exd)$class
table(exf.class, direcao)

## Verificando a fração geral de predições corretas da QDA
mean(exf.class == direcao)

############################# LETRA H #############################

## Regressão Logistica e LDA dão testes de erro bem parecidos

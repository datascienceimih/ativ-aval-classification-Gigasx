
############################# LETRA A #############################

## Instalar pacotes do ISLR para pegar as bibliotecas.

install.packages("ISLR")

## Biblioteca ISLR aonde tem o dataset Weekly para comparação.

library(ISLR)
summary(Auto)

## Criando a variável mpg01 aonde 1 seria caso o valor fosse acima da mediana, e 0 abaixo da mediana

attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

############################# LETRA B #############################

## Comparando numericamente e graficamente os valores

cor(Auto[, -9])
pairs(Auto)

## MPG possui uma correlação negativa quanto a Cylinders, Weight, Displacement e Horsepower

## Separando em treino e teste

train = (year%%2 == 0)  # if the year is even
test = !train
Auto.train = Auto[train, ]
Auto.test = Auto[test, ]
mpg01.test = mpg01[test]

############################# LETRA D #############################

## Treino de LDA para predizer mpg01

library(MASS)
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
lda.pred = predict(lda.fit, Auto.test)
mean(lda.pred$class != mpg01.test)

## Teste de erro igual a 12.6%

############################# LETRA E #############################

## Treino de QDA para predizer mpg01

qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
qda.pred = predict(qda.fit, Auto.test)
mean(qda.pred$class != mpg01.test)

## Teste de erro igual a 13.2%

############################# LETRA F #############################

## Regressão logistica no treino afim de predizer mpg01

rl.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              family = binomial, subset = train)
rl.probs = predict(rl.fit, Auto.test, type = "response")
rl.pred = rep(0, length(rl.probs))
rl.pred[rl.probs > 0.5] = 1
mean(rl.pred != mpg01.test)

## Teste de erro: 12.08%

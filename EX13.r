
## Instalar pacotes do ISLR para pegar as bibliotecas.

install.packages("ISLR")

## Biblioteca MASS aonde tem o dataset Boston para comparação.

library(MASS)
summary(Boston)

## O objetivo das predições a seguir é para verificar se o bairro tem sua taxa de criminalidade
## abaixo ou acima da mediana.

attach(Boston)
crime01 = rep(0, length(crim))
crime01[crim > median(crim)] = 1
Boston = data.frame(Boston, crime01)

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime01.test = crime01[test]

## Teste com Regressão Logística

rl.fit = glm(crime01 ~ . - crime01 - crim, data = Boston, family = binomial, 
              subset = train)

rl.probs = predict(rl.fit, Boston.test, type = "response")
rl.pred = rep(0, length(rl.probs))
rl.pred[rl.probs > 0.5] = 1
mean(rl.pred != crime01.test)

## Teste de erro: 18.18%

rl.fit = glm(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, family = binomial, 
              subset = train)

rl.probs = predict(rl.fit, Boston.test, type = "response")
rl.pred = rep(0, length(rl.probs))
rl.pred[rl.probs > 0.5] = 1
mean(rl.pred != crime01.test)

## Teste de erro: 18.57%

## Teste com LDA
lda.fit = lda(crime01 ~ . - crime01 - crim, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

## Teste de erro: 13.4%

lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

## Teste de erro: 12.2%

lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax - lstat - indus - age, 
              data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

## Teste de erro: 11.9%

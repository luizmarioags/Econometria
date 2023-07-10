# Carregar as bibliotecas necessárias
library(tidyverse) 
library(performance)
library(wooldridge) 
library(robustbase)
library(lmtest)
library(modelsummary)
library(stargazer)
library(jtools) 
library(plm) # painel
library(car) # testes
library(ppcor)
# Carregar os dados do Wooldridge que estão em Painel 
data("cement", package = "wooldridge")
# Criar o modelo oferecido pela questão
modelo_item_1 = gprc ~ gcem + gprcpet + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec
# Estimar o MQO
reg = plm(modelo_item_1, data = cement, model = "within")
summary(reg)
#Era esperado que o coeficiente de gcm fosse negativo dado que existe uma relação inversa entre oferta e demanda 
#Já o coeficiente relacionado ao preço do petroleo seria positivo dado que os custos de transporte intereferem no preço 
# Teste de correlação entre gdefs e gcem
cor_test = cor.test(cement$gdefs, cement$gcem)
print(cor_test)
# Dessa forma não rejeitamos Ho, concluindo assim que gcem não é uma boa VI para gdefs
# Teste de correlação parcial entre gcem, gres e gnon
partial_correlation = pcor.test(cement$gcem, cement$gres, cement$gnon, method = "pearson")
print(partial_correlation)
# Removendo valores nulos 
casos_completos = complete.cases(cement$gcem, cement$gres, cement$gnon)
df = cement[casos_completos, ]
# Teste
teste = pcor.test(df$gcem, df$gres, df$gnon, method = "pearson")
print(teste)
# De acordo com o teste, podemos usar como VI 

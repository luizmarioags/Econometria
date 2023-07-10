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
# Dataframe
df = expendshares
?expendshares
#Formando modelo 
formula_1 = sfood ~ ltotexpend + stransport + sother
#Regredindo
reg = lm(formula_1, data = df)
summary(reg)
# Estimando o outro modelo 
formula_2 = sfood ~ ltotexpend + age + kids
#Regredindo 
reg_2 = lm(formula_2, data = df)
summary(reg_2)

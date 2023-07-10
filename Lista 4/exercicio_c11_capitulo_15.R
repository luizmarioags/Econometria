#Chamando as bibiliotecas necessárias
library(tidyverse)
library(AER) #Rodar com variável instrumental
library(dplyr)
library(wooldridge) 
library(robustbase)
library(lmtest)
library(plm)
library(modelsummary)
library(stargazer)
library(jtools) 
#Criando o df 
df = voucher
?voucher
view(df)
#Fazendo uma regressao simples de choiceyears em selectyears
reg_year = choiceyrs ~ selectyrs 
model = lm(formula = reg_year, data = df)
summary(model)
#Regredindo mnce em choiceyrs
reg_mnce = mnce ~ choiceyrs 
modelo_reg = lm(formula = reg_mnce, data = df)
summary(modelo_reg)
# O que acontece se adicionarmos variaveis de genero e raça ? 
reg_mnce2 = mnce ~ choiceyrs + black + hispanic + female
modelo_rg = lm(formula = reg_mnce2, data = df)
summary(modelo_rg)
# Estimando com choiceyrs tendo selectyrs como instrumento
reg_vi = mnce ~  black + hispanic + female + choiceyrs | selectyrs 
modelo_vi = ivreg(formula = reg_vi, data = df)
summary(modelo_vi)

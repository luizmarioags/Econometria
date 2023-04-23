#Chamando as bibiliotecas necessárias
library(tidyverse) 
library(wooldridge) 
library(robustbase)
library(lmtest)
library(modelsummary)
library(stargazer)
library(jtools) 
# Identificando o dataframe
df = injury
#Descrição da Base
?injury
#Vizualizando o dataframe
view(df)
#Filtrando para mostrar apenas os dados de Kentucky 
df_t = df[df$ky == 1,] #1 indica positivo para Kentucky
view(df_t)
#Fazendo a regressao
formula1 = ldurat ~ afchnge + highearn + afhigh
#Regredindo 
model = lm(formula = formula1, data = df_t)
summary(model)
#Construindo a tabela 
stargazer(model, title="Exercício 7 do Wooldridge, estimando sem omissão", align=TRUE)

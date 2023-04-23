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
#Transformando as colunas de indústria
#Transformando a coluna de uma variável contínua para uma dummie
df_t$indust <- factor(df_t$indust)
#Fazendo o modelo 
formula2 = ldurat ~ afchnge + highearn + male + married + hosp + indust + afhigh
model = lm(formula = formula2, data = df_t)
summary(model)
#Construindo a tabela 
stargazer(model, title="Exercício C4 do Wooldridge para o Kentucky", align=TRUE)
#Estimando para o Michigan 
#Filtrando para mostrar apenas os dados de Michigan
df_m = df[df$mi == 1,] #1 indica positivo para Michigan
view(df_m)
#Transformando as colunas de indústria
#Transformando a coluna de uma variável contínua para uma dummie
df_m$indust <- factor(df_m$indust)
#Fazendo o modelo 
formula2 = ldurat ~ afchnge + highearn + male + married + hosp + indust + afhigh
model = lm(formula = formula2, data = df_t)
summary(model)
#Construindo a tabela 
stargazer(model, title="Exercício C4 do Wooldridge para o Michigan", align=TRUE)

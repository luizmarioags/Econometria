#Chamando as bibiliotecas necessárias
library(tidyverse) 
library(wooldridge) 
library(robustbase)
library(lmtest)
library(modelsummary)
library(stargazer)
library(jtools) 
library(plm) # painel
library(car) # testes
# Identificando o dataframe
df = wagepan
#Descrição da Base
?wagepan
#Vizualizando o dataframe
#view(df)
#Modelo de Primeiras Diferenças 
formulapd = lwage ~ union + year*educ
#Regredindo 
modelo_pd = plm(formula = formulapd, data = df,
                 model = "fd",
                index = c("nr", "year"))
summary(modelo_pd)
#Fazendo o teste F das interações 
linearHypothesis(modelo_pd,
                 c("year1981:educ=0", "year1982:educ=0", "year1983:educ=0",
                   "year1984:educ=0", "year1985:educ=0", "year1986:educ=0",
                   "year1987:educ=0"))
#Construindo a tabela 
stargazer(modelo_pd, title="Exercício C13 do Wooldridge", align=TRUE)

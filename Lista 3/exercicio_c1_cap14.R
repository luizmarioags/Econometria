#Chamando as bibiliotecas necessárias
library(tidyverse) 
library(wooldridge) 
library(robustbase)
library(lmtest)
library(plm)
library(modelsummary)
library(stargazer)
library(jtools) 
#Criando o dataframe
df = rental
?rental
view(df)
# Fazendo o modelo 
reg = lrent ~ y90 + lpop + lavginc + pctstu 
#Regredindo na forma padrão 
modelo = lm(formula = reg, data = df)
summary(modelo)
#Diferenciando o modelo (eliminamos y90 dado que é fixo no tempo)
diff = lrent ~ lpop + lavginc + pctstu 
#Regredindo
modelo_diff = lm(formula = diff, data = df)
summary(modelo_diff)
#Estimando por efeitos fixos 
ef = lrent ~ y90 + lpop + lavginc + pctstu 
#Fazendo a regressao 
modelo_ef <- plm(formula = ef, data = df,
                 model = "within",
                 index = c("city", "year"))
summary(modelo_ef)


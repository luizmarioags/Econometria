#Chamando as bibiliotecas necessárias
library(tidyverse)
library(dplyr)
library(wooldridge) 
library(robustbase)
library(lmtest)
library(plm)
library(modelsummary)
library(stargazer)
library(jtools) 
#Criando o df 
df = card 
?card
view(df)
#Regredindo Qi em near
reg = IQ ~ nearc4
modelo = lm(reg, data = df)
summary(modelo)
# Aqui vemos que existe uma correlação entre Qi e Near. 
#porém a correlação entre 'nearc4' e 'QI', uma vez que as outras variáveis 
#explicativas são compensadas, é indiscutivelmente zero. Pelo menos, não é estatisticamente diferente de zero.

#Regredindo Qi em Near e outras variáveis regionais
reg2 = IQ ~ nearc4 + smsa66 + reg661 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669 + south66
modelo = lm(reg2, data = df) 
summary(modelo)
#Aqui vemos que near perde efeito sobre QI, tendo sendo afetada principalmente pelas dummies regionais 
#Assim, dado os dados obtidos, se faz mais efeito controlar as variáveis regionais do que as que exigem proximidade 


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
library(ggplot2)
#Criando o df 
df = card 
?card
#view(df)
#Regredindo Qi em near
reg = IQ ~ nearc4
modelo = lm(reg, data = df)
summary(modelo)
#Fazendo um gráfico dessa regressão 
plot_1 = ggplot(df,aes(x = nearc4,y = IQ)) + geom_smooth(method = lm, se = FALSE, color = 'blue') + geom_point(color = 'red')
plot_1
# Aqui vemos que existe uma correlação entre Qi e Near. 
#porém a correlação entre 'nearc4' e 'QI', uma vez que as outras variáveis 
#explicativas são compensadas, é indiscutivelmente zero. Pelo menos, não é estatisticamente diferente de zero.

#Regredindo Qi em Near e outras variáveis regionais
reg2 = IQ ~ nearc4 + smsa66 + reg661 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669 + south66
modelo = lm(reg2, data = df) 
summary(modelo)
#Aqui vemos que near perde efeito sobre QI, tendo sendo afetada principalmente pelas dummies regionais 
#Assim, dado os dados obtidos, se faz mais efeito controlar as variáveis regionais do que as que exigem proximidade 
# Exercício 4 do Livro 
# item i) Pode existir uma interferência do estado na análise, fazendo com que gMINt e ut são
# correlacionados,tornando inconsistente a estimação. 
#item ii) A variável relacionada ao PIB corresponde a toda a atividade econômica, não tendo efeitos em relação ao desemprego para estados particulares
#item iii) Pode existir uma exogeneidade, se o salário minimo estadual crescer de acordo com o salário minimo nacional

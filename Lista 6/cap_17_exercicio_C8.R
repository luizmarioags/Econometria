library(tidyverse)
library(wooldridge)
#Df 
df <- jtrain2
View(df)
?jtrain2  
#Contando total de participantes
participantes <- sum(df$train == 1)
print(paste("O número de homens que participaram do programa de treinamento profissional é:", participantes))
#Duracao
maior_duracao <- max(df$mostrn)
print(paste("O maior número de meses acumulado por um participante do programa é:", maior_duracao))
#Fazendo a regressão linear 
modelo <- lm(train ~ unem74 + unem75 + age + educ + black + hisp + married, data = df)
summary(modelo)
#Pelo P-valor não são significativas 
#Fazendo o modelo probit 
modelo_probit <- glm(train ~ unem74 + unem75 + age + educ + black + hisp + married, data = df, family = binomial(link = "probit"))
modelo_referencia <- glm(train ~ 1, data = df, family = binomial(link = "probit"))
#Faznedo o teste
teste_rl <- anova(modelo_referencia, modelo_probit, test = "LRT")
print(teste_rl)
#Fazendo a regressao para UNEM 
modelo_2 <- lm(unem78 ~ train, data = df)
summary(modelo_2)
#Modelo probit 
modelo_probit_2 <- glm(unem78 ~ train, data = df, family = binomial(link = "probit"))
summary(modelo_probit_2)
#Modelos lInear e probit com as outras variaveis 
modelo_probit_va <- glm(unem78 ~ train + unem74 + unem75 + age + educ + black + hisp + married, data = df, family = binomial(link = "probit"))
modelo_linear_va <- lm(unem78 ~ train + unem74 + unem75 + age + educ + black + hisp + married, data = df)
#Probabilidades
prob_linear <- predict(modelo_linear_va, type = "response")
prob_probit <- predict(modelo_probit_va, type = "response")
#Correlacao
correlacao <- cor(prob_linear, prob_probit)
print(correlacao)
#Vendo coeficientes 
coeficientes <- coef(modelo_linear_va)
epm_train <- coeficientes["train"]
print(paste("O efeito parcial médio de train na probabilidade de desemprego de 1978 é:", epm_train))


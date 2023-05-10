#Chamando as bibliotecas
library(tidyverse) 
library(wooldridge) 
library(robustbase)
library(car) 
library(lmtest)
library(plm)
library(modelsummary)
library(stargazer)
library(jtools) 
#Gerando o dataframe 
df = airfare
?airfare
view(df)
#Gerando a variável concenbar das médias e colocando elas no dataframe 
df %<>%
  group_by(id) %>%
  mutate(concenbar = mean(concen))
view(df)
#Descobrindo o valor minimo e maximo 
print(paste0("O valor máximo é: ", max(df$concenbar)))
print(paste0("O valor máximo é: ", min(df$concenbar)))
#Estimando por efeitos aleatórios 
ea = lfare ~ y98 + y99 + y00 + concen + ldist + ldistsq + concenbar
modelo_ea = plm(formula = ea, data = df,
                      model = "random",
                      index = c("id", "year"))
summary(modelo_ea)
#Excluindo ldist e ldistsq
ea_1 = lfare ~ y98 + y99 + y00 + concen + concenbar
modelo_ea_1 = plm(formula = ea_1, data = df,
                model = "random",
                index = c("id", "year"))
summary(modelo_ea_1)
#Teste de Hipotese para o coeficiente de concenbar 
linearHypothesis(modelo_ea, c("concenbar=0"))
#Fazendo o teste t
#Covariância Robusta
cov <- vcovHC(modelo_ea, type = "HC1")
#Erro padrão 
robust.se.modelo_ea <- sqrt(diag(cov))
#Tabela 
stargazer(modelo_ea,
          title = "Modelo de Efeitos Aleatórios com Erros Robustos",
          type = "text",
          se = list(robust.se.modelo_ea),
          decimal.mark = ",", digit.separator = ".",
          align = T, no.space = T, single.row = T)



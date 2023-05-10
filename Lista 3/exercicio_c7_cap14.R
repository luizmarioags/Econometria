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
#Criando o dataframe 
df = murder
?murder
view(df)
#Estimando o modelo usando as dummies anuais por efeitos fixos
#Index = state e year
ef_murder = mrdrte ~ exec + unem + d90 + d93 
modelo_murder = plm(formula = ef_murder, data = df,
                        model = "within",
                        index = c("state", "year"))
summary(modelo_murder)
#Covariância Robusta
cov = vcovHC(modelo_murder, type = "HC1")
#Erro padrão 
robust.se.modelo_murder = sqrt(diag(cov))
stargazer(modelo_murder,
          title = "Efeitos Fixos com Erros Robustos",
          type = "text",
          se = list(robust.se.modelo_murder),
          decimal.mark = ",", digit.separator = ".",
          align = T, no.space = T, single.row = T)
#Maior número para 1993 e identificar o Estado 
df_93 = df[df$d93 == 1,]
view(df_93)
#Estado com valor máximo para exec
max_exec = which.max(df_93$exec)
state = df_93$state[max_exec]
#Printando o Estado
print(state)
#Excluindo o Texas da análise 
df_fd = df[df$state != "TX",]
view(df_fd)
#Estimando o modelo usando Primeiras diferenças 
fd = mrdrte ~ exec + unem + d90 + d93 
modelo_fd <- plm(formula = fd, data = df_fd,
                 index = c("state", "year"),
                 model = "fd")
summary(modelo_fd)
#Estimando por Efeitos Fixos com o Texas(usamos o df original)
#Gerando uma dummie para o ano de 1987 
df$year = factor(df$year)
view(df)
ef_murder_total = mrdrte ~ exec + unem + year
modelo_ef = plm(formula = ef_murder_total, data = df,
                      model = "within",
                      index = c("state", "year"))
summary(modelo_ef)

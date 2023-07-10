library(tidyverse)
library(wooldridge)
#Dataframe 
df <- loanapp
?loanapp
View(df)
# Estimar o modelo Probit
probit_model <- glm(approve ~ white, data = loanapp, family = binomial(link = "probit"))
summary(probit_model)
# Obter as probabilidades estimadas de aprovação para brancos e não brancos
prob_white <- predict(probit_model, newdata = data.frame(white = 1), type = "response")
prob_nonwhite <- predict(probit_model, newdata = data.frame(white = 0), type = "response")

# Imprimir as probabilidades estimadas
print(paste("Probabilidade estimada de aprovação para brancos:", prob_white))
print(paste("Probabilidade estimada de aprovação para não brancos:", prob_nonwhite))
# Estimar o modelo Probit com a variável hrat
probit_model_2 <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, data = df, family = binomial(link = "probit"))
summary(probit_model_2)

# Estimar o modelo Logit com a variável hrat
logit_model <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, data = df, family = binomial(link = "logit"))
summary(logit_model)

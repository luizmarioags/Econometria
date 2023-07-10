library(tidyverse)
library(wooldridge)
library(margins)
library(AER)
#Df
df <- charity
?charity
#  Parcela de pessoas que responderam apenas recentemente
parcela_resposta_recente <- sum(df$respond == 1 & df$resplast == 0) / nrow(df)
print(paste("A parcela das pessoas que responderam apenas recentemente é:", parcela_resposta_recente))
#  Estimar um modelo probit para respond
modelo_probit <- glm(respond ~ resplast + weekslast + propresp + mailsyear + avggift, data = df, family = binomial(link = "probit"))
summary(modelo_probit)
#  Efeito parcial médio para mailsyear no modelo probit
efeito_parcial_mailsyear <- margins(modelo_probit, variables = "mailsyear")
print(efeito_parcial_mailsyear)

#  Estimar um modelo tobit para gift
modelo_tobit <- tobit(gift ~ resplast + weekslast + propresp + mailsyear + avggift, data = df)
resumo_tobit <- summary(modelo_tobit)
print(resumo_tobit)

# Comparar APE tobit para mailsyear com o da regressão linear
modelo_linear <- lm(gift ~ resplast + weekslast + propresp + mailsyear + avggift, data = df)
efeito_parcial_mailsyear_linear <- margins(modelo_linear, variables = "mailsyear")
print(efeito_parcial_mailsyear_linear)

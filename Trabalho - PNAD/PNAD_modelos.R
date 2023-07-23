library(data.table)
library(dplyr)
library(ggplot2)
library(xtable)
library(tidyr)
library(jtools)
library(margins)
library(censReg)
library(plm)
library(stargazer)
library(fastDummies)
library(ivreg)

# Lendo painel
painel_pnad <- fread("https://raw.githubusercontent.com/luizmarioags/Econometria/main/Trabalho%20-%20PNAD/Painel3_pia_nova.csv", data.table = FALSE)

# Deletando coluna Estados que está corrompida
painel_pnad <- painel_pnad %>% select(-Estado)

# Criar uma coluna "Estado" e fazer o match com a coluna "UF"
estado_ufs <- c(
  "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba",
  "Pernambuco", "Alagoas", "Sergipe", "Bahia"
)
estado_ufs <- setNames(estado_ufs, 21:29)
painel_pnad$Estado <- estado_ufs[as.character(painel_pnad$UF)]

# Verificar se existem NaNs ou NAs em todo o dataframe painel_pnad
has_any_na <- any(is.na(painel_pnad))

# Exibir o resultado
print(has_any_na)

# Criando a variável "ID_Entrevistado"
painel_pnad <- painel_pnad %>%
  dplyr::mutate(ID_Entrevistado = paste(UPA, V1008, V1016, V2003, sep = "-")) %>% 
  dplyr::select(ID_Entrevistado, Ano, Trimestre, everything()) %>% 
  dplyr::arrange(ID_Entrevistado, Ano, Trimestre)

# criando uma variável Ano_trim
painel_pnad$Ano_trim <- paste0(painel_pnad$Ano, "_", painel_pnad$Trimestre)
painel_pnad$Ano_trim <- factor(painel_pnad$Ano_trim)

# Criando a variável log(salario/hora)
# como log(0) = -inf, temos que adicionar uma constante para fazer o log
# qual o mínimo?
painel_pnad %>% 
  select(salario_hora) %>% 
  filter(salario_hora > 0) %>%
  summarise(min = min(salario_hora))

# vamos adicionar um décimo desse mínimo como constante
painel_pnad <- painel_pnad %>%
  mutate(log_salario_hora = log(salario_hora + 0.01666667))

# Dummies Educacionais agrupadas
painel_pnad$educ_prealf <- ifelse(is.na(painel_pnad$VD3004), 1, 0)
painel_pnad$educ_instrunull <- ifelse(painel_pnad$VD3004 == 1, 1, 0)
painel_pnad$educ_fundincomp <- ifelse(painel_pnad$VD3004 == 2, 1, 0)
painel_pnad$educ_fundcomp <- ifelse(painel_pnad$VD3004 %in% c(3, 4), 1, 0)
painel_pnad$educ_medcomp <- ifelse(painel_pnad$VD3004 %in% c(5, 6), 1, 0)
painel_pnad$educ_supcomp <- ifelse(painel_pnad$VD3004 == 7, 1, 0)
#Criando a variável educ referente a instrução completa do individuo 
#1 para quem tem o nível superior completo, 0 para quem não tem 
#Condensar o nível educacional para o logit
painel_pnad$educ <- ifelse(painel_pnad$educ_supcomp == 1,1,0)
#Dummy de emprego (item 4)
painel_pnad <- painel_pnad %>% 
  mutate(d_emprego = ifelse(VD4002 %in% c(2) | 
                              (VD4001 == 2 & VD4002 %in% c(NA)), 0, 1),)
#0: pessoas desocupadas, fora da força de trabalho ou  aplicável
#1: pessoas ocupadas ou na força de trabalho

#dummies de área urbana tirando resto da UF
painel_pnad <- dummy_cols(
  painel_pnad,
  select_columns = 'V1023',
  remove_first_dummy = FALSE,
  remove_most_frequent_dummy = TRUE,
  ignore_na = TRUE,
  split = TRUE,
  remove_selected_columns = FALSE
)


#Dummy de zona 
painel_pnad <- painel_pnad %>%
  filter(V1022 == 1 | V1022 == 2)%>%
  mutate(d_zona = ifelse(V1022 == 1, 1,0)) #1: urbano; 0: rural
# Criando dummies para rural e cidade
painel_pnad <- painel_pnad %>%
  mutate(cidade = ifelse(d_zona == 1, 1, 0),
         rural = ifelse(d_zona == 0, 1, 0))
#Dummy área          
painel_pnad <- painel_pnad %>%
  mutate(d_capital = ifelse(V1023 == 1, 1, 0))

#Dummy empregado doméstico presente no domicílio
painel_pnad <- painel_pnad  %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(trab_domestico = if_else(any(V2005 == 18),1,0))
#Dummy casado 
painel_pnad <- painel_pnad %>% 
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(casado = if_else(any(V2005 == 2) | any(V2005 == 3), 1, 0))
#Num filhos
painel_pnad <- painel_pnad %>%
  group_by(ID_Entrevistado) %>%
  mutate(num_filhos = sum(V2005 %in% c(4, 5, 6)))
print(painel_pnad$num_filhos)
# Criando a variável filho_menor_10 com base nas condições
painel_pnad <- painel_pnad %>%
  mutate(filho_menor_10 = ifelse(V2009 <= 10 & V2005 %in% c(4, 5, 6), 1, 0))

# Criando a variável filho_11a18 com base nas condições
painel_pnad <- painel_pnad %>%
  mutate(filho_11a18 = ifelse(V2009 <= 18 & V2005 %in% c(4, 5, 6), 1, NA)) %>%
  mutate(filho_11a18 = ifelse(V2009 <= 10, NA, filho_11a18))

# Criando a variável filho_menor_18 com base nas condições
painel_pnad <- painel_pnad %>%
  mutate(filho_menor_18 = ifelse(V2009 <= 18 & V2005 %in% c(4, 5, 6), 1, 0))
# Calculando o total de filhos menores de 18 anos para cada ID_Entrevistado
painel_pnad <- painel_pnad %>%
  group_by(ID_Entrevistado) %>%
  mutate(num_filho_18 = sum(filho_menor_18, na.rm = TRUE)) 

# Calculando o total de filhos menores de 10 anos para cada ID_Entrevistado
painel_pnad <- painel_pnad %>%
  group_by(ID_Entrevistado) %>%
  mutate(num_filho_menor10 = sum(filho_menor_10, na.rm = TRUE)) 

# Calculando o total de filhos entre 11 e 18 anos para cada ID_Entrevistado
painel_pnad <- painel_pnad %>%
  group_by(ID_Entrevistado) %>%
  mutate(num_filho_11a18 = sum(filho_11a18, na.rm = TRUE)) 
# Calculando o número total de filhos para cada ID_Entrevistado
painel_pnad <- painel_pnad %>%
  group_by(ID_Entrevistado) %>%
  mutate(num_total_filhos = num_filho_18 + num_filho_menor10 + num_filho_11a18)
print(painel_pnad$num_total_filhos)
# Criando a variável "mulher"
painel_pnad$mulher <- ifelse(painel_pnad$dummy_sexo == 0, 1, 0)
# Criando a variável PPI para identificar Pretos, Pardos e Indígenas
painel_pnad$ppi <- ifelse(painel_pnad$dummy_raca == 1, 1, 0)
# Criando a dummy para identificar os chefes
painel_pnad <- painel_pnad %>%
  group_by(ID_Entrevistado,Ano_trim) %>%
  mutate(chefe_domicilio = ifelse(V2005 == 1, 1, 0))
# Criando a variável renda_chefe apenas com informações dos chefes
painel_pnad <- painel_pnad %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(renda_chefe = ifelse(chefe_domicilio == 1, VD4019, NA)) %>%
  mutate(renda_chefe = replace_na(renda_chefe, 0)) %>%
  ungroup()
#idade ao quadrado
painel_pnad <- painel_pnad %>%
  mutate(idade2 = (V2009^2))


#dummy de tempo (anuais)
painel_pnad <- painel_pnad %>%
  mutate(d_2013 = ifelse(Ano == 2013, 1,0),
         d_2014 = ifelse(Ano == 2014, 1, 0),
         d_2015 = ifelse(Ano == 2015,1,0))
#Ocupados
painel_pnad_ocupados <- painel_pnad %>%
  filter(d_emprego == 1)
# Verifique os nomes das colunas (Se as variaveis foram criadas)
colnames(painel_pnad)

#Modelando o Pooling 
formula_reg = log_salario_hora ~ mulher + ppi + educ + casado + num_total_filhos + V2009 + cidade + rural + d_2013 + d_2014 + d_2015 + d_capital + educ*ppi + educ*mulher + educ*d_capital + educ*rural + educ*cidade + educ*idade2 + educ*V2009 + casado*educ + num_total_filhos*educ + mulher*casado + mulher*ppi
#Fazendo a reg
reg_pooled <- plm(formula = formula_reg, 
                  data = painel_pnad,
                  model = "pooling",
                  index = c("ID_Entrevistado", "Ano_trim"))

summary(reg_pooled)
#Interpretação dos resultados 
########
#mulher: É uma variável binária que indica se o entrevistado é do sexo feminino (1) ou masculino (0). O coeficiente estimado é -1.2284. O efeito marginal de ser mulher no logaritmo do salário por hora é uma diminuição média de 1.2284 unidades, ceteris paribus (ou seja, mantendo todas as outras variáveis constantes).

#ppi: Representa uma variável contínua relacionada ao Índice de Pobreza multidimensional. O coeficiente estimado é -0.083639. O efeito marginal de um aumento em uma unidade no índice de pobreza é uma diminuição média de 0.083639 unidades no logaritmo do salário por hora, ceteris paribus.

#educ: É uma variável contínua que representa o nível de educação do entrevistado. O coeficiente estimado é -2.0444. O efeito marginal de um aumento em uma unidade no nível de educação é uma diminuição média de 2.0444 unidades no logaritmo do salário por hora, ceteris paribus.

#casado: É uma variável binária que indica se o entrevistado é casado (1) ou não (0). O coeficiente estimado é 0.22959. O efeito marginal de ser casado no logaritmo do salário por hora é um aumento médio de 0.22959 unidades, ceteris paribus.

#num_total_filhos: Representa o número total de filhos do entrevistado. O coeficiente estimado é -0.45581. O efeito marginal de um aumento em uma unidade no número total de filhos é uma diminuição média de 0.45581 unidades no logaritmo do salário por hora, ceteris paribus.

#V2009: É uma variável contínua que representa a idade do entrevistado. O coeficiente estimado é 0.31853. O efeito marginal de um aumento em uma unidade na idade é um aumento médio de 0.31853 unidades no logaritmo do salário por hora, ceteris paribus.

#cidade: É uma variável binária que indica se o entrevistado vive em uma cidade (1) ou não (0). O coeficiente estimado é 0.73362. O efeito marginal de viver em uma cidade no logaritmo do salário por hora é um aumento médio de 0.73362 unidades, ceteris paribus.

#rural: É uma variável binária que indica se o entrevistado vive em área rural (1) ou não (0). Como não foram fornecidos os resultados para essa variável no resumo do modelo, não é possível interpretar seu efeito marginal a partir dessas informações.

#d_2013, d_2014, d_2015: São variáveis binárias que indicam se a observação é referente aos anos 2013, 2014 ou 2015, respectivamente. Como não foram fornecidos os resultados para essas variáveis no resumo do modelo, não é possível interpretar seus efeitos marginais a partir dessas informações.

#d_capital: É uma variável binária que indica se o entrevistado vive na capital (1) ou não (0). O coeficiente estimado é 0.40677. O efeito marginal de viver na capital no logaritmo do salário por hora é um aumento médio de 0.40677 unidades, ceteris paribus.

#educ*ppi, educ*mulher, educ*d_capital, educ*rural, educ*cidade, educ*idade2, educ*V2009, casado*educ, num_total_filhos*educ, mulher*casado, mulher*ppi: São termos de interação entre as variáveis educacionais e outras variáveis no modelo. Os efeitos marginais dessas interações dependem do valor específico de cada variável envolvida na interação e, portanto, precisariam ser analisados com mais detalhes para uma interpretação completa.


########
#Modelo Efeitos Fixos e Aleatorios 
#efeitos fixos 
mod1.ef <- plm(formula = formula_reg, data = painel_pnad, 
               model = 'within', index = c('Ano_trim'))
summary(mod1.ef)
#Interpretação do Modelo within 

#O modelo "within" é uma das abordagens de estimação de dados em painel e é útil quando estamos interessados principalmente em eliminar o efeito fixo individual (efeito inerente a cada indivíduo observado ao longo do tempo) das variáveis explicativas. A estimação dentro do modelo "within" remove o efeito fixo individual e mantém apenas a variação das variáveis dentro de cada indivíduo, permitindo que analisemos o efeito das mudanças dessas variáveis ao longo do tempo.

#Vamos interpretar cada variável incluída no modelo "within" e discutir seus efeitos marginais:
  
# A Estimação do modelo within resultou com coeficientes próximos aos do pooling, porém é necessário dizer que o p valor de cada varíavel é distinto

# Os valores de R-squared e F-statistic são estatísticas utilizadas para avaliar o ajuste do modelo e a significância conjunta das variáveis explicativas. O R-squared (0.27546) representa a proporção da variabilidade total da variável dependente (log_salario_hora) que é explicada pelas variáveis independentes do modelo. O F-statistic (13533.5) é usado para testar a significância global do modelo, ou seja, se as variáveis explicativas, em conjunto, têm um efeito significativo sobre a variável dependente. O baixo p-value (< 2.22e-16) indica que o modelo é estatisticamente significativo.

# A principal diferença entre o modelo Pooling e o modelo "within" é a forma como eles tratam o efeito fixo individual. Enquanto o modelo Pooling estima um único coeficiente para cada variável, considerando todos os indivíduos em conjunto, o modelo "within" estima o efeito de cada variável dentro de cada indivíduo, eliminando o efeito fixo individual e mantendo apenas as variações das variáveis ao longo do tempo. Portanto, a interpretação dos coeficientes e efeitos marginais pode ser mais apropriada no modelo "within" para identificar as relações entre as variáveis ao longo do tempo e dentro de cada indivíduo.

#efeitos aleatórios
# Balancear o painel removendo observações com valores NA
painel_balanceado <- na.omit(painel_pnad)

# Ajustar o modelo com efeitos aleatórios usando o método "between"
mod1.ea_between <- plm(formula = formula_reg, data = painel_balanceado,
                       model = "between", index = c("ID_Entrevistado", "Ano_trim"))
summary(mod1.ea_between)

#O modelo "Between" é outra abordagem de estimação de dados em painel, mas com foco na estimação do efeito médio das variáveis entre os indivíduos, ao invés de estimar o efeito ao longo do tempo ou dentro de cada indivíduo. Nesse modelo, os efeitos fixos individuais são removidos por meio da média ao longo do tempo de cada variável para cada indivíduo.

#Aqui estão as interpretações das variáveis e seus efeitos marginais no modelo "Between":
  
# mulher: Como nos modelos anteriores, é uma variável binária que indica se o entrevistado é do sexo feminino (1) ou masculino (0). O coeficiente estimado é -0.3124232. O efeito marginal de ser mulher no logaritmo do salário por hora é uma diminuição média de 0.3124232 unidades, ceteris paribus.

# ppi: Como nos modelos anteriores, é uma variável contínua relacionada ao Índice de Pobreza multidimensional. O coeficiente estimado é 0.0299743. O efeito marginal de um aumento em uma unidade no índice de pobreza é um aumento médio de 0.0299743 unidades no logaritmo do salário por hora, ceteris paribus. No entanto, o valor-p (0.1383753) indica que essa relação não é estatisticamente significativa.

# educ: Variável contínua que representa o nível de educação do entrevistado.
# Coeficiente estimado é -0.7280898.
# Efeito marginal de um aumento em uma unidade no nível de educação é uma diminuição média de 0.7280898 unidades no logaritmo do salário por hora, ceteris paribus.
# No entanto, o valor-p (0.7982228) indica que essa relação não é estatisticamente significativa.

# V2009: Variável contínua que representa a idade do entrevistado.
# Coeficiente estimado é -1.4850714.
# Efeito marginal de um aumento em uma unidade na idade é uma diminuição média de 1.4850714 unidades no logaritmo do salário por hora, ceteris paribus.

# cidade: Variável binária que indica se o entrevistado vive em uma cidade (1) ou não (0).
# Coeficiente estimado é 0.1239001.
# Efeito marginal de viver em uma cidade no logaritmo do salário por hora é um aumento médio de 0.1239001 unidades, ceteris paribus.

# d_2013 e d_2014: Variáveis binárias que indicam se a observação pertence ao ano de 2013 ou 2014, respectivamente.
# Coeficientes estimados são 0.1024933 e 0.0803429, respectivamente.
# Efeitos marginais indicam que pertencer ao ano de 2013 ou 2014 está associado a um aumento médio de 0.1024933 e 0.0803429 unidades no logaritmo do salário por hora, ceteris paribus.

# d_capital: Variável binária que indica se o entrevistado vive na capital (1) ou não (0).
# Coeficiente estimado é -0.1499227.
# Efeito marginal de viver na capital no logaritmo do salário por hora é uma diminuição média de 0.1499227 unidades, ceteris paribus.

# idade2: Variável que representa o quadrado da idade do entrevistado.
# Coeficiente estimado é 0.0555685.
# Efeito marginal da idade no logaritmo do salário por hora é não linear e depende do valor específico da idade do entrevistado.

# ppi:educ, educ:d_capital, mulher:ppi: São termos de interação entre as variáveis no modelo.
# Assim como nos modelos anteriores, os efeitos marginais dessas interações dependem do valor específico de cada variável envolvida na interação e requerem uma análise mais detalhada.

# R-squared (0.074763): Representa a proporção da variabilidade total da variável dependente (log_salario_hora) que é explicada pelas variáveis independentes do modelo.
# O valor baixo de R-squared indica que as variáveis explicativas têm uma capacidade limitada de explicar a variabilidade da variável dependente no modelo "Between".

# Valor-p: É utilizado para avaliar a significância estatística das variáveis independentes.
# Variáveis com valor-p menor que 0.05 (nível de significância de 5%) são consideradas estatisticamente significativas.
# Nesse modelo, todas as variáveis têm valor-p significativo, exceto ppi, educ e mulher:educ.
# Isso indica que as variáveis ppi, educ e a interação mulher:educ não têm um efeito estatisticamente significativo no logaritmo do salário por hora, ceteris paribus.


#comparando EF com EA
stargazer(mod1.reg, mod1.ea_between,
          title = "Comparação de Modelos de Painel",
          decimal.mark = ",", digit.separator = ".",
          align = TRUE, no.space = TRUE, column.sep.width = "2pt",
          type = "latex", dep.var.labels = c("logsalh"), model.names = F,
          column.labels = c("EF", "EA"))

#teste de hausman
phtest(mod1.ea_between, mod1.ef) #p-valor < 0,0001 (efeitos fixos é melhor)
# Resultado do Teste de Hausman
# O teste de Hausman é usado para comparar os estimadores dos modelos "Pooling" e "Within"
# A hipótese nula do teste é que os estimadores do modelo "Between" são consistentes e eficientes
# O valor-p extremamente baixo (p-value < 2.2e-16) indica que há uma diferença significativa entre os estimadores dos dois modelos
# Portanto, rejeitamos a hipótese nula e concluímos que os estimadores do modelo "Between" são inconsistentes

# Resultado do Teste de Hausman
# O teste de Hausman é usado para comparar os estimadores dos modelos "Pooling" e "Within"
# A hipótese nula do teste é que os estimadores do modelo "Between" são consistentes e eficientes
# O valor-p extremamente baixo (p-value < 2.2e-16) indica que há uma diferença significativa entre os estimadores dos dois modelos
# Portanto, rejeitamos a hipótese nula e concluímos que os estimadores do modelo "Between" são inconsistentes

# Comparação entre os modelos "Pooling" e "Within"

# Modelo "Pooling":
# - Assume que os efeitos individuais são constantes ao longo do tempo
# - A variável "ID_Entrevistado" é tratada como uma variável fixa, ignorando efeitos individuais únicos
# - Eficiente apenas quando os efeitos fixos são constantes para todos os indivíduos ao longo do tempo
# - Mais adequado para amostras grandes e quando os efeitos individuais não variam muito entre entrevistados

# Modelo "Within":
# - Remove os efeitos individuais (efeitos fixos) da regressão, considerando variações em relação à média do indivíduo ao longo do tempo
# - Apropriado quando os efeitos individuais são importantes e variam ao longo do tempo
# - Mais eficiente do que o modelo "Pooling" quando os efeitos fixos variam entre os indivíduos
# - Mais adequado quando a variabilidade dos efeitos individuais é grande e a amostra não é muito pequena

# Com base no resultado do teste de Hausman, que indica a presença de efeitos individuais importantes e variáveis ao longo do tempo,
# concluímos que o modelo "Within" é mais adequado para a estimação. Esse modelo leva em conta a variabilidade dos efeitos individuais,
# tornando os estimadores mais eficientes e consistentes na análise dos dados.




#Primeiras diferenças
mod1.pd <- plm(formula = formula_reg, data = painel_pnad,
               model = "fd", index = c("Ano_trim"))
summary(mod1.pd)
stargazer(mod1.pd,
          title = "Regressão MQ2E com Erros Robustos",
          type = "latex",
          se = list(sqrt(diag(vcovHC(mod1.pd, type = "HC1")))),
          decimal.mark = ",", digit.separator = ".",
          align = TRUE, no.space = TRUE, single.row = TRUE)

#interpretação do retorno de educ no salário/h
mod1.pd$coefficients[2] + mod1.pd$coefficients[12] + mod1.pd$coefficients[13]*mean(painel_pnad$V2009) + mod1.pd$coefficients[14]*mean(painel_pnad$idade2)

# O efeito marginal estimado da variável "mulher" no modelo de primeiras diferenças é de 744.1811 unidades no logaritmo do salário por hora, ceteris paribus. Isso significa que, em média, ser mulher está associado a um aumento de aproximadamente 744.1811 unidades no logaritmo do salário por hora, levando em conta as interações com a idade (V2009) e o quadrado da idade (idade2), e mantendo todas as outras variáveis constantes.

#MQO
mod.MQO <- lm(formula_reg, data = painel_pnad)
summary(mod.MQO)
#interpretação do retorno de educ no salário/h
mod.MQO$coefficients[2] + mod.MQO$coefficients[12] + 
  mod.MQO$coefficients[13] + mod.MQO$coefficients[14]+
  mod.MQO$coefficients[15]*mean(painel_pnad$V2009) + 
  mod.MQO$coefficients[16]*mean(painel_pnad$idade2) +
  mod.MQO$coefficients[17]



#MQ2E
#Variáveis instrumentais
#Média de anos de educação da familia\domicílio menos anos de educação da própria pessoa
painel_pnad <- painel_pnad  %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(media_educ_dom = (sum(VD3005) - VD3005)/(V2001-1)) %>% ungroup

painel_pnad$media_educ_dom <- painel_pnad$media_educ_dom

##Renda familiar do domicílio POR PESSOA
painel_pnad <- painel_pnad  %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(renda_familiar = sum(VD4019)/V2001) %>%
  ungroup()


# Educação do Conjugue
painel_pnad <- painel_pnad  %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(educ_conj = ifelse(casado == 1, rev(VD3005), NA)
  ) %>%
  ungroup()


#Média de anos de educação por UF
painel_pnad <- painel_pnad %>% 
  group_by(UF)  %>% 
  mutate(media_educ_territorio = mean(VD3005)) %>% ungroup()

# MQ2E COM INTERAÇÕES
mod1.MQ2E <- ivreg(log_salario_hora ~ mulher + ppi + educ + casado + num_total_filhos + V2009 + cidade + rural + d_2013 + d_2014 + d_2015 + d_capital + educ*ppi + educ*mulher + educ*d_capital + educ*rural + educ*cidade + educ*idade2 + educ*V2009 + casado*educ + num_total_filhos*educ + mulher*casado + mulher*ppi, 
                   data = painel_pnad)
summary(mod1.MQ2E , diagnostics= TRUE)
stargazer(mod1.MQ2E,
          title = "Regressão MQ2E com Erros Robustos",
          type = "text",
          se = list(sqrt(diag(vcovHC(mod1.MQ2E, type = "HC1")))),
          decimal.mark = ",", digit.separator = ".",
          align = TRUE, no.space = TRUE, single.row = TRUE)
# Interpretação dos efeitos nas variáveis no modelo IV

# No modelo de regressão IV dado por log_salario_hora ~ mulher + ppi + educ + casado + num_total_filhos + V2009 + cidade + rural + d_2013 + d_2014 + d_2015 + d_capital + educ * ppi + educ * mulher + educ * d_capital + educ * rural + educ * cidade + educ * idade2 + educ * V2009 + casado * educ + num_total_filhos * educ + mulher * casado + mulher * ppi, temos os seguintes efeitos nas variáveis:

# 1. Coeficiente estimado de "educ" (educ): Representa o efeito médio de um aumento de uma unidade no nível de educação do entrevistado no logaritmo do salário por hora, mantendo todas as outras variáveis constantes.

# 2. Coeficiente estimado da interação "educ:d_capital" (educ:d_capital): Representa a variação adicional no efeito de "educ" no logaritmo do salário por hora para aqueles que vivem na capital (d_capital = 1) em comparação com aqueles que não vivem na capital (d_capital = 0).

# 3. Coeficiente estimado da interação "educ:V2009" (educ:V2009): Representa a variação adicional no efeito de "educ" no logaritmo do salário por hora para um aumento de uma unidade na variável "V2009".

# 4. Coeficiente estimado da interação "educ:idade2" (educ:idade2): Representa a variação adicional no efeito de "educ" no logaritmo do salário por hora para um aumento de uma unidade na variável "idade2".

# 5. Coeficiente estimado da interação "mulher:educ" (mulher:educ): Representa o efeito adicional da interação entre "mulher" e "educ" no logaritmo do salário por hora, ceteris paribus.

# 6. Coeficiente estimado da interação "educ:cidade" (educ:cidade): Esta parte do código está incompleta, pois não foi fornecido o valor exato do coeficiente.

# Os coeficientes para as demais variáveis (mulher, ppi, casado, num_total_filhos, V2009, cidade, rural, d_2013, d_2014, d_2015, mulher:casado e mulher:ppi) foram explicados nas respostas anteriores.

# É importante lembrar que, neste modelo, estamos usando a regressão IV (Instrumental Variables), o que implica que algumas variáveis podem ter sido instrumentadas para controlar possíveis endogeneidades e vieses de seleção. A interpretação dos efeitos nas variáveis deve levar em consideração o contexto e o propósito da regressão IV.

#interpretação do retorno de educ no salário/h
mod1.MQ2E$coefficients[2] + mod1.MQ2E$coefficients[12] + 
  mod1.MQ2E$coefficients[13] + mod1.MQ2E$coefficients[14]+
  mod1.MQ2E$coefficients[15]*mean(painel_pnad$V2009) + 
  mod1.MQ2E$coefficients[16]*mean(painel_pnad_ocupados$idade2) +
  mod1.MQ2E$coefficients[17]


###Tabela comparando os modelos em painel e os modelo MQO e MQ2E

stargazer(mod1.ef, mod1.ea, mod1.pd, mod.MQO, mod1.MQ2E,
          list(sqrt(diag(vcovHC(mod1.ef, type = "HC1"))),
               sqrt(diag(vcovHC(mod1.ea, type = "HC1"))),
               sqrt(diag(vcovHC(mod1.pd, type = "HC1"))),
               sqrt(diag(vcovHC(mod.MQO, type = "HC1"))),
               sqrt(diag(vcovHC(mod1.MQ2E, type = "HC1")))),
          title = "Comparação de Modelos de Painel com os modelos MQO e MQ2E",
          decimal.mark = ",", digit.separator = ".",
          align = TRUE, no.space = TRUE, column.sep.width = "2pt",
          type = "latex", dep.var.labels = c("logsalh"), model.names = F,
          column.labels = c("EF", "EA", "PD", "MQO", "MQ2E"))


#Item 4
#Criando as dummies para o número de filhos (Menor que 10) e entre 11 a 18
painel_pnad <- painel_pnad %>%
  mutate(d_10filhos = ifelse(num_total_filhos <= 10, 1, 0),
         d_11_18f = ifelse(num_total_filhos >= 11 & num_total_filhos <= 18, 1, 0))
#renda familiar sem a pessoa (Dividido por mil)
painel_pnad_ocupados$VD4019_0 <- painel_pnad_ocupados$VD4019
painel_pnad_ocupados$VD4019_0[is.na(painel_pnad$VD4019_0)] = 0
print(painel_pnad$VD4019)
print(painel_pnad$VD4019_0)
#Soma das Rendas 
painel_pnad_ocupados <- painel_pnad_ocupados  %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(soma_rendas = sum(VD4019)) %>%
  ungroup()
print(painel_pnad_ocupados$soma_rendas)
painel_pnad<- painel_pnad  %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(soma_rendas = sum(VD4019)) %>%
  ungroup()
#Renda Individual

painel_pnad_ocupados <- painel_pnad_ocupados  %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(n_renda_ind = (sum(VD4019) - renda_chefe/1000)) %>% 
  ungroup()
print(painel_pnad_ocupados$n_renda_ind)
painel_pnad <- painel_pnad  %>%
  group_by(ID_Entrevistado, Ano_trim) %>%
  mutate(n_renda_ind = (sum(VD4019) - renda_chefe/1000)) %>% 
  ungroup()
print(painel_pnad_ocupados$n_renda_ind)
#Modelos Probit e Logit 

#Modelo Probit 
# Base para mulheres (somente mulheres)
base_Probit_M <- painel_pnad %>%
  dplyr::select(mulher, d_emprego, n_renda_ind, casado, educ, ppi,cidade,rural,
                V2009, idade2, d_11_18f, d_10filhos, trab_domestico) %>%
  filter(mulher == 1)
# Probit model para mulheres
Probit_M <- glm(formula = d_emprego ~ n_renda_ind + casado + educ + ppi +
                  V2009 + idade2 + d_11_18f + d_10filhos + trab_domestico + cidade + rural,
                family = binomial(link = "probit"),
                data = base_Probit_M)
summ(Probit_M,robust = "HC1",digits = getOption("jtools-digits", 5))

check_model(Probit_M)
#Efeitos Marginais Médios 
ProbitScalar_M <- mean(dnorm(predict(Probit_M, type = "link")))

# Neste modelo Probit, a variável dependente é "d_emprego", que representa a probabilidade de emprego da mulher entrevistada.
# O modelo examina a relação entre essa variável dependente e várias variáveis independentes.

# Interpretação dos resultados:
# O coeficiente de n_renda_ind é 0.32821, indicando que um aumento na renda individual está associado a um aumento na probabilidade de emprego para mulheres.
# O coeficiente de casado é 0.44355, mostrando que mulheres casadas têm uma probabilidade significativamente maior de estar empregadas.
# O coeficiente de educ é -0.58942, indicando que um aumento na educação está associado a uma diminuição na probabilidade de emprego para mulheres.
# O coeficiente de ppi é -0.01236 e não é estatisticamente significativo, sugerindo que a pertença a um grupo racial específico não está relacionada à probabilidade de emprego para mulheres.
# O coeficiente de V2009 é -0.13803, indicando que mulheres que são chefes do domicílio têm uma probabilidade significativamente menor de estar empregadas.
# O coeficiente de idade2 é 0.00221, mostrando que um aumento na idade está associado a um aumento na probabilidade de emprego para mulheres.
# O coeficiente de trab_domestico é -0.36432 e não é estatisticamente significativo, indicando que o trabalho doméstico não tem um efeito significativo na probabilidade de emprego para mulheres.
# O coeficiente de cidade é -0.47854, mostrando que mulheres que vivem em áreas urbanas têm uma probabilidade significativamente menor de estar empregadas em comparação com mulheres que vivem em áreas rurais.

# Pseudo-R² e outras estatísticas de ajuste do modelo também são apresentados.


# Probit model para homens
base_Probit_H <- painel_pnad %>%
  dplyr::select (mulher, d_emprego, n_renda_ind, casado, educ, ppi,cidade,rural,
                 V2009, idade2, d_11_18f, d_10filhos, trab_domestico) %>%
  filter(mulher == 0)
# Probit model para homens
Probit_H <- glm(formula = d_emprego ~ n_renda_ind + casado + educ + ppi +
                  V2009 + idade2 + d_11_18f + d_10filhos + trab_domestico + cidade + rural,
                family = binomial(link = "probit"),
                data = base_Probit_H)
summ(Probit_H,robust = "HC1",digits = getOption("jtools-digits", 5))

#Efeitos Marginais 
ProbitScalar_H <- mean(dnorm(predict(Probit_M, type = "link")))
ProbitScalar_H * coef(Probit_H)
# Neste modelo Probit para homens, a variável dependente é "d_emprego", que representa a probabilidade de emprego do homem entrevistado.

# O coeficiente de n_renda_ind é 0.37293, indicando que um aumento na renda individual está associado a um aumento na probabilidade de emprego para homens.
# O coeficiente de casado é -0.17381, mostrando que homens casados têm uma probabilidade significativamente menor de estar empregados.
# O coeficiente de educ é -0.30243, indicando que um aumento na educação está associado a uma diminuição na probabilidade de emprego para homens.
# O coeficiente de ppi é -0.01546 e não é estatisticamente significativo, sugerindo que a pertença a um grupo racial específico não está relacionada à probabilidade de emprego para homens.
# O coeficiente de V2009 é -0.16979, indicando que homens que são chefes do domicílio têm uma probabilidade significativamente menor de estar empregados.
# O coeficiente de idade2 é 0.00242, mostrando que um aumento na idade está associado a um aumento na probabilidade de emprego para homens.
# O coeficiente de trab_domestico é 6.13853, indicando que o trabalho doméstico tem um efeito significativamente positivo na probabilidade de emprego para homens.
# O coeficiente de cidade é -0.45940, mostrando que homens que vivem em áreas urbanas têm uma probabilidade significativamente menor de estar empregados em comparação com homens que vivem em áreas rurais.

# Pseudo-R² e outras estatísticas de ajuste do modelo também são apresentados.

# Efeitos Marginais:
# Os efeitos marginais representam a mudança na probabilidade de emprego para homens associada a um aumento de uma unidade nas variáveis independentes, mantendo todas as outras variáveis constantes.
# Por exemplo, um aumento de uma unidade em n_renda_ind está associado a um aumento de aproximadamente 0.030 na probabilidade de emprego para homens.
# De maneira semelhante, um aumento de uma unidade em educ está associado a uma diminuição de aproximadamente 0.024 na probabilidade de emprego para homens.não ser incluída no modelo final.

#Modelo Tobit usando o mesmo modelo do MQO
# Verificar se há censura nos dados
censored <- painel_pnad$log_salario_hora <= 0

# Ajuste o modelo Tobit
# Criar o objeto Surv corretamente
surv_time <- with(painel_pnad_ocupados, Surv(pmax(log_salario_hora, 0)))
unique(painel_pnad_ocupados$log_salario_hora)

# Ajustar o modelo Tobit
tobit_model <- censReg(surv_time ~ mulher + ppi + educ + casado + num_filhos + V2009 + cidade + rural + d_2013 + d_2014 + d_2015 + d_capital + educ:ppi + educ:mulher + educ:d_capital + educ:rural + educ:cidade + educ:idade2 + educ:V2009 + casado:educ + num_filhos:educ + mulher:casado + mulher:ppi,
                       data = painel_pnad_ocupados)
summary(tobit_model)
#Poisson para numero de filhos
painel_pnad_mulher <- painel_pnad %>% 
  filter(mulher == 1)
formula1_pois <- num_filhos ~ educ + n_renda_ind + V2009 + idade2 + 
  rural + d_capital + ppi  + casado + d_emprego

mod1.pois <- glm(formula1_pois, family=poisson, 
                 data =painel_pnad_mulher,
                 subset = (mulher == 1) )
summary(mod1.pois)
summ(mod1.pois, robust = "HC1",digits = getOption("jtools-digits", 5))
#ou usando stargzar
stargazer(mod1.pois,
          title = "Regressão Poisson com Erros Robustos",
          type = "latex",
          se = list(sqrt(diag(vcovHC(mod1.pois, type = "HC1")))),
          decimal.mark = ",", digit.separator = ".",
          align = TRUE, no.space = TRUE, single.row = TRUE)

stargazer(mod1.pois,
          title = "Regressão Poisson com Erros Robustos",
          type = "text",
          se = list(sqrt(diag(vcovHC(mod1.pois, type = "HC1")))),
          decimal.mark = ",", digit.separator = ".",
          align = TRUE, no.space = TRUE, single.row = TRUE)
# Comentário sobre os resultados do modelo Poisson:

# O modelo Poisson busca entender a relação entre o número de filhos das mulheres e um conjunto de variáveis independentes.
# Vamos focar principalmente nas variáveis "educ" (educação) e "n_renda_ind" (renda individual) e como elas afetam o número médio de filhos.

# Coeficiente de "educ":
# O coeficiente para a variável "educ" é positivo (0.4365) e altamente significativo (p < 0.001).
# Isso significa que, mantendo todas as outras variáveis constantes, cada aumento de uma unidade no nível educacional das mulheres
# está associado a um aumento de aproximadamente 0.44 unidades no número médio de filhos.
# Portanto, mulheres com maior nível de educação tendem a ter, em média, mais filhos em relação às mulheres com menor nível de educação.

# Coeficiente de "n_renda_ind":
# O coeficiente para a variável "n_renda_ind" é negativo (-7.169e-05) e altamente significativo (p < 0.001).
# Isso indica que, mantendo todas as outras variáveis constantes, um aumento de uma unidade na renda individual das mulheres
# está associado a uma diminuição de aproximadamente 0.00007169 unidades no número médio de filhos.
# Em outras palavras, mulheres com maior renda individual tendem a ter, em média, menos filhos em relação às mulheres com menor renda individual.

# As outras variáveis no modelo também têm efeitos significativos no número médio de filhos das mulheres.
# Por exemplo, a variável "idade2" (idade ao quadrado) possui um efeito negativo, indicando que o efeito da idade na quantidade de filhos
# diminui à medida que a mulher envelhece. A variável "ppi" (índice de pobreza pessoal) também tem um efeito negativo,
# sugerindo que mulheres com maior índice de pobreza tendem a ter menos filhos.

# Vale ressaltar que os resultados são baseados nos dados disponíveis na amostra e na especificação do modelo utilizado.
# Além disso, as interpretações dos coeficientes são feitas mantendo todas as outras variáveis constantes,
# o que pode não refletir completamente a realidade complexa das relações entre as variáveis estudadas.
# Portanto, é importante interpretar os resultados com cautela e considerar possíveis limitações e outras variáveis
# não incluídas no modelo que também podem influenciar o número de filhos das mulheres.


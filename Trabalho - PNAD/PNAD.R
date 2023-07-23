#Importando bibliotecas 
library(tidyverse)
library(openxlsx)
library(stargazer)   
library(xtable)
library(ggridges)
#Df
caminho <- "C:\\Users\\luizm\\OneDrive\\Documents\\2023 - 1\\Econometria\\Gráficos Pnad\\Nordeste"
painel_pnad <- read_csv("C:\\Users\\luizm\\OneDrive\\Documents\\2023 - 1\\Econometria\\Painel_final_PNAD.csv")
#Vizualizando df 
View(painel_pnad)
#Replace NA
painel_pnad <- painel_pnad %>%
  mutate_all(~replace(., is.na(.), 0))

print(painel_pnad)
# Criar uma coluna "Estado" e fazer o match com a coluna "UF"
estado_ufs <- c(
  "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba",
  "Pernambuco", "Alagoas", "Sergipe", "Bahia"
)
estado_ufs <- setNames(estado_ufs, 21:29)
painel_pnad$Estado <- estado_ufs[as.character(painel_pnad$UF)]
View(painel_pnad)
# Criando variável de categorias de anos de estudo
painel_pnad$Categoria_Ano_Estudo <- cut(painel_pnad$anos_estudo, breaks = c(0, 4, 8, 12, Inf),
                                        labels = c("Ano 1", "Ano 2", "Ano 3", "Ano 4"),
                                        right = FALSE, include.lowest = TRUE)
# Criando variável de categorias de idade
painel_pnad$Categoria_Idade <- cut(painel_pnad$idade, breaks = c(0, 20, 40, 60, Inf),
                                   labels = c("Até 20 anos", "21 a 40 anos", "41 a 60 anos", "Mais de 60 anos"),
                                   right = FALSE, include.lowest = TRUE)

# Filtrando dados para homens e mulheres de cada raça
total_ppi_homens <- filter(painel_pnad, dummy_raca == 1, dummy_sexo == 1)
total_brancos_homens <- filter(painel_pnad, dummy_raca == 0, dummy_sexo == 1)
total_brancos_mulheres <- filter(painel_pnad, dummy_raca == 0, dummy_sexo == 0)
total_ppi_mulheres <- filter(painel_pnad, dummy_raca == 1, dummy_sexo == 0)

# Contando o número de homens e mulheres
total_homens <- nrow(total_ppi_homens) + nrow(total_brancos_homens)
total_mulheres <- nrow(total_brancos_mulheres) + nrow(total_ppi_mulheres)

# Calculando os percentuais de cada raça por gênero
percentual_homens_brancos <- (nrow(total_brancos_homens) / total_homens) * 100
percentual_homens_ppi <- (nrow(total_ppi_homens) / total_homens) * 100
percentual_mulheres_brancas <- (nrow(total_brancos_mulheres) / total_mulheres) * 100
percentual_mulheres_ppi <- (nrow(total_ppi_mulheres) / total_mulheres) * 100

# Criando um data frame com os percentuais
df_percentuais <- data.frame(
  Gênero = c("Homem", "Homem", "Mulher", "Mulher"),
  Raça = c("Branco", "PPI", "Branco", "PPI"),
  Percentual = c(percentual_homens_brancos, percentual_homens_ppi, percentual_mulheres_brancas, percentual_mulheres_ppi),
  Total = c(nrow(total_brancos_homens), nrow(total_ppi_homens), nrow(total_brancos_mulheres), nrow(total_ppi_mulheres))
)
#Montando a tabela

# Caminho e nome do arquivo Excel de destino
caminho_arquivo <- "C:\\Users\\luizm\\OneDrive\\Documents\\2023 - 1\\Econometria\\Gráficos Pnad\\Nordeste\\Tabela\\tabela_raca_genero.xlsx"

# Escrever o dataframe para o arquivo Excel
write.xlsx(df_percentuais, caminho_arquivo, rowNames = FALSE)


# Visualizando o data frame com os percentuais
print(df_percentuais)

# Plotando o gráfico stacked
grafico_stacked <- ggplot(df_percentuais, aes(x = Gênero, y = Percentual, fill = Raça)) +
  geom_bar(stat = "identity") +
  labs(x = "Gênero", y = "Percentual", fill = "Raça") +
  ggtitle("Percentual de Raça por Gênero (PNAD)") +
  theme_minimal()
print(grafico_stacked)
#Estatísticas Descritivas das Variáveis
# Calcular estatísticas descritivas
# Calculando estatísticas descritivas para cada grupo
estatisticas_ppi_homens <- summarise(total_ppi_homens, Media = mean(anos_estudo), 
                                     Mediana = median(anos_estudo), 
                                     Desvio_Padrao = sd(anos_estudo),
                                     Minimo = min(anos_estudo),
                                     Maximo = max(anos_estudo))
estatisticas_brancos_homens <- summarise(total_brancos_homens, Media = mean(anos_estudo), 
                                         Mediana = median(anos_estudo), 
                                         Desvio_Padrao = sd(anos_estudo),
                                         Minimo = min(anos_estudo),
                                         Maximo = max(anos_estudo))
estatisticas_brancos_mulheres <- summarise(total_brancos_mulheres, Media = mean(anos_estudo), 
                                           Mediana = median(anos_estudo), 
                                           Desvio_Padrao = sd(anos_estudo),
                                           Minimo = min(anos_estudo),
                                           Maximo = max(anos_estudo))
estatisticas_ppi_mulheres <- summarise(total_ppi_mulheres, Media = mean(anos_estudo), 
                                       Mediana = median(anos_estudo), 
                                       Desvio_Padrao = sd(anos_estudo),
                                       Minimo = min(anos_estudo),
                                       Maximo = max(anos_estudo))


# Criando tabela com as estatísticas descritivas, frequência e porcentagem para cada grupo
tabela_estatisticas <- data.frame(
  Grupo = c("Homem PPI", "Homem Branco", "Mulher Branca", "Mulher PPI"),
  Média = c(estatisticas_ppi_homens$Media, estatisticas_brancos_homens$Media, 
            estatisticas_brancos_mulheres$Media, estatisticas_ppi_mulheres$Media),
  Mediana = c(estatisticas_ppi_homens$Mediana, estatisticas_brancos_homens$Mediana, 
              estatisticas_brancos_mulheres$Mediana, estatisticas_ppi_mulheres$Mediana),
  Desvio_Padrão = c(estatisticas_ppi_homens$Desvio_Padrao, estatisticas_brancos_homens$Desvio_Padrao, 
                    estatisticas_brancos_mulheres$Desvio_Padrao, estatisticas_ppi_mulheres$Desvio_Padrao),
  Mínimo = c(estatisticas_ppi_homens$Minimo, estatisticas_brancos_homens$Minimo, 
             estatisticas_brancos_mulheres$Minimo, estatisticas_ppi_mulheres$Minimo),
  Máximo = c(estatisticas_ppi_homens$Maximo, estatisticas_brancos_homens$Maximo, 
             estatisticas_brancos_mulheres$Maximo, estatisticas_ppi_mulheres$Maximo)
)

# Formatando tabela em LaTeX
tabela_latex <- stargazer(tabela_estatisticas, title = "Estatísticas Descritivas - Anos de Estudo",
                          type = "latex", header = FALSE)

# Caminho e nome do arquivo Excel de destino
caminho_arquivo <- "C:\\Users\\luizm\\OneDrive\\Documents\\2023 - 1\\Econometria\\Gráficos Pnad\\Nordeste\\Tabela\\tabela_estatisticas_descritivas.xlsx"

# Escrever o dataframe para o arquivo Excel
write.xlsx(tabela_estatisticas, caminho_arquivo, rowNames = FALSE)



# Calculando frequência e porcentagem para cada grupo e categoria de anos de estudo
frequencia_ppi_homens <- count(total_ppi_homens, Categoria_Ano_Estudo) %>%
  mutate(Porcentagem = n / sum(n) * 100)
frequencia_brancos_homens <- count(total_brancos_homens, Categoria_Ano_Estudo) %>%
  mutate(Porcentagem = n / sum(n) * 100)
frequencia_brancos_mulheres <- count(total_brancos_mulheres, Categoria_Ano_Estudo) %>%
  mutate(Porcentagem = n / sum(n) * 100)
frequencia_ppi_mulheres <- count(total_ppi_mulheres, Categoria_Ano_Estudo) %>%
  mutate(Porcentagem = n / sum(n) * 100)

# Criando tabela com a frequência e porcentagem para cada grupo e categoria de anos de estudo
tabela_frequencia <- data.frame(
  Grupos = c("Homem PPI", "Homem Branco", "Mulher PPI", "Mulher Branca"),
  Ano_1_Frequencia = c(frequencia_ppi_homens$n[1], frequencia_brancos_homens$n[1],
                       frequencia_ppi_mulheres$n[1], frequencia_brancos_mulheres$n[1]),
  Ano_1_Porcentagem = c(frequencia_ppi_homens$Porcentagem[1], frequencia_brancos_homens$Porcentagem[1],
                        frequencia_ppi_mulheres$Porcentagem[1], frequencia_brancos_mulheres$Porcentagem[1]),
  Ano_2_Frequencia = c(frequencia_ppi_homens$n[2], frequencia_brancos_homens$n[2],
                       frequencia_ppi_mulheres$n[2], frequencia_brancos_mulheres$n[2]),
  Ano_2_Porcentagem = c(frequencia_ppi_homens$Porcentagem[2], frequencia_brancos_homens$Porcentagem[2],
                        frequencia_ppi_mulheres$Porcentagem[2], frequencia_brancos_mulheres$Porcentagem[2]),
  Ano_3_Frequencia = c(frequencia_ppi_homens$n[3], frequencia_brancos_homens$n[3],
                       frequencia_ppi_mulheres$n[3], frequencia_brancos_mulheres$n[3]),
  Ano_3_Porcentagem = c(frequencia_ppi_homens$Porcentagem[3], frequencia_brancos_homens$Porcentagem[3],
                        frequencia_ppi_mulheres$Porcentagem[3], frequencia_brancos_mulheres$Porcentagem[3]),
  Ano_4_Frequencia = c(frequencia_ppi_homens$n[4], frequencia_brancos_homens$n[4],
                       frequencia_ppi_mulheres$n[4], frequencia_brancos_mulheres$n[4]),
  Ano_4_Porcentagem = c(frequencia_ppi_homens$Porcentagem[4], frequencia_brancos_homens$Porcentagem[4],
                        frequencia_ppi_mulheres$Porcentagem[4], frequencia_brancos_mulheres$Porcentagem[4])
)
View(tabela_frequencia)
# Formatando tabela em LaTeX
tabela_latex <- stargazer(tabela_frequencia, title = "Frequência dos Anos de Estudo por Grupo",
                          type = "latex", header = TRUE)




# Formatando tabela em LaTeX
tabela_latex <- stargazer(tabela_frequencia, title = "Frequência dos Anos de Estudo por Grupo",
                          type = "latex", header = TRUE)

# Caminho e nome do arquivo Excel de destino
caminho_arquivo <- "C:\\Users\\luizm\\OneDrive\\Documents\\2023 - 1\\Econometria\\Gráficos Pnad\\Nordeste\\Tabela\\tabela_frequencia_anos_estudo.xlsx"

# Escrever o dataframe para o arquivo Excel
write.xlsx(tabela_frequencia, caminho_arquivo, rowNames = FALSE)

# Calculando estatísticas descritivas para a variável "idade" e cada grupo
estatisticas_ppi_homens <- summarise(total_ppi_homens, 
                                     Media = mean(idade), 
                                     Mediana = median(idade), 
                                     Desvio_Padrao = sd(idade),
                                     Minimo = min(idade),
                                     Maximo = max(idade))
estatisticas_brancos_homens <- summarise(total_brancos_homens, 
                                         Media = mean(idade), 
                                         Mediana = median(idade), 
                                         Desvio_Padrao = sd(idade),
                                         Minimo = min(idade),
                                         Maximo = max(idade))
estatisticas_brancos_mulheres <- summarise(total_brancos_mulheres, 
                                           Media = mean(idade), 
                                           Mediana = median(idade), 
                                           Desvio_Padrao = sd(idade),
                                           Minimo = min(idade),
                                           Maximo = max(idade))
estatisticas_ppi_mulheres <- summarise(total_ppi_mulheres, 
                                       Media = mean(idade), 
                                       Mediana = median(idade), 
                                       Desvio_Padrao = sd(idade),
                                       Minimo = min(idade),
                                       Maximo = max(idade))

# Criando tabela com as estatísticas descritivas para cada grupo
tabela_estatisticas <- data.frame(
  Grupo = c("Homem PPI", "Homem Branco", "Mulher Branca", "Mulher PPI"),
  Média = c(estatisticas_ppi_homens$Media, estatisticas_brancos_homens$Media, 
            estatisticas_brancos_mulheres$Media, estatisticas_ppi_mulheres$Media),
  Mediana = c(estatisticas_ppi_homens$Mediana, estatisticas_brancos_homens$Mediana, 
              estatisticas_brancos_mulheres$Mediana, estatisticas_ppi_mulheres$Mediana),
  Desvio_Padrão = c(estatisticas_ppi_homens$Desvio_Padrao, estatisticas_brancos_homens$Desvio_Padrao, 
                    estatisticas_brancos_mulheres$Desvio_Padrao, estatisticas_ppi_mulheres$Desvio_Padrao),
  Mínimo = c(estatisticas_ppi_homens$Minimo, estatisticas_brancos_homens$Minimo, 
             estatisticas_brancos_mulheres$Minimo, estatisticas_ppi_mulheres$Minimo),
  Máximo = c(estatisticas_ppi_homens$Maximo, estatisticas_brancos_homens$Maximo, 
             estatisticas_brancos_mulheres$Maximo, estatisticas_ppi_mulheres$Maximo)
)

# Formatando tabela em LaTeX usando xtable
tabela_latex <- xtable(tabela_estatisticas, caption = "Estatísticas Descritivas - Idade")

# Imprimir código LaTeX da tabela
print(tabela_latex)

# Caminho e nome do arquivo Excel de destino
caminho_arquivo <- "C:\\Users\\luizm\\OneDrive\\Documents\\2023 - 1\\Econometria\\Gráficos Pnad\\Nordeste\\Tabela\\tabela_estatisticas_descritivas_idade.xlsx"

# Escrever o dataframe para o arquivo Excel
write.xlsx(tabela_estatisticas, caminho_arquivo, rowNames = FALSE)

# Calculando frequência e porcentagem para cada grupo e categoria de idade
frequencia_ppi_homens <- count(total_ppi_homens, Categoria_Idade) %>%
  mutate(Porcentagem = n / sum(n) * 100)
frequencia_brancos_homens <- count(total_brancos_homens, Categoria_Idade) %>%
  mutate(Porcentagem = n / sum(n) * 100)
frequencia_brancos_mulheres <- count(total_brancos_mulheres, Categoria_Idade) %>%
  mutate(Porcentagem = n / sum(n) * 100)
frequencia_ppi_mulheres <- count(total_ppi_mulheres, Categoria_Idade) %>%
  mutate(Porcentagem = n / sum(n) * 100)

# Criando tabela com a frequência e porcentagem para cada grupo e categoria de idade
tabela_frequencia <- data.frame(
  Grupos = c("Homem PPI", "Homem Branco", "Mulher PPI", "Mulher Branca"),
  Categoria_Idade_1_Frequencia = c(frequencia_ppi_homens$n[1], frequencia_brancos_homens$n[1],
                                   frequencia_ppi_mulheres$n[1], frequencia_brancos_mulheres$n[1]),
  Categoria_Idade_1_Porcentagem = c(frequencia_ppi_homens$Porcentagem[1], frequencia_brancos_homens$Porcentagem[1],
                                    frequencia_ppi_mulheres$Porcentagem[1], frequencia_brancos_mulheres$Porcentagem[1]),
  Categoria_Idade_2_Frequencia = c(frequencia_ppi_homens$n[2], frequencia_brancos_homens$n[2],
                                   frequencia_ppi_mulheres$n[2], frequencia_brancos_mulheres$n[2]),
  Categoria_Idade_2_Porcentagem = c(frequencia_ppi_homens$Porcentagem[2], frequencia_brancos_homens$Porcentagem[2],
                                    frequencia_ppi_mulheres$Porcentagem[2], frequencia_brancos_mulheres$Porcentagem[2]),
  Categoria_Idade_3_Frequencia = c(frequencia_ppi_homens$n[3], frequencia_brancos_homens$n[3],
                                   frequencia_ppi_mulheres$n[3], frequencia_brancos_mulheres$n[3]),
  Categoria_Idade_3_Porcentagem = c(frequencia_ppi_homens$Porcentagem[3], frequencia_brancos_homens$Porcentagem[3],
                                    frequencia_ppi_mulheres$Porcentagem[3], frequencia_brancos_mulheres$Porcentagem[3]),
  Categoria_Idade_4_Frequencia = c(frequencia_ppi_homens$n[4], frequencia_brancos_homens$n[4],
                                   frequencia_ppi_mulheres$n[4], frequencia_brancos_mulheres$n[4]),
  Categoria_Idade_4_Porcentagem = c(frequencia_ppi_homens$Porcentagem[4], frequencia_brancos_homens$Porcentagem[4],
                                    frequencia_ppi_mulheres$Porcentagem[4], frequencia_brancos_mulheres$Porcentagem[4])
)

View(tabela_frequencia)
# Formatando tabela em LaTeX usando xtable
tabela_latex <- xtable(tabela_frequencia, caption = "Frequências e Porcentagens - Idade")
print(tabela_latex)
# Caminho e nome do arquivo Excel de destino
caminho_arquivo <- "C:\\Users\\luizm\\OneDrive\\Documents\\2023 - 1\\Econometria\\Gráficos Pnad\\Nordeste\\Tabela\\tabela_frequencia_idade.xlsx"

# Escrever o dataframe para o arquivo Excel
write.xlsx(tabela_frequencia, caminho_arquivo, rowNames = FALSE)
#Distribuiçoes e Densidades
#Organizando dataframes
painel_pnad<- painel_pnad %>%
  group_by(Estado,anos_estudo,idade)
#Homens PPIs
total_ppi_homens <- total_ppi_homens %>%
  group_by(Estado,anos_estudo,idade)
#Homens Brancos
total_brancos_homens <- total_brancos_homens %>%
  group_by(Estado,anos_estudo,idade)
#Mulheres PPIs
total_ppi_mulheres <- total_ppi_mulheres %>%
  group_by(Estado,anos_estudo,idade)
#Mulheres Brancas
total_brancos_mulheres <- total_brancos_mulheres %>%
  group_by(Estado,anos_estudo,idade)
#Gráficos
plot_dist_anos_total <- ggplot(painel_pnad, aes(x = anos_estudo, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Distribuição dos Anos de Estudo por Estado da Região Nordeste",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario",
    x = 'Anos de Estudo',
    y = 'Estado'
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_anosestudo_nordeste.jpg"),
  plot = plot_dist_anos_total,
  width = 10,
  height = 6,
  dpi = 300
)

#Gráficos
plot_dist_anos_total <- ggplot(painel_pnad, aes(x = anos_estudo, y = ..density..)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Distribuição dos Anos de Estudo por idade da Região Nordeste",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario",
    x = 'Anos de Estudo',
    y = 'Densidade'
  )

print(plot_dist_anos_total)

ggsave(
  filename = paste0(caminho, "grafico_dist_anosestudo_idade_nordeste.jpg"),
  plot = plot_dist_anos_total,
  width = 10,
  height = 6,
  dpi = 300
)

#Anos de Estudo para Homens PPIs
plot_dist_anos_homens_ppi <- ggplot(total_ppi_homens, aes(x = anos_estudo, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Distribuição dos Anos de Estudo por Estado do Nordeste para Homens PPIs",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario"
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_anosestudo_homens_ppi.jpg"),
  plot = plot_dist_anos_homens_ppi,
  width = 10,
  height = 6,
  dpi = 300
)
#Anos de Estudo para Homens Brancos 
plot_dist_anos_homens_brancos <- ggplot(total_brancos_homens, aes(x = anos_estudo, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Distribuição dos Anos de Estudo por Estado do Nordeste para Homens Brancos",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario"
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_anosestudo_homens_brancos.jpg"),
  plot = plot_dist_anos_homens_brancos,
  width = 10,
  height = 6,
  dpi = 300
)
#Anos de Estudo para Mulheres 
plot_dist_anos_mulheres_ppis <- ggplot(total_ppi_mulheres, aes(x = anos_estudo, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Anos de Estudo por Estado do Nordeste para Mulheres PPIs",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario"
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_anosestudo_mulheres_ppis.jpg"),
  plot = plot_dist_anos_mulheres_ppis,
  width = 10,
  height = 6,
  dpi = 300
)
#Anos de Estudo para Mulheres 
plot_dist_anos_mulheres_brancas <- ggplot(total_brancos_mulheres, aes(x = anos_estudo, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Anos de Estudo por Estado do Nordeste para Mulheres Brancas",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario"
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_anosestudo_mulheres_brancas.jpg"),
  plot = plot_dist_anos_mulheres_brancas,
  width = 10,
  height = 6,
  dpi = 300
)

#Distribuição de Idade para PPIs Homens vs Brancos 


#Homens PPIs
plot_dist_idade_homens_ppis <- ggplot(total_ppi_homens, aes(x = idade, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Idade por estado do Nordeste para Homens PPIs",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario"
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_idade_homens_ppis.jpg"),
  plot = plot_dist_idade_homens_ppis,
  width = 10,
  height = 6,
  dpi = 300
)
#Idade para Homens Brancos
plot_dist_idade_homens_brancos <- ggplot(total_brancos_homens, aes(x = idade, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Idade por estado do Nordeste para Homens Brancos",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario"
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_idade_homens_brancos.jpg"),
  plot = plot_dist_idade_homens_brancos,
  width = 10,
  height = 6,
  dpi = 300
)
#Idade para mulheres PPIs 
plot_dist_idade_mulheres_ppis <- ggplot(total_ppi_mulheres, aes(x = idade, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Idade por estado do Nordeste para Mulheres PPIs",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario"
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_idade_mulheres_ppis.jpg"),
  plot = plot_dist_idade_mulheres_ppis,
  width = 10,
  height = 6,
  dpi = 300
)
#Idade para mulheres Brancas
plot_dist_idade_mulheres_brancas <- ggplot(total_brancos_mulheres, aes(x = idade, y = Estado, fill = Estado)) +
  geom_density_ridges(scale = 2, alpha = 0.6) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(
    title = "Idade por estado do Nordeste para Mulheres Brancas",
    subtitle = "Fonte: PNAD. Elaborado por: Luiz Mario"
  )
ggsave(
  filename = paste0(caminho, "grafico_dist_idade_mulheres_brancas.jpg"),
  plot = plot_dist_idade_mulheres_brancas,
  width = 10,
  height = 6,
  dpi = 300
)
#Boxplots 
View(painel_pnad)
painel_boxplot <- painel_pnad %>%
  group_by(dummy_raca,anos_estudo)
View(painel_boxplot)
print(painel_boxplot)

# Criar o boxplot para a variável "anos de estudo" por "raça"
ggplot(painel_boxplot, aes(x = dummy_raca, y = anos_estudo, fill = dummy_raca)) +
  geom_boxplot() +
  labs(title = "Boxplot - Raça e Anos de Estudo",
       x = "Raça",
       y = "Anos de Estudo",
       fill = dummy_raca) +
  scale_fill_manual(values = c("Branco" = "blue", "PPI" = "red")) +
  theme_minimal()



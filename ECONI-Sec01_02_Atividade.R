#############################
####### Métodos Exatos ######
### www.metodosexatos.com ###
#############################

# Autor: André Santos | andre@metodosexatos.com.br
# 20/07/2019

# To cite R in publications use:
# citation()

###################################################################

#  Atividade Final Seção I - A Natureza da Análise de Regressão   #

###################################################################

#--------------------- Diretórios e Arquivos ---------------------#

# getwd() # Qual o diretório que o script está apontando
# list.files() # Quais arquivos estão contidos no diretório
# setwd("C:/Users/andre/OneDrive/Documentos/PROJETOS/Metodos Exatos/Cursos/Curso017_Econometria_I/Curso-ECON_Material_apoio/Datasets_Econ-I")

# Leitura de uma base externa
# leitura_csv2 <- read.csv2(file = "exemplo.csv")

# Exportação de um arquivo no formato csv2 (formato brasileiro):
# write.csv2(frame_carros, "exemplo.csv")

# Leitura de arquivo externo usando pacotes

# if (!require(package)) install.packages("xlsx")
# library(xlsx)

# read.xlsx("exemplo.xlsx", sheetName = "nome_planilha")

# Salvar uma arquivo no formato xlsx
# write.xlsx(nome_dataframe, "exemplo.xlsx")

#---------------------------------------------------------------#

# RESOLUÇÃO:

#- Preparar o ambiente no R para ler base de dados:

getwd() # diretório que o R está apontando
# mudar pasta de trabalho:
setwd("C:/Users/andre/OneDrive/Documentos/PROJETOS/Metodos Exatos/Cursos/Curso017_Econometria_I/Curso-ECON_Material_apoio/Datasets_Econ-I")
list.files() # arquivos contidos no diretório

#- Leitura de uma base externa:
dados_tab13 <- read.csv2(file = "Tab_1-3_inflacao.csv") # carrega dataset para ambiente de trabalho
str(dados_tab13) # analisa a estrutura dos dados
head(dados_tab13) # exibe as primeiras linhas do dataset

#- Análise gráfica

#-- Instalação dos pacotes

if(!require(package)) install.packages("dplyr")        # instala pacote para manipulação de dados
if(!require(package)) install.packages("ggplot2")      # instala pacote para construir os gráficos
if(!require(package)) install.packages("gridExtra")    # instala pacote para colocar todos gráficos na mesma janela
library(dplyr)
library(ggplot2)
library(gridExtra)

####################### Atividades Tab 1.3 #####################
#******************* Início Atividade 1.1.b *******************#

#-- Gráficos:

gl_us_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = us)) +
  ggtitle("Estados Unidos") +
  xlab("Data em anos") +
  ylab("Taxa de inflação")

gl_canada_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = canada)) +
  ggtitle("Canada") +
  xlab("Data em anos") +
  ylab("Taxa de inflação")

gl_japan_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = japan)) + 
  ggtitle("Japan") +
  xlab("Data em anos") +
  ylab("Taxa de inflação")

gl_france_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = france)) +
  ggtitle("France") +
  xlab("Data em anos") +
  ylab("Taxa de inflação")

gl_germany_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = germany)) +
  ggtitle("Germany") +
  xlab("Data em anos") +
  ylab("Taxa de inflação")

gl_italy_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = italy)) +
  ggtitle("Italy") +
  xlab("Data em anos") +
  ylab("Taxa de inflação")

gl_uk_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = uk)) +
  ggtitle("United Kingdom") +
  xlab("Data em anos") +
  ylab("Taxa de inflação")

#-- gráficos em uma mesma janela

windows() # abri uma janela para exibir os gráficos
grid.arrange(gl_us_tab13, gl_canada_tab13, gl_japan_tab13,
             gl_france_tab13, gl_germany_tab13, gl_italy_tab13, gl_uk_tab13,
             ncol=3, nrow=3)

#******************* Fim Atividade 1.1.b *******************#

#******************* Início Atividade 1.2.a *******************#

#-- Gráficos:

gp_canada_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = canada)) +
  ggtitle("Canada") +
  xlab("Inflação US") +
  ylab("Inflação Canada")

gp_japan_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = japan)) + 
  ggtitle("Japan") +
  xlab("Inflação US") +
  ylab("Inflação Japão")

gp_france_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = france)) +
  ggtitle("France") +
  xlab("Inflação US") +
  ylab("Inflação França")

gp_germany_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = germany)) +
  ggtitle("Germany") +
  xlab("Inflação US") +
  ylab("Inflação Alemanha")

gp_italy_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = italy)) +
  ggtitle("Italy") +
  xlab("Inflação US") +
  ylab("Inflação Itália")

gp_uk_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = uk)) +
  ggtitle("United Kingdom") +
  xlab("Inflação US") +
  ylab("Inflação Reino Unido")

#-- gráficos em uma mesma janela

windows() # abri uma janela para exibir os gráficos
grid.arrange(gp_canada_tab13, gp_japan_tab13, gp_france_tab13,
             gp_germany_tab13, gp_italy_tab13, gp_uk_tab13,
             ncol=3, nrow=2)

#******************* Fim Atividade 1.2.a *******************#

####################### Atividades Tab 1.4 #####################
#******************* Início Atividade 1.3.a *******************#

#- Leitura de uma base externa:
dados_tab14 <- read.csv2(file = "Tab_1-4_moedas.csv") # carrega dataset para ambiente de trabalho
str(dados_tab14) # analisa a estrutura dos dados
head(dados_tab14) # exibe as primeiras linhas do dataset

#-- Gráficos:

gl_aus_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = dollar_aus)) +
  ggtitle("Australia") +
  xlab("Data em anos") +
  ylab("Câmbio")

gl_can_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = dollar_can)) +
  ggtitle("Canada") +
  xlab("Data em anos") +
  ylab("Câmbio")

gl_chi_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = yuan_chi)) +
  ggtitle("China") +
  xlab("Data em anos") +
  ylab("Câmbio")

gl_jap_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = yen_jap)) +
  ggtitle("Japão") +
  xlab("Data em anos") +
  ylab("Câmbio")

gl_mex_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = peso_mex)) +
  ggtitle("México") +
  xlab("Data em anos") +
  ylab("Câmbio")

gl_kor_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = won_kor)) +
  ggtitle("Korea") +
  xlab("Data em anos") +
  ylab("Câmbio")

gl_swe_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = krona_swe)) +
  ggtitle("Suécia") +
  xlab("Data em anos") +
  ylab("Câmbio")

gl_swi_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = franc_swi)) +
  ggtitle("Suíça") +
  xlab("Data em anos") +
  ylab("Câmbio")

gl_uk_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = pound_uk)) +
  ggtitle("Reino Unido") +
  xlab("Data em anos") +
  ylab("Câmbio")


#-- gráficos em uma mesma janela

windows() # abri uma janela para exibir os gráficos
grid.arrange(gl_aus_tab14, gl_can_tab14, gl_chi_tab14,
             gl_jap_tab14, gl_mex_tab14, gl_kor_tab14,
             gl_swe_tab14, gl_swi_tab14, gl_uk_tab14,
             ncol=3, nrow=3)

#******************* Fim Atividade 1.3.a *******************#

####################### Atividades Tab 1.6 #####################
#******************* Início Atividade 1.4.a *******************#

#-- Gráfico:

#- Leitura de uma base externa:
dados_tab16 <- read.csv2(file = "Tab_1-6_publicidade.csv") # carrega dataset para ambiente de trabalho
str(dados_tab16) # analisa a estrutura dos dados
head(dados_tab16) # exibe as primeiras linhas do dataset

gp_tab16 <- ggplot(dados_tab16) +
  geom_point(aes(x = despesa, y = impressao)) +
  ggtitle("Publicidade") +
  xlab("Investimento") +
  ylab("Impressões")

#-- gráficos em uma mesma janela

windows() # abri uma janela para exibir os gráficos
gp_tab16

#******************* Fim Atividade 1.4.a *******************#

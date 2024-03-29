#############################
####### M�todos Exatos ######
### www.metodosexatos.com ###
#############################

# Autor: Andr� Santos | andre@metodosexatos.com.br
# 20/07/2019

# To cite R in publications use:
# citation()

###################################################################

#  Atividade Final Se��o I - A Natureza da An�lise de Regress�o   #

###################################################################

#--------------------- Diret�rios e Arquivos ---------------------#

# getwd() # Qual o diret�rio que o script est� apontando
# list.files() # Quais arquivos est�o contidos no diret�rio
# setwd("C:/Users/andre/OneDrive/Documentos/PROJETOS/Metodos Exatos/Cursos/Curso017_Econometria_I/Curso-ECON_Material_apoio/Datasets_Econ-I")

# Leitura de uma base externa
# leitura_csv2 <- read.csv2(file = "exemplo.csv")

# Exporta��o de um arquivo no formato csv2 (formato brasileiro):
# write.csv2(frame_carros, "exemplo.csv")

# Leitura de arquivo externo usando pacotes

# if (!require(package)) install.packages("xlsx")
# library(xlsx)

# read.xlsx("exemplo.xlsx", sheetName = "nome_planilha")

# Salvar uma arquivo no formato xlsx
# write.xlsx(nome_dataframe, "exemplo.xlsx")

#---------------------------------------------------------------#

# RESOLU��O:

#- Preparar o ambiente no R para ler base de dados:

getwd() # diret�rio que o R est� apontando
# mudar pasta de trabalho:
setwd("C:/Users/andre/OneDrive/Documentos/PROJETOS/Metodos Exatos/Cursos/Curso017_Econometria_I/Curso-ECON_Material_apoio/Datasets_Econ-I")
list.files() # arquivos contidos no diret�rio

#- Leitura de uma base externa:
dados_tab13 <- read.csv2(file = "Tab_1-3_inflacao.csv") # carrega dataset para ambiente de trabalho
str(dados_tab13) # analisa a estrutura dos dados
head(dados_tab13) # exibe as primeiras linhas do dataset

#- An�lise gr�fica

#-- Instala��o dos pacotes

if(!require(package)) install.packages("dplyr")        # instala pacote para manipula��o de dados
if(!require(package)) install.packages("ggplot2")      # instala pacote para construir os gr�ficos
if(!require(package)) install.packages("gridExtra")    # instala pacote para colocar todos gr�ficos na mesma janela
library(dplyr)
library(ggplot2)
library(gridExtra)

####################### Atividades Tab 1.3 #####################
#******************* In�cio Atividade 1.1.b *******************#

#-- Gr�ficos:

gl_us_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = us)) +
  ggtitle("Estados Unidos") +
  xlab("Data em anos") +
  ylab("Taxa de infla��o")

gl_canada_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = canada)) +
  ggtitle("Canada") +
  xlab("Data em anos") +
  ylab("Taxa de infla��o")

gl_japan_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = japan)) + 
  ggtitle("Japan") +
  xlab("Data em anos") +
  ylab("Taxa de infla��o")

gl_france_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = france)) +
  ggtitle("France") +
  xlab("Data em anos") +
  ylab("Taxa de infla��o")

gl_germany_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = germany)) +
  ggtitle("Germany") +
  xlab("Data em anos") +
  ylab("Taxa de infla��o")

gl_italy_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = italy)) +
  ggtitle("Italy") +
  xlab("Data em anos") +
  ylab("Taxa de infla��o")

gl_uk_tab13 <- ggplot(dados_tab13) +
  geom_line(aes(x = ano, y = uk)) +
  ggtitle("United Kingdom") +
  xlab("Data em anos") +
  ylab("Taxa de infla��o")

#-- gr�ficos em uma mesma janela

windows() # abri uma janela para exibir os gr�ficos
grid.arrange(gl_us_tab13, gl_canada_tab13, gl_japan_tab13,
             gl_france_tab13, gl_germany_tab13, gl_italy_tab13, gl_uk_tab13,
             ncol=3, nrow=3)

#******************* Fim Atividade 1.1.b *******************#

#******************* In�cio Atividade 1.2.a *******************#

#-- Gr�ficos:

gp_canada_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = canada)) +
  ggtitle("Canada") +
  xlab("Infla��o US") +
  ylab("Infla��o Canada")

gp_japan_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = japan)) + 
  ggtitle("Japan") +
  xlab("Infla��o US") +
  ylab("Infla��o Jap�o")

gp_france_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = france)) +
  ggtitle("France") +
  xlab("Infla��o US") +
  ylab("Infla��o Fran�a")

gp_germany_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = germany)) +
  ggtitle("Germany") +
  xlab("Infla��o US") +
  ylab("Infla��o Alemanha")

gp_italy_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = italy)) +
  ggtitle("Italy") +
  xlab("Infla��o US") +
  ylab("Infla��o It�lia")

gp_uk_tab13 <- ggplot(dados_tab13) +
  geom_point(aes(x = us, y = uk)) +
  ggtitle("United Kingdom") +
  xlab("Infla��o US") +
  ylab("Infla��o Reino Unido")

#-- gr�ficos em uma mesma janela

windows() # abri uma janela para exibir os gr�ficos
grid.arrange(gp_canada_tab13, gp_japan_tab13, gp_france_tab13,
             gp_germany_tab13, gp_italy_tab13, gp_uk_tab13,
             ncol=3, nrow=2)

#******************* Fim Atividade 1.2.a *******************#

####################### Atividades Tab 1.4 #####################
#******************* In�cio Atividade 1.3.a *******************#

#- Leitura de uma base externa:
dados_tab14 <- read.csv2(file = "Tab_1-4_moedas.csv") # carrega dataset para ambiente de trabalho
str(dados_tab14) # analisa a estrutura dos dados
head(dados_tab14) # exibe as primeiras linhas do dataset

#-- Gr�ficos:

gl_aus_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = dollar_aus)) +
  ggtitle("Australia") +
  xlab("Data em anos") +
  ylab("C�mbio")

gl_can_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = dollar_can)) +
  ggtitle("Canada") +
  xlab("Data em anos") +
  ylab("C�mbio")

gl_chi_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = yuan_chi)) +
  ggtitle("China") +
  xlab("Data em anos") +
  ylab("C�mbio")

gl_jap_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = yen_jap)) +
  ggtitle("Jap�o") +
  xlab("Data em anos") +
  ylab("C�mbio")

gl_mex_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = peso_mex)) +
  ggtitle("M�xico") +
  xlab("Data em anos") +
  ylab("C�mbio")

gl_kor_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = won_kor)) +
  ggtitle("Korea") +
  xlab("Data em anos") +
  ylab("C�mbio")

gl_swe_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = krona_swe)) +
  ggtitle("Su�cia") +
  xlab("Data em anos") +
  ylab("C�mbio")

gl_swi_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = franc_swi)) +
  ggtitle("Su��a") +
  xlab("Data em anos") +
  ylab("C�mbio")

gl_uk_tab14 <- ggplot(dados_tab14) +
  geom_line(aes(x = ano, y = pound_uk)) +
  ggtitle("Reino Unido") +
  xlab("Data em anos") +
  ylab("C�mbio")


#-- gr�ficos em uma mesma janela

windows() # abri uma janela para exibir os gr�ficos
grid.arrange(gl_aus_tab14, gl_can_tab14, gl_chi_tab14,
             gl_jap_tab14, gl_mex_tab14, gl_kor_tab14,
             gl_swe_tab14, gl_swi_tab14, gl_uk_tab14,
             ncol=3, nrow=3)

#******************* Fim Atividade 1.3.a *******************#

####################### Atividades Tab 1.6 #####################
#******************* In�cio Atividade 1.4.a *******************#

#-- Gr�fico:

#- Leitura de uma base externa:
dados_tab16 <- read.csv2(file = "Tab_1-6_publicidade.csv") # carrega dataset para ambiente de trabalho
str(dados_tab16) # analisa a estrutura dos dados
head(dados_tab16) # exibe as primeiras linhas do dataset

gp_tab16 <- ggplot(dados_tab16) +
  geom_point(aes(x = despesa, y = impressao)) +
  ggtitle("Publicidade") +
  xlab("Investimento") +
  ylab("Impress�es")

#-- gr�ficos em uma mesma janela

windows() # abri uma janela para exibir os gr�ficos
gp_tab16

#******************* Fim Atividade 1.4.a *******************#

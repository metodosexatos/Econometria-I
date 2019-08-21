############################
####### Métodos Exatos ######
### www.metodosexatos.com ###
#############################

# Autor: André Santos | andre@metodosexatos.com.br
# 18/08/2019

# To cite R in publications use:
# citation()

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

#************* Módulo 08 - Mudança Estrutural  *************#

# Nota:
#      Ano =	Período						
#        Y =	poupança						
#        X =	renda				

# Prepararando base de dados
ano <- as.factor(c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995))
y <- as.integer(c(61,	68.6,	63.6,	89.6,	97.6,	104.4, 96.4, 92.5,	112.6,	130.1, 161.8, 199.1, 205.5, 167, 235.7,	206.2, 196.5,	168.4, 189.1,	187.8, 208.7, 246.4, 272.6, 214.4, 189.4, 249.3))
x <- as.integer(c(727.1,	790.2, 855.3, 965,	1054.2,	1159.2,	1273,	1401.4,	1580.1,	1769.5,	1973.3,	2200.2,	2347.3,	2522.4,	2810,	3002,	3187.6,	3363.1,	3640.8,	3894.5,	4166.8,	4343.7,	4613.7,	4790.2,	5021.7,	5320.8))
base <- data.frame(ano,y,x)

# Teste de Chow
install.packages("strucchange")                     # pacote para realizar teste de estabilidade estrutural
library("strucchange")
sctest(y~x, type = "Chow", data = base, point = 12) # se p-value for maior que o nível de significância, então falha em rejeitar H0, logo não há mudança estrutural

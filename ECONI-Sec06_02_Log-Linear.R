############################
####### M�todos Exatos ######
### www.metodosexatos.com ###
#############################

# Autor: Andr� Santos | andre@metodosexatos.com.br
# 08/08/2019

# To cite R in publications use:
# citation()

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

#************* Atividade M�dulo 6 - Regress�o log-linear *************#

# Prepararando base de dados
setwd("E:/Dropbox/M�todos Exatos/Cursos/Curso017_Econometria_I/Curso-ECON_Material_apoio/Datasets_Econ-I") # aponta o R para a respectiva pasta
getwd() # confere qual pasta o R est� apontando
list.files() # indica quais arquivos est�o contidos na pasta de trabalho
base <- read.csv2("Tab_6-3_elasticidade.csv") # carrega base de dados
str(base) # ver a estrutura da base

base.log <- data.frame(base[,1],log(base[,2:5])) # padroniza as vari�veis pela m�dia e transforma em um data frame e acrescenta a coluna de data da base original
print(base.log)
#- Nota:
#       DESPDUR (Y):  despesas com bens dur�veis.
#       DESPTCP (X):  despesas totais de consumo pessoal.

#- Modelo log-linear
FRA.log <- lm(DESPDUR~DESPTCP, data = base.log)   # modelo de regress�o amostral
summary(FRA.log)                                  # estat�sticas do modelo

# Valores previsto do modelo
EY.log <- predict(FRA.log)  # valores estimados de logY
EY <- exp(EY.log)           # valores estimados de Y
print(EY)
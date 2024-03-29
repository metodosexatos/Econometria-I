############################
####### M�todos Exatos ######
### www.metodosexatos.com ###
#############################

# Autor: Andr� Santos | andre@metodosexatos.com.br
# 05/08/2019

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

#************* Intervalo de Confian�a para os Coeficientes *************#

# Vari�veis
X <- c(6, 7, 8, 9, 10, 11, 12, 13, 14,15, 16, 17, 18)
Y <- c(4.4567, 5.77, 5.9787, 7.3317, 7.312, 6.5844, 7.8182, 7.851, 11.022, 10.674, 10.836, 13.615, 13.531)

# Fun��o de Regress�o Amostral (FRA)

FRA <- lm(Y~X) # modelo de regress�o linear
summary(FRA)   # resumo estat�stico do modelo

# Coeficientes

install.packages("broom") # pacote para facilitar a extra��o de informa��es do FRA
library("broom")

info_cf <- tidy(FRA)      # tratamento do modelo com o pacote "broom"
info_cf

# Estimadores dos coeficientes
beta1 <- info_cf$estimate[1]  # armazena apenas o valor do estimador do coeficiente beta 1 (intercepto)
beta2 <- info_cf$estimate[2]  # armazena apenas o valor do estimador do coeficiente beta 2 (coeficiente angular)
beta1                         # exibe o valor de beta 1
beta2                         # exibe o valor de beta 2

# Erro padr�o dos estimadores
erro_beta1 <- info_cf$std.error[1]  # armazena o termo de erro de beta 1
erro_beta2 <- info_cf$std.error[2]  # armazena o termo de erro de beta 2
erro_beta1                          # exibe o valor do termo de erro de beta 1
erro_beta2                          # exibe o valor do termo de erro de beta 1

# Estatistica t
t <- t.test(X, Y, "two.sided", conf.level = 0.95)  # armazena o resultado do teste "t"
t                                                  # exibe o resultado do teste "t"
estatistica_t <- t$statistic[1]                    # armazena apenas o resultado da estat�stica t
estatistica_t                                      # exibe o valor da estat�stica t

# Intervalo de Confian�a (IC)
#- IC beta 1
li_beta1 <- beta1 - estatistica_t*erro_beta1       # calcula o limite inferior de controle (LIC) para beta 1 e armazena na respectiva vari�vel
li_beta1                                           # exibe o LIC para beta 1
ls_beta1 <- beta1 + estatistica_t*erro_beta1       # calcula o limite superior de controle (LSC) para beta 1 e armazena na respectiva vari�vel
ls_beta1                                           # exibe o LSC para beta 1

#- IC beta 2
li_beta2 <- beta2 - estatistica_t*erro_beta2       # calcula o limite inferior de controle (LIC) para beta 2 e armazena na respectiva vari�vel
li_beta2                                           # exibe o LIC para beta 2
ls_beta2 <- beta2 + estatistica_t*erro_beta2       # calcula o limite superior de controle (LSC) para beta 2 e armazena na respectiva vari�vel
ls_beta2                                           # exibe o LSC para beta 2

#- IC da vari�ncia
alfa <- 0.05                                            # n�vel de signific�ncia
gl <- df.residual(FRA)                                  # graus de liberdade
qq_a2 <- qchisq(alfa/2, lower.tail = FALSE, df = gl)    # valor cr�tico X� para alfa/2
qq_1a2 <- qchisq(alfa/2, lower.tail = TRUE, df = gl)    # valor cr�tico X� para 1-alfa/2

EY <- beta1+beta2*X   # valores estimados de Y: E(Y|X)
EY                    # exibe os valores estimados de Y

Subt_Y <- Y-EY          # res�duos
Subt_Y                  # exibe os res�duos
sy2 <- Subt_Y^2         # res�duos elevados ao quadrado
sy2                     # exibe os res�duos elevados ao quadrado
soma_y2 <- sum(sy2)     # soma dos res�duos elevados ao quadrado
soma_y2                 # exibe a soma dos res�duos elevados ao quadrado
var_fra<- soma_y2/gl    # determina a vari�ncia do modelo
var_fra                 # exibe a vari�ncia do modelo

li_var <- gl*var_fra/qq_a2    # LIC para vari�ncia
li_var                        # exibe o LIC para vari�ncia
ls_var <- gl*var_fra/qq_1a2   # LSC para vari�ncia
ls_var                        # exibe o LSC para vari�ncia







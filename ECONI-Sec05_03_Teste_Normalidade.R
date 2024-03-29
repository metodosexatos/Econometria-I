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

#************* Modelo e Teste de Normalidade *************#

# Vari�veis
X <- c(6, 7, 8, 9, 10, 11, 12, 13, 14,15, 16, 17, 18)
Y <- c(4.4567, 5.77, 5.9787, 7.3317, 7.312, 6.5844, 7.8182, 7.851, 11.022, 10.674, 10.836, 13.615, 13.531)
base <- data.frame(X,Y) # cria um data.frame
is.data.frame(base)     # verifica se � um data.frame

# Fun��o de Regress�o Amostral
FRA <- lm(Y~X, data = base) # modelo de regress�o amostral
print(FRA)                  # sa�da do modelo
summary(FRA)                # estat�stica da FRA

FRA$fitted.values  # calcula os E(Yi|Xi): faz uma previs�o
FRA$residuals      # calcula os termos de erro (res�duos) para cada ponto da amostra
FRA$coefficients   # exibe os resultados da estimativa dos coeficientes de regress�o

# An�lise gr�fica:

#- Gr�fico de dispers�o com a reta de regress�o
windows()       # abri uma janela para exibir o gr�fico
disp <- plot(X,Y)  # armazena o gr�fico de dispers�o na respectivo objeto
grid(disp)         # aplica grade ao gr�fico
abline(FRA)     # adiciona a linha de regress�o ao gr�fico

#- Histograma dos res�duos
windows()              # abri uma janela para exibir o gr�fico
hist(FRA$residuals, main = "Resposta em sal�rio-hora m�dio", 
     xlab = "Res�duo", ylab = "Frequ�ncia", col = "darkblue", border = "black")
abline(v=median(FRA$residuals), col="green", lwd=2, lty=2)
abline(v=mean(FRA$residuals), col="red", lwd=2, lty=2)
legend(x="topleft",c("Mediana", "Media"), col = c("green","red"), lty = c(2,2), lwd = c(2,2), box.lty = 0)

#- Gr�fico de Probabilidade Normal (GPN)
windows()
qqnorm(residuals(FRA), ylab = "Quantis te�ricos", xlab = "Res�duos", main = "GPN dos Res�duos")
qqline(residuals(FRA))


# Teste de normalidade dos res�duos (Jarque-Bera)
install.packages("normtest")
library("normtest")
jb.norm.test(FRA$residuals)


















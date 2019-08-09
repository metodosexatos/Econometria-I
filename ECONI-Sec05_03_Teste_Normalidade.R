############################
####### Métodos Exatos ######
### www.metodosexatos.com ###
#############################

# Autor: André Santos | andre@metodosexatos.com.br
# 08/08/2019

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

#************* Modelo e Teste de Normalidade *************#

# Variáveis
X <- c(6, 7, 8, 9, 10, 11, 12, 13, 14,15, 16, 17, 18)
Y <- c(4.4567, 5.77, 5.9787, 7.3317, 7.312, 6.5844, 7.8182, 7.851, 11.022, 10.674, 10.836, 13.615, 13.531)
base <- data.frame(X,Y) # cria um data.frame
is.data.frame(base)     # verifica se é um data.frame

# Função de Regressão Amostral
FRA <- lm(Y~X, data = base) # modelo de regressão amostral
print(FRA)                  # saída do modelo
summary(FRA)                # estatística da FRA

FRA$fitted.values  # calcula os E(Yi|Xi): faz uma previsão
FRA$residuals      # calcula os termos de erro (resíduos) para cada ponto da amostra
FRA$coefficients   # exibe os resultados da estimativa dos coeficientes de regressão

# Análise gráfica:

#- Gráfico de dispersão com a reta de regressão
windows()       # abri uma janela para exibir o gráfico
disp <- plot(X,Y)  # armazena o gráfico de dispersão na respectivo objeto
grid(disp)         # aplica grade ao gráfico
abline(FRA)     # adiciona a linha de regressão ao gráfico

#- Histograma dos resíduos
windows()              # abri uma janela para exibir o gráfico
hist(FRA$residuals, main = "Resposta em salário-hora médio", 
     xlab = "Resíduo", ylab = "Frequência", col = "darkblue", border = "black")
abline(v=median(FRA$residuals), col="green", lwd=2, lty=2)
abline(v=mean(FRA$residuals), col="red", lwd=2, lty=2)
legend(x="topleft",c("Mediana", "Media"), col = c("green","red"), lty = c(2,2), lwd = c(2,2), box.lty = 0)

#- Gráfico de Probabilidade Normal (GPN)
windows()
qqnorm(residuals(FRA), ylab = "Quantis teóricos", xlab = "Resíduos", main = "GPN dos Resíduos")
qqline(residuals(FRA))


# Teste de normalidade dos resíduos (Jarque-Bera)
install.packages("normtest")
library("normtest")
jb.norm.test(FRA$residuals)


















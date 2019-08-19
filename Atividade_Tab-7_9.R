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

#************* Módulo 07 - Atividade Tabela 7.9  *************#

# Nota:
#      Ano =	Período						
#        Y =	consumo per capita de frango em libras (peso)						
#       X2 =	renda real disponível per capita, em $						
#       X3 =	preço real do frango no varejo, em centavos de dólar por libra (peso)						
#       X4 =	preço real da carne suína no varejo, em centavos de dólar por libra (peso)						
#       X5 =	preço real da carne bovina no varejo, em centavos de dólar por libra (peso)						
#       X6 =	preço real dos substitutos da carne de frango, em centavos de dólar por libra (peso), que é uma média ponderada dos preços reais das carnes suína e bovina, usando como pesos o consumo relativo de cada uma dessas carnes em relação ao consumo total delas.						
#     Fonte:  os dados relativos a Y são da Citibase e os relativos às variáveis X2 a X6 são do Departamento de Agricultura dos Estados Unidos.

# Prepararando base de dados
ano <- as.factor(c(1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982))
y <- as.integer(c(27.8, 29.9, 29.8, 30.8, 31.2, 33.3, 35.6, 36.4, 36.7, 38.4, 40.4, 40.3, 41.8, 40.4, 40.7, 40.1, 42.7, 44.1, 46.7, 50.6, 50.1, 51.7, 52.9))
x2 <- as.integer(c(397.5, 413.3, 439.2, 459.7, 492.9, 528.6, 560.3, 624.6, 666.4, 717.8, 768.2, 843.3, 911.6, 931.1, 1021.5, 1165.9, 1349.6, 1449.4, 1575.5, 1759.1, 1994.2, 2258.1, 2478.7))
x3 <- as.integer(c(42.2, 38.1, 40.3, 39.5, 37.3, 38.1, 39.3, 37.8, 38.4, 40.1, 38.6, 39.8, 39.7, 52.1, 48.9, 58.3, 57.9, 56.5, 63.7, 61.6, 58.9, 66.4, 70.4))
x4 <- as.integer(c(50.7, 52, 54, 55.3, 54.7, 63.7, 69.8, 65.9, 64.5, 70, 73.2, 67.8, 79.1, 95.4, 94.2, 123.5, 129.9, 117.6, 130.9, 129.8, 128, 141, 168.2))
x5 <- as.integer(c(78.3, 79.2, 79.2, 79.2, 77.4, 80.2, 80.4, 83.9, 85.5, 93.7, 106.1, 104.8, 114, 124.1, 127.6, 142.9, 143.6, 139.2, 165.5, 203.3, 219.6, 221.6, 232.6))
x6 <- as.integer(c(65.8, 66.9, 67.8, 69.6, 68.7, 73.6, 76.3, 77.2, 78.1, 84.7, 93.3, 89.7, 100.7, 113.5, 115.3, 136.7, 139.2, 132, 132.1, 154.4, 174.9, 180.8, 189.4))

base <- data.frame(ano,y,x2,x3,x4,x5,x6)  # base de dados
str(base)                                 # ver estrutura da base
head(base)                                # ver as 6 primeiras linhas da base
tail(base)                                # ver as 6 últimas linhas da base

# Função de regressão amostral de demanda (5): lnY = b1 + b2lnX2 + b3lnX3 + b4lnX6 + ui
FRA.5 <- lm(log(y)~log(x2)+log(x3)+log(x6), data = base) # modelo de regressão amostral (para excluir o intercepto colocar "-1" depois da última variável explanatória:"llog(x2)+log(x3)+log(x6)-1" )
print(FRA.5)                                             # saída do modelo
summary(FRA.5)                                           # estatística da FRA

# Pacotes requeridos para extração de informações do modelo
install.packages("broom")     # pacote para extração de informações do modelo. Usar comando: "tidy(FRA)"
install.packages("normtest")  # pacote para teste de normalidade. Usar comando: "jb.norm.test()"
library("broom")
library("normtest")

# Armazenamento das estatísticas para análises
estatisticas <- summary(FRA.5)  # estatísticas do modelo
analise_var <- anova(FRA.5)     # análise de variância
fra.5 <- tidy(FRA.5)              # permite extrair estatísticas do modelo

# Obtém as estimativas dos parâmetros, os erros padrão, R², SQR e SQE.
fra.5$estimate             # obtém as estimativas dos parâmetros beta 1 (intercepto) e beta 2 (coeficiente angular), respectivamente
fra.5$std.error            # obtém os termos de erro dos estimadores dos coeficientes parciais
estatisticas$r.squared     # obtém o R² do modelo (medida de qualidade de ajuste da linha de regressão)
estatisticas$adj.r.squared # obtém o R² ajustado (ajuste pelos graus de liberdade)
analise_var$`Sum Sq`       # obtém as somas dos quadrados (SQ) dos valores estimados e dos resíduos, respectivamente

# Estabelece um intervalo de confiança de 95% para os coeficientes parciais
ic_coef <- confint.lm(FRA.5, level = 0.95)          # determina os intervalos de confiança para os estimadores dos coeficientes
IC.Coef <- data.frame(ic_coef,FRA.5$coefficients)   # tabela com IC e estimativas dos coeficientes parciais
names(IC.Coef) <- c("LI","LS","Estimativas")        # renomeia as colunas para limite inferior (LI), limite superior (LS) e valores estimados dos coeficientes parciais (Estimativas)
print(IC.Coef)                                      # exibe o intervalo de confiança e coeficientes parciais. Se os valores estimados cairem fora da região de aceitação rejeita-se a...
                                                    # ...hipótese nula. Ou seja, nas condições propostas podemos considerar que o valor encontrado não representa o verdadeiro dos...
                                                    # ...coeficientes parciais. Logo não são significativos

# Estabelece os intervalos de confiança para a média real e para o valor individual.
prev.media <- exp(predict(FRA.5, interval = "confidence"))   # intervalo de confiança para a média
prev.indiv <- exp(predict(FRA.5, interval = "prediction"))   # intervalo de confiança para a resposta individual
head(prev.media)
head(prev.indiv)

# Estima um novo valor para Y com base em novas entradas de Xs
lnx2 <- log(397.5)                                                        # novo valor de log de X2 para usar para prever novo valor de Y
lnx3 <- log(42.2)                                                         # novo valor de log de X3 para usar para prever novo valor de Y
lnx6 <- log(65.8)                                                         # novo valor de log de X6 para usar para prever novo valor de Y
nova.estimativa <- exp(FRA.5$coefficients[1]+FRA.5$coefficients[2]*lnx2+FRA.5$coefficients[3]*lnx3+FRA.5$coefficients[4]*lnx6)
print(nova.estimativa)

# Teste de hipótese de normalidade do termo de erro
#- Histograma dos resíduos
windows()
hist(FRA.5$residuals, main = "Análise do Histograma dos Resíduos", 
     xlab = "Resíduo", ylab = "Frequência", col = "darkblue", border = "black")
abline(v=median(FRA.5$residuals), col="green", lwd=2, lty=2)
abline(v=mean(FRA.5$residuals), col="red", lwd=2, lty=2)
legend(x="topright",c("Mediana", "Media"), col = c("green","red"), lty = c(2,2), lwd = c(2,2), box.lty = 0)

#- Gráfico de Probabilidade Normal (GPN)
windows()
qqnorm(residuals(FRA.5), ylab = "Quantis teóricos", xlab = "Resíduos", main = "GPN dos Resíduos")
qqline(residuals(FRA.5))

# Teste de normalidade dos resíduos (Jarque-Bera)
jb.norm.test(FRA.5$residuals) # se o valor da estatística JB=0 e p<alfa, então podemos rejeitar a hipótese nula de que a distribuição de resíduos é normal.

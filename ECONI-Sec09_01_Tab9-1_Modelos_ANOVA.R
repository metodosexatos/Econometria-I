############################
####### Métodos Exatos ######
### www.metodosexatos.com ###
#############################

# Autor: André Santos | andre@metodosexatos.com.br
# 23/08/2019

# To cite R in publications use:
# citation()

###################################################################

#--------------------- Diretórios e Arquivos ---------------------#

# getwd() # Qual o diretório que o script está apontando
# list.files() # Quais arquivos estão contidos no diretório
# setwd("E:/Dropbox/Métodos Exatos/Cursos/Curso017_Econometria_I/Curso-ECON_Material_apoio/Datasets_Econ-I")

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

#************* Módulo 09::Tabela 9.1 - Salários de professores da rede pública por região geográfica *************#

# Nota:
#              Obs =	total de observações no dataset						
#           Estado =	50 estados + distrito de Colúmbia (EUA)						
#      Salário(Yi) =	salário médio de professores da rede pública para o ano escolar de 2005-2006					
#      Gastos (Xi) =  gastos com escolas públicas por aluno ($)
#               D2 =	variável qualitativa (1 se o estado for do Nordeste ou Norte Central, zero caso contrário)						
#               D3 =	variável qualitativa (1 se o estado for da região Sul, zero caso contrário)

# Prepararando base de dados
dados <- read.csv2("Tab_9-1_modelos_anova.txt", sep = "\t" )  # base de dados
str(dados)                                                    # ver estrutura da base
d2 <- as.factor(dados$D2)                                     # variável qualitativa para região Norte e Centro-Norte
d3 <- as.factor(dados$D3)                                     # variável qualitativa para região Sul
base <- data.frame(salario=dados$Salario, d2, d3)             # base de dados tratada com as variáveis qualitativas
str(base)                                                     # estrutura da base de dados
head(base)                                                    # ver as 6 primeiras linhas da base
tail(base)                                                    # ver as 6 últimas linhas da base

# Função de regressão amostral: Y = b1 + b2D2 + b3D3 + ui
FRA <- lm(salario~d2+d3, data = base)  # modelo de regressão amostral (para excluir o intercepto colocar "-1" depois da última variável explanatória:"llog(x2)+log(x3)+log(x6)-1" )
print(FRA)                             # saída do modelo
summary(FRA)                           # estatística da FRA

# Pacotes requeridos para extração de informações do modelo
if(!require(normtest)) install.packages("normtest")  # se necessário instala pacote para teste de normalidade dos resíduos. Usar comando: "jb.norm.test()"
library("normtest")                                  # carrega pacote para teste de normalidade dos resíduos

# Valores estimados das regiões:
N <- FRA$coefficients[[1]]+FRA$coefficients[[2]] # Região Norte e Centro-Norte: E(Y|D2=1, D3=0) = beta1 + beta2
S <- FRA$coefficients[[1]]+FRA$coefficients[[3]] # Região Sul: E(Y|D2=0, D3=1) = beta1 + beta3
O <- FRA$coefficients[[1]]                       # Região Oeste: E(Y|D2=0, D3=1) = beta1
cbind(N,S,O)                                     # Exibe o resultado por região

# Estabelece um intervalo de confiança de 95% para os coeficientes parciais
ic_coef <- confint.lm(FRA, level = 0.95)            # determina os intervalos de confiança para os estimadores dos coeficientes
IC.Coef <- data.frame(ic_coef,FRA$coefficients)     # tabela com IC e estimativas dos coeficientes parciais
names(IC.Coef) <- c("LI","LS","Estimativas")        # renomeia as colunas para limite inferior (LI), limite superior (LS) e valores estimados dos coeficientes parciais (Estimativas)
print(IC.Coef)                                      # exibe o intervalo de confiança e coeficientes parciais. Se os valores estimados cairem fora da região de aceitação rejeita-se a...
# ...hipótese nula. Ou seja, nas condições propostas podemos considerar que o valor encontrado não representa o verdadeiro dos...
# ...coeficientes parciais. Logo não são significativos

# Teste de hipótese de normalidade do termo de erro
#- Histograma dos resíduos
windows()
hist(FRA$residuals, main = "Análise do Histograma dos Resíduos", 
     xlab = "Resíduo", ylab = "Frequência", col = "darkblue", border = "black")
abline(v=median(FRA$residuals), col="green", lwd=2, lty=2)
abline(v=mean(FRA$residuals), col="red", lwd=2, lty=2)
legend(x="topright",c("Mediana", "Media"), col = c("green","red"), lty = c(2,2), lwd = c(2,2), box.lty = 0)

#- Gráfico de Probabilidade Normal (GPN)
windows()
qqnorm(residuals(FRA), ylab = "Quantis teóricos", xlab = "Resíduos", main = "GPN dos Resíduos")
qqline(residuals(FRA))

# Teste de normalidade dos resíduos (Jarque-Bera)
jb.norm.test(FRA$residuals) # se o valor da estatística JB=0 e p<alfa, então podemos rejeitar a hipótese nula de que a distribuição de resíduos é normal.













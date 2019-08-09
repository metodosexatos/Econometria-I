library("normtest")

# Função de Regressão Amostral (FRA) e armazenamento das estatísticas para análises

FRA <- lm(rem~gast, data = base)   # modelo de regressão amostral
estatisticas <- summary(FRA)  # estatísticas do modelo
analise_var <- anova(FRA)     # análise de variância
fra <- tidy(FRA)              # permite extrair estatísticas do modelo

# a. Obtenha as estimativas dos parâmetros, os erros padrão, r², SQR e SQE.

fra$estimate             # obtém as estimativas dos parâmetros beta 1 (intercepto) e beta 2 (coeficiente angular), respectivamente
fra$std.error            # obtém os termos de erro dos estimadores dos coeficientes beta 1 e beta 2, respectivamente
estatisticas$r.squared   # obtém o r² do modelo (medida de qualidade de ajuste da linha de regressão)
analise_var$`Sum Sq`     # obtém as somas dos quadrados (SQ) dos valores estimados e dos resíduos, respectivamente

# b. Represente graficamente os dados com a linha de regressão.

windows()          # abri uma janela para exibir o gráfico
disp <- plot(base$gast,base$rem, main = "Salários (prof.) x Despesas (alunos)",
             xlab = "Despesas com Alunos", ylab = "Remuneração Anual")  # armazena o gráfico de dispersão na respectivo objeto
grid(disp)         # aplica grade ao gráfico
abline(FRA, col = "red")        # adiciona a linha de regressão ao gráfico

# c. Interprete os resultados da regressão. Faz sentido do ponto de vista econômico?

#- O modelo indica uma relação positiva entre as despesas com alunos e remuneração anual dos professores.
#- De acordo com as estimativas do modelo, a remuneração inicial (anual) para um professor é de aproximadamente $12.129 (beta 1), e
#- para cada dolar a mais de gastos com alunos, espera-se um acrescimo no salário anual de um professor de $3,30 (beta 2).
#- Do ponto de vista econômico faz sentido. De acordo com a secretária executiva do MEC, Maria Helena Guimarães de Castro,
#- todos países acabam tendo gastos maiores com estudantes da universidade em diante e uma das justificativas, segundo ela,
#- seria os salários dos professores (fonte: https://oglobo.globo.com/sociedade/educacao/relatorio-destaca-diferenca-entre-salarios-de-professores-custo-por-aluno-no-brasil-20122101)

# d. Estabeleça um intervalo de confiança de 95% para beta 2. Você rejeitaria a hipótese de que o verdadeiro coeficiente angular é 3,0?

ic_betas <- confint.lm(FRA, level = 0.95) # determina os intervalos de confiança para os estimadores dos coeficientes
print(ic_betas[2,])                       # exibe o intervalo de confiança de beta 2
# Como H0: beta 2 = 3.0 caiu dentro da região de aceitação eu falho em rejeitar a hipótese nula. Ou seja,
# nas condições propostas podemos considerar o valor de 3.0 como sendo o verdadeiro valor de beta 2.

# e. Obtenha a média e o valor individual previsto de Rem se as despesas por aluno forem de $ 5.000. Estabeleça também intervalos de confiança para a média real e para o valor individual de Rem para a despesa dada.

Xi <- data.frame(gast=5000)                           # novo conjunto de preditoras. Requer que seja um data.frame com o mesmo nome do banco de dados original.
predict(FRA, newdata = Xi, interval = "confidence")   # intervalo de confiança para média
predict(FRA, newdata = Xi, interval = "prediction")   # intervalo de confiança para a resposta individual

# f. Como você testaria a hipótese de normalidade do termo de erro? Mostre o(s) teste(s) que usou.

#- Histograma dos resíduos
windows()
hist(FRA$residuals, main = "Resposta em salário-hora médio", 
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

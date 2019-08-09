library("normtest")

# Fun��o de Regress�o Amostral (FRA) e armazenamento das estat�sticas para an�lises

FRA <- lm(rem~gast, data = base)   # modelo de regress�o amostral
estatisticas <- summary(FRA)  # estat�sticas do modelo
analise_var <- anova(FRA)     # an�lise de vari�ncia
fra <- tidy(FRA)              # permite extrair estat�sticas do modelo

# a. Obtenha as estimativas dos par�metros, os erros padr�o, r�, SQR e SQE.

fra$estimate             # obt�m as estimativas dos par�metros beta 1 (intercepto) e beta 2 (coeficiente angular), respectivamente
fra$std.error            # obt�m os termos de erro dos estimadores dos coeficientes beta 1 e beta 2, respectivamente
estatisticas$r.squared   # obt�m o r� do modelo (medida de qualidade de ajuste da linha de regress�o)
analise_var$`Sum Sq`     # obt�m as somas dos quadrados (SQ) dos valores estimados e dos res�duos, respectivamente

# b. Represente graficamente os dados com a linha de regress�o.

windows()          # abri uma janela para exibir o gr�fico
disp <- plot(base$gast,base$rem, main = "Sal�rios (prof.) x Despesas (alunos)",
             xlab = "Despesas com Alunos", ylab = "Remunera��o Anual")  # armazena o gr�fico de dispers�o na respectivo objeto
grid(disp)         # aplica grade ao gr�fico
abline(FRA, col = "red")        # adiciona a linha de regress�o ao gr�fico

# c. Interprete os resultados da regress�o. Faz sentido do ponto de vista econ�mico?

#- O modelo indica uma rela��o positiva entre as despesas com alunos e remunera��o anual dos professores.
#- De acordo com as estimativas do modelo, a remunera��o inicial (anual) para um professor � de aproximadamente $12.129 (beta 1), e
#- para cada dolar a mais de gastos com alunos, espera-se um acrescimo no sal�rio anual de um professor de $3,30 (beta 2).
#- Do ponto de vista econ�mico faz sentido. De acordo com a secret�ria executiva do MEC, Maria Helena Guimar�es de Castro,
#- todos pa�ses acabam tendo gastos maiores com estudantes da universidade em diante e uma das justificativas, segundo ela,
#- seria os sal�rios dos professores (fonte: https://oglobo.globo.com/sociedade/educacao/relatorio-destaca-diferenca-entre-salarios-de-professores-custo-por-aluno-no-brasil-20122101)

# d. Estabele�a um intervalo de confian�a de 95% para beta 2. Voc� rejeitaria a hip�tese de que o verdadeiro coeficiente angular � 3,0?

ic_betas <- confint.lm(FRA, level = 0.95) # determina os intervalos de confian�a para os estimadores dos coeficientes
print(ic_betas[2,])                       # exibe o intervalo de confian�a de beta 2
# Como H0: beta 2 = 3.0 caiu dentro da regi�o de aceita��o eu falho em rejeitar a hip�tese nula. Ou seja,
# nas condi��es propostas podemos considerar o valor de 3.0 como sendo o verdadeiro valor de beta 2.

# e. Obtenha a m�dia e o valor individual previsto de Rem se as despesas por aluno forem de $ 5.000. Estabele�a tamb�m intervalos de confian�a para a m�dia real e para o valor individual de Rem para a despesa dada.

Xi <- data.frame(gast=5000)                           # novo conjunto de preditoras. Requer que seja um data.frame com o mesmo nome do banco de dados original.
predict(FRA, newdata = Xi, interval = "confidence")   # intervalo de confian�a para m�dia
predict(FRA, newdata = Xi, interval = "prediction")   # intervalo de confian�a para a resposta individual

# f. Como voc� testaria a hip�tese de normalidade do termo de erro? Mostre o(s) teste(s) que usou.

#- Histograma dos res�duos
windows()
hist(FRA$residuals, main = "Resposta em sal�rio-hora m�dio", 
     xlab = "Res�duo", ylab = "Frequ�ncia", col = "darkblue", border = "black")
abline(v=median(FRA$residuals), col="green", lwd=2, lty=2)
abline(v=mean(FRA$residuals), col="red", lwd=2, lty=2)
legend(x="topright",c("Mediana", "Media"), col = c("green","red"), lty = c(2,2), lwd = c(2,2), box.lty = 0)

#- Gr�fico de Probabilidade Normal (GPN)
windows()
qqnorm(residuals(FRA), ylab = "Quantis te�ricos", xlab = "Res�duos", main = "GPN dos Res�duos")
qqline(residuals(FRA))


# Teste de normalidade dos res�duos (Jarque-Bera)

jb.norm.test(FRA$residuals) # se o valor da estat�stica JB=0 e p<alfa, ent�o podemos rejeitar a hip�tese nula de que a distribui��o de res�duos � normal.
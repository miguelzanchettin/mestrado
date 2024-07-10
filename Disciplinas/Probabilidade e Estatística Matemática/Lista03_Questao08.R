
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 03
# Questao 08
# 

rm(list=ls())

install.packages('scatterplot3d')
library(scatterplot3d)

# Enunciado:
#   Considere um vetor bivariado de variáveis aleatórias normalmente distribuído, com 
#   E(Y1) = 0 
#   E(Y2) = 4
#   V(Y1) = 1 
#   V(Y2) = 9
#   Cov(Y1, Y2) = 2.
# a) Escreva a função de densidade probabilidade.

# Respondida no papel.
# Resultado:
#   f(y_1, y_2) = (2 * pi * sqrt(5))^(-1) * exp(-1/10 * (y_1^2 2 * y_1 * y_2 - 8 * y_1 + (y_2 - 4) * (y_1 + 9 * y_2 - 36)))

# Implementando:

set.seed(123)

# Configuracoes do enunciado
n       <- 100
mu      <- c(0, 4)
cov.mtx <- matrix(c(1, 2, 2, 9), ncol = 2)

# Implementa calculo ponto a ponto
calcular_densidade_normal_bivariada <-function(y, mu, cov.mtx) {
  
  # Verifica se y é uma matriz
  if (!is.matrix(y)) {
    stop("y deve ser uma matriz.")
  }
  
  # Quantidade de esperancas
  ro <- length(mu)
  
  # Calcula valores da matriz de covariancia
  Sigma_det <- det(cov.mtx)
  Sigma_inv <- solve(cov.mtx)
  
  # Vetor para receber as densidades
  densidade <- numeric(nrow(y))
  
  # Calcula densidade ponto a ponto
  for (i in 1:nrow(y)) {
  
    expoente <- -0.5 * t(y[i, ] - mu) %*% Sigma_inv %*% (y[i, ] - mu)
    densidade[i] <- -1 / sqrt((2 * pi)^ro * Sigma_det) * expoente
    
  }
  
  # Retorna densidade  
  return(densidade)
  
}

# Gera uma sequencia de V.As 
n <- 1000
y <- mvrnorm(n, mu, cov.mtx)

# <<< Resposta b
d <- calcular_densidade_normal_bivariada(y, mu, cov.mtx) 

# <<< Resposta c
# Plota grafico
par(family='Arial', mfrow=c(1, 1))
scatterplot3d(x = y[, 1], y = y[, 2], z = d, 
              xlab='f(y1)', ylab='f(y2)', zlab='f(y1, y2)', type='p')



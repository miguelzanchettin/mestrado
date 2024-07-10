
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 10
# Questao H
# 

rm(list=ls())

# Para cada uma das distribuições de probabilidade abaixo construa a estatística de teste conforme solicitado.

# a)  Distribuição Poisson de parâmetro \lambda. 
#     Construa um teste para avaliar se \lambda = \lambda_0 contra \lambda \ne \lambda_0. 
#     Faça um exemplo com dados simulados.


set.seed(123)

# Nivel de significancia
alpha <- 0.05 

# Numero de amostras
n <- 100

# Parametro da Poisson
lambda <- 30

# Gera amostra aleatoria
y.LAMBDA <- rpois(n, lambda=lambda)
y.LAMBDA2 <- rpois(n, lambda=lambda^2)

# Funcao para testar hipotese
testar_hipotese <- function(y, lambda) 
{
  # Estima o lambda pelo MLE
  lambda.hat <- 1/length(y) * sum(y)
  
  # Estatistica de wald
  z.lambda <- (lambda.hat - lambda) / sqrt(lambda)
  
  # Estatistica z para a significancia desejada
  z.alpha <- -qnorm(p=alpha/2)
  
  # Valida hipotese para o MLE
  if (z.alpha >= z.lambda) 
  {
    print('Aceitaria H0')
  } else
  {
    print('Rejeitaria H0 frente a Ha')
  }
}

# Realiza teste para os dois casos
testar_hipotese(y = y.LAMBDA, lambda = lambda) # Deve aceitar
testar_hipotese(y = y.LAMBDA2, lambda = lambda) # Deve rejeitar


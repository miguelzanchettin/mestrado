
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 10
# Questao D
# 

rm(list=ls())

# Para cada uma das distribuições de probabilidade abaixo construa a estatística de teste conforme solicitado.

# D)  Distribuição normal de parâmetros \mu e \sigma^2.
#     Construa um teste escore para testar se \sigma^2 = 1 contra σ \sigma^2 \ne 1. 
#     Faça um estudo de simulação para verificar a qualidade do teste proposto. 
#     Você teria umasugestão para melhorar este teste?

set.seed(123)

# Nivel de significancia
alpha <- 0.05 

# Numero de amostras
n.amostras <- 100

# Parametro da Exponencial
mu <- 10
sigma.2 <- 4

# Gera amostra aleatoria assumindo que n é dado
y.aceitar <- rnorm(n = n.amostras, mean = mu, sd = sqrt(sigma.2))
y.negar   <- rnorm(n = n.amostras, mean = mu, sd = sqrt(sigma.2) + 3)

# Funcao para testar hipotese
testar_hipotese <- function(y, sigma.2, alpha) 
{
 
  # Estimadores de MLE 
  n <- length(y)
  mu.hat <- 1/n * sum(y)
  sigma.2.hat <- 1/n * sum((y - mu.hat)^2)
  
  # Estatistica escore escore
  score.stat <- sigma.2.hat - sigma.2
  
  # Estatistica z para a significancia desejada
  z.alpha <- -qnorm(p=alpha/2)
  
  # Valida hipotese
  if (z.alpha >= score.stat) 
  {
    print('Aceitaria H0')
  } else
  {
    print('Rejeitaria H0 frente a Ha')
  }
  
}

testar_hipotese(y.aceitar, sigma.2 = sigma.2, alpha = alpha)
testar_hipotese(y.negar, sigma.2 = sigma.2, alpha = alpha)

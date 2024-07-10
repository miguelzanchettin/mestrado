
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 10
# Questao B
# 

rm(list=ls())

# Para cada uma das distribuições de probabilidade abaixo construa a estatística de teste conforme solicitado.

# b)  Distribuição binomial de parâmetros n e p. 
#     Construa um teste para avaliar se p = 0.5 contra p 6= 0.5. 
#     Faça um exemplo com dados simulados. 
#     Para o valor de p = 0.8 calcule o poder do teste.

set.seed(123)

# Nivel de significancia
alpha <- 0.05 

# Numero de amostras
n.amostras <- 50

# Parametro da Binomial
n <- 10
p <- 0.5

# Gera amostra aleatoria assumindo que n é dado
y.aceitar <- rbinom(n = n.amostras, size = n, prob = p)
y.negar   <- rbinom(n = n.amostras, size = n, prob = p + 0.3)


# Funcao para testar hipotese
testar_hipotese <- function(y, n) 
{
  # Estima o lambda pelo MLE
  p.hat <- 1/(length(y)*n) * sum(y)
  
  # Estatistica de wald
  z <- (p.hat - 0.5) / sqrt(0.5 / length(y))
  
  # Estatistica z para a significancia desejada
  z.alpha <- -qnorm(p=alpha/2)
  
  # Valida hipotese para o MLE
  if (z.alpha >= z) 
  {
    print('Aceitaria H0')
  } else
  {
    print('Rejeitaria H0 frente a Ha')
  }
  
  # Calcula o poder do teste
  z.prob <- pnorm(z.alpha - z) + pnorm(-z.alpha - z)
  print(paste('Poder:', z.prob))
  
}

testar_hipotese(y.aceitar, n=n)
testar_hipotese(y.negar, n=n)




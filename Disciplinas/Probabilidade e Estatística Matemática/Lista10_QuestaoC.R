
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 10
# Questao C
# 

rm(list=ls())

# Para cada uma das distribuições de probabilidade abaixo construa a estatística de teste conforme solicitado.

# C)  Distribuição exponencial de parâmetro \lambda. 
#     Construa o teste da razão de verossimilhança para avaliar se \lambda = \lambda_0 contra \lambda \ne \lambda_0. 
#     Forneça uma implementação computacional.

set.seed(123)

# Nivel de significancia
alpha <- 0.05 

# Numero de amostras
n.amostras <- 100

# Parametro da Exponencial
lambda <- 1/2

# Gera amostra aleatoria assumindo que n é dado
y.aceitar <- rexp(n = n.amostras, rate = lambda)
y.negar   <- rexp(n = n.amostras, rate = 1/5)

# Funcao para testar hipotese
testar_hipotese <- function(y, lambda0, alpha) 
{
  
  # Estima o lambda pelo MLE
  n <- length(y)
  lambda.hat <- 1/(sum(y) / n)
  
  # Funcao de verossimilhança
  ll <- function(lambda, data) 
  {
    n * log(lambda) - lambda * sum(data)
  }
  
  # Estima as verossimilhancas
  ll.H0 <- ll(lambda0, y)
  ll.Ha <- ll(lambda.hat, y)
  
  # Estatistica do teste
  est.tst <- -2 * (ll.H0 - ll.Ha)
  
  # P-valor
  p.value <- pchisq(est.tst, df = 1, lower.tail = F)
  print(paste('p-valor: ', p.value))
  
  # Valida hipotese
  if (p.value >= alpha) 
  {
    print('Aceitaria H0')
  } else
  {
    print('Rejeitaria H0 frente a Ha')
  }
  
}

testar_hipotese(y.aceitar, lambda0 = lambda, alpha = alpha)
testar_hipotese(y.negar, lambda0 = lambda, alpha = alpha)


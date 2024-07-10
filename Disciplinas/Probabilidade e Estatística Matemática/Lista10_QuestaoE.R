
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 10
# Questao E
# 

library(statmod)
library(tweedie)

rm(list=ls())

# Para cada uma das distribuições de probabilidade abaixo construa a estatística de teste conforme solicitado.

# E)  Distribuição Tweeedie de parâmetros \mu, \phi e \ro. 
#     Considere que o p é conhecido. Construa um teste
#     Wald para testar se \phi = 1 contra \phi \ne 1 . 
#     Faça um estudo de simulação para verificar a qualidade do teste proposto. 
#     Você teria uma sugestão para melhorar este teste?

set.seed(123)

# Nivel de significancia
alpha <- 0.05 

# Numero de amostras
n.amostras <- 100

# Parametro da Tweedie
mu <- 5
phi <- 1
p <- 1.5  # valor conhecido de p

# Gera amostra aleatoria assumindo que n é dado
#y.aceitar <- rtweedie(n.amostras, mu, phi, p)
#y.negar   <- rtweedie(n.amostras, mu, phi + 1, p)

# Funcao para testar hipotese
testar_hipotese <- function(y, phi, alpha) 
{
 
  # Ajustar o modelo Tweedie
  fit <- tweedie.profile(y ~ 1, p.vec = p)
  
  # Estimativas dos parametros
  mu.hat <- fit$power.mu
  phi.hat <- fit$phi
  
  # Variancia de phi_hat
  # ara simplicidade, o erro padrao fornecido pelo ajuste)
  se.phi.hat <- summary(fit)$coefficients[2, 2]
  var.phi.hat <- se.phi.hat^2
  
  # Estatística do teste Wald
  W <- (phi.hat - phi)^2 / var.phi.hat
  
  # P-valor
  p.value <- 1 - pchisq(W, df = 1)

  # Valida hipotese
  if (p.value >= alpha) 
  {
    print('Aceitaria H0')
  } else
  {
    print('Rejeitaria H0 frente a Ha')
  }
  
}

testar_hipotese(y.aceitar, sigma.2 = sigma.2, alpha = alpha)
testar_hipotese(y.negar, sigma.2 = sigma.2, alpha = alpha)

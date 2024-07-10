
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 09
# Questao H
# 

# Para cada uma das distribuições de probabilidade abaixo encontre o EMV e um intervalo de confiança usando 
# pelo menos duas estratégias. Note que implementação computacional será necessário.
#   h.Distribuição Tweeedie de parâmetros \mu, \phi, \ro. Considere que o p é conhecido

# Instalar e carregar o pacote tweedie
#install.packages("tweedie")
library(tweedie)

# Simulando dados de uma distribuição Tweedie
set.seed(123)
n <- 100
p <- 1.5
mu <- 10
phi <- 2
y <- rtweedie(n, mu=mu, phi=phi, power=p)

# Estimando os parâmetros µ e φ
fit <- tweedie.profile(y ~ 1, p.vec = p)

# Extraindo os parâmetros estimados
mu_est <- fit$mu
phi_est <- fit$phi

mu_est
phi_est

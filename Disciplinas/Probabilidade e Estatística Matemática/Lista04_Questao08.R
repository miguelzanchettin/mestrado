
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 04
# Questao 8
# 

rm(list=ls())

# 8. Seja Y_1, ... , Y_n v.a iid com E(Y_i) = \mu e V(Y_i) = \simga^2 < \infty.
#   a) Mostre que \bar{\sigma}^2 converge em probabilidade para \sigma^2 onde \bar{\sigma}^2 = \frac{1}{n} \sum_{i=1]^{n}{(Y_i - \mu)^2}
#     Respondido no papel

#   b) Obtenha a distribuição aproximada de \bar{\sigma}^2
#     Respondido no papel
#     Resposta encontrada: X \sim \sigma / n \Chi_{n}^{2}


#   c) Faça uma ilustração computacional da distribuição aproximada conforme o tamanho da amostra cresce. Use n = 50, 250 e 1000.
#   d) Compare computacionalmente a distribuição aproximada com a distribuição exata.

# Define um sigma
sigma <- 2

# Gera simulacoes da distribuicao aproximada
sim.50 <- sigma^2/50 * rchisq(n = 50, df = 50)
sim.250 <- sigma^2/250 * rchisq(n = 250, df = 250)
sim.1000 <- sigma^2/1000 * rchisq(n = 1000, df = 1000)

# Gera simulacoes de distribuicao exata
# Assumindo que segue uma distribuicao normal
ext.50 <- replicate(50, var(rnorm(n = 50, mean = 0, sd = sigma)))
ext.250 <- replicate(250, var(rnorm(n = 250, mean = 0, sd = sigma)))
ext.1000 <- replicate(1000, var(rnorm(n = 1000, mean = 0, sd = sigma)))

# Plota as distribuicoes
par(family='Arial', mfrow=c(2, 3))
hist(sim.50, xlab='', ylab='Frequência', main='N=50')
hist(sim.250, xlab='Aproximado pela Qui-Quadrado', ylab='', main='N=250')
hist(sim.1000, xlab='', ylab='', main='N=1000')

hist(ext.50, xlab='', ylab='Frequência', main='N=50')
hist(ext.250, xlab='Distribuição exata', ylab='', main='N=250')
hist(ext.1000, xlab='', ylab='', main='N=1000')


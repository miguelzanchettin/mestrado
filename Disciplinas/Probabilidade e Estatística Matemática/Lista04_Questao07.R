
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 04
# Questao 7
# 

rm(list=ls())

# 7. Seja Y1, ... , Yn uma v.a iid da distribuição de Poisson com parâmetro \lambda.
#   a) Mostre que a média amostral converge em probabilidade para \lambda quando n \to \infty.
#     Respondido a mao

#   b) Encontre a distribuição aproximada da média amostral nesta situação.
#     Respondido a mao
#     Resposta dada:
#       \hat{y} \sim N(\lambda, \lambda/n)

#   c) Faça uma ilustração computacional e compare a distribuição empírica com a distribuição aproximada.

set.seed(123)


# Utilizarei do fato de que E(Y) = \lambda, pois Y ~ P(\lambda)
# Para aproximar pela media amostral

estimar_lambdas <- function(n, repeticoes, lambda_real) {
  lambdas <- colMeans(replicate(repeticoes, rpois(n=n, lambda=lambda_real)))
  return(lambdas)
}

lambda.10 <- estimar_lambdas(n = 10, repeticoes = 10, lambda_real = 5)
lambda.1000 <- estimar_lambdas(n = 100, repeticoes = 1000, lambda_real = 5)
lambda.100000 <- estimar_lambdas(n = 100, repeticoes = 100000, lambda_real = 5)

# Estimativa assintotica pela normal
assint.10 <- rnorm(n = 10, mean = 5, sd = sqrt(5/10))
assint.1000 <- rnorm(n = 1000, mean = 5, sd = sqrt(5/1000))
assint.100000 <- rnorm(n = 100000, mean = 5, sd = sqrt(5/100000)) 

par(family='Arial', mfrow=c(2, 3))
hist(lambda.10, xlab='', ylab='Frequência', main='N=10')
hist(lambda.1000, xlab='Lambda empírico', ylab='Frequência', main='N=100')
hist(lambda.100000, xlab='', ylab='Frequência', main='N=1000')

hist(assint.10, xlab='', ylab='Frequência', main='N=10')
hist(assint.1000, xlab='Lambda assintótico', ylab='Frequência', main='N=100')
hist(assint.100000, xlab='', ylab='Frequência', main='N=1000')

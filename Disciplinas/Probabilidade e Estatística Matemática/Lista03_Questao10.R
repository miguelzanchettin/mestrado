
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 03
# Questao 10
# 

rm(list=ls())

# Um dado é lançado 12 vezes. Seja Xi o número de jogadas em que cada i caiu para cima, para i = 1, ... , 6.
#   a) Calcule a esperança de Xi
#   b) Calcule a variância de Xi
#   c) Calcule a probabilidade de cada uma das faces cair para cima exatamente duas vezes.
#   d) Implemente um código computacional ilustrando essa situação. Tente de forma aproximada calcular a probabilidade do item c).


n <- 12

#-----------------------------------------------------------------------------
# Via simulacao empirica

n.sim <- 100000

# Gera lancamentos aleatoriamente, com reposicao
set.seed(123)
sim <- matrix(sample(1:6, n * n.sim, replace = TRUE), nrow = n.sim)

# Conta cada caso em que todos os valores apareceram duas vezes
resultado_desejado <- function(row) {all(table(sim[1, ]) == 2)}
empirica <- apply(sim, 1, resultado_desejado)

# Calcula a probabilidade empirica
p_empirica <- sum(empirica) / length(empirica)
p_empirica

#-----------------------------------------------------------------------------
# Via calculo de probabilidade

prob <- function(ocorrencia, em, chance) 
{
    p <- factorial(em)/(factorial(ocorrencia) * factorial(em - ocorrencia)) * (chance)^ocorrencia * (1-chance)^(em - ocorrencia)
    return(p)
}

# Jogadas sao decrescentes
f1 <- prob(ocorrencia = 2, em = 12, chance = 1/6)
f2 <- prob(ocorrencia = 2, em = 10, chance = 1/6)
f3 <- prob(ocorrencia = 2, em = 08, chance = 1/6)
f4 <- prob(ocorrencia = 2, em = 06, chance = 1/6)
f5 <- prob(ocorrencia = 2, em = 04, chance = 1/6)
f6 <- prob(ocorrencia = 2, em = 02, chance = 1/6)

# Calcula a probabilidade do evento
p_evento <- f1 * f2 * f2 * f3 * f4 * f5 * f6


#-----------------------------------------------------------------------------
# Comparacao

print(paste('Simulação computacional: ', format(p_empirica * 100), '%'))
print(paste('Calculo de probabilidade: ', format(p_evento * 100), '%'))



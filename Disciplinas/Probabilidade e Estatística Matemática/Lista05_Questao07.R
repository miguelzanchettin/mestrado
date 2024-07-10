
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 05
# Questao 7
# 

rm(list=ls())

# 7. Para cada uma das 3 distribuições propostas no exercício 6, encontre o valor de y tal que:
#   a) P(Y < y) = 0.90
#   b) P(Y < y) = 0.025
#   c) P(Y < y) = 0.01
#   d) P(Y > y) = 0.975 

# Distribuicoes do exercicio 6:
# Y \sim t_{20} 
# T \sim \Chi_{16}
# Y \sim F_{(10, 7)}

respostas <- list()

# Para Y \sim t_{20}
  
  dist_1 <- 'Y ~ t_{20}'
  respostas[[dist_1]] <- list()
  respostas[[dist_1]][['y | P(Y < y) = 0.900']] <- qt(p = 0.900, df = 20)
  respostas[[dist_1]][['y | P(Y < y) = 0.025']] <- qt(p = 0.025, df = 20)
  respostas[[dist_1]][['y | P(Y < y) = 0.010']] <- qt(p = 0.010, df = 20)
  respostas[[dist_1]][['y | P(Y < y) = 0.975']] <- qt(p = 0.975, df = 20)

# Para T \sim \Chi_{16}

  dist_2 <- 'T ~ \\Chi_{16}'
  respostas[[dist_2]] <- list()
  respostas[[dist_2]][['y | P(Y < y) = 0.900']] <- qchisq(p = 0.900, df = 16)
  respostas[[dist_2]][['y | P(Y < y) = 0.025']] <- qchisq(p = 0.025, df = 16)
  respostas[[dist_2]][['y | P(Y < y) = 0.010']] <- qchisq(p = 0.010, df = 16)
  respostas[[dist_2]][['y | P(Y < y) = 0.975']] <- qchisq(p = 0.975, df = 16)
  
# Para Y \sim F_{(10, 7)}

  dist_3 <- 'Y ~ F_{(10, 7)}'
  respostas[[dist_3]] <- list()
  respostas[[dist_3]][['y | P(Y < y) = 0.900']] <- qf(p = 0.900, df1 = 10, df2 = 7)
  respostas[[dist_3]][['y | P(Y < y) = 0.025']] <- qf(p = 0.025, df1 = 10, df2 = 7)
  respostas[[dist_3]][['y | P(Y < y) = 0.010']] <- qf(p = 0.010, df1 = 10, df2 = 7)
  respostas[[dist_3]][['y | P(Y < y) = 0.975']] <- qf(p = 0.975, df1 = 10, df2 = 7)
  
print(respostas)
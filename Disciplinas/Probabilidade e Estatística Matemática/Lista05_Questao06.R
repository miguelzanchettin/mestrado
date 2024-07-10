
# Miguel Zanchettin de Oliveira
# Probabilidade e Estatistica Matematica I, 1S2024
#

# Lista 05
# Questao 6
# 

rm(list=ls())

# 6. Utilizando algum recurso computacional ou tabela, calcule as probabilidades a seguir, conforme a distribuição da v.a. Y:
#
# \hline
# Y \sim t_{20}   | T \sim \Chi_{16}    | Y \sim F_{(10, 7)} \\
# \hline
# P(Y < −2, 85)   | P(Y > 8, 91)        | P(Y > 0, 15) \\
# P(Y > 2, 85)    | P(Y > 32, 85)       | P(Y > 5, 35) \\
# P(Y > 2, 12)    | P(Y > 22, 80)       | P(Y < 7, 41) \\
# P(Y < −3, 01)   | P(Y < 10, 12)       | P(Y < 1) \\
# \hline

respostas <- list()

# Para Y \sim t_{20}
  
  dist_1 <- 'Y ~ t_{20}'
  respostas[[dist_1]] <- list()
  respostas[[dist_1]][['P(Y < −2,85)']]  <- pt(q = -2.85, df = 20)
  respostas[[dist_1]][['P(Y > 2,85)']]   <- 1 - pt(q = 2.85, df = 20)
  respostas[[dist_1]][['P(Y > 2,12)']]   <- 1 - pt(q = 2.12, df = 20)
  respostas[[dist_1]][['P(Y < -3,01)']]  <- pt(q = -3.01, df = 20)

# Para T \sim \Chi_{16}

  dist_2 <- 'T ~ \\Chi_{16}'
  respostas[[dist_2]] <- list()
  respostas[[dist_2]][['P(Y > 8,91)']]   <- 1 - pchisq(q = 8.91, df = 16) 
  respostas[[dist_2]][['P(Y > 32,85)']]  <- 1 - pchisq(q = 32.85, df = 16) 
  respostas[[dist_2]][['P(Y > 22,80']]   <- 1 - pchisq(q = 22.80, df = 16) 
  respostas[[dist_2]][['P(Y < 10,12)']]  <- pchisq(q = 10.12, df = 16) 
  
# Y \sim F_{(10, 7)}

  dist_3 <- 'Y ~ F_{(10, 7)}'
  respostas[[dist_3]] <- list()
  respostas[[dist_3]][['P(Y > 0,15)']]  <- 1 - pf(q = 0.15, df1 = 10, df2 = 7) 
  respostas[[dist_3]][['P(Y > 5,37)']]  <- 1 - pf(q = 5.37, df1 = 10, df2 = 7) 
  respostas[[dist_3]][['P(Y < 7,41)']]  <- pf(q = 7.41, df1 = 10, df2 = 7) 
  respostas[[dist_3]][['P(Y < 1)']]     <- pf(q = 1, df1 = 10, df2 = 7) 
  
print(respostas)

##########################################################################
# Define ambiente
#-------------------------------------------------------------------------

# Reseta variaveis
rm(list=ls())

# Define diretorio de trabalho
setwd('~/Documentos/Repositório GIT/mestrado/Pesquisas/Atas do Copom')

# Pacotes necessarios
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  "rbcb", 
  "tidyverse",
  "tidytext",
  "dplyr", 
  "xts",
  "forecast", 
  "kernlab", 
  "randomForest", 
  "xgboost"
)

##########################################################################
# Funcoes auxiliares
#-------------------------------------------------------------------------

# Calcula as metricas de desempenho dos modelos
calcular_metricas <- function(y, y_pred) 
{
  
  rmse <- sqrt(mean((y_pred - y)^2))
  mae <- mean(abs(y_pred - y))
  
  return(list(
    'RMSE' = rmse, 
    'MAE' = mae
  ))
  
}

# Converte intervalo continuo em discreto de 0.25 p.p.
discretizar_intervalo <- function(x) 
{
  return(round(x / 0.25, 0) * 0.25)
}

# Para criar uma coluna em uma matriz apenas se a coluna ainda nao existir
criarColuna <- function(data, coluna) 
{
   
  if ((coluna %in% colnames(data)) == FALSE)
  {
    col  <- matrix(NA, nrow = nrow(data), ncol = 1)
    colnames(col) <- c(coluna)
    
    data <- cbind(data, col)
  }
  
  return(data)
  
}


##########################################################################
# Extracao
#-------------------------------------------------------------------------

get_atas <- function(atas.csv) 
{
  
  # Apenas importa o CSV
  if (file.exists(atas.csv) == TRUE)
  {
    
    atas <- read.csv(file = atas.csv, header = T, sep = ',') %>%
      dplyr::mutate(date = lubridate::ymd(date))
    
    return(atas)
  }
  
  url_raw  <- 'https://www.bcb.gov.br'
  url_api  <- "api/servico/sitebcb/copomminutes/ultimas"
  filters  <- 'quantidade=10000&filtro='
  origin   <- glue::glue('{url_raw}/{url_api}?{filters}')
  
  # Coleta o indice
  raw_index <- jsonlite::fromJSON(origin)$conteudo %>%
    dplyr::as_tibble() %>%
    dplyr::select(date    = 'DataReferencia', 
                  meeting = 'Titulo',
                  url     = 'Url' 
    ) %>%
    dplyr::mutate(url = utils::URLencode(paste0(url_raw, url)))
  
  # Le todas as atas do indice 
  raw_minutes <- raw_index %>%
    dplyr::mutate(content = purrr::map(url, 
                                       pdftools::pdf_text))
  
  # Processamento basico
  
  minutes <- raw_minutes
  
  ## Corrige datas, remove atas de mudancas no COPOM e quebra em paginas
  minutes <- minutes %>%
    dplyr::mutate(
      date = as.Date(as.POSIXct(date, 
                                format = '%Y-%m-%dT%H:%M:%SZ', 
                                tz='UTC'))
    ) %>%
    dplyr::filter(!meeting == "Changes in Copom meetings") %>%
    tidyr::unnest(content) %>%
    dplyr::group_by(meeting) %>%
    dplyr::mutate(page = dplyr::row_number(), 
                  content = strsplit(content, '\r') %>% 
                    gsub('\n', ' ', .),
                  meeting = stringr::str_sub(meeting, 1, 3) %>% 
                    stringr::str_remove('[:alpha:]') %>%
                    as.numeric()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(content) %>% 
    dplyr::arrange(date)
  
  # Exporta para CSV
  write.csv(x = minutes, 
            file = 'AtasCopom.csv',
            quote = TRUE, 
            fileEncoding = 'utf-8',
            row.names = FALSE
  )
  
  atas <- read.csv(file = atas.csv, header = T, sep = ',') %>%
    dplyr::mutate(date = lubridate::ymd(date))
  
  rm(list=c('raw_minutes', 'raw_index', 'minutes'))
  return(atas)
  
}

get_selic <- function(dates) 
{
  
  # Importa taxas
  rates <- rbcb::get_series(c(selic = 432), 
                            start_date = min(dates), 
                            end_date = max(dates))
  
  # Enriquece os dados com variaveis de lag
  rates <- rates %>%
    dplyr::filter(date %in% dates) %>%
    dplyr::mutate(selic.lag = dplyr::lag(selic)) %>%
    dplyr::mutate(selic.var = selic - selic.lag) %>%
    dplyr::mutate(selic.next.var = lead(selic.var))
  
  return(rates)
  
}

#-------------------------------------------------------------------------
# Executa

dfAtas  <- get_atas('AtasCopom.csv')
dfSelic <- get_selic(dates = dfAtas$date)

dfUnida <- dfAtas %>% 
           left_join(dfSelic, by = 'date') %>%
           filter(is.na(selic.next.var) == F) %>%
           filter(is.na(selic.var) == F)

##########################################################################
# Data separation
#-------------------------------------------------------------------------

# Separa a base de dados em 70% treino e 30% teste
pcTreino <- 0.7

meetings <- dfUnida %>% select(meeting) %>% distinct() %>% as.vector()
meetings <- meetings[[1]]

indTrain <- meetings[1:length(meetings) * pcTreino]

dfTrain <- dfUnida %>% filter((meeting %in% indTrain))
dfTest  <- dfUnida %>% filter(!(meeting %in% indTrain))

##########################################################################
# Time series prediction
#-------------------------------------------------------------------------

projetarSerie <- function(dfUnida, meetings, pcTreino)
{
  
  # Extrai serie
  dfUnidaTS <- dfUnida %>%
    select(date, selic.next.var) %>%
    distinct() %>%
    mutate(date = as.Date(date))
  
  # Intervalo do loop, considerando treino
  loopIni <- as.integer(length(meetings) * pcTreino)
  loopFim <- length(meetings)
  
  # Para receber resultados
  tsPredVector <- numeric(loopFim - loopIni)
  
  # Treina modelo
  for (i in loopIni:(loopFim+1))
  {
    
    tsSelic <- xts(dfUnidaTS$selic.next.var[1:(i-1)], 
                   order.by = dfUnidaTS$date[1:(i-1)])
    tsFit <- forecast::auto.arima(tsSelic)
    tsPred <- forecast(tsFit, h=1)$mean
    
    tsPredVector[i - loopIni] <- tsPred 
    
  }
  
  # Vetor de projecao
  ts.y_hat    <- as.numeric(tsPredVector)
  ts.y_hat    <- discretizar_intervalo(ts.y_hat)
  
  # Vetor de testes
  ts.y        <- as.numeric(dfUnidaTS$selic.next.var[loopIni:loopFim])
  ts.y        <- discretizar_intervalo(ts.y)
  
  # Metricas em testes
  ts.metricas <- calcular_metricas(ts.y, ts.y_hat)
  
  # Grafico em testes
  par(family = 'Arial')
  plot(ts.y, type = 'l', col='gray', lwd=3)
  lines(ts.y_hat, type = 'l', col='red', lwd=3)
  ts.plot <- recordPlot()
  
  # Retornos
  resultados <- list(
    'y'        = ts.y,
    'y_hat'    = ts.y_hat,
    'plot'     = ts.plot, 
    'metricas' = ts.metricas, 
    'loopIni'  = loopIni, 
    'loopFim'  = loopFim
  )
  
  return(resultados)

}

#-------------------------------------------------------------------------
# Executa

tsResultados <- projetarSerie(dfUnida, meetings, pcTreino)

##########################################################################
# Text mining
#-------------------------------------------------------------------------

padronizarDados <- function(extrairDe, aplicarEm) 
{
  
  amostralMu <- extrairDe %>% sapply(mean)
  amostralSigma <- extrairDe %>% sapply(sd)
  
  resultado <- scale(aplicarEm, 
                     center = amostralMu, 
                     scale = amostralSigma)
  
  return(resultado)
  
}

aplicaEmbeddings <- function(gloveName, df, incluirTS = F) 
{
  
  # Gera o lexico
  dfLexico <- df %>%
    group_by(meeting) %>%
    select(meeting, selic.next.var, content) %>%
    unnest_tokens(word, content) %>%
    ungroup()
  
  # Importa o embedding
  gloveEmbedding <- switch(
    gloveName,
    '6B_D50' = textdata::embedding_glove6b(dimensions = 50),
    '6B_D100' = textdata::embedding_glove6b(dimensions = 100),
    '6B_D200' = textdata::embedding_glove6b(dimensions = 200),
    '6B_D300' = textdata::embedding_glove6b(dimensions = 300),
    '27B_D25' = textdata::embedding_glove27b(dimensions = 25),
    '27B_D50' = textdata::embedding_glove27b(dimensions = 50),
    '27B_D100' = textdata::embedding_glove27b(dimensions = 100), 
    '27B_D200' = textdata::embedding_glove27b(dimensions = 200) 
  )
  
  # Matriz de pesos
  mtxEmbedding <- gloveEmbedding %>%
    pivot_longer(contains('d'), names_to = 'dimension') %>%
    inner_join(by='token',
               dfLexico %>%
                 distinct(word) %>%
                 rename(token=word)
                ) %>%
    cast_sparse(token, dimension, value)
  
  # Matriz de frequencias
  mtxFrequency <- dfLexico %>%
    rename(token=word) %>%
    dplyr::inner_join(by = 'token',
                      gloveEmbedding %>%
                        pivot_longer(contains('d'), 
                                     names_to = 'dimension') %>%
                        distinct(token)
    ) %>%
    dplyr::count(meeting, token) %>%
    tidytext::cast_sparse(meeting, token, n)
  
  # Calcula matriz completa
  mtxCompleta <- mtxFrequency %*% mtxEmbedding
  
  # Traz selic.next.var para a matriz
  dfFinal <- mtxCompleta %>%
    as.matrix() %>%
    as.data.frame() %>%
    mutate(meeting = as.double(rownames(.))) %>%
    left_join(by = 'meeting', 
                     df %>%
                       dplyr::select(meeting, selic.var, selic.next.var) %>%
                       dplyr::distinct())
  
  # Remove a serie
  if (incluirTS == FALSE)
  {
    dfFinal <- dfFinal %>% select(-selic.var)  
  }
  
  return(dfFinal)

}

##########################################################################
# Modelagem
#-------------------------------------------------------------------------

estimarModelos <- function (dfUnida, meetings, pcTreino, consideraTS)
{
  
  loopIni <- as.integer(length(meetings) * pcTreino)
  loopFim <- length(meetings)
  
  # Para receber resultados
  yMatrix <- matrix(0, nrow = (loopFim - loopIni + 1), ncol = 1)
  colnames(yMatrix) <- c('y')
  
  # Inicia uma barra de progresso
  pb <- txtProgressBar(min = loopIni, max = (loopFim + 1), style = 3)
  
  for (i in loopIni:loopFim)
  {
    
    dfEmbeddingsTrain <- aplicaEmbeddings('27B_D25', dfUnida %>% filter(meeting %in% meetings[1:(i-1)]), consideraTS)
    dfEmbeddingsTest  <- aplicaEmbeddings('27B_D25', dfUnida %>% filter(meeting %in% meetings[i]), consideraTS)
    
    y.train <- dfEmbeddingsTrain$selic.next.var
    X.train <- padronizarDados(extrairDe = dfEmbeddingsTrain %>% select(-meeting, -selic.next.var), 
                               aplicarEm = dfEmbeddingsTrain %>% select(-meeting, -selic.next.var))
    
    y.test  <- dfEmbeddingsTest$selic.next.var
    X.test <-  padronizarDados(extrairDe = dfEmbeddingsTrain %>% select(-meeting, -selic.next.var), 
                               aplicarEm = dfEmbeddingsTest %>% select(-meeting, -selic.next.var))
    
    
    estimarResultado <- function(model, utiliza_df = F)
    {
      
      if (utiliza_df == T) y_hat <- predict(model, as.data.frame(X.test))
      else y_hat <- predict(model, X.test)
      
      y_hat <- discretizar_intervalo(y_hat)
      y_hat <- as.double(y_hat)
      return(y_hat)
    }
    
    # Valor correto
    yMatrix <- criarColuna(yMatrix, 'y')
    yMatrix[(i - loopIni + 1), 'y'] <- as.double(y.test)
    
    # SVR Linear
    yMatrix <- criarColuna(yMatrix, 'svrLinear')
    model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'vanilladot', kpar = list(), type = 'eps-svr')
    yMatrix[(i - loopIni + 1), 'svrLinear'] <- estimarResultado(model)
  
    # SVR Polynomial Grau 2
    yMatrix <- criarColuna(yMatrix, 'svrPoly2degree')
    model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'polydot', kpar = list('degree' = 2), type = 'eps-svr')
    yMatrix[(i - loopIni + 1), 'svrPoly2degree'] <- estimarResultado(model)
   
    # SVR Polynomial Grau 3
    yMatrix <- criarColuna(yMatrix, 'svrPoly3degree')
    model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'polydot', kpar = list('degree' = 3), type = 'eps-svr')
    yMatrix[(i - loopIni + 1), 'svrPoly3degree'] <- estimarResultado(model)
    
    # SVR Polynomial Grau 4
    yMatrix <- criarColuna(yMatrix, 'svrPoly4degree')
    model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'polydot', kpar = list('degree' = 4), type = 'eps-svr')
    yMatrix[(i - loopIni + 1), 'svrPoly4degree'] <- estimarResultado(model)
  
    # SVR Polynomial Grau 5
    yMatrix <- criarColuna(yMatrix, 'svrPoly5degree')
    model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'polydot', kpar = list('degree' = 5), type = 'eps-svr')
    yMatrix[(i - loopIni + 1), 'svrPoly5degree'] <- estimarResultado(model)
    
    # SVR Radial Basis Function
    yMatrix <- criarColuna(yMatrix, 'svrRadial')
    model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'rbfdot', kpar = list(), type = 'eps-svr')
    yMatrix[(i - loopIni + 1), 'svrRadial'] <- estimarResultado(model)
   
    # Random Forest
    yMatrix <- criarColuna(yMatrix, 'randomForest')
    model <- randomForest(x = X.train, y = y.train)
    yMatrix[(i - loopIni + 1), 'randomForest'] <- estimarResultado(model)
    
    # XGBoost
    yMatrix <- criarColuna(yMatrix, 'xgBoost')
    model <- xgboost(data = X.train, label = y.train, nrounds = 100, objective = 'reg:squarederror', verbose = F)
    yMatrix[(i - loopIni + 1), 'xgBoost'] <- estimarResultado(model)
   
    # Regressao linear multipla
    yMatrix <- criarColuna(yMatrix, 'linearRegression')
    model <- lm(y ~ ., data=as.data.frame(cbind(X.train, y = y.train)))
    yMatrix[(i - loopIni + 1), 'linearRegression'] <- estimarResultado(model, T)
    
    # Atualiza barra de progresso
    setTxtProgressBar(pb, i)
    
  }
  
  # Encerra a barra de progresso
  close(pb)
  
  # Retorna a matriz
  return(yMatrix)

}

ySemTS <- estimarModelos(dfUnida, meetings, pcTreino, FALSE)
colnames(ySemTS) <- paste0(colnames(ySemTS), '.semTS')

yComTS <- estimarModelos(dfUnida, meetings, pcTreino, TRUE)
colnames(yComTS) <- paste0(colnames(yComTS), '.comTS')

yMatrix <- cbind(ySemTS, yComTS)

# Adiciona as series temporais
yMatrix <- cbind(yMatrix, timeSeries.comTS = tsResultados$y_hat[2:length(tsResultados$y_hat)])
yMatrix <- cbind(yMatrix, timeSeries.semTS = tsResultados$y_hat[2:length(tsResultados$y_hat)])

# Define itens para calcular metricas
modelos <- colnames(yMatrix)
metricas <- list()

# Calcula metricas
for (i in 2:length(modelos))
{
  metricas[[modelos[i]]] <- calcular_metricas(yMatrix[, 1], yMatrix[, i])
}

# Formata metricas
metricas <- t(as.data.frame(metricas))
metricas <- cbind(str_split_fixed(rownames(metricas), "\\.", 3), metricas)
metricas <- as.data.frame(metricas)

colnames(metricas) <- c('Modelo', 'TS','Metrica', 'Valor')
rownames(metricas) <- c()

metricas <- metricas %>%
  select(Modelo, TS, Metrica, Valor) %>%
  mutate(Valor = as.double(Valor)) %>%
  group_by(Modelo, TS, Metrica) %>%
  pivot_wider(names_from = c(Metrica, TS), values_from = Valor, values_fn = sum)

# Mostra metricas
metricas




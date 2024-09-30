
##########################################################################
# Define ambiente
#-------------------------------------------------------------------------

# Modo DEBUG
debuggingState(on=FALSE)

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
  "xgboost", 
  "ggplot2"
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

get_selic <- function(file, dates) 
{
  
  # Apenas importa o CSV
  if (file.exists(file) == FALSE)
  {
    
    # Importa taxas
    rates <- rbcb::get_series(c(selic = 432))
    
    # Enriquece os dados com variaveis de lag
    rates <- rates %>%
      dplyr::filter(date %in% dates) %>%
      dplyr::mutate(selic.lag = dplyr::lag(selic)) %>%
      dplyr::mutate(selic.var = selic - selic.lag) %>%
      dplyr::mutate(selic.next.var = lead(selic.var))
    
    # Exporta para CSV
    write.csv(x = rates, 
              file = file,
              quote = TRUE, 
              fileEncoding = 'utf-8',
              row.names = FALSE
    )
    
  }
  
  
  # Importa CSV
  selic <- read.csv(file = file, header = T, sep = ',') %>%
    dplyr::mutate(date = lubridate::ymd(date))

  return(selic)
  
}

#-------------------------------------------------------------------------
# Executa

dfAtas  <- get_atas('AtasCopom.csv')
dfSelic <- get_selic('SerieSELIC.csv', dates = dfAtas$date)

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

##########################################################################
# Time series prediction
#-------------------------------------------------------------------------

projetarSerie <- function(dfUnida, meetings, pcTreino)
{
  
  # Extrai serie
  ts.df <- dfUnida %>%
    select(date, selic.next.var) %>%
    mutate(date = as.Date(date)) %>%
    distinct()
  
  # Intervalo do loop, considerando treino
  i.start <- as.integer(length(meetings) * pcTreino)
  i.end <- length(meetings)
  
  # Para receber resultados
  ts.pred.vector <- numeric(i.end - i.start)
  
  # Para receber ordens
  ts.order.matrix <- matrix(NA, nrow=(i.end - i.start), ncol=3)
  colnames(ts.order.matrix) <- c('p', 'd', 'q')
  
  # Treina modelo
  for (i in i.start:(i.end - 1))
  {
    
    ts.value <- ts.df[1:(i - 1), 'selic.next.var']
    ts.date <- ts.df[1:(i - 1), 'date']
    ts <- xts(ts.value, order.by = ts.date)
    
    fit <- forecast::auto.arima(ts)
    fit.order <- arimaorder(fit)
    fit.pred <- forecast(fit, h=2)$mean[2]
    
    # Save prediction
    row = i - i.start + 1
    
    ts.pred.vector[row] <- fit.pred
    
    for(j in names(fit.order))
    {
      if (j %in% colnames(ts.order.matrix))
      {
        ts.order.matrix[row, j] <- as.double(fit.order[j])  
      }
    }
    
  }
    
  # Vetor de projecao
  ts.y_hat    <- as.numeric(ts.pred.vector)
  ts.y_hat    <- discretizar_intervalo(ts.y_hat)
  
  # Vetor de testes
  ts.y        <- as.numeric(ts.df$selic.next.var[i.start:(i.end - 1)])
  ts.y        <- discretizar_intervalo(ts.y)
  
  # Metricas em testes
  ts.metricas <- calcular_metricas(ts.y, ts.y_hat)
  
  # Retornos
  resultados <- list(
    'y'             = ts.y,
    'y_hat'         = ts.y_hat,
    'orders'        = round(colMeans(ts.order.matrix), 0),
    'orders_matrix' = ts.order.matrix, 
    'metricas'      = ts.metricas,
    'loopIni'       = i.start, 
    'loopFim'       = i.end
  )
  
  return(resultados)

}

#-------------------------------------------------------------------------
# Executa

tsResultados <- projetarSerie(dfUnida, meetings, pcTreino)

##########################################################################
# Text mining
#-------------------------------------------------------------------------

aplicaEmbeddings <- function(gloveName, df) 
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
    inner_join(by='token',
               dfLexico %>%
                 distinct(word) %>%
                 rename(token=word)
    ) %>%
    pivot_longer(contains('d'), names_to = 'dimension') %>%
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
  
  # Limpa memoria
  rm(list=c('gloveEmbedding'))
  
  # Calcula matriz completa
  mtxCompleta <- mtxFrequency %*% mtxEmbedding
  
  # Limpa memoria
  rm(list=c('mtxFrequency', 'mtxEmbedding'))
  
  # Traz selic.next.var para a matriz
  dfFinal <- mtxCompleta %>%
    as.matrix() %>%
    as.data.frame() %>%
    mutate(meeting = as.double(rownames(.))) %>%
    left_join(by = 'meeting', 
                     df %>%
                       dplyr::select(meeting, 
                                     selic.var, 
                                     selic.next.var) %>%
                       dplyr::distinct())
  
  rm(list=c('mtxCompleta'))

  return(dfFinal)

}

simularARIMA <- function(data, time_series_col, p, q) {
  
  # Ensure data is a data frame and has the time series column
  if (!is.data.frame(data) || !(time_series_col %in% names(data))) {
    stop("Invalid data frame or column name.")
  }
  
  # Rename the time series column for convenience
  colnames(data)[which(names(data) == time_series_col)] <- "y"
  
  # Create lagged features for AR component
  for (i in 1:p) {
    data <- data %>%
      mutate(!!paste0("y_t_", i) := lag(y, i))
  }
  
  # Drop rows with NA values introduced by lagging
  data <- drop_na(data)
  
  # Fit the initial AR model
  ar_features <- select(data, starts_with("y"))
  ar_model <- lm(y ~ ., data = ar_features)
  
  # Calculate residuals
  data$residuals <- residuals(ar_model)
  
  # Create lagged residuals for MA component
  for (i in 1:q) {
    data <- data %>%
      mutate(!!paste0("residuals_t_", i) := lag(residuals, i))
  }
  
  # Drop rows with NA values introduced by lagging
  data <- drop_na(data)
  
  # Select final features and target variable
  #final_features <- c(paste0("y_t_", 1:p), paste0("residuals_t_", 1:q))
  #final_data <- data %>%
  #  select(y, all_of(final_features))
  
  return(data)
  
}

gerarBase <- function(dfUnida, embeddingName, consideraTS = NA)
{
  
  # Resgata qual o embedding atual
  currentEmbeddingName <- embeddingName
  
  # Monta nome do arquivo
  file <- paste0('AtasComEmbedding', currentEmbeddingName, '.csv')
  
  if (file.exists(file) == FALSE)
  {
  
    # Gera base
    dfEmbeddings <- aplicaEmbeddings(currentEmbeddingName, dfUnida)
    
    # Exporta para CSV
    write.csv(x = dfEmbeddings, 
              file = file,
              quote = TRUE, 
              fileEncoding = 'utf-8',
              row.names = FALSE
              )
    
    # Limpa da memoria, para garantir sempre versao de importacao
    rm(list=c('dfEmbeddings'))
  
  }
  
  # Importa CSV
  dfEmbeddings <- read.csv(file = file, header = T, sep = ',')
  
  # Simula ARIMA
  if (typeof(consideraTS) != 'logical')
  {
    p <- consideraTS$orders['p']
    q <- consideraTS$orders['q']
    dfEmbeddings <- simularARIMA(dfEmbeddings, 'selic.var', p, q)
  }
  else
  {
    dfEmbeddings <- dfEmbeddings %>% select(-selic.var)
  }
  
  return(dfEmbeddings)
  
}

#-------------------------------------------------------------------------
# Executa

embeddingName <- "27B_D100"
basesTratadasSemTS <- gerarBase(dfUnida, embeddingName)
basesTratadasComTS <- gerarBase(dfUnida, embeddingName, tsResultados)

##########################################################################
# Modelagem
#-------------------------------------------------------------------------

# Funcao auxiliar para padronizar dados
padronizarDados <- function(extrairDe, aplicarEm) 
{
  
  amostralMu <- extrairDe %>% sapply(mean)
  amostralSigma <- extrairDe %>% sapply(sd)
  
  resultado <- scale(aplicarEm, 
                     center = amostralMu, 
                     scale = amostralSigma)
  
  return(resultado)
  
}

estimarModelos <- function (dfEmbeddings, currentEmbeddingName, meetings, pcTreino)
{
  
  i.start <- as.integer(length(meetings) * pcTreino)
  i.end <- length(meetings)
  
  y <- matrix(NA, nrow=(i.end - i.start), ncol=5)
  colnames(y) <- c('y', 'randomForest', 'xgBoost', 'linearRegression', 'SVRLinear')
  
  # Loop da janela de treinamento
  for (i in i.start:(i.end - 2))
  {
    
    # Coleta as bases
    dfEmbeddingsTrain <- dfEmbeddings %>% 
      filter(meeting %in% meetings[1:i])
    
    dfEmbeddingsTest  <- dfEmbeddings %>% 
      filter(meeting %in% meetings[(i + 2)])
    
    y.train <- dfEmbeddingsTrain$selic.next.var
    y.test  <- dfEmbeddingsTest$selic.next.var
    
    X.train <- padronizarDados(
      extrairDe = select(dfEmbeddingsTrain, starts_with('d')),
      aplicarEm = select(dfEmbeddingsTrain, starts_with('d'))
      ) %>%
      as.array()
    
    X.test <-  padronizarDados(
      extrairDe = select(dfEmbeddingsTrain, starts_with('d')),
      aplicarEm = select(dfEmbeddingsTest, starts_with('d'))
      ) %>%
      as.array()
    
    X.train <- cbind(X.train, select(dfEmbeddingsTrain, starts_with('y'))) %>% data.matrix()
    X.test <- cbind(X.test, select(dfEmbeddingsTest, starts_with('y'))) %>% data.matrix()
    
    # Funcao auxiliar para estimar resultado do modelo
    estimarResultado <- function(model, utilizaDF = F)
    {
      
      if (utilizaDF == T) y_hat <- predict(model, as.data.frame(X.test))
      else y_hat <- predict(model, X.test)
      
      y_hat <- discretizar_intervalo(y_hat)
      y_hat <- as.double(y_hat)
      return(y_hat)
    
    }
    
    # Regressao linear multipla
    linearRegression.model <- lm(vlr ~ ., data=as.data.frame(cbind(X.train, vlr = y.train)))
    linearRegression.y <- estimarResultado(linearRegression.model, utilizaDF = T)
    
    # Random Forest
    randomForest.model <- randomForest(x = X.train, y = y.train)
    randomForest.y <- estimarResultado(randomForest.model, utilizaDF = F)

    # SVRLinear
    SVRLinear.model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'vanilladot', kpar = list(), type = 'eps-svr')
    SVRLinear.y <- estimarResultado(SVRLinear.model, utilizaDF = F)
    
    # XGBoost
    #xgBoost.model <- xgboost(data = X.train, label = y.train, nrounds = 100, objective = 'reg:squarederror', verbose = T)
    #xgBoost.y <- estimarResultado(xgBoost.model, utilizaDF = F)
    
    # Valor correto
    ind <- (i - i.start + 1) 
    y[ind, 'y'] <- as.double(y.test)
    y[ind, 'linearRegression'] <- as.double(linearRegression.y)
    y[ind, 'randomForest'] <- as.double(randomForest.y)
    y[ind, 'SVRLinear'] <- as.double(SVRLinear.y)
    #y[ind, 'xgBoost'] <- as.double(xgBoost.y)
    
  }
  
  return(y)
  
}

#-------------------------------------------------------------------------
# Executa

ySemTS <- estimarModelos(basesTratadasSemTS, embeddingName, meetings, pcTreino)
yComTS <- estimarModelos(basesTratadasComTS, embeddingName, meetings, pcTreino)

##########################################################################
# Model selection
#-------------------------------------------------------------------------

calcularMetricas <- function(y)
{
  
  metricas <- y[, colnames(y)[which(colnames(y) != 'y')]]
  metricas <- (metricas - y[, 'y']) ^ 2
  metricas <- metricas[is.na(y[, 'y']) == FALSE, ]
  metricas <- sqrt(colMeans(metricas))

  return(metricas)

}

metricasSemTS <- calcularMetricas(ySemTS)
metricasComTS <- calcularMetricas(yComTS)

print(metricasSemTS)
print(metricasComTS)
print(tsResultados$metricas$RMSE)

#-------------------------------------------------------------------------
# Define ambiente

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
  "TSA", 
  "aTSA", 
  
  "tm", 
  "e1071",
  "kernlab",
  "caret", 
  "doParallel", 
  "MLmetrics", 
  "forecast", 
  "textdata", 
  "xgboost"
)


#-------------------------------------------------------------------------
# Parametros

considera.ts = TRUE
considera.var.atual = TRUE

#-------------------------------------------------------------------------
# Implementa metricas de validacao

calcular_metricas <- function(y, y_pred) 
{
  
  rmse <- sqrt(mean((y_pred - y)^2))
  mae <- mean(abs(y_pred - y))
  
  return(list(
    'RMSE' = rmse, 
    'MAE' = mae
  ))
  
}


#-------------------------------------------------------------------------
# Funcoes de extracao

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
# Extracao

atas  <- get_atas('AtasCopom.csv')
glimpse(atas)

selic <- get_selic(dates = atas$date)
glimpse(selic)

df <- atas %>% dplyr::left_join(selic, by = 'date') %>%
  filter(is.na(selic.next.var) == F) %>%
  filter(is.na(selic.var) == F)
glimpse(df)

#-------------------------------------------------------------------------
# Realiza estimacao por Series Temporais com janelas moveis

# Gera a serie temporal da selic
selic.df <- selic %>%
            select(date, selic) %>%
            mutate(date = as.Date(date))

selic.df <- xts(selic.df$selic, order.by = selic.df$date)
selic.df

# Plota a serie temporal
par(family = 'Arial', mfrow=c(1, 3))
plot(selic.df, main = "Time series data")
acf(selic.df, main = "ACF of Original Data")
pacf(selic.df, main = "PACF of Original Data")

# Gera vetor de zeros para resultados
y_hat <- y <- numeric((nrow(selic.df)-3))

# Para cada ponto na serie, calcula a proxima taxa e armazena no vetor
for (i in 3:(nrow(selic.df)-1))
{

  # Valor real
  var.real <- as.double(selic.df[i-1]) - as.double(selic.df[i])
  if (var.real != 0) y[i-2] <- var.real
  
  # Calcula projecao
  fit <- auto.arima(selic.df[1:(i-1)])
  proj <- forecast(fit, h=1)$mean[1]
  
  # Valor estimado
  var.hat <- as.double(selic.df[i-1] - proj)
  if (var.real != 0) y_hat[i-2] <- var.hat 
  
}

# Discretiza para intervalos de 0.25
y <- round(y / 0.25, 0) * 0.25
y_hat <- round(y_hat / 0.25, 0) * 0.25


y_hat %>%
  as.data.frame() %>%
  mutate(date = index())

selic.df[3:(nrow(selic.df) - 1)] %>%
  as.data.frame() %>%
  mutate(y_hat = y) %>%
  select(y_hat) 

length(y)


# Mostra o vetor resultado
par(family = 'Arial')
plot(matrix(y), type = 'l', col='gray', lwd=3)
lines(y_hat, type = 'l', col='red', lwd=3)

# Mostra o histograma
par(family = 'Arial', mfrow=c(1, 2))
hist(matrix(y))
hist(y_hat)

# Calcula o RMSE
ts.metricas <- calcular_metricas(y_hat, y)
ts.metricas

#------------------------------------------------------------------------
# Gera dicionario

# Apenas as duas ultimas paginas
dicionario <- df %>%
  group_by(meeting) %>%
  filter(
    page == max(page) 
    #| page == (max(page) - 1)
    ) %>%
  ungroup() %>%
  dplyr::select(meeting, selic.next.var, content) %>%
  tidytext::unnest_tokens(word, content)

#-------------------------------------------------------------------------
# Separa base em treino e teste

set.seed(2002)

meetings.nums <- unique(dicionario$meeting)

ind.tst <- sample(1:length(meetings.nums), 
                  size = length(meetings.nums) * 0.3, 
                  replace = FALSE)

data.test <- dicionario %>%
             filter(meeting %in% ind.tst)

data.train <- dicionario %>%
              filter(!(meeting %in% ind.tst))

#-------------------------------------------------------------------------
# Embedding do texto

#options(timeout = 600)

gloves <- c(
  '6B_D50'
  , '6B_D100'
  #, '6B_D200' # Nao executa por falta de RAM
  #, '6B_D300' # Nao executa por falta de RAM
  , '27B_D25'
  , '27B_D50'
  #, '27B_D100' # Nao executa por falta de RAM
  #, '27B_D200' # Nao executa por falta de RAM
)

resultados_finais <- list()

for (i in 1:length(gloves))
{
    
    current_glove <- gloves[i]
    
    glove <- switch(
      current_glove,
      '6B_D50' = textdata::embedding_glove6b(dimensions = 50),
      '6B_D100' = textdata::embedding_glove6b(dimensions = 100),
      '6B_D200' = textdata::embedding_glove6b(dimensions = 200),
      '6B_D300' = textdata::embedding_glove6b(dimensions = 300),
      '27B_D25' = textdata::embedding_glove27b(dimensions = 25),
      '27B_D50' = textdata::embedding_glove27b(dimensions = 50),
      '27B_D100' = textdata::embedding_glove27b(dimensions = 100), 
      '27B_D200' = textdata::embedding_glove27b(dimensions = 200) 
    )
    
    tidy_glove <- glove %>%
      tidyr::pivot_longer(dplyr::contains('d'), 
                          names_to = 'dimension') %>%
      dplyr::rename(item1 = token)
    
    
    # Limpa memoria
    rm(list=c('glove'))
  
    #-----------------------------------------------------------------------
    # Base de treino
    
    # Cria uma matriz de embedding
    embed.mtx.train <- tidy_glove %>%
      dplyr::inner_join(by = 'item1',
                        data.train %>%
                          distinct(word) %>%
                          rename(item1 = word)
      ) %>%
      tidytext::cast_sparse(item1, dimension, value)
    
    # Cria uma matriz esparsa de termos
    wmtx.train  <- data.train %>%
      dplyr::inner_join(by = 'word',
                        tidy_glove %>%
                          distinct(item1) %>%
                          rename(word = item1)
      ) %>%
      dplyr::count(meeting, word) %>%
      tidytext::cast_sparse(meeting, word, n)
    
    # Gera matriz de embedding
    dmtx.train <- wmtx.train %*% embed.mtx.train
  
    # Limpa memoria utilizada
    rm(list=c('wmtx.train', 'embed.mtx.train'))
    
    # Matriz de treino
    train.df <- dmtx.train %>%
      as.matrix() %>%
      as.data.frame() %>%
      dplyr::mutate(meeting = as.double(rownames(.))) %>%
      dplyr::left_join(by = 'meeting', 
                       df %>%
                         dplyr::select(meeting, selic.var, selic.next.var) %>%
                         dplyr::distinct()) %>%
      # Passa Meeting para primeira coluna
      dplyr::mutate(meeting = as.character(meeting)) %>%
      dplyr::relocate(where(is.numeric), 
                      .after = where(is.character)) %>%
      dplyr::mutate(meeting = as.numeric(meeting))
  
    #---------------------------------------------------------------------
    # Base de testes
  
    # Cria uma matriz de embedding
    embed.mtx.test <- tidy_glove %>%
                        dplyr::inner_join(by = 'item1',
                                          data.test %>%
                                            distinct(word) %>%
                                            rename(item1 = word)
                                          ) %>%
                        tidytext::cast_sparse(item1, dimension, value)
    
    # Cria uma matriz esparsa de termos
    wmtx.test  <- data.test %>%
                  dplyr::inner_join(by = 'word',
                                    tidy_glove %>%
                                      distinct(item1) %>%
                                      rename(word = item1)
                                    ) %>%
                  dplyr::count(meeting, word) %>%
                  tidytext::cast_sparse(meeting, word, n)
    
    # Gera matriz de embedding
    dmtx.test <- wmtx.test %*% embed.mtx.test
    
    # Limpa memoria utilizada
    rm(list=c('wmtx.test', 'embed.mtx.test', 'tidy_glove'))
    
    # Matriz de testes
    test.df <- dmtx.test %>%
      as.matrix() %>%
      as.data.frame() %>%
      dplyr::mutate(meeting = as.double(rownames(.))) %>%
      dplyr::left_join(by = 'meeting', 
                       df %>%
                         dplyr::select(meeting, selic.var, selic.next.var) %>%
                         dplyr::distinct()) %>%
      # Passa Meeting para primeira coluna
      dplyr::mutate(meeting = as.character(meeting)) %>%
      dplyr::relocate(where(is.numeric), .after = where(is.character)) %>%
      dplyr::mutate(meeting = as.numeric(meeting))
    
    #---------------------------------------------------------------------
    # Adiciona previoes por series temporais como covariaveis
    
    if (considera.var.atual != TRUE)
    {
      test.df <- test.df %>% select(-selic.var)
      train.df <- train.df %>% select(-selic.var)
    }
    
    if (considera.ts == TRUE) 
    {
      
      previsoes.ts <- selic.df[3:(nrow(selic.df) - 1)] %>%
        as.data.frame() %>%
        mutate(date = index(selic.df[3:(nrow(selic.df) - 1)]), 
               y_hat_ts = y_hat) %>%
        select(date, y_hat_ts) %>%
        left_join(df %>%
                    select(meeting, date) %>%
                    unique(),
                  by='date') %>%
        select(meeting, y_hat_ts)
      
      test.df.y_ts <- test.df %>%
        left_join(previsoes.ts, by='meeting') %>%
        select(y_hat_ts)
      
      test.df.y_ts[is.na(test.df.y_ts)] <- 0
      
      train.df.y_ts <- train.df %>%
        left_join(previsoes.ts, by='meeting') %>%
        select(y_hat_ts)
      
    }
    
    #---------------------------------------------------------------------
    # Escala os dados com base na base de treino
    
    train.mean <- train.df %>% 
                  select(-meeting, -selic.next.var) %>% 
                  sapply(mean)
    
    train.std <- train.df %>% 
                  select(-meeting, -selic.next.var) %>% 
                  sapply(sd)
    
    X.train <- scale(train.df %>% select(-meeting, -selic.next.var), 
                     center = train.mean, scale = train.std)
    y.train <- train.df$selic.next.var
    
    X.test <- scale(test.df %>% select(-meeting, -selic.next.var), 
                    center = train.mean, scale = train.std)
    y.test <- test.df$selic.next.var
    
    
    
    if (considera.ts == TRUE)
    {
      
      # Evita padronizar os dados preditos
      
      X.train <- X.train %>%
                  as.data.frame() %>%
                  mutate(y_hat_ts = train.df.y_ts[, 'y_hat_ts']) %>%
                  as.matrix()
      
      X.test <- X.test %>%
                as.data.frame() %>%
                mutate(y_hat_ts = test.df.y_ts[, 'y_hat_ts']) %>%
                as.matrix()
      
    }
    
    # Limpa memoria utilizada
    rm(list=c('train.df', 'test.df'))
    
  #-----------------------------------------------------------------------
  # Tunning do modelo
  
  gridSearchSVR <- function(params, X.train, y.train, X.test, y.test)
  {
    
    resultados <- numeric(nrow(params))
    best.mae <- 0.0
    
    for (i in 1:nrow(params))
    {
      
      # Kernel linear
      svm <- NA
      svm <- kernlab::ksvm(x = X.train, 
                           y = y.train, 
                           type='eps-svr',
                           cross=10,
                           kernel=as.character(params$kernel[i]),
                           kpar='automatic',
                           epsilon=params$epsilon[i], 
                           cost=params$C[i]
      )
      
      # Prediz valor
      y.test.hat <- predict(svm, X.test)
      y.test.hat <- round(y.test.hat / 0.25, 0) * 0.25
      
      # Metricas
      metricas <- calcular_metricas(y.test, y.test.hat)
      mae <- metricas[['MAE']]
      rmse <- metricas[['RMSE']]
      
      # Para calcular MAE inicial
      if (best.mae == 0) best.mae <- rmse
      
      # Guarda o melhor modelo
      if (mae <= best.mae) {
        best.mae <- mae
        best.rmse <- rmse
        best.model <- svm
      }
      
      # Guarda resultado
      resultados[i] <- mae
      
    }
    
    return(list(
      'best.model'=best.model, 
      'best.RMSE'=best.rmse,
      'best.MAE'=best.mae,
      'params.MAE'=resultados
    ))
    
  }
    
  kernels_params <- expand.grid(
    kernel = c('rbfdot', 
               'polydot', 
               'vanilladot', 
               'splinedot', 
               'laplacedot',
               'anovadot'),
    epsilon = 0.45, 
    C = 12
  )
  
  # First find the best kernel
  kernelGS <- gridSearchSVR(params = kernels_params, 
                           X.train = X.train, 
                           y.train = y.train, 
                           X.test = X.test, 
                           y.test = y.test)
  
  # Escolhe o melhor kernel
  best.kernel <- kernels_params$kernel[
    kernelGS$params.MAE == min(kernelGS$params.MAE)
    ]
  
  best.kernel <- as.character(best.kernel)
  
  # Monta grid de demais parametros
  params <- expand.grid(
    kernel = best.kernel,
    epsilon = sample(1:100, 3) / 100, 
    C = sample(1:1000, 3) / 100
  )
  
  # Tuna parametros do SVR
  bestOfBests <- gridSearchSVR(params = params, 
                               X.train = X.train, 
                               y.train = y.train, 
                               X.test = X.test, 
                               y.test = y.test)
  
  best.model <- bestOfBests$best.model
  
  # Calcula o predito com SVR
  svr.y.test.hat <- predict(best.model, X.test)
  svr.y.test.hat <- round(svr.y.test.hat / 0.25, 0) * 0.25
  
  svr.metricas <- calcular_metricas(y.test, svr.y.test.hat)
  svr.metricas$kernel <- best.kernel
  
  # Mostra o vetor resultado
  par(family = 'Arial')
  plot(y.test, type = 'l', col='gray', lwd=3)
  lines(svr.y.test.hat, type = 'l', col='red', lwd=3)
  #svr.metricas$plot <- recordPlot()

  print('Sucesso no SVR', i)
  
  #-----------------------------------------------------------------------
  # Tenta por XGBoost
  
  set.seed(2002)
  
  # Monta o tipo de dados necessario
  xgb.df.train <- xgboost::xgb.DMatrix(
                    data = X.train[1:(length(y.train)-1), ], 
                    label = y.train[1:(length(y.train)-1)])
  
  xgb.df.test <- xgboost::xgb.DMatrix(
                    data = X.test, 
                    label = y.test)
  
  # Parametros
  xgb.params <- list(
    objective = "reg:squarederror",
    eval_metric = "mae",
    eta = 0.3,
    max_depth = 6,
    subsample = 0.2,
    colsample_bytree = 0.7
  )
  
  # Treina o modelo
  xgb_model <- xgb.train(
    params = xgb.params,
    data = xgb.df.train,
    nrounds = 1000,
    watchlist = list(train = xgb.df.train, test = xgb.df.test),
    early_stopping_rounds = 200
  )
  
  xgb.y.test.hat <- predict(xgb_model, xgb.df.test)
  xgb.y.test.hat <- round(xgb.y.test.hat / 0.25, 0) * 0.25
  
  xgb.metricas <- calcular_metricas(y.test, matrix(xgb.y.test.hat))
  
  # Mostra o vetor resultado
  par(family = 'Arial')
  plot(y.test, type = 'l', col='gray', lwd=3)
  lines(xgb.y.test.hat, type = 'l', col='red', lwd=3)
  #xgb.metricas$plot <- recordPlot()
  
  print('Sucesso no XGB', i)
  
  resultados_finais[[current_glove]] <- list(
    'SVR' = svr.metricas,
    'XGBOOST' = xgb.metricas
  )

}

# Mostra resultados finais
df.final <- t(as.data.frame(resultados_finais)[1, ])
df.final <- cbind(str_split_fixed(rownames(df.final), "\\.", 3), df.final)
colnames(df.final) <- c('Glove Embedding', 'Modelo', 'Item', 'Valor')
rownames(df.final) <- c()
df.final <- df.final %>%
            as.data.frame() %>%
            pivot_wider(id_cols=c('Glove Embedding', 'Modelo'), 
                        names_from = 'Item', 
                        values_from = 'Valor'
                        )

df.final
print(ts.metricas)


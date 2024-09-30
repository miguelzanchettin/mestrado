
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

gerarBases <- function(dfUnida, consideraTS = T)
{
  
  possibleEmbeddings <- c('6B_D50',
                        '6B_D100',
                        '6B_D200',
                        '6B_D300',
                        '27B_D25',
                        '27B_D50',
                        '27B_D100'
                        #'27B_D200' # Nao consigo processar por falta de RAM
                        )
  
  resultados <- list()

  # Inicia uma barra de progresso
  pb <- txtProgressBar(min     = 0, 
                       max     = length(possibleEmbeddings), 
                       initial = 1,
                       style   = 3, 
                       label   = paste('Estimando embeddings'))
  
  for (indexEmbedding in 1:length(possibleEmbeddings))
  {
    
    # Resgata qual o embedding atual
    currentEmbeddingName <- possibleEmbeddings[indexEmbedding]
    
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
    
    # Remove serie temporal
    if (consideraTS == F)
    {
      dfEmbeddings <- dfEmbeddings %>% select(-selic.var)
    }
    
    # Adiciona a lista
    resultados[[currentEmbeddingName]] <- dfEmbeddings
    
    # Atualiza a barra de progresso
    setTxtProgressBar(pb, indexEmbedding)
    
  }
  
  
  # Fecha barra de progresso
  close(pb)
  
  return(resultados)
  
}

#-------------------------------------------------------------------------
# Executa

basesTratadasComTS <- gerarBases(dfUnida, T)
basesTratadasSemTS <- gerarBases(dfUnida, F)

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


estimarModelos <- function (bases, meetings, pcTreino)
{
  
  # Calcula os limites do loop de treinamento
  loopIni <- as.integer(length(meetings) * pcTreino)
  loopFim <- length(meetings)
  
  # Para receber resultados
  yMatrix <- matrix(0, nrow = (loopFim - loopIni + 1), ncol = 1)
  colnames(yMatrix) <- c('y')
  
  # Resgata possiveis embedings
  possibleEmbeddings <- names(bases)
  
  # Inicia uma barra de progresso
  pb <- txtProgressBar(min     = 0, 
                       max     = length(possibleEmbeddings), 
                       initial = 1,
                       style   = 3, 
                       title   = paste('Estimando embeddings')
                       )
  
  for (indexEmbedding in 1:length(possibleEmbeddings))
  {
    
    # Resgata qual o embedding atual
    currentEmbeddingName <- possibleEmbeddings[indexEmbedding]
    
    # Resgata os embeddings
    dfEmbeddings <- bases[[currentEmbeddingName]]
    
    # Loop da janela de treinamento
    for (i in loopIni:loopFim)
    {
      
      # Coleta as bases
      dfEmbeddingsTrain <- dfEmbeddings %>% 
        filter(meeting %in% meetings[1:(i-1)])
      
      dfEmbeddingsTest  <- dfEmbeddings %>% 
        filter(meeting %in% meetings[i])
      
      # Padroniza dados de treino, 
      # com base nos parametros dos dados de treino
      y.train <- dfEmbeddingsTrain$selic.next.var
      X.train <- padronizarDados(
        extrairDe = dfEmbeddingsTrain %>% 
         select(-meeting, -selic.next.var), 
        aplicarEm = dfEmbeddingsTrain %>% 
         select(-meeting, -selic.next.var)
        ) %>%
        as.array()
      
      # Padroniza dados de teste, 
      # com base nos parametros dos dados de treino
      y.test  <- dfEmbeddingsTest$selic.next.var
      X.test <-  padronizarDados(
        extrairDe = dfEmbeddingsTrain %>% 
          select(-meeting, -selic.next.var), 
        aplicarEm = dfEmbeddingsTest %>% 
          select(-meeting, -selic.next.var)
        ) %>%
        as.array()
      
      # Funcao auxiliar para estimar resultado do modelo
      estimarResultado <- function(model, utilizaDF = F)
      {
        
        if (utilizaDF == T) y_hat <- predict(model, as.data.frame(X.test))
        else y_hat <- predict(model, X.test)
        
        y_hat <- discretizar_intervalo(y_hat)
        y_hat <- as.double(y_hat)
        return(y_hat)
      }
      
      # Funcao auxiliar para adicionar vetor de resultados 
      # estimados do modelo na matriz de resultados
      adicionarModelo <- function(
        matriz, 
        model, 
        nomeModelo, 
        currentEmbeddingName, 
        utilizaDF
      )
      {
        
        # Adiciona o embedding ao nome do modelo
        nomeTratado <- paste(sep = '.', 
                             currentEmbeddingName, 
                             nomeModelo)
        
        # Adiciona vetor na matriz
        matriz <- criarColuna(matriz, nomeTratado)
        
        # Adiciona resultado no vetor
        matriz[(i - loopIni + 1), nomeTratado] <- estimarResultado(
          model, 
          utilizaDF
          )
        
        return(matriz)
        
      }
      
      # Valor correto
      yMatrix <- criarColuna(yMatrix, 'y')
      yMatrix[(i - loopIni + 1), 'y'] <- as.double(y.test)
      
      # SVR Linear
      nomeModelo <- 'svrLinear'
      model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'vanilladot', kpar = list(), type = 'eps-svr')
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = F)
      
      # SVR Polynomial Grau 2
      nomeModelo <- 'svrPoly2degree'
      model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'polydot', kpar = list('degree' = 2), type = 'eps-svr')
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = F)
      
      # SVR Polynomial Grau 3
      nomeModelo <- 'svrPoly3degree'
      model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'polydot', kpar = list('degree' = 3), type = 'eps-svr')
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = F)
      
      # SVR Polynomial Grau 4
      nomeModelo <- 'svrPoly4degree'
      model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'polydot', kpar = list('degree' = 4), type = 'eps-svr')
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = F)
      
      # SVR Polynomial Grau 5
      nomeModelo <- 'svrPoly5degree'
      model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'polydot', kpar = list('degree' = 5), type = 'eps-svr')
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = F)
      
      # SVR Radial Basis Function
      nomeModelo <- 'svrRadial'
      model <- kernlab::ksvm(x = X.train, y = y.train, kernel = 'rbfdot', kpar = list(), type = 'eps-svr')
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = F)
      
      # Random Forest
      nomeModelo <- 'randomForest'
      model <- randomForest(x = X.train, y = y.train)
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = F)
      
      # XGBoost
      nomeModelo <- 'xgBoost'
      model <- xgboost(data = X.train, label = y.train, nrounds = 300, objective = 'reg:squarederror', verbose = F)
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = F)
      
      # Regressao linear multipla
      nomeModelo <- 'linearRegression'
      model <- lm(y ~ ., data=as.data.frame(cbind(X.train, y = y.train)))
      yMatrix <- adicionarModelo(yMatrix, model, nomeModelo, currentEmbeddingName, utilizaDF = T)
      
    }
    
    # Atualiza barra de progresso
    setTxtProgressBar(pb, indexEmbedding)
  
  }
  
  # Encerra a barra de progresso
  close(pb)
  
  # Retorna a matriz
  return(yMatrix)

}

#-------------------------------------------------------------------------
# Executa

ySemTS <- estimarModelos(basesTratadasSemTS, meetings, pcTreino)
yComTS <- estimarModelos(basesTratadasComTS, meetings, pcTreino)

##########################################################################
# Model selection
#-------------------------------------------------------------------------

calcularMetricas <- function(ys)
{
  
  ys = ySemTS
  
  # Define itens para calcular metricas
  modelos <- colnames(ys)
  metricas <- list()
  
  # Calcula metricas
  for (i in 2:length(modelos))
  {
    metricas[[modelos[i]]] <- calcular_metricas(ys[, 1], ys[, i])
  }
  
  # Formata metricas
  metricas <- t(as.data.frame(metricas))
  metricas <- cbind(str_split_fixed(rownames(metricas), "\\.", 3), 
                    metricas)
  metricas <- as.data.frame(metricas)
  
  # Corrige indices
  colnames(metricas) <- c('Embedding', 'Modelo','Metrica', 'Valor')
  rownames(metricas) <- c()
  
  # Gera Data Frame final
  metricas <- metricas %>%
    select(Embedding, Modelo, Metrica, Valor) %>%
    mutate(Valor = as.double(Valor)) %>%
    group_by(Embedding, Modelo, Metrica) %>%
    pivot_wider(names_from = Metrica, 
                values_from = Valor, 
                values_fn = sum) %>% 
    filter(Modelo != 'y')
  
  return(metricas)

}

#-------------------------------------------------------------------------
# Executa

metricasSemTS <- calcularMetricas(ySemTS)
metricasComTS <- calcularMetricas(yComTS)

metricasSemTS %>% arrange(MAE)
metricasComTS %>% arrange(MAE)


##########################################################################
# Graficos de desempenho
#-------------------------------------------------------------------------

mostrarGraficos <- function()
{
  
  # Plota grafico de todos os modelos
  yMatrix %>%
    as.data.frame() %>%
    pivot_longer(cols = colnames(yMatrix), names_to = 'modelo', values_to = 'y') %>%
    group_by(modelo) %>%
    mutate(x = row_number()) %>%
    mutate(tipo = str_split_fixed(modelo, "\\.", 2)[, 2]) %>%
    mutate(modelo = str_split_fixed(modelo, "\\.", 2)[, 1]) %>%
    ungroup() %>%
    filter(tipo == 'semTS' & str_detect(modelo, 'svrPoly') == F) %>%
    ggplot2::ggplot(aes(x = x, y = y)) +
    ggplot2::geom_line(aes(colour = modelo), alpha = 0.3) +
    ggplot2::theme_minimal(base_family = 'Arial') +
    ggplot2::scale_colour_viridis_d(direction = 1) + 
    ggplot2::coord_cartesian(ylim = c(-3, 3))
  
  # Plota grafico dos melhores
  yMatrix %>%
    as.data.frame() %>%
    ggplot2::ggplot(aes(x=meetings[(length(meetings) * pcTreino):length(meetings)])) +
    geom_line(aes(y = y.comTS, colour='Variação Real'), linewidth = 3) +
    geom_line(aes(y = randomForest.semTS, colour='Random Forests (Apenas texto)'), linetype='solid', linewidth = 1) +
    geom_line(aes(y = xgBoost.semTS, colour='XGBOOST (Apenas texto)'), linetype='solid', linewidth = 1) +
    geom_line(aes(y = timeSeries.comTS, colour='Auto Arima (Apenas série temporal)'), linetype='solid', linewidth = 1) +
    ggplot2::theme_minimal(base_family = 'Arial') +
    ggplot2::scale_colour_viridis_d(direction = 1) +
    labs(
      title = 'Desempenho de modelos', 
      subtitle = 'Apurados com janela de aprendizado móvel.\nIniciando com 154 e indo até 219 observações e prevendo um período a frente.\nEmbedding com 300 dimensões.',
      x = 'Reunião Nº', y = 'Variação % na SELIC decidida na reunião seguinte'
    )

}

#mostrarGraficos()

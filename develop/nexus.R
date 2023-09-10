nex_datasource <- function(name) {
  obj <- dal_base()
  obj$name <- name
  class(obj) <- append("nex_datasource", class(obj))
  return(obj)
}


getnext <- function(obj, ...) {
  UseMethod("getnext")
}

getnext.default <- function(obj) {
  return(obj)
}

fetch <- function(obj, ...) {
  UseMethod("fetch")
}

fetch.default <- function(obj) {
  return(NA)
}

nex_simulated_datasource <- function(name, serie) {
  obj <- nex_datasource(name)
  obj$pos <- 0
  obj$serie <- serie
  obj$name <- name
  class(obj) <- append("nex_simulated_datasource", class(obj))
  return(obj)
}

getnext.nex_simulated_datasource <- function(obj) {
  result <- NULL
  if (obj$pos < length(obj$serie)) {
    obj$pos <- obj$pos + 1
    result <- obj
  }
  return(result)
}

fetch.nex_simulated_datasource <- function(obj) {
  return(result <- obj$serie[obj$pos])
}


nexus <- function(datasource, detector, warm_size = 30, batch_size = 30, mem_batches = 0) {
  obj <- dal_base()
  obj$detection <- NULL
  obj$stable_detection <- NULL
  obj$datasource <- datasource
  obj$detector <- detector
  obj$warm_size <- warm_size
  obj$batch_size <- batch_size
  obj$mem_size <- batch_size * mem_batches
  obj$base_pos <- 1
  obj$serie <- NULL
  obj$stable_serie <- NULL
  class(obj) <- append("nexus", class(obj))
  return(obj)
}


warmup <- function(obj, ...) {
  UseMethod("warmup")
}

warmup.default <- function(obj) {
  return(obj)
}

warmup.nexus <- function(obj) {
  while (length(obj$serie) < obj$warm_size - 1) {
    obj$datasource <- getnext(obj$datasource)
    if (is.null(obj$datasource))
      break
    obj$serie <- c(obj$serie, fetch(obj$datasource))
  }
  return(obj)
}

adjust_memory <- function(obj) {
  if (obj$mem_size == 0)
    return(obj) # full memory
  n <- length(obj$serie)
  if (n >= obj$mem_size) {
    obj$stable_detection <-rbind(obj$stable_detection, obj$detection[1:obj$batch_size, ])
    obj$stable_detection$idx <- 1:nrow(obj$stable_detection)
    obj$stable_serie <- c(obj$stable_serie, obj$serie[1:obj$batch_size])
    obj$serie <- obj$serie[(obj$batch_size+1):n]
  }
  return(obj)
}

detect.nexus <- function(obj) {
  obj$datasource <- getnext(obj$datasource)
  if (!is.null(obj$datasource)) {
    obj$serie <- c(obj$serie, fetch(obj$datasource))
    if ((length(obj$serie) %% obj$batch_size) == 0) {
      obj$detector <- fit(obj$detector, obj$serie)
      obj <- adjust_memory(obj)
    }
    idxref <- 0
    if (!is.null(obj$stable_detection))
      idxref <- nrow(obj$stable_detection)
    
    detection <- detect(obj$detector, obj$serie)
    
    detection$idx <- detection$idx + idxref
    detection$event <- as.integer(detection$event)

    obj$detection <- rbind(obj$stable_detection, detection)
  }
  return(obj)
}

# Run Nexus ---------------------------------------------------------------
run_nexus <- function(model, data, warm_size = 30, batch_size = 30, mem_batches = 0) {
  #Create auxiliary batch and slide counters
  bt_num <- 1
  sld_bt <- 1
  exec_time <- c()
  ef_start <- FALSE
  
  datasource <- nex_simulated_datasource("data", data$series)
  online_detector <- nexus(datasource, model, warm_size = warm_size, batch_size = batch_size, mem_batches = mem_batches)
  online_detector <- warmup(online_detector)
  
  #Sliding batches through series
  start_time <- Sys.time()
  while (!is.null(online_detector$datasource)) {
    online_detector <- detect(online_detector)
    #Update batch and slide counters
    print(paste("Current position:", sld_bt+warm_size))
    sld_bt <- sld_bt + 1
    
    if (sld_bt %% batch_size == 0) {
      exec_time <- append(exec_time, as.numeric(difftime(Sys.time(),start_time, units = "secs")))
      if (!ef_start) {
        #First batch annotation
        parc <- online_detector$detection[which(online_detector$detection$event == 1),]
        if(nrow(parc) != 0) {
          parc$ef <- 1
          parc$fdb <- 0
          parc$fdb[which(parc$ef == 1)] <- bt_num
          ef_start <- TRUE
        }
      } else {
        temp <- online_detector$detection[which(online_detector$detection$event == 1),]
        if (!is.null(temp)) {
          #Event frequency counter
          parc <- merge(temp, parc, all = TRUE)
          parc$ef[is.na(parc$ef)] <- 0
          parc$ef[which(parc$event == 1)] <- parc$ef[which(parc$event == 1)] + 1
          #First batch annotation
          parc$fdb[is.na(parc$fdb)] <- 0
          parc$fdb[which(parc$ef == 1)] <- bt_num
        }
      }
      #Batch number update
      bt_num <- bt_num + 1
    }
    #Print partial results
    print("Results:")
    print(table(online_detector$detection$event))
    print("--------------------------")
    print(paste("Batch:", bt_num))
    print("==========================")
  }
  
  #Last batch execution time record
  exec_time <- append(exec_time, as.numeric(difftime(Sys.time(),start_time, units = "secs")))
  #Last batch update
  temp <- online_detector$detection[which(online_detector$detection$event == 1),]
  if (!is.null(temp)) {
    parc <- merge(temp, parc, all = TRUE)
    parc$ef[is.na(parc$ef)] <- 0
    parc$ef[which(parc$event == 1)] <- parc$ef[which(parc$event == 1)] + 1
  }
  #First batch annotation
  parc$fdb[is.na(parc$fdb)] <- 0
  parc$fdb[which(parc$ef == 1)] <- bt_num
  #Batch frequency of a time series point t
  parc$bf <- ceiling((nrow(data)/batch_size)) - floor(parc$idx/(batch_size))
  #Event probability of a time series point t
  parc$pe <- parc$ef / parc$bf
  online_detector$prob <- parc[,c(1,5,4,6,7)]
  online_detector$time <- exec_time
  return(online_detector)
}
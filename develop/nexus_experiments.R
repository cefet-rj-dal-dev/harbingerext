#############################################################################
## Integration between Nexus + Harbinger + DAL Events
#############################################################################
# install.packages("devtools")
library(devtools)


# Install and load Harbinger, Nexus and DalEvents -----------------------------
devtools::install_github("cefet-rj-dal/daltoolbox", force=TRUE, dependencies=FALSE, upgrade="never", build_vignettes = TRUE)
devtools::install_github("cefet-rj-dal/harbinger", force=TRUE, dependencies=FALSE, upgrade="never", build_vignettes = TRUE)
devtools::install_github("cefet-rj-dal/event_datasets", force = TRUE, dep=FALSE, upgrade="never")


# Load packages
library(daltoolbox)
library(harbinger)
source("https://raw.githubusercontent.com/cefet-rj-dal/harbingerext/main/develop/nexus.R")
library(dalevents)


# Load dataset ------------------------------------------------------------
#Select desired series and time interval

##==== 
#ph variable
#Interval of a day with anomalies
data(gecco)
data <- subset(gecco$gecco[16500:18000,], select = c(ph, event))
data <- data[1:250,] #Use it only for fast test

##==== 
###==== Finance - Oil brent prices ===
data(fi_br)
data <- subset(fi_br$Commodity, select = c(`Oil Brent`, Event))

##==== 
#Adjust variables names
names(data) <- c("series", "event")


# Nexus -------------------------------------------------------------------
# Run Nexus ---------------------------------------------------------------
run_nexus <- function(model, data, warm_size = 30, batch_size = 30, mem_batches = 0, png_folder="dev/plots/") {
  #Create auxiliary batch and slide counters
  bt_num <- 1
  sld_bt <- 1
  ef_start <- FALSE
  
  #Prepare data to experiment
  datasource <- nex_simulated_datasource("data", data$series)
  online_detector <- nexus(datasource, model, warm_size = warm_size, batch_size = batch_size, mem_batches = mem_batches)
  online_detector <- warmup(online_detector)
  
  #Sliding batches through series
  while (!is.null(online_detector$datasource)) {
    online_detector <- detect(online_detector)
    
    #Update batch and slide counters
    print(paste("Current position:", sld_bt+warm_size))
    sld_bt <- sld_bt + 1
    
    if (sld_bt %% batch_size == 0) {
      
      if (!ef_start) {
        parc <- online_detector$detection[which(online_detector$detection$event == 1),]
        parc$ef <- 1
        ef_start <- TRUE
      } else {
        temp <- online_detector$detection[which(online_detector$detection$event == 1),]
        if (!is.null(temp)) {
          parc <- merge(temp, parc, all = TRUE)
          parc$ef[is.na(parc$ef)] <- 0
          parc$ef[which(parc$event == 1)] <- parc$ef[which(parc$event == 1)] + 1
        }
        print('...')
      }
      bt_num <- bt_num + 1
    }
    
    #Print partial results
    print("Results:")
    print(table(online_detector$detection$event))
    print("--------------------------")
    print(paste("Batch:", bt_num))
    print("==========================")
    
  }
  
  ##EVENT PROBABILITY
  #Last batch update
  temp <- online_detector$detection[which(online_detector$detection$event == 1),]
  if (!is.null(temp)) {
    parc <- merge(temp, parc, all = TRUE)
    parc$ef[is.na(parc$ef)] <- 0
    parc$ef[which(parc$event == 1)] <- parc$ef[which(parc$event == 1)] + 1
  }
  
  #Batch frequency of a time series point t
  parc$bf <- ceiling((nrow(data)/batch_size)) - floor(parc$idx/(batch_size))
  
  #Event probability of a time series point t
  parc$pe <- parc$ef / parc$bf
  
  online_detector$prob <- parc
  return(online_detector)
}


#Create and setup objects
bt_size <- c(1,3,9,27,81)
wm_size <- c(1,3,9,27,81)

# establishing method
#FBI-AD - ARIMA - CF - LSTM? - RN com regressão
#https://nbviewer.org/github/cefet-rj-dal/harbinger-examples/blob/main/anomalies/hanr_ml_lstm.ipynb
model <- hanr_fbiad() #FBI-AD
model <- hanr_arima() #ARIMA
model <- hcp_cf_arima() #CF using Arima - PROBLEM
model <- hcp_cf_lr() #CF using Linear Regression - PROBLEM
model <- hanr_garch() #GARCH
model <- hanr_ml() #ML Linear Regression

library("reticulate")
source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox-examples/main/ts_tlstm.R")
reticulate::source_python("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox-examples/main/ts_tlstm.py")
model <- hanr_ml(ts_tlstm(ts_gminmax(), input_size=4, epochs=10000)) #LSTM

## --------------------------------------------------------
result <- run_nexus(model=model, data=data, warm_size=wm_size[5], batch_size=bt_size[5], mem_batches=0, png_folder="dev/plots/")
View(result$detection)
View(result$prob)

prob <- result$prob
save(prob, file = "~/janio/harbinger/dev/prob_ph_81.RData")


#Filter by limit
plim = 0.9999
prob_lim <- subset(prob, pe > plim)

det_prob <- result$detection
det_prob$event <- 0
det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1

View(det_prob)

#Sum of events
sum(result$detection$event)
sum(det_prob$event)

#Head of detections
ev_idx <- which(result$detection$event == 1)
head(result$detection[ev_idx,])
head(result$detection[-ev_idx,])


# evaluating the detections
evaluation <- evaluate(result$detector,
                       result$detection$event,
                       data$event)

print(evaluation$confMatrix)


#Evaluate limit query detection
print(evaluate(result$detector, det_prob$event, data$event)$confMatrix)



# plotting the results
grf <- har_plot(result$detector, data$series, result$detection, data$event)
plot(grf)


# plotting limit query detection
grf_lim <- har_plot(result$detector, data$series, det_prob, data$event)
plot(grf_lim)

#############################################################################
## Integration between Nexus + Harbinger + DAL Events
#############################################################################
# install.packages("devtools")
library(devtools)


# Install and load Harbinger, Nexus and DalEvents -----------------------------
#devtools::install_github("cefet-rj-dal/daltoolbox", force=TRUE, dependencies=FALSE, upgrade="never", build_vignettes = TRUE)
#devtools::install_github("cefet-rj-dal/harbinger", force=TRUE, dependencies=FALSE, upgrade="never", build_vignettes = TRUE)
#devtools::install_github("cefet-rj-dal/event_datasets", force = TRUE, dep=FALSE, upgrade="never")


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

# establishing method
#FBI-AD - ARIMA - CF - LSTM? - RN com regressão
#https://nbviewer.org/github/cefet-rj-dal/harbinger-examples/blob/main/anomalies/hanr_ml_lstm.ipynb
model <- hanr_fbiad() #FBI-AD
model <- hanr_arima() #ARIMA
model <- hcp_cf_arima() #CF using Arima - PROBLEM
model <- hcp_cf_lr() #CF using Linear Regression - PROBLEM
model <- hanr_garch() #GARCH
model <- hanr_ml() #ML Linear Regression


source("https://raw.githubusercontent.com/cefet-rj-dal/harbinger-examples/main/jupyter_harbinger.R")
load_harbinger()
library("reticulate")
source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox-examples/main/jupyter_daltoolbox.R")
source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox-examples/main/ts_lstm.R")
reticulate::source_python("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox-examples/main/ts_lstm.py")

model <- hanr_ml(ts_lstm(ts_norm_gminmax(), input_size=4, epochs=10000)) #LSTM - PROBLEM


## --------------------------------------------------------
# Run Experiments

#Create and setup objects
bt_size <- c(1,3,9,27,81)
wm_size <- c(1,3,9,27,81)

result <- run_nexus(model=model, data=data, warm_size=wm_size[5], batch_size=bt_size[5], mem_batches=0, png_folder="dev/plots/")

#View results
View(result$detection)
View(result$prob)



#Execution time analysis
plot(result$time,
     xlab = "Batch",
     ylab = "Exec. Time (s)",
     type = "l",
     main = "Accumulated Execution Time")

time_per_batch <- diff(result$time)

plot(time_per_batch,
     xlab = "Batch",
     ylab = "Exec. Time (s)",
     type = "l",
     main = "Execution Time per Batch")

lines(x = 1:length(time_per_batch), y = rep(mean(time_per_batch), length(time_per_batch)),
      lty = 2, #Tipo de linha
      lwd = 1, #Tipo de linha
      col="red")

#Adição de legendas
legend(x = "topleft",                 #Posição da legenda
       legend = "Average time per batch",   #Legenda
       lty = 2, lwd = 1,              #Configurações do símbolo (neste caso linha)
       bty = "n",                     #Caixa ao redor da legenda "n" = nenhuma
       col="red")



prob <- result$prob
#save(prob, file = "~/janio/harbinger/dev/prob_ph_81.RData")

#Filter by limit
plim = 0.5
prob_lim <- subset(prob, pe > plim)

det_prob <- result$detection
det_prob$event <- 0
det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1

View(det_prob)

#Sum of events
sum(result$detection$event)
sum(det_prob$event)


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

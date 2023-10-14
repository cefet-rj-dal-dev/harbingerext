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

#ph variable
#Interval of a day with anomalies
data(gecco)
data <- subset(gecco$gecco[16500:18000,], select = c(ph, event))

#Adjust variables names
  #"series" refers to the time series values
  #"event" refers to the event labels
  #the line index refers to the position of the analyzed point
names(data) <- c("series", "event")

head(data)
head(data[data$event==1,])

# Nexus -------------------------------------------------------------------
# Run Nexus ---------------------------------------------------------------

# Establishing method
model <- hanr_fbiad() #FBI-AD
#model <- hcp_cf_lr() #CF using Linear Regression


# Run Experiments ---------------------------------------------------------
#Create and setup objects
bt_size <- c(1,3,9,27,81)
wm_size <- c(1,3,9,27,81)
mem_bt <- c(0,3,9)

#Choose the model in lines 38-39 before running the experiment
result <- run_nexus(model=model, data=data, warm_size=wm_size[5], batch_size=bt_size[5], mem_batches=mem_bt[1])


# View Stream Detections --------------------------------------------------
head(result$detection)


# View Stream Results ----------------------------------------------------
#Event Probability
head(result$prob)

#Detection lag to t=36
lag_evaluate(pos=36, nexus_result=result, reference = data$event)


# View Detections Metrics -------------------------------------------------
# evaluating the detections

evaluation <- evaluate(result$detector,
                       result$detection$event,
                       data$event)
#Confusion matrix
print(evaluation$confMatrix)

#Metrics Example
evaluation$accuracy
evaluation$F1


# Visual Analysis ---------------------------------------------------------
# plotting the results
grf <- har_plot(result$detector, data$series, result$detection, data$event)
plot(grf)


# Probability comparison --------------------------------------------------
#Recover probabilites from Stream Results
prob <- result$prob

#Filter by limit
plim = 0.8
prob_lim <- subset(prob, pe > plim)

det_prob <- result$detection
det_prob$event <- 0
det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1

View(det_prob)

#Sum of events
sum(result$detection$event)
sum(det_prob$event)

#Evaluate limit query detection
print(evaluate(result$detector, det_prob$event, data$event)$confMatrix)

# plotting limit query detection
grf_lim <- har_plot(result$detector, data$series, det_prob, data$event)
plot(grf_lim)


# Execution time analysis -------------------------------------------------
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


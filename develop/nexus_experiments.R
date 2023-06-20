#############################################################################
## Integration between Nexus + Harbinger + DALEvents
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
#source("https://raw.githubusercontent.com/cefet-rj-dal/harbingerext/main/develop/nexus.R")
source("E://Users//janio//Documents//Education//Mestrado e Doutorado//CEFET//2. Pesquisa//DAL_Events//harbingerNimbus//nexus.R")
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

##Under development
#Probability of the observation being an event
#detection$ef <- 0 #Events frequency of a series point
#detection$bf <- 0 #Batch frequency of a series point
#detection$pe <- detection$ef / detection$bf

##Under development
#Detection temporal lag
#detection$first_batch <- 0 #First batch where the event was detected
#detection$last_batch <- 0 #Last batch where the event was detected

# Run Nexus ---------------------------------------------------------------
run_nexus <- function(model, data, warm_size = 30, batch_size = 30, mem_batches = 0, png_folder="dev/plots/") {
  #require(tibble)
  
  #Empty list to store results across batches
  hist <- list()
  
  #Create auxiliary batch and slide counters
  bt_num <- 1
  sld_bt <- 1
  
  #Prepare data to experiment
  datasource <- nex_simulated_datasource("data", data$series)
  online_detector <- nexus(datasource, model, warm_size = warm_size, batch_size = batch_size, mem_batches = mem_batches)
  online_detector <- warmup(online_detector)
  
  #Sliding batches through series
  while (!is.null(online_detector$datasource)) {
    #Online detection
    online_detector <- detect(online_detector)
    
    if (sld_bt == 1) {
      ef <- 0
    }
    
    #Update batch and slide counters and historical results
    print(paste("Current position:", sld_bt+warm_size))
    sld_bt <- sld_bt + 1
    
    if (sld_bt %% batch_size == 0) {
      #Store result metrics parameters across batches - Historic
      hist_sld <- tibble::tibble(batch = bt_num,
                         idx = online_detector$detection$idx,
                         ev = online_detector$detection$event)
      
      hist[[bt_num]] <- hist_sld
      
      bt_num <- bt_num + 1
    }
    
    #Print partial results
    print("Results:")
    print(table(online_detector$detection$event))
    print("--------------------------")
    print(paste("Batch:", bt_num))
    print("==========================")
    
  }
  
  #Adding last batch results
  hist_sld <- tibble::tibble(batch = bt_num,
                     idx = online_detector$detection$idx,
                     ev = online_detector$detection$event)
  
  hist[[bt_num]] <- hist_sld
  
  online_detector$detection$bf <- ceiling((nrow(data)/batch_size)) - floor(online_detector$detection$idx/(batch_size)) #DONE
  
  ev_idx <- which(online_detector$detection$event == 1) #Event indexes
  
  online_detector$detection$ef <- 0
  online_detector$detection[ev_idx,]$ef <- ceiling((nrow(data)/batch_size)) - floor(online_detector$detection[ev_idx,]$idx/(batch_size)) ##CORRIGIR
  
  online_detector$detection$pe <- online_detector$detection$ef / online_detector$detection$bf#DONE
  
  online_detector$hist <- hist
  return(online_detector)
}


#Create and setup objects
bt_size <- 100
wm_size <- 30


# establishing method
model <- hanr_fbiad()


result <- run_nexus(model=model, data=data, warm_size=wm_size, batch_size=bt_size, mem_batches=0, png_folder="dev/plots/")
View(result$detection)

#Sum of events
sum(result$detection$event)

#Head of detections
ev_idx <- which(result$detection$event == 1)
head(result$detection[ev_idx,])
head(result$detection[-ev_idx,])


# evaluating the detections
evaluation <- evaluate(result$detector,
                       result$detection$event,
                       data$event)

print(evaluation$confMatrix)


# ploting the results
grf <- har_plot(result$detector, data$series,
                      result$detection, data$event)
plot(grf)



#View results across batches (example using Gecco Challenge complete ph series)
View(result$hist[[1]])
View(result$hist[[length(result$hist)/3]])
View(result$hist[[length(result$hist)/2]])
View(result$hist[[length(result$hist)]])

out_file <- "result_exp2.RData"
save(result, file = out_file, compress = TRUE)


merged_result <- Reduce(
  function(x, y, ...) merge(x, y, by = "idx", all = TRUE, ...),
  result$hist
)

View(merged_result)

out_file <- "merged_result_exp2.RData"
save(merged_result, file = out_file, compress = TRUE)
 
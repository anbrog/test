#main script making main results
#first part imports data
#then data manipulation
#then create results. First main and then robustness and variations
#then print results ie make tables and figures

#start needed libraries
library(RPostgres)
library(ggplot2)
#install.packages("EventStudy") Maybe this one is worth trying too to check
#library(eventstudies)
#devtools::install_github("nipfpmf/eventstudies", ref="v1.2")
# Looks like I have to do it myself now or use EventStudy package
#library(tidyverse)
library(readxl)
library(dplyr) #part of tidyverse used eg for filtering nad pipe (?)
library(tibble)
library(EventStudy)
library(zoo)

# Initiate functions
getData <- function () {
  #log in to server
  wrds <- dbConnect(Postgres(),
                    host='wrds-pgdata.wharton.upenn.edu',
                    port=9737,
                    dbname='wrds',
                    sslmode='require',
                    user = 'anbr',
                    password = 'Hvilkenkode1'
  )
  
  # establish connection and make data demand
  res <- dbSendQuery(wrds, "select a.gvkeyx, a.prccd, a.datadate, 
                   b.indexgeo, b.indexval, b.indextype, b.indexid, b.spii, b.conm
                   from comp.g_idx_daily a join comp.g_idx_index b
                   on a.gvkeyx = b.gvkeyx
                   where a.datadate between '2012-12-12'
                   and '2019-12-30'")
  # collect data from connection
  data <- dbFetch(res, n = -1)
  # release connection
  dbClearResult(res)
  #data
  
  #save data
  file <- "data_from_function_wrds/compustat.csv"
  write.csv(data, file)
  
  # fil <- tempfile("compustat", fileext = ".rds")
  # saveRDS(data,fil)
  #test <- readRDS(fil)
  result <- read.csv(file)
  return(result)
}

# Data collection
# NB can also change function above to not return the data and run silently.
data_compustat <- getData()

# Set up data
# NB make into function later or now..
makeData <- function() {
  fileComp <- "data_from_function_wrds/compustat.csv"
  compustat <- read.csv(fileComp)
  fileEvents <- "data/EventDates.xlsx"
  events <- read_excel(fileEvents, sheet = "Large changes >=0.5")
  events <- events[,-c(3)]
  events$date <- as.Date(events$date)
  events <- as.data.frame(events)
  names(events)[1] = "name"
  names(events)[2] = "when"
  
  oldDataDailyR <- read_excel("data/DailyCombinedLevels.xlsx", sheet = "Return")
  Returns <- oldDataDailyR
  Returns$Date <- as.Date(oldDataDailyR$Date, format = "%d-%m-%Y")
  #get into % maybe keep in fractions?
  Returns[,-1] = oldDataDailyR[,-1]*100
  eventreturns <- zoo(Returns[,-1],Returns$Date)
  str(eventreturns)
  
  # market model - first get market data
  factors <- read.csv("data/factors.csv")
  mkt = factors
  mkt[2] = factors$Mkt.RF + factors$RF
  mkt[1] = as.Date(strptime(factors$X, "%Y%m%d"))
  mkt = mkt[,-c(3,4,5)]
  names(mkt)[1] = "date"
  names(mkt)[2] = "mkt"
  mkt <- zoo(mkt$mkt, mkt$date)
  
  results <- list(compustat, events, eventreturns, mkt)
  results
  return(results)
}
list <- makeData()
#split list into the two datasets
compustat <- as_tibble(list[[1]])
events <- list[[2]]
events$country <- as_factor(events$country)
#rename countries to match rest ie into 3 char names
events$country <- recode(events$country,
       "Bulgaria" = "BGR",
       "Czech Republic" = "CZE",
       "Denmark" = "DNK",
       "Ireland" = "IRL",
       "Lithuania" = "LTU",
       "Norway" = "NOR",
       "Slovakia" = "SVK",
       "Sweden" = "SWE",
       "United Kingdom" = "GBR",
       )

#filter out any country which we don't have events for
compustat_filterd  <- 
  filter(compustat,
         indexgeo %in% events$country)

#old version
#delete next time I read this
#compustat_filterd  <- 
#  filter(compustat,
#    indexgeo == "BGR" |
#    indexgeo == "CZE" |
#    indexgeo == "DNK" |
#    indexgeo == "IRL" |
#    indexgeo == "LTU" |
#    indexgeo == "NOR" |
#    indexgeo == "SVK" |
#    indexgeo == "SWE" |
#    indexgeo == "GBR"
#      )


# Test data
unique(data_compustat$indexgeo)
# Ie I get 63 different countries

# Do analysis
# plot average event study
# NB can also plot individuals using arPlot
getSP500ExampleFiles()
ls()
checkFiles()




EventSTudy::aarPlot(ResultParserObj, cumSum = F, group = NULL, window = NULL,
        ciStatistics = NULL, p = 0.95, ciType = "band", xlab = "",
        ylab = "Averaged Abnormal Returns", facet = T, ncol = 4)



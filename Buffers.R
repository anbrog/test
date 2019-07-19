#main script making main results
#first part imports data
#then data manipulation
#then create results. First main and then robustness and variations
#then print results ie make tables and figures

#NB Need to get a year more data for norway!
#NB Need to get longer dataseries for market!!
#NB Made inner join between country days in market days

#ASK JDN
#geo return looks off?

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
library(tsibble)

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

#split list into the datasets
compustat <- as_tibble(list[[1]])
events <- list[[2]]
eventreturns <- list[[3]]
mkt <-  list[[4]]


#not used atm
##filter out any country which we don't have events for
#compustat_filterd  <- 
#  filter(compustat,
#         indexgeo %in% events$country)
#
#old
#events$country <- as_factor(events$country)
#rename countries to match rest ie into 3 char names
#events$country <- recode(events$country,
#       "Bulgaria" = "BGR",
#       "Czech Republic" = "CZE",
#       "Denmark" = "DNK",
#       "Ireland" = "IRL",
#       "Lithuania" = "LTU",
#       "Norway" = "NOR",
#       "Slovakia" = "SVK",
#       "Sweden" = "SWE",
#       "United Kingdom" = "GBR",
#       )
#


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


#not used atm
#EventSTudy::aarPlot(ResultParserObj, cumSum = F, group = NULL, window = NULL,
#
#ciStatistics = NULL, p = 0.95, ciType = "band", xlab = "",
#
#ylab = "Averaged Abnormal Returns", facet = T, ncol = 4)
#



#Try and redo old results. ie make my own event study
plot(eventreturns)
plot(mkt)
#plot(compustat)
#tmp <- zoo(compustat$indexgeo,compustat$datadate)
#plot(tmp)

# put into event-time together with market
# estimate betas
# calculate expected return ie beta*market (cumulative)
# do for loop over all events
#eventreturns <- as_tibble(eventreturns)
#eventreturns <- zoo(eventreturns)
#event window to record nb just take all?
L_pre_record = 100
L_post_record = 100 #NB eventwindow is 1 longer than this
#clear plot window to make sure just the next are recorded
dev.off()
#set so views 20 graphs
par(mfrow=c(4,5))
beta <- vector(length = length(events$when))
abnret_out <- matrix(data = NA, nrow = (L_pre_record + L_post_record + 1), ncol =  (length(events$when) + 1) )
abnret_out[,1] = ((0:(L_pre_record + L_post_record)) - L_pre_record)
for (i in 1:length(events$when)) {
  if (i == 9 | i == 12) next
  #get right date and country
  date <- events$when[i]
  country <- events$name[i]
  print(i)
  print(date)
  print(country)
  
  #get correct return-series
  returns <- eventreturns[,country]
  returnsEventTime = time(returns) - date
  mktEventTime = time(mkt) - date

  #make new data format and join in terms of event time
  tbl1 <- tibble(eventTime = returnsEventTime, ret_i = coredata(returns))
  tbl2 <- tibble(eventTime = mktEventTime, ret_m = coredata(mkt))
  data <- full_join(tbl1,tbl2, by = "eventTime")
  
  #estimate beta
  #parameters
  #start with standard for wrds event study
  estimation_window = 365 #change to a year later ie 252 #nb this is actual days #wrds uses 100
  #nb should I intruduce a gap between estimation and event date?
  #use like mackinlay without gap
  data_estimation = filter(data,(data$eventTime %in% -rev(c(1:(estimation_window + 1)))), )
  tmp <- lm(data_estimation$ret_i ~ data_estimation$ret_m)
  beta[i] <- tmp$coefficients[2]
  
  #get abnormal return
  #do for up to a year before even though this doesnt really make sense
  return_abnormal = data$ret_i - beta[i] * data$ret_m
  
  #plot
  #test <- zoo(return_abnormal,data$eventTime + date)
  #test2 <- coredata(test) %>% tibble(ret = .) %>% filter(!is.na(.))
  #test2 <- test2 + 1
  #test2 <- cumprod(test2)
  #test3 <- coredata(test) %>% tibble(ret = .) %>% filter(!is.na(.))
  #test3 <- cumsum(test3)
  #test <- test[!is.na(coredata(test))]
  #plot(test)
  #plot((1:length(test2$ret)),test2$ret,xlab = "time",ylab = "ret")
  #ggplot(test2, aes((1:length(test2$ret)), test2$ret)) + geom_point() + theme_classic() +
  #  xlab("time") + ylab("return") + scale_y_log10()
  #ggplot(test3, aes((1:length(test3$ret)), test3$ret)) + geom_point() + theme_classic() +
  #  xlab("time") + ylab("return")
  ##why is geo return and arith return so different
  ##geo looks weird
  #plot(test[-(1:7000)])
  
  #plot around event
  L_pre = 5
  L_post = 10 #NB eventwindow is 1 longer than this
  abnret = tibble(ret = return_abnormal,eventTime = data$eventTime) %>%
    filter(data$eventTime %in% ((0:(L_pre + L_post)) - L_pre))
  #plot(abnret$eventTime,abnret$ret,xlab = "event time",ylab = "ret")
  #! NB This is not cum returns!
  plot(abnret$eventTime,cumprod(abnret$ret + 1),xlab = "event time",ylab = "ret",main = country)
  
  #output
  eventDates_out = ((0:(L_pre_record + L_post_record)) - L_pre_record)
  for (j in 1:length(eventDates_out) ) {
    eventTime_j = eventDates_out[j]
    abnret_out[j,(i + 1)] = tibble(ret = return_abnormal,eventTime = data$eventTime) %>% 
      filter(data$eventTime == eventTime_j) %>% select(.,ret) %>% as.numeric(.)
  }
}
#save graphs
dev.copy(pdf,"output/eventPlots_first.pdf")
dev.off()


#Calculate mean and do inference
#NB this doesnt seem to work
data <- as_tibble(t(abnret_out))
means <- rowMeans(abnret_out, na.rm = TRUE)
#means <- summarise(data, funs(mean))
plot(abnret_out[90:111,1],means[90:111])
 

# Clear the plot window?
dev.off() 
#main script making main results
#first part imports data
#then data manipulation
#then create results. First main and then robustness and variations
#then print results ie make tables and figures

#NB Need to get a year more data for norway!
#NB Need to get longer dataseries for market!!
#NB Made inner join between country days in market days
#NB Make estimation window smaller? Seems too wrong/long?

#ASK JDN
#geo return looks off?

#start needed libraries
library(RPostgres)
library(ggplot2)
#library(tidyverse)
library(readxl)
library(dplyr) #part of tidyverse used eg for filtering nad pipe (?)
library(tibble)
library(EventStudy) #not used
library(zoo)
library(tsibble)
library(tidyr) #for gather
library(broom) #for tidy on lm (tidy.lm)
library(lubridate) #for function ymd
library(eventstudies)
## Initiate functions
# Getting Fama factors
source("getFactorsDaily.R")
# Data collection
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
  
  # NB change to 2012 later
  # establish connection and make data demand
  res <- dbSendQuery(wrds, "select a.gvkeyx, a.prccd, a.datadate, 
                   b.indexgeo, b.indexval, b.indextype, b.indexid, b.spii, b.conm
                   from comp.g_idx_daily a join comp.g_idx_index b
                   on a.gvkeyx = b.gvkeyx
                   where a.datadate between '2018-12-12'
                   and '2019-12-30'")
  # collect data from connection
  data <- dbFetch(res, n = -1)
  # release connection
  dbClearResult(res)
  #data
  
  #save data
  file <- "data_from_function_wrds/compustat.csv"
  write.csv(data, file)
  
  result <- read.csv(file)
  return(result)
}
# Set up data
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
  
  eventsAllChanges <- read_excel(fileEvents, sheet = "Positive changes")
  eventsAllChanges <- eventsAllChanges[,-c(3)]
  eventsAllChanges$date <- as.Date(eventsAllChanges$date)
  eventsAllChanges <- as.data.frame(eventsAllChanges)
  names(eventsAllChanges)[1] = "name"
  names(eventsAllChanges)[2] = "when"
  
  eventsNoChange <- read_excel(fileEvents, sheet = "zero changes")
  eventsNoChange <- eventsNoChange[,-c(3,4)]
  eventsNoChange$date <- as.Date(eventsNoChange$date)
  eventsNoChange <- as.data.frame(eventsNoChange)
  names(eventsNoChange)[1] = "name"
  names(eventsNoChange)[2] = "when"
  
  
  
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
  
  results <- list(compustat, events, eventsAllChanges, eventsNoChange, eventreturns, mkt)
  results
  return(results)
}
# Collect on individual firms
# NB CAN GET MUCH MORE INFO ON SECTOR! AND NAME! CITY ETC
# Can also get amount traded, high low, etc ie intraday volatility, might be useful!
#nb some are different currencies. can get this data. need?
getFirmData <- function() {
  #log in to server
  wrds <- dbConnect(Postgres(),
                    host='wrds-pgdata.wharton.upenn.edu',
                    port=9737,
                    dbname='wrds',
                    sslmode='require',
                    user = 'anbr',
                    password = 'Hvilkenkode1'
  )
  
  #Make country list
  # first make it work for just 1 country
  countries = list('BGR','CZE','DNK','IRL','LTU','NOR','SVK','SWE','GBR')
  
  # get data
  res <- dbSendQuery(wrds, "SELECT a.gvkey,a.conm,a.loc,a.gsector,
                            a.cik,  a.ggroup, a.gind, a.gsubind, a.naics,
                            a.sic, a.spcindcd, a.spcseccd, a.stko,
                            b.gvkey, b.iid, b.datadate, b.prccd, b.ajexdi,
                            b.curcdd, b.cshoc, b.cshtrd, b.prchd, b.prcld,
                            b. prcstd, b.qunit
                            FROM comp.company a join compg.g_sec_dprc b
                            ON a.gvkey = b.gvkey
                            WHERE b.datadate between '2012-12-12'
                            AND '2019-12-30'
                            AND a.loc IN
                     ('BGR','CZE','DNK','IRL','LTU','NOR','SVK','SWE','GBR')
                            ORDER BY loc, gsector, a.gvkey, conm, iid, datadate, prccd, ajexdi")
  #nb the n=10 limits to 10 rows, to test. Delete this later when SQL correct
  data <- dbFetch(res, n=-1)
  #dbClearResult(res) closes the connection, readying for another query.
  dbClearResult(res)
  #make tibble & only keep stuff I need
  # nb I can get all this stuff!!!
  data_all <- as_tibble(data) %>% arrange(gvkey,conm, iid, loc,gsector,gvkey,datadate)
  
  #save data
  file <- "data_from_function_wrds/compustat_firms.csv"
  write.csv(data_all, file)
  
  result <- read.csv(file)
    return(result)
}


# Reload data from wrds
firms <- getFirmData()

# OR just load from saved file
file <- "data_from_function_wrds/compustat_firms.csv"
firms <- read.csv(file)


# NB can also change function above to not return the data and run silently.
data_compustat <- getData()

#get data using fn earlier defined
list <- makeData()
#split list into the datasets
compustat <- as_tibble(list[[1]])
events <- list[[2]]
eventsAllChanges <- list[[3]]
eventsNoChange <- list[[4]]
eventreturns <- list[[5]]
mkt <-  list[[6]]

###############################################################################
## Try and redo old results. ie make my own event study
# put into event-time together with market
# estimate betas
# calculate expected return ie beta*market (cumulative)
# do for loop over all events
# NB made so can run just this part of the script

# Clear the plot window
dev.off() 
# deletes all objects in workspace except makeData
rm(list=setdiff(ls(), "makeData"))
# Force R to release memory it is no longer using
gc()

# get data again
list <- makeData()
#split list into the datasets
compustat <- as_tibble(list[[1]])
events <- list[[2]]
eventsAllChanges <- list[[3]]
eventsNoChange <- list[[4]]
eventreturns <- list[[5]]
mkt <-  list[[6]]

#clear plot window to make sure just the next are recorded
#dev.off()

#nb I think it works even with errors just not plots but that is fine
doEventStudy <- function(events, eventreturns, mkt, L_pre_record, L_post_record){
  
  #set so views 20 graphs
  par(mfrow = c(4,5))
  #initiate output data objects as na
  beta <- vector(length = length(events$when))
  retabn_out <- matrix(data = NA, nrow = (L_pre_record + L_post_record + 1), ncol =  (length(events$when) + 1) )
  retact_out <- matrix(data = NA, nrow = (L_pre_record + L_post_record + 1), ncol =  (length(events$when) + 1) )
  #make first column the event times
  retabn_out[,1] = ((0:(L_pre_record + L_post_record)) - L_pre_record)
  retact_out[,1] = ((0:(L_pre_record + L_post_record)) - L_pre_record)
  #NB using #can plot either actual or abnormal returns!
  for (i in 1:length(events$when)) {
    flag <- TRUE
    tryCatch({
      #nb right now two observations for norway not working as not data early enough for norway
      #...and not long enough for market ie event 9 and 12.
      #this is ones with errors for large changes (standard)
      #if (i == 9 | i == 12) next
      #this is ones with error for nochanges
      if (i %in% c(13,24,36,75,90,111,124,137,161,190,202,225,288)) next
      #get right date and country
      date <- events$when[i]
      country <- events$name[i]
      print(i)
      print(date)
      print(country)
      
      #get correct return-series
      returns <- eventreturns[,country]
      #make event time for country returns and market returns
      returnsEventTime = time(returns) - date
      mktEventTime = time(mkt) - date
    
      #make new data format and join in terms of event time
      tbl1 <- tibble(eventTime = returnsEventTime, ret_i = coredata(returns))
      tbl2 <- tibble(eventTime = mktEventTime, ret_m = coredata(mkt))
      data <- full_join(tbl1,tbl2, by = "eventTime")
      
      #estimate beta
      #NB Maybe make estimation window smaller. Betas seem wrong?
      #parameters
      #start with standard for wrds event study
      estimation_length = 365 #change to a year later ie 252 #nb this is actual days #wrds uses 100
      estimation_window = -rev(c(1:(estimation_length + 1)))
      #nb should I intruduce a gap between estimation and event date?
      #use like mackinlay without gap
      data_estimation = filter(data,data$eventTime %in% estimation_window)
      beta[i] <- lm(data_estimation$ret_i ~ data_estimation$ret_m) %>% tidy() %>%
        select(estimate) %>% slice(2) %>% as.numeric()
      #[i] <- tmp$coefficients[2]
      
      #get abnormal return
      #do for up to a year before even though this doesnt really make sense
      return_abnormal = data$ret_i - beta[i] * data$ret_m
      #also record actual return
      return_actual = data$ret_i
      
      ## Used before for fun. Not needed anymore. Delete next time I see this
      ##plot around event
      #L_pre = 5
      #L_post = 10 #NB eventwindow is 1 longer than this
      #retabn = tibble(ret = return_abnormal,eventTime = data$eventTime) %>%
      #  filter(data$eventTime %in% ((0:(L_pre + L_post)) - L_pre))
      #retact = tibble(ret = return_actual,eventTime = data$eventTime) %>%
      #  filter(data$eventTime %in% ((0:(L_pre + L_post)) - L_pre))
      ##plot(retabn$eventTime,retabn$ret,xlab = "event time",ylab = "ret")
      ##! NB This is not cum returns!
      ## NB This is cumprod returns plotted
      #plot(retabn$eventTime,cumprod(retabn$ret + 1) - 1,xlab = "event time",ylab = "ret abn",main = country)
      ## try plotting normal returns instead
      ##plot(retact$eventTime,cumprod(retact$ret + 1) - 1,xlab = "event time",ylab = "ret act",main = country)
      
      
      #output
      eventDates_out = ((0:(L_pre_record + L_post_record)) - L_pre_record)
      for (j in 1:length(eventDates_out) ) {
        eventTime_j = eventDates_out[j]
        retabn_out[j,(i + 1)] = tibble(ret = return_abnormal,eventTime = data$eventTime) %>% 
          filter(data$eventTime == eventTime_j) %>% select(ret) %>% as.numeric()
        retact_out[j,(i + 1)] = tibble(ret = return_actual,eventTime = data$eventTime) %>% 
          filter(data$eventTime == eventTime_j) %>% select(ret) %>% as.numeric()
      }
    },
    # ... but if an error occurs, tell me what happened: 
    error = function(error_message) {
      message("This is my custom message.")
      message("And below is the error message from R:")
      message(error_message)
      flag <<- FALSE
    })
    if (!flag) next
  }
  results = list(retabn_out, retact_out)
  return(results)
}
#event window to record nb just take all?
L_pre_record = 60#100
L_post_record = 30#100 #NB eventwindow is 1 longer than this
list <- doEventStudy(events, eventreturns, mkt, L_pre_record, L_post_record)
retabno <- list[[1]]
retactu <- list[[2]]

#try and do eventstudy with nochange events
L_pre_record = 60#100
L_post_record = 30#100 #NB eventwindow is 1 longer than this
list <- doEventStudy(eventsNoChange, eventreturns, mkt, L_pre_record, L_post_record)
retabno <- list[[1]]
retactu <- list[[2]]

## Save results from analysis
#save graphs
dev.copy(pdf,"output/eventPlotsAbnormal_first.pdf")
#dev.copy(pdf,"output/eventPlotsActual_first.pdf")
dev.off()
#save data
data <- retactu
file <- "output_to_input/retctual.csv"
write.csv(data, file)
data <- retabno
file <- "output_to_input/retabnormal.csv"
write.csv(data, file)

###############################################################################
## Start analysis/plotting of data created
# NB should do lines instead of points?

# Clear the plot window
dev.off() 
# If you want to delete all the objects in the workspace and start with a clean slate
rm(list = ls())
# Force R to release memory it is no longer using
gc()
file <- "output_to_input/retctual.csv"
retactu <- read.csv(file)
file <- "output_to_input/retabnormal.csv"
retabno <- read.csv(file) 

##Calculate mean and do inference
#NB this doesnt seem to work

# A. doing for pure returns each do (no sums)
#setup data
# A.1
data <- as_tibble(retactu) %>% gather(key = "event", value = "ret", -V1) %>%
  filter(event != "X")
#plot
#NB divide by 100 as already in percentages
ggplot(data = data) + 
  geom_point(mapping = aes(x = V1, y = ret/100, color = event),na.rm = TRUE) +
  ylab("day to day returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),na.rm = TRUE) +
  ylab("day to day returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
# a bit more stuff added on
ggplot(data = data) + 
geom_smooth(mapping = aes(x = V1, y = ret/100),
            na.rm = TRUE, method = "loess", level = 0.9, span = 0.2,) +
  ylab("day to day returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = mean, shape = 17, size = 3)

# A.2 do for abnormal return
data <- as_tibble(retabno) %>% gather(key = "event", value = "ret", -V1) %>%
  filter(event != "X")
#plot
#NB divide by 100 as already in percentages
ggplot(data = data) + 
  geom_point(mapping = aes(x = V1, y = ret/100, color = event),na.rm = TRUE) +
  ylab("day to day returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),
              na.rm = TRUE, method = "loess", level = 0.9, span = 0.2,) +
  ylab("day to day returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = mean, shape = 17, size = 3)
#use gam method instead
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),
              na.rm = TRUE, method = "auto", level = 0.9) +
  ylab("day to day returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = mean, shape = 17, size = 3)



# B. Do for cumulative returns.
# Try first with just adding
data <- as_tibble(retactu) %>% mutate_at(-c(1,2),~ replace(., is.na(.), 0)) %>% 
  mutate_at(-c(1,2), cumsum) %>% gather(key = "event", value = "ret", -V1) %>%
  filter(event != "X")
#plot
ggplot(data = data) + 
  geom_point(mapping = aes(x = V1, y = ret/100, color = event),na.rm = TRUE) +
  ylab("cum sum actual returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#plot smooth
#NB This is level 0.9!
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),na.rm = TRUE, level = 0.9) +
  ylab("cum sum actual returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#specify model
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),
              na.rm = TRUE, method = "loess", span = 0.1, level = 0.9) +
  ylab("cum sum actual returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

#also do for abnormal returns
data <- as_tibble(retabno) %>% mutate_at(-c(1,2),~ replace(., is.na(.), 0)) %>% 
  mutate_at(-c(1,2), cumsum) %>% gather(key = "event", value = "ret", -V1) %>%
  filter(event != "X")
#plot
ggplot(data = data) + 
  geom_point(mapping = aes(x = V1, y = ret/100, color = event),na.rm = TRUE) +
  ylab("cum sum abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#plot smooth
#NB This is level 0.9!
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),na.rm = TRUE, level = 0.9) +
  ylab("cum sum abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#more advanced version
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),
            na.rm = TRUE, method = "loess", level = 0.9, span = 0.1,) +
  ylab("cum sum abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = mean, shape = 17, size = 3)


# C. ok now do for cumprod returns
#fn needed below
addOne <- function (x) {x + 1}
subOne <- function (x) {x - 1}
# C.1 actual returns
data <- as_tibble(retactu) %>% mutate_at(-c(1,2),~ replace(., is.na(.), 0)) %>% 
  mutate_at(-c(1,2), addOne) %>% mutate_at(-c(1,2), cumprod) %>% 
  mutate_at(-c(1,2), subOne) %>%
  gather(key = "event", value = "ret", -V1) %>% filter(event != "X")
#plot
ggplot(data = data) + 
  geom_point(mapping = aes(x = V1, y = ret/100, color = event),na.rm = TRUE) +
  ylab("cum prod actual returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#plot smooth
#NB This is level 0.9!
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),na.rm = TRUE, level = 0.9) +
  ylab("cum prod actual returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# C.2 abn ret
data <- as_tibble(retabno) %>% mutate_at(-c(1,2),~ replace(., is.na(.), 0)) %>% 
  mutate_at(-c(1,2), addOne) %>% mutate_at(-c(1,2), cumprod) %>% 
  mutate_at(-c(1,2), subOne) %>%
  gather(key = "event", value = "ret", -V1) %>% filter(event != "X")
#plot
ggplot(data = data) + 
  geom_point(mapping = aes(x = V1, y = ret/100, color = event),na.rm = TRUE) +
  ylab("cum prod abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#plot smooth
#NB This is level 0.9!
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = ) +
  ylab("cum prod abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
#more advanced version
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),
              na.rm = TRUE, method = "loess", level = 0.9, span = 0.2,) +
  ylab("cum prod abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = mean, shape = 17, size = 3)

#NB doing a model to get standard errors, is better than just taking quantiles!
# ie maybe because it is estimated from all data
#NBNB geom_smooth uses the loess model which is a fit which uses a polynomial weighting
# depending on difference and then least squares after that
# NB should I use a lower span?
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),na.rm = TRUE, level = 0.9, span = 0.1) +
  ylab("cum prod abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = quantile, 
               fun.args=(list(probs = c(0.10, 0.90))), shape = 17, size = 3)
#try with gam
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),na.rm = TRUE, level = 0.9, method="gam") +
  ylab("cum prod abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = quantile, 
               fun.args=(list(probs = c(0.10, 0.90))), shape = 17, size = 3)
# mean instead of quantiles here
ggplot(data = data) + 
geom_smooth(mapping = aes(x = V1, y = ret/100),na.rm = TRUE, level = 0.9, formula = "y~x") +
  ylab("cum prod abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = mean, shape = 17, size = 3)
# also make points data
ggplot(data = data) + geom_point(mapping = aes(x = V1, y = ret/100, colour = event)) +
  geom_smooth(mapping = aes(x = V1, y = ret/100),na.rm = TRUE, level = 0.9, formula = "y~x") +
  ylab("cum prod abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
# lm instead of lowess here. but lm ofc doesnt make sense
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = V1, y = ret/100),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess",span = 0.1) +
  ylab("cum prod abnormal returns (%)") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  stat_summary(aes(x = V1, y = ret/100), geom = "point", fun.y = mean, shape = 17, size = 3)

###############################################################################
# Compute returns from prices
# Assume prices are adjusted as mentioned on webpage below
# https://wrds-support.wharton.upenn.edu/hc/en-us/articles/115003135651-Is-there-sample-code-to-adjust-prices-and-earnings-for-splits-mergers-etc-

#AH! some firms have two types of shares!
# filter iid = 01W ie get main ones!
# could grp by iid too to get seperate
# Use log or actual returns?
# now use actual

# OR just load from saved file
file <- "data_from_function_wrds/compustat_firms.csv"
firms <- read.csv(file)


head(firms)

# basically the same for daily!
firm_returns <- firms %>% filter(iid == "01W") %>%
  select(datadate,gvkey,prccd,loc) %>%
  rename(date = datadate) %>%
  mutate(date = ymd(parse_date_time(date, "%Y%m%d") ) ) %>%
  group_by(loc,gvkey) %>%
  rename(returns = prccd) %>%
  mutate(returns = (returns/lag(returns) - 1)) %>%
  arrange(gvkey, date)

head(firm_returns)

#NB log returns!
firm_returns_alt_alt <- firms %>% filter(iid == "01W") %>%
  select(datadate,gvkey,prccd,loc) %>%
  rename(date = datadate) %>%
  mutate(date = ymd(parse_date_time(date, "%Y%m%d") ) ) %>%
  group_by(loc,gvkey) %>%
  rename(returns = prccd) %>%
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  arrange(gvkey, date)

head(firm_returns_alt_alt)


firm_returns_alt <- firms %>% filter(iid == "01W") %>%
  select(datadate,gvkey,prccd,loc) %>%
  rename(date = datadate) %>%
              mutate(date = ymd(parse_date_time(date, "%Y%m%d") ) ) %>%
               group_by(loc,gvkey) %>%
  tq_transmute(prccd,mutate_fun = dailyReturn) %>%
  arrange(gvkey, date)


head(firm_returns_alt)


# same! which is great! 
# means these probably are log returns in the first one too!
# not used right now

# find a way to get the prices into a matrix like eventreturns

# copy events so that there comes a row with each gvkey in a country

# get factor(s)
# NB it is an issue that one has a - in the name! makes it much harder
Europe_3_Factors_Daily <- getFactorsDaily("Europe_3_Factors_Daily")
Factors <- Europe_3_Factors_Daily %>%
  mutate('Mkt' = (!!Europe_3_Factors_Daily$'Mkt-RF' + !!Europe_3_Factors_Daily$RF)) %>% 
  select(date,Mkt)
Factors
#Europe_5_Factors_Daily <- factorDaily("Europe_5_Factors_Daily")
#Europe_Mom_Factor_Daily <- factorDaily("Europe_MOM_Factor_Daily")

#now merge
firmdata <- left_join(firm_returns,Factors, by = "date")
firmdata

#now make data so can do event study
# make wide
# NB does not work with country!
firmdata 
wide <- firmdata %>% group_by(gvkey) %>% select(-loc) %>% spread(.,gvkey,returns)
head(wide,n=30)

###############################################################################
# Try and do event study using package again

## Make data so it looks like the old version
# done
## Make events copied so there is one for each gvkey in loc
# ie copy event length(gvkey in loc) times. Then rename to gvkey in loc

# get data again
list <- makeData()
#split list into the datasets
compustat <- as_tibble(list[[1]])
events <- list[[2]]
eventsAllChanges <- list[[3]]
eventsNoChange <- list[[4]]
eventreturns <- list[[5]]
mkt <-  list[[6]]

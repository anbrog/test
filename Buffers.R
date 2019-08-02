#main script making main results
#first part imports data
#then data manipulation
#then create results. First main and then robustness and variations
#then print results ie make tables and figures

#NB Need to get a year more data for norway!
#NB Need to get longer dataseries for market!!
#NB Made inner join between country days in market days
#NB Make estimation window smaller? Seems too wrong/long?
#NB Novo has iid of W02 but no W01 !!
#NB Add eventid at end
#NB TO DO change to business days!

#NB To do. Check if other countries benefit when another country announces?

#ASK JDN
#geo return looks off?

## HEADER ######################################################################
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
library(broom) #for tidy lm
library(tidyquant) #for tq_mutate
#library(tikzDevice) # try to make tikz graphs
## Initiate functions
# Getting Fama factors
source("getFactorsDaily.R")
# Makes index data
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

getCDSData <- function() {
  # NB need to choose tier, docclause and ccy
  # Experiment with different ones later!
  
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
  #countries = list('BGR','CZE','DNK','IRL','LTU','NOR','SVK','SWE','GBR')
  list('Bulgaria','Czech Republic','Denmark','Ireland','Lithuania','Norway','Slovakia','Sweden','United Kingdom')
  # get data
  # NB need to choose tier, docclause and ccy
  # Experiment with different ones later!
  res <- dbSendQuery(wrds, "
                  SELECT * FROM (
                    SELECT * FROM markit.cds2019
                    UNION ALL
                    SELECT * FROM markit.cds2018
                    UNION ALL
                    SELECT * FROM markit.cds2017
                    UNION ALL
                    SELECT * FROM markit.cds2016
                    UNION ALL
                    SELECT * FROM markit.cds2015
                    UNION ALL
                    SELECT * FROM markit.cds2014
                    UNION ALL
                    SELECT * FROM markit.cds2013
                    UNION ALL
                    SELECT * FROM markit.cds2012
                  ) AS foo
                  WHERE date BETWEEN '2012-12-12'
                    AND '2019-12-30'
                  AND country IN
                    ('Bulgaria','Czech Republic','Denmark','Ireland','Lithuania','Norway','Slovakia','Sweden','United Kingdom')
                  --AND tier = 'SNRFOR'
                  --AND docclause = 'MM'
                  --AND ccy = 'EUR'
                  --AND country = 'Denmark'
                  --AND date = '2012-12-12'
                  ORDER BY redcode,date,tier,docclause,ccy;
                     ")
  #nb the n=10 limits to 10 rows, to test. Delete this later when SQL correct
  #data <- dbFetch(res, n=10)
  data <- dbFetch(res, n=-1)
  #dbClearResult(res) closes the connection, readying for another query.
  dbClearResult(res)
  data
  #make tibble & only keep stuff I need
  # nb I can get all this stuff!!!
  data_all <- as_tibble(data) %>%
    filter(tier == 'SNRFOR' & ccy == 'EUR' & docclause == 'MM') %>%
    arrange(redcode,ticker,shortname,country,sector,tier,docclause,ccy,date)
  #save data
  file <- "data_from_function_wrds/markit_firms.csv"
  write.csv(data_all, file)
  
  result <- read.csv(file)
  return(result)
}
#END HEADER

############ Reload data from WRDS and Ken French etc #########################
# Reload data from wrds
firms <- getFirmData()
CDS <- getCDSData()

# OR just load from saved file
file <- "data_from_function_wrds/compustat_firms.csv"
firms <- read.csv(file)
file <- "data_from_function_wrds/markit_firms.csv"
CDS <- read.csv(file)


#This is the index data
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


#### step 1 Try own instead - 1. a Data ready. Can be skipped#################

## START DATA
# get firm returns
file <- "data_from_function_wrds/compustat_firms.csv"
firms <- read.csv(file)
file <- "data_from_function_wrds/markit_firms.csv"
CDS <- read.csv(file)
# compute returns
firm_returns <- firms %>% filter(iid == "01W") %>%
  select(datadate,gvkey,prccd,loc) %>%
  rename(date = datadate) %>%
  mutate(date = ymd(parse_date_time(date, "%Y%m%d") ) ) %>%
  group_by(loc,gvkey) %>%
  rename(returns = prccd) %>%
  mutate(returns = (returns/lag(returns) - 1)) %>%
  arrange(gvkey, date)


# Get factor(s)
# NB it is an issue that one has a - in the name! makes it much harder
Europe_3_Factors_Daily <- getFactorsDaily("Europe_3_Factors_Daily")
Factors <- Europe_3_Factors_Daily %>%
  mutate('Mkt' = (!!Europe_3_Factors_Daily$'Mkt-RF' + !!Europe_3_Factors_Daily$RF)) %>% 
  select(date,Mkt)
# Europe_5_Factors_Daily <- factorDaily("Europe_5_Factors_Daily")
# Europe_Mom_Factor_Daily <- factorDaily("Europe_MOM_Factor_Daily")
#now merge
firmdata <- left_join(firm_returns,Factors, by = "date")

# Do events
# Events
list <- makeData()
#split list into the datasets
events <- list[[2]]
eventsAllChanges <- list[[3]]
eventsNoChange <- list[[4]]
# change event country names to match firms
events <- events %>% as_tibble() %>% rename(loc = "name")
events$loc <- recode(events$loc,
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

eventsNoChange <- eventsNoChange %>% as_tibble() %>% rename(loc = "name")
eventsNoChange$loc <- recode(eventsNoChange$loc,
                     "Austria" = "AUS",
                     "Belgium" = "BEL",
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

# change cds country names too
CDS$country <- recode(CDS$country,
                      "Austria" = "AUS",
                      "Belgium" = "BEL",
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
# change date to date
CDS$date <- as.Date(CDS$date)

# NB DO BELOW FOR CDS DATA TOO!
# create an event for each firm within the country experiencing buffer
eventsnew <- tibble()
i = 0
eventloc = events$loc[1] #test
eventloc = "DNK" #test
for (eventloc in events$loc) {
  i = i + 1
  #get firms
  currentfirms <- filter(firms,loc == eventloc) %>% select(gvkey, conm) %>%
    distinct() %>%
    #add date
    mutate(when = events$when[i])
  
  #add to main tibble
  eventsnew <- bind_rows(eventsnew,currentfirms)
} 
## data done. Now prepare data
events <- eventsnew %>% rowwise() %>% mutate(gvkey = paste("c",toString(gvkey),sep = "")) %>%
  rename(name = "gvkey") %>% select(-conm) %>%
  as.data.frame()
eventandmktreturns <- firmdata %>% rowwise() %>% mutate(gvkey = paste("c",toString(gvkey),sep = ""))

# NB DO BELOW FOR CDS DATA TOO!
# NB need to recode countries into short version
eventsnew <- tibble()
i = 0
eventloc = events$loc[1] #test
eventloc = "DNK" #test
for (eventloc in events$loc) {
  i = i + 1
  #get firms
  currentfirms <- filter(CDS,country == eventloc) %>% select(redcode,shortname) %>%
    distinct() %>%
    #add date
    mutate(when = events$when[i])
  
  #add to main tibble
  eventsnew <- bind_rows(eventsnew,currentfirms)
} 
## data done. Now prepare data
eventsCDS <- eventsnew %>% rowwise() %>%
  rename(name = "redcode") %>% select(-shortname) %>%
  as.data.frame()
#eventandmktreturns <- firmdata %>% rowwise() %>% mutate(gvkey = paste("c",toString(gvkey),sep = ""))
CDS <- CDS

# also do for no events
eventsnew <- tibble()
i = 0
eventloc = eventsNoChange$loc[1] #test
eventloc = "DNK" #test
for (eventloc in eventsNoChange$loc) {
  i = i + 1
  #get firms
  currentfirms <- filter(firms,loc == eventloc) %>% select(gvkey, conm) %>%
    distinct() %>%
    #add date
    mutate(when = eventsNoChange$when[i])
  
  #add to main tibble
  eventsnew <- bind_rows(eventsnew,currentfirms)
} 
## data done. Now prepare data
eventsNoChange <- eventsnew %>% rowwise() %>% mutate(gvkey = paste("c",toString(gvkey),sep = "")) %>%
  rename(name = "gvkey") %>% select(-conm) %>%
  as.data.frame()
eventandmktreturns <- firmdata %>% rowwise() %>% mutate(gvkey = paste("c",toString(gvkey),sep = ""))

# also do for CDS no events
eventsnew <- tibble()
i = 0
eventloc = eventsNoChange$loc[1] #test
eventloc = "DNK" #test
for (eventloc in eventsNoChange$loc) {
  i = i + 1
  #get firms
  currentfirms <- filter(CDS,country == eventloc) %>% select(redcode, shortname) %>%
    distinct() %>%
    #add date
    mutate(when = eventsNoChange$when[i])
  
  #add to main tibble
  eventsnew <- bind_rows(eventsnew,currentfirms)
} 
## data done. Now prepare data
eventsNoChangeCDS <- eventsnew %>% rowwise() %>%
  rename(name = "redcode") %>% select(-shortname) %>%
  as.data.frame()
eventandmktreturns <- firmdata %>% rowwise() %>% mutate(gvkey = paste("c",toString(gvkey),sep = ""))
## END DATA


### 1.b now create analysis data NB Can be skipped ############################

#event window to record nb just take all?
L_pre_record = 5#100
L_post_record = 10#100 #NB eventwindow is 1 longer than this
L_Estimation = 365

# old version without lapply
#eventdata <- tibble()
##when <- events$when[1]
#for (i in 1:length(events$when)) {
#  print(paste(i,"out of",length(events$when)))
#  thisdata <- slice(events,i)
#  eventdataAdd <- filter(eventandmktreturns,gvkey == thisdata$name) %>%
#    mutate(eventdate = date - thisdata$when) %>%
#    filter(eventdate >= -L_Estimation-10 & eventdate <= L_post_record+10) %>%
#    mutate(eventid = i)
#  eventdata <- bind_rows(eventdata,eventdataAdd)
#}

# NB CHANGE THESE TO BUSINESS DAYS

# first for significant changes   
eventdata <- tibble()
d <- lapply( seq_len(length(events$when)), function(i) {
  print(paste(i,"out of",length(events$when)))
  thisdata <- slice(events,i)
  eventdataAdd <- filter(eventandmktreturns,gvkey == thisdata$name) %>%
    mutate(eventdate = date - thisdata$when) %>%
    ### CHANGE THIS ABOVE TO BUSINESS DAYS! bizdays package?####
    filter(eventdate >= -L_Estimation-10 & eventdate <= L_post_record+10) %>%
    mutate(eventid = i)   
  return(eventdataAdd)
})
eventdata <- do.call(bind_rows,d)
#save data
file <- "output_to_input/eventStudyDataRaw.csv"
write.csv(eventdata, file)

# first for significant changes - CDS
eventdata <- tibble()
d <- lapply( seq_len(length(eventsCDS$when)), function(i) {
  print(paste(i,"out of",length(eventsCDS$when)))
  thisdata <- slice(eventsCDS,i)
  eventdataAdd <- filter(CDS,redcode == thisdata$name) %>%
    mutate(eventdate = date - thisdata$when) %>%
    ### CHANGE THIS ABOVE TO BUSINESS DAYS! bizdays package?####
  filter(eventdate >= -L_Estimation-10 & eventdate <= L_post_record+10) %>%
    mutate(eventid = i)   
  return(eventdataAdd)
})
eventdata <- do.call(bind_rows,d)
#save data
file <- "output_to_input/eventStudyDataRawCDS.csv"
write.csv(eventdata, file)



# then for no changes
eventdata <- tibble()
d <- lapply( seq_len(length(eventsNoChange$when)), function(i) {
  print(paste(i,"out of",length(eventsNoChange$when)))
  thisdata <- slice(eventsNoChange,i)
  eventdataAdd <- filter(eventandmktreturns,gvkey == thisdata$name) %>%
    mutate(eventdate = date - thisdata$when) %>%
    ### CHANGE THIS ABOVE TO BUSINESS DAYS! bizdays package?####
  filter(eventdate >= -L_Estimation-10 & eventdate <= L_post_record+10) %>%
    mutate(eventid = i)   
  return(eventdataAdd)
})
eventdata <- do.call(bind_rows,d)
#save data
file <- "output_to_input/eventStudyNoChangeDataRaw.csv"
write.csv(eventdata, file)

# then for no data changes - CDS
eventdata <- tibble()
d <- lapply( seq_len(length(eventsNoChangeCDS$when)), function(i) {
  print(paste(i,"out of",length(eventsNoChangeCDS$when)))
  thisdata <- slice(eventsNoChangeCDS,i)
  eventdataAdd <- filter(CDS,redcode == thisdata$name) %>%
    mutate(eventdate = date - thisdata$when) %>%
    ### CHANGE THIS ABOVE TO BUSINESS DAYS! bizdays package?####
  filter(eventdate >= -L_Estimation-10 & eventdate <= L_post_record+10) %>%
    mutate(eventid = i)   
  return(eventdataAdd)
})
eventdata <- do.call(bind_rows,d)
#save data
file <- "output_to_input/eventStudyNoChangeDataRawCDS.csv"
write.csv(eventdata, file)


  
  
##### step 2.A (can be start) Own event study ################################################
file <- "output_to_input/eventStudyDataRaw.csv"
eventdata <- read.csv(file)
file <- "output_to_input/eventStudyNoChangeDataRaw.csv"
noEventdata  <- read.csv(file)
file <- "output_to_input/eventStudyDataRawCDS.csv"
eventdataCDS <- read.csv(file)
file <- "output_to_input/eventStudyNoChangeDataRawCDS.csv"
noEventdataCDS  <- read.csv(file)
#NB these defined earlier if choosing very large might be larger than current
# data and hence need to remake it
# these are original settings
L_pre_record = 5#100
L_post_record = 10#100 #NB eventwindow is 1 longer than this
L_Estimation = 365

# change here if you want
L_pre_record = L_pre_record
L_post_record = L_post_record
L_Estimation = L_Estimation

#compute betas
eventdata <- eventdata %>% group_by(eventid)
betas <- eventdata %>% group_by(eventid) %>%
  filter(eventdate >= (-L_Estimation - 10) & eventdate <= -10) %>%
  do(model = lm(returns ~ Mkt, data = .)) %>%
  tidy(model) %>% filter(term == "Mkt") %>% select(estimate) %>%
  rename(beta = estimate)
eventdatafitted <- left_join(eventdata,betas,by = "eventid") %>%
#compute excess returns
mutate(retabn = returns - beta*Mkt)

#compute betas for no events
noEventdata <- noEventdata %>% group_by(eventid)
betas <- noEventdata %>% group_by(eventid) %>%
  filter(eventdate >= (-L_Estimation - 10) & eventdate <= -10) %>%
  do(model = lm(returns ~ Mkt, data = .)) %>%
  tidy(model) %>% filter(term == "Mkt") %>% select(estimate) %>%
  rename(beta = estimate)
noEventdatafitted <- left_join(noEventdata,betas,by = "eventid") %>%
  #compute excess returns
  mutate(retabn = returns - beta*Mkt)

##### step 2.B make plot data! #############################################
#first make plot data. THen plot
## 3.1 Make plot data
#NB these defined earlier if choosing very large might be larger than current
# data and hence need to remake it
L_pre_record <- 5
L_post_record <- 5

plotdata <- eventdatafitted %>%
  filter(eventdate >= -L_pre_record & eventdate <= L_post_record) %>%
  group_by(eventid) %>%
  mutate(cumsabn = cumsum(retabn %>% replace_na(0))) %>%
  mutate(cumpabn = cumprod((1 + retabn) %>% replace_na(1)) - 1) %>%
  mutate(cumsabs = cumsum(returns %>% replace_na(0))) %>%
  mutate(cumpabs = cumprod((1 + returns) %>% replace_na(1)) - 1)%>%
  select(-X)

plotdataCDS <- eventdataCDS %>%
  filter(eventdate >= -L_pre_record & eventdate <= L_post_record) %>%
  group_by(eventid) %>%
  #mutate(cumsabn = cumsum(retabn %>% replace_na(0))) %>%
  #mutate(cumpabn = cumprod((1 + retabn) %>% replace_na(1)) - 1) %>%
  #mutate(cumsabs = cumsum(returns %>% replace_na(0))) %>%
  #mutate(cumpabs = cumprod((1 + returns) %>% replace_na(1)) - 1)%>%
  select(-X)


plotdataNoEvent <- noEventdatafitted %>%
  filter(eventdate >= -L_pre_record & eventdate <= L_post_record) %>%
  group_by(eventid) %>%
  mutate(cumsabn = cumsum(retabn %>% replace_na(0))) %>%
  mutate(cumpabn = cumprod((1 + retabn) %>% replace_na(1)) - 1) %>%
  mutate(cumsabs = cumsum(returns %>% replace_na(0))) %>%
  mutate(cumpabs = cumprod((1 + returns) %>% replace_na(1)) - 1)%>%
  select(-X)

plotdataNoEventCDS <- noEventdataCDS %>%
  filter(eventdate >= -L_pre_record & eventdate <= L_post_record) %>%
  group_by(eventid) %>%
  #mutate(cumsabn = cumsum(retabn %>% replace_na(0))) %>%
  #mutate(cumpabn = cumprod((1 + retabn) %>% replace_na(1)) - 1) %>%
  #mutate(cumsabs = cumsum(returns %>% replace_na(0))) %>%
  #mutate(cumpabs = cumprod((1 + returns) %>% replace_na(1)) - 1)%>%
  select(-X)

## add on their sector
# Can see sector meanings here
#  https://en.wikipedia.org/wiki/Global_Industry_Classification_Standard
# nb also do it with industry groups
# nb I trust loess more!! But hard with many datapoints like for noevents

# get firm data
file <- "data_from_function_wrds/compustat_firms.csv"
firms <- read.csv(file)

plotdatasuper <- left_join(plotdata,
                           firms %>% select(gvkey,gsector,ggroup) %>% 
                             mutate(gvkey = paste("c",gvkey,sep = ""))  %>%
                             distinct(),
                           by = "gvkey") %>%
  mutate(gsector = paste("s",gsector,sep="")) %>%
  mutate(ggroup = paste("g",ggroup,sep="")) %>%
  # create new variable with either finance or nonfinance
  mutate(finance = (gsector == "s40")) %>%
  mutate(bank = (ggroup == "g4010")) %>%
  mutate(type = if (ggroup == "g4010") "bank fin" else {
    if (gsector == "s40") "non-bank fin" else "other" } ) %>%
  group_by(gsector)

#for cds
plotdatasuperCDS <- 
  # create new variable with either finance or nonfinance
  mutate(plotdataCDS, finance = (sector == "Financials")) %>%
  #mutate(bank = (ggroup == "g4010")) %>%
  #mutate(type = if (ggroup == "g4010") "bank fin" else {
  #  if (gsector == "s40") "non-bank fin" else "other" } ) %>%
  group_by(sector)


plotdatasuperNoEvent <- left_join(plotdataNoEvent,
                                  firms %>% select(gvkey,gsector,ggroup) %>% 
                                    mutate(gvkey = paste("c",gvkey,sep = ""))  %>%
                                    distinct(),
                                  by = "gvkey") %>%
  mutate(gsector = paste("s",gsector,sep="")) %>%
  mutate(ggroup = paste("g",ggroup,sep="")) %>%
  # create new variable with either finance or nonfinance
  mutate(finance = (gsector == "s40")) %>%
  mutate(bank = (ggroup == "g4010")) %>%
  mutate(type = if (ggroup == "g4010") "bank fin" else {
    if (gsector == "s40") "non-bank fin" else "other" } ) %>%
  group_by(gsector)

plotdatasuperNoEventCDS <- #left_join(plotdataNoEvent,
    #                              firms %>% select(gvkey,gsector,ggroup) %>% 
   #                                 mutate(gvkey = paste("c",gvkey,sep = ""))  %>%
  #                                  distinct(),
   #                               by = "gvkey") %>%
  #mutate(gsector = paste("s",gsector,sep="")) %>%
  #mutate(ggroup = paste("g",ggroup,sep="")) %>%
  # create new variable with either finance or nonfinance
  mutate(plotdataNoEventCDS, finance = (sector == "Financials")) %>%
#  mutate(bank = (ggroup == "g4010")) %>%
#  mutate(type = if (ggroup == "g4010") "bank fin" else {
#    if (gsector == "s40") "non-bank fin" else "other" } ) %>%
  group_by(sector)



## Step 3 Now plot and save ##################################################
#test tikzDevice
#tikz("testTikzD2.tex")

ggplot(data = plotdata) +
  geom_smooth(mapping = aes(x = eventdate, y = returns ),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual raw returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = returns), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("figures/retraw.pdf", width = 5.86, height = 5.86)

#cds spread
ggplot(data = plotdataCDS) +
  geom_smooth(mapping = aes(x = eventdate, y = spread3y),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("2 year spread") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = spread3y), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("figures/spreadCDS.pdf", width = 5.86, height = 5.86)

# with no events
ggplot(data = plotdataNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = returns ),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual raw returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = returns), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retrawNoEvent.pdf", width = 5.86, height = 5.86)

#cds spread no events
ggplot(data = plotdataNoEventCDS) +
  geom_smooth(mapping = aes(x = eventdate, y = spread3y),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("2 year spread") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = spread3y), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("figures/spreadCDSNoEvent.pdf", width = 5.86, height = 5.86)


# with excess returns
ggplot(data = plotdata) +
  geom_smooth(mapping = aes(x = eventdate, y = retabn ),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual abnormal returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = retabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabn.pdf", width = 5.86, height = 5.86)
### !! This above actually looks really good !!! Both for 5 and 10 days##############

# with excess returns
ggplot(data = plotdataNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = retabn ),
              na.rm = TRUE, level = 0.9, formula = "y~x",method = "loess") +
  ylab("individual abnormal returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = retabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabnNoEvent.pdf", width = 5.86, height = 5.86)
# NB notice that very high return on day 1!

# with excess returns cums
ggplot(data = plotdata) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn ),
              na.rm = TRUE, level = 0.9, formula = "y~x", method ="loess") +
  ylab("cumulative sum of abnormal returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncum.pdf", width = 5.86, height = 5.86)
### !! This above too! ###

# no event
ggplot(data = plotdataNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn ),
              na.rm = TRUE, level = 0.9, formula = "y~x") +
  ylab("cumulative sum of abnormal returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
# no event loess method
ggplot(data = plotdataNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn ),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("cumulative sum of abnormal returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumNoEvent.pdf", width = 5.86, height = 5.86)

# with excess return cump
ggplot(data = plotdata) +
  geom_smooth(mapping = aes(x = eventdate, y = cumpabn ),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual abnormal returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumpabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncump.pdf", width = 5.86, height = 5.86)

### Now figure out which firms are hit the most! ##############################


### step 4 plot ####
# NB is robust to cum product vs cum sum

# with excess return indvidual
ggplot(data = plotdatasuper) +
  geom_smooth(mapping = aes(x = eventdate, y = retabn, group = gsector,
                            linetype = gsector, color = gsector),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess", se = FALSE) +
  ylab("individual abnormal returns") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = retabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabnSplit.pdf", width = 5.86, height = 5.86)
# cds
ggplot(data = filter(plotdatasuperCDS)) +
  geom_smooth(mapping = aes(x = eventdate, y = spread2y, group = sector,
                            linetype = sector, color = sector),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess", se = FALSE) +
  ylab("2 year spread") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = spread2y), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/spreadSplitCDSwMat.pdf", width = 5.86, height = 5.86)
# cds minus materials
ggplot(data = filter(plotdatasuperCDS, sector != "Basic Materials")) +
  geom_smooth(mapping = aes(x = eventdate, y = spread2y, group = sector,
                            linetype = sector, color = sector),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess", se = FALSE) +
  ylab("2 year spread") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = spread2y), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/spreadSplitCDS.pdf", width = 5.86, height = 5.86)

# no event
ggplot(data = plotdatasuperNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = retabn, group = gsector,
                            linetype = gsector, color = gsector),
              na.rm = TRUE, level = 0.9, formula = "y~x", se = FALSE) +
  ylab("individual abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = retabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabnSplitNoEvent.pdf", width = 5.86, height = 5.86)
# no event cds
ggplot(data = plotdatasuperNoEventCDS) +
  geom_smooth(mapping = aes(x = eventdate, y = spread2y, group = sector,
                            linetype = sector, color = sector),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess", se = FALSE) +
  ylab("2 year spread ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = spread2y), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/spreadSplitNoEventCDS.pdf", width = 5.86, height = 5.86)


# with excess return cump
ggplot(data = plotdatasuper) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = gsector,
                            linetype = gsector, color = gsector),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("cumulative product of abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumSplitShade.pdf", width = 5.86, height = 5.86)
# noevent cums
ggplot(data = plotdatasuperNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = gsector,
                            linetype = gsector, color = gsector),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("cumulative product of abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumSplitNoEventShade.pdf", width = 5.86, height = 5.86)
# NB Why does cump look so different to cums?!

## next two same but no shade
# with excess return cump
ggplot(data = plotdatasuper) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = gsector,
                            linetype = gsector, color = gsector),
              na.rm = TRUE, level = 0.9, formula = "y~x", se = FALSE) +
  ylab("cumulative product of abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumSplit.pdf", width = 5.86, height = 5.86)
# noevent cums
ggplot(data = plotdatasuperNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = gsector,
                            linetype = gsector, color = gsector),
              na.rm = TRUE, level = 0.9, formula = "y~x", se = FALSE) +
  ylab("cumulative product of abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumSplitNoEvent.pdf", width = 5.86, height = 5.86)
# NB Why does cump look so different to cums?!

# group by industry group rather than sector
ggplot(data = plotdatasuper) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = ggroup,
                             color = ggroup),
              na.rm = TRUE, level = 0.9, formula = "y~x", se = FALSE) +
  ylab("cumulative product of abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumSplitSplit.pdf", width = 5.86, height = 5.86)
# noevent
ggplot(data = plotdatasuperNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = ggroup,
                            color = ggroup),
              na.rm = TRUE, level = 0.9, formula = "y~x", se = FALSE) +
  ylab("cumulative product of abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumSplitSplitNoEvent.pdf", width = 5.86, height = 5.86)

# group by finance or non-finance
ggplot(data = plotdatasuper) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = finance,
                            linetype = finance, color = finance),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("cumulative product of abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn, group = finance), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumFin.pdf", width = 5.86, height = 5.86)
# cds
ggplot(data = plotdatasuperCDS) +
  geom_smooth(mapping = aes(x = eventdate, y = spread2y, group = finance,
                            linetype = finance, color = finance),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("2 year spread") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = spread2y, group = finance), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/spreadFinCDS.pdf", width = 5.86, height = 5.86)
# noevents cums. nb normal event data looks same for cums and cump
ggplot(data = plotdatasuperNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = finance,
                            linetype = finance, color = finance),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("cumulative product of abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn, group = finance), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumFinNoEvent.pdf", width = 5.86, height = 5.86)
# noevents cds
ggplot(data = plotdatasuperNoEventCDS) +
  geom_smooth(mapping = aes(x = eventdate, y = spread2y, group = finance,
                            linetype = finance, color = finance),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("2 year spreads") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = spread2y, group = finance), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/spreadFinNoEventCDS.pdf", width = 5.86, height = 5.86)

# try finance non-bank vs finance bank
ggplot(data = plotdatasuper) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = type,
                            linetype = type, color = type),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn, group = type), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumType.pdf", width = 5.86, height = 5.86)
# no event cums
ggplot(data = plotdatasuperNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = type,
                            linetype = type, color = type),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn, group = type), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumTypeNoEvent.pdf", width = 5.86, height = 5.86)


# group by bank or non-bank
ggplot(data = plotdatasuper) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = bank,
                            linetype = bank, color = bank),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn, group = bank), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumBank.pdf", width = 5.86, height = 5.86)
# Interesting!! Banks less affected than non-banks fin, who are all less
# afftected than non-fins
# try finance non-bank vs finance bank Other method.
# Better but doesnt work for too many datapoints 
# NB Below not used???? Same as one just before?
ggplot(data = plotdatasuper) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = bank,
                            linetype = bank, color = bank),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn, group = bank), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))


# no event
ggplot(data = plotdatasuperNoEvent) +
  geom_smooth(mapping = aes(x = eventdate, y = cumsabn, group = bank,
                            linetype = bank, color = bank),
              na.rm = TRUE, level = 0.9, formula = "y~x", method = "loess") +
  ylab("individual abnormal returns ") + xlab("event time") + theme_classic() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  stat_summary(aes(x = eventdate, y = cumsabn, group = bank), geom = "point", fun.y = mean, shape = 17, size = 3)+
  theme(panel.border = element_rect(colour = "black", fill=NA))
ggsave("figures/retabncumBankNoEvent.pdf", width = 5.86, height = 5.86)
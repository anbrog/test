#main script making main results
#first part imports data
#then data manipulation
#then create results. First main and then robustness and variations
#then print results ie make tables and figures

#start needed libraries
library(RPostgres)
library(ggplot2)
library(dplyr)

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
                   where a.datadate between '2019-06-01'
                   and '2019-12-30'")
  # collect data from connection
  data <- dbFetch(res, n = -1)
  # release connection
  dbClearResult(res)
  #data
  
  #save data
  file = "data_from_function_wrds/compustat.csv"
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

# Test data
unique(data_compustat$indexgeo)
# Ie I get 63 different countries


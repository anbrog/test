#This script gets the markit and optionm data
#NB remember to save the data as archived once you download and start making results

#COOL! On COMPUSTAT global can check all kinds of real effects. Investment etc after buffers.
#COOL! There is much more data on compustat than you immediately think!
#check the d_bank dataset. Such like comp.bank_afnd1 and bank_funda

library(RPostgres)
library(ggplot2)
library(dplyr)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user = 'anbr',
                  password = 'Hvilkenkode1'
                  )

# This seems to be main datapull. Save somewhere
res <- dbSendQuery(wrds, "select a.gvkeyx, a.prccd, a.datadate, 
                   b.indexgeo, b.indexval, b.indextype, b.indexid, b.spii, b.conm
                   from comp.g_idx_daily a join comp.g_idx_index b
                   on a.gvkeyx = b.gvkeyx
                   where a.datadate between '2019-06-01'
                   and '2019-12-30'")
data <- dbFetch(res, n = -1)
dbClearResult(res)
data
fil <- tempfile("compustat", fileext = ".rds")
saveRDS(data,fil)
write.csv(data, "/data/compustat.csv")

res <- dbSendQuery(wrds, "select * from comp.company")
#nb the n=10 limits to 10 rows, to test. Delete this later when SQL correct
data <- dbFetch(res, n=10)
#dbClearResult(res) closes the connection, readying for another query.
dbClearResult(res)
data

#now test querying metadata of libraries (vendor like compustat) to get all available tables (datasets) 
#NOTE: When referencing library and dataset names, you must use all lowercase. This applies to both information_schema.tables and information_schema.columns files.
res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

#there are many comp, so try compg

#Determine the datasets within a given library:
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='compd'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

#Determine the variables (column headers) within a given dataset:
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='comp'
                   and table_name='currency'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

#To query the crsp.dsf dataset:
res <- dbSendQuery(wrds, "select * from comp.currency")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

#COOL! On COMPUSTAT global can check all kinds of real effects. Investment etc after buffers.

#COOL! There is much more data on compustat than you immediately think!

#check the d_bank dataset. Such like comp.bank_afnd1 and bank_funda


# IMP: need to use compd.g_sec_dtrt for total returns and merge with their sector codes to get it split by sector?
# merge on gvkey from compd.g_funda (or g_fundaq for quarterly fundamental updates)
# or is it comp.g_sec_dtrt? I think it's compd
# comp.G_CO_INDUSTRY (lowercase) can link company (gvkey) to industry (sich) each year. 
# can also use compd.G_CO_INDUSTRY (lowercase) to get different index and their info


# Can use the index with index info to get index returns for different countries
# ie compd.g_idx_daily and merged with compd.g_idx_index and compd.g_idx_hist has historical constituents if you need
# has country identifier!
# Do this first. Test
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='compg'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data
# seems like maybe can use both comp and compg, but not compd!

#now figure out which tables
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='comp'
                   and table_name='g_idx_daily'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='comp'
                   and table_name='g_idx_index'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data
#both seem to work still and also both tables work


## So now collect the data and merge (I guess!)
# Start with return data
# to query data:
res <- dbSendQuery(wrds, "select * from comp.g_idx_daily")
data <- dbFetch(res, n=10)
dbClearResult(res)
data
#Once you looked at the metadata and see the available variables, you probably want to specify only those you are interested in. 
res <- dbSendQuery(wrds, "select gvkeyx,prccd,datadate from comp.g_idx_daily")
ret <- dbFetch(res, n=10)
dbClearResult(res)
ret

res <- dbSendQuery(wrds, "select * from comp.g_idx_index")
data <- dbFetch(res, n=10)
dbClearResult(res)
data
#Once you looked at the metadata and see the available variables, you probably want to specify only those you are interested in. 
res <- dbSendQuery(wrds, "select gvkeyx, indexgeo, indexval, indextype, indexid, spii, conm from comp.g_idx_index")
fun <- dbFetch(res, n=10)
dbClearResult(res)
fun

#try and limit to last year for test reasons
res <- dbSendQuery(wrds, "select gvkeyx,prccd,datadate
                   from comp.g_idx_daily
                   where datadate between '2019-01-01'
                   and '2019-12-30'")
ret <- dbFetch(res, n=-1)
dbClearResult(res)
ret

res <- dbSendQuery(wrds, "select gvkeyx, indexgeo, indexval, indextype, indexid, spii, conm
                   from comp.g_idx_index")
fun <- dbFetch(res, n=-1)
dbClearResult(res)
fun

# do it joint! Cool!!!
res <- dbSendQuery(wrds, "select a.gvkeyx, a.prccd, a.datadate, 
                   b.indexgeo, b.indexval, b.indextype, b.indexid, b.spii, b.conm
                   from comp.g_idx_daily a join comp.g_idx_index b
                   on a.gvkeyx = b.gvkeyx
                   where a.datadate between '2019-06-01'
                   and '2019-12-30'")
data <- dbFetch(res, n = -1)
dbClearResult(res)
data


#test of display of data using potentially ggplot2
res <- dbSendQuery(wrds,"SELECT date,dji FROM djones.djdaily")
data <- dbFetch(res, n = -1)
dbClearResult(res)
plot(as.Date(data$date,'%Y-%m-%d'),data$dji,xlab="date",ylab="dji",type='l',col='red')
qplot(as.Date(data$date,'%Y-%m-%d'),data$dji,xlab="date",ylab="dji",type='l',col='red')

subdata <- subset(data, data$conm == "Composite DAX Index")
subdata <- subdata[order(subdata$datadate),]
plot(as.Date(subdata$datadate,'%Y-%m-%d'),subdata$prccd,xlab="date",ylab="ret_tot",type='l',col='red')


## Try seeing what CDS data there is
#Determine the datasets within a given library:
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='markit'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

#Determine the variables (column headers) within a given dataset:
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='markit'
                   and table_name='itraxxeucomps'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='markit'
                   and table_name='chars'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

#THIS IS A FANTASTIC DATABASE! SEE IF SPREAD CHANGES! :D
res <- dbSendQuery(wrds, "select * from markit.cds2019")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

# this dataset has jurisdiction which covers European countries too. So you can pair ticker/cusip/shortname/lei with a country! And sector too!
# THIS IS GREAT!
res <- dbSendQuery(wrds, "select * from markit.redent")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

# many many names... >1000. Also on countries??? Fin and industrial
res <- dbSendQuery(wrds, "select * from markit.itraxxeucomps")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

#same here. maaybe?
res <- dbSendQuery(wrds, "select * from markit.itraxxeuconst")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

#maybe these two next, but no countries.
res <- dbSendQuery(wrds, "select * from markit.cdxcomps")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

res <- dbSendQuery(wrds, "select * from markit.cdxconst
                   where region = 'Europe' ")
data <- dbFetch(res, n=10)
dbClearResult(res)
data




res <- dbSendQuery(wrds, "select distinct isdatt_name from markit.isdatt")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

res <- dbSendQuery(wrds, "select distinct indexid from markit.itraxxeucomps")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

res <- dbSendQuery(wrds, "select distinct shortname from markit.itraxxeuconst")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

res <- dbSendQuery(wrds, "select distinct jurisdiction from markit.redent")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

#weird! why does and not work when on next line??
# can use entity_cusip too
res <- dbSendQuery(wrds, "select distinct shortname from markit.redent
                   where jurisdiction = 'Denmark' and markitsector = 'Financials'")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data



## Try seeing what options data there is
#Determine the datasets within a given library:
res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='optionm'
                   order by table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

#Determine the variables (column headers) within a given dataset:
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='optionm'
                   and table_name='country'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

# this is maybe useful? Histvol too. What does opprcd mean? option pr
res <- dbSendQuery(wrds, "select * from optionm.opprcd2017")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

# maybe useful. has decription...
res <- dbSendQuery(wrds, "select * from optionm.option")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

# maybe useful. has isin link w securityid isser too .optionnames too
# issuer good for type of firm
res <- dbSendQuery(wrds, "select * from optionm.optionmeurnames")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

# This has country!
res <- dbSendQuery(wrds, "select * from optionm.security")
data <- dbFetch(res, n=10)
dbClearResult(res)
data

# This has issuer!
res <- dbSendQuery(wrds, "select * from optionm.security_name")
data <- dbFetch(res, n=10)
dbClearResult(res)
data
# This script downloads the Compustat databases from WRDS (Wharton Research Data Services)
# These databases are the input for creating the global LIVA database

# Copyright (C) 2019-2021, Phebo Wibbens and Nicolaj Siggelkow

#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(tidyverse)
library(RPostgres)

# This script requires a connection to the WRDS cloud, for set up and troubleshooting see:
#   https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/r-from-your-computer/

writeOutput <- T
username <- "INSERT_YOUR_WRDS_USERNAME_HERE"
wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737, dbname='wrds',
                  sslmode='require', user=username)

# NA Security data
res <- dbSendQuery(wrds, "select gvkey,iid,datadate,ajexm,curcdm,prccm,trfm,trt1m,cshom,exchg,fic
                          from comp.secm
                          where primiss = 'P' and tpci = '0' and cshom > 0 and
                            datadate between '1998-12-01' and '2021-01-01'")
dfNASec <- dbFetch(res, n=-1)
dbClearResult(res)

# Global security data
res <- dbSendQuery(wrds, "select gvkey,iid,datadate,ajexdi,curcdd,prccd,trfd,cshoc,exchg,fic
                          from comp.g_secd
                          where monthend = 1 and tpci = '0' and qunit = 1 and
                            datadate between '1998-12-01' and '2021-01-01'")
dfGlobalSec <- dbFetch(res, n=-1)
dbClearResult(res)

# NA company info
res <- dbSendQuery(wrds, "select gvkey,conm,gsubind,loc,stko from comp.company")
dfNACo <- dbFetch(res, n=-1)
dbClearResult(res)
stopifnot(!anyDuplicated(dfNACo$gvkey))

# Global company info
res <- dbSendQuery(wrds, "select gvkey,conm,gsubind,loc,stko from comp.g_company")
dfGlobalCo <- dbFetch(res, n=-1)
dbClearResult(res)
stopifnot(!anyDuplicated(dfGlobalCo$gvkey))

# FX database
res <- dbSendQuery(wrds, "select * from comp.g_exrt_dly")
dfFx <- dbFetch(res, n=-1)
dbClearResult(res)

# Merge databases and write to disk
dfNA <- inner_join(dfNASec, dfNACo)
dfGlobal <- inner_join(dfGlobalSec, dfGlobalCo)

if(writeOutput) {
  write_csv(dfNA, "db-compustat-na-security.csv")
  write_csv(dfGlobal, "db-compustat-global-security.csv")
  write_csv(dfFx, "db-G_EXRT_DLY.csv")
}

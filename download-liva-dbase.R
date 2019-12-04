# This script downloads the global LIVA database from WRDS (Wharton Research Data Services) 

# Copyright (C) 2019, Phebo Wibbens and Nicolaj Siggelkow

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

username <- "INSERT_YOUR_WRDS_USERNAME_HERE"
wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737, dbname='wrds',
                  sslmode='require', user=username)

#### To download specific industries, countries or years ####
# As an example: all US companies in pharma (GICS code 3520xxxx) between 2009 and 2018
res <- dbSendQuery(wrds, "select * from contrib.liva where
                   year between '2009' and '2018' and
                   loc = 'USA' and
                   gsubind >= 35200000 and gsubind < 35210000 ")
dfSub <- dbFetch(res, n=-1) 
dbClearResult(res)


#### To download and save the entire database ####
# This might take a (long) while, depending on your internet speed
res <- dbSendQuery(wrds, "select * from contrib.liva")
df <- dbFetch(res, n=-1) 
write_csv(df, "db-liva.csv")
dbClearResult(res)

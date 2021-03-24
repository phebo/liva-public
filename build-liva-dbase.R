# This script generates the global LIVA database and can be adapted to create a custom LIVA database
# The following input files need to be supplied:
# - db-compustat-na-security.csv: Compustat NA Security database (monthly)
# - db-compustat-global-security.csv: Compustat Global Security database (daily, with end-of-month data only)
# - db-G_EXRT_DLY.csv: Compustat daily exchange rate database
# They can be either created using the download-compustat-data script or downloaded manualy (e.g. from WRDS)
# Please see the file db-compustat-variables.csv for an overview of the variables needed and selection criteria used
# The file db-liva-variable-descriptions.csv contains an overview of the variables in the resulting database

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

minMc <- 0.1  # (in USD B) Include only companies wich have reached market cap of >$100M at any point in their history
years <- c(1999, 2020)  # Both years inclusive
baseYr <- 2018 # Base year for discounting LIVA (end-of-year); changing this affects all LIVAs by a constant factor
excludeCompanies <- c("123916", "033625", "181283", "313077", "290168", "015520", "034290") # GVKEYs of companies with data integrity issues
excludeCountries <- c("ZWE", "BRA", "VEN", "ARG") # Countries with data integrity issues (e.g. due to hyper-inflation)

# Read files, adjust path names as necessary
dfNA <- read_csv("db-compustat-na-security.csv")  # Compustat NA security monthly
dfGlobal <- read_csv("db-compustat-global-security.csv") # Compustat Global security daily
dfFx <- read_csv("db-G_EXRT_DLY.csv", col_types = cols(exrattpd = "c"))  # Compustat currency conversion daily

# Make currency conversion to USD
dfFx <- dfFx %>% filter(tocurd == "USD") %>% select(datadate, usd = exratd) %>%
  full_join(dfFx, .) %>%
  mutate(usdconv = exratd / usd) %>%
  select(curcd = tocurd, datadate, usdconv) %>% arrange(curcd, datadate)

# Combine and clean-up databases
df <- bind_rows(dfNA %>% rename(curcd = curcdm, prcc = prccm, trf = trfm, csho = cshom, ajex = ajexm),
                dfGlobal %>% rename(curcd = curcdd, prcc = prccd, trf = trfd, csho = cshoc, ajex = ajexdi))
df <- inner_join(df, dfFx)
df$date <- if(class(df$datadate) == "Date") df$datadate else as.Date(as.character(datadate), format = "%Y%m%d")

df <- df %>%
  filter(
    !gvkey %in% excludeCompanies, 
    !loc %in% excludeCountries, 
    !is.na(prcc + ajex + trf + csho),
    pmin(prcc, ajex, trf) >= 0.01,  # To prevent rounding issues
    date >= as.Date(paste(years[1]-1, 12, 1, sep="-")),
    date <= as.Date(paste(years[2], 12, 31, sep="-"))) %>%
  mutate(
    prc = prcc / usdconv,
    mcend = prc * csho / 1e9,
    month = as.numeric(format(date,"%Y%m")),
    year = as.numeric(format(date,"%Y")))
dfCo <- df %>% group_by(gvkey) %>% summarize(mc=max(mcend)) %>% filter(mc >= minMc)
df <- df %>%
  filter(gvkey %in% dfCo$gvkey) %>%
  arrange(gvkey, month, iid)
df <- df %>%  distinct(gvkey, month, .keep_all = T)

# Calculate TSR (for same issue in consecutive months only)
df <- inner_join(df,
                 df %>% group_by(month) %>% summarize() %>% arrange(month) %>% mutate(monthno = row_number() - 1))
df <- df %>% group_by(gvkey, iid) %>% mutate(
    cons = (monthno == lag(monthno) + 1),
    mcbeg = lag(mcend),
    tsr = (prc * trf / ajex) / lag(prc * trf / ajex) - 1
  ) %>% ungroup()
df <- df %>% filter(cons & !is.na(tsr + mcbeg) ) %>%
  mutate(usdtsr = tsr * mcbeg)

# Calculate market index and LIVA
dfMkt <- df %>% group_by(month) %>%
  summarize(
    mcbeg = sum(mcbeg),
    usdtsr = sum(usdtsr)) %>%
  mutate(
    mkttsr = usdtsr / mcbeg,
    lmkttsr = log1p(mkttsr),
    lmktex = cumsum(lmkttsr),
    index = exp(lmktex),
    mcindex = lead(mcbeg)/first(mcbeg),
    delta = index[month == baseYr * 100 + 12] / index)
df <- inner_join(df, dfMkt %>% select(month, mkttsr, delta))
df <- df %>% mutate(
  ler = log1p(tsr) - log1p(mkttsr),
  liva = (tsr - mkttsr) * mcbeg * delta,
  cf = (1 + tsr) * mcbeg - mcend,
  livac = cf * delta,  # Cash contribution to LIVA
  mcadj = mcbeg * delta
)
dfCoYr <- df %>% group_by(gvkey, year) %>% summarize(
  conm = last(conm),
  loc = last(loc),
  gsubind = last(gsubind),
  liva = sum(liva),
  ler = sum(ler),
  mcbeg = first(mcbeg),
  mcend = last(mcend),
  nmo = n()
) %>% ungroup()
stopifnot(all(abs((dfCoYr %>% group_by(year) %>% summarize(liva = sum(liva)))$liva) < 1e-10)) # LIVA for each year should be 0

dfCo <- dfCoYr %>% group_by(gvkey) %>% summarize(
  conm = last(conm),
  loc = last(loc),
  gsubind = last(gsubind),
  liva = sum(liva),
  ler = sum(ler),
  mcbeg = first(mcbeg),
  mcend = last(mcend),
  begin = first(year),
  end = last(year),
  nmo = sum(nmo)
) %>% mutate(
  er = expm1(ler / (end - begin))
) %>% arrange(-liva) %>%
  mutate(
    cumliva = cumsum(liva),
    rank = row_number())

# Write database to CSV file
write_csv(dfCoYr, "db-liva.csv")

# liva-public
R code to create and use the global LIVA database and web app.
There are a couple of different ways to use this repository:

A. Download the complete global LIVA database 1999-2011 from the following files:
- [db-liva.csv](db-liva.csv) : LIVA by company-year ([direct download](https://raw.githubusercontent.com/phebo/liva-public/master/db-liva.csv))
- [db-gics.csv](db-gics.csv) : Description of GICS codes ([direct download](https://raw.githubusercontent.com/phebo/liva-public/master/db-gics.csv))
- [db-countries.csv](db-countries.csv) : Description of country codes ([direct download](https://raw.githubusercontent.com/phebo/liva-public/master/db-countries.csv))

B. Recreate the database, for instance to build it with a different set of countries / time periods, or with different parameters / methods.
Recreating the database from scratch can also be useful for replication or understanding the precise methodology used. Recreation requires two steps:
1. Download the security information from the Compustat NA and Global databases. This can be done either with the script [download-compustat-data.R](download-compustat-data.R) directly accessing the WRDS (Wharton Research Data Services) cloud (make sure to follow the WRDS set-up instructions and add your WRDS username), or manually based on the variables outlined in the file [db-compustat-variables.csv](db-compustat-variables.csv).
2. Build the LIVA database using the [build-liva-dbase.R](build-liva-dbase.R) script. This should not take more than a few minutes on a regular PC.
The LIVA database in this repo was created using the two scripts mentioned above.

C. Adapt the web app in the script [app.R](app.R). A working version of the app is posted on https://phebo.shinyapps.io/liva-public/.

For more information see www.liva-measure.com.
Scientific methods and background can be found in Wibbens, P. D., & Siggelkow, N. (2020). Introducing LIVA to measure long‐term firm performance. *Strategic Management Journal*, 41(5), 867-890.
The paper is available open access for everyone via https://doi.org/10.1002/smj.3114.
Please cite the paper when using any LIVA data in your work.

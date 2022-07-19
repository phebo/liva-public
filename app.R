# "Shiny" web app as published on www.liva-measure.com
# This app needs the Global LIVA database as input in the file db-liva.csv
# This file can be obtained through either of the following methods:
# - using the script download-liva-dbase.R to download from the WRDS database
# - directly downloaded from www.liva-measure.com 
# - generated using the script build-liva-dbase.R
# The app also needs the db-gics.csv and db-countries.csv files that are included in this repository

# Copyright (C) 2019-2022, Phebo Wibbens and Nicolaj Siggelkow
  
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

library(shiny)
library(tidyverse)

df <- read_csv("db-liva.csv")
dfGics <- read_csv("db-gics.csv")
dfLoc <- read_csv("db-countries.csv")

df <- left_join(df %>% mutate(gics4d = gsubind %/% 1e4),
                dfGics %>% select(gics4d, sector))
df <- left_join(df, dfLoc)
df <- df %>% mutate(sector = ifelse(is.na(gics4d), "Not recorded", sector)) %>%
  mutate(gics4d = ifelse(is.na(gics4d), 9999, gics4d)) %>%
  mutate(sector = ifelse(gics4d == 2540, "Media & Entertainment", sector)) %>%   # GICS 2540 ("Media") has been re-classified
  mutate(sector = ifelse(gics4d == 4040, "Real Estate", sector)) %>%   # GICS 2540 ("Media") has been re-classified
  mutate(country = ifelse(is.na(country), "Not recorded", country))

sectors <- c("All", df %>% group_by(sector) %>% summarize(gics4d = last(gics4d)) %>% arrange(gics4d) %>% pull(sector))
countries <- c("All", df %>% group_by(country) %>% summarize(mc = sum(mcend)) %>% arrange(-mc) %>% pull(country))
years <- with(df, c(min(year), max(year)))

convString <- function(vec, n=30) {
  # Make a vector of strings lower case except for first character, and truncate to first n characters
  paste0(substr(vec, 1, 1),
         tolower(substr(vec, 2, n)))
}

ui <- fluidPage(
  titlePanel("Top and bottom performers in the global LIVA database"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("years", "Total over all years from / to", min = years[1], max = years[2], value = c(years[2]-19, years[2]), sep=""),
      #checkboxGroupInput("active", "Only include companies active in", c("First year" = "first", "Last year" = "last"), inline=T),
      selectInput("country", "Country", countries),
      selectInput("sector", "Sector", sectors),
      radioButtons("show", "Show", c("Companies", "Countries", "Sectors")),
      checkboxInput("table", "Add table below chart"),
      sliderInput("nco", "Maximum number of rows", min = 10, max = 100, value = 20, step=10),
      textInput("search", "Text search"),
      #p(tags$small("*Statutory headquarter location.")),
      #p(tags$small("**Sector based on 4-digit GICS classification.")),
      #p(tags$small("Source: Wibbens & Siggelkow 2019; WRDS; Compustat")),
      p(tags$small(HTML("&copy 2019-2022 Phebo Wibbens and Nicolaj Siggelkow")))
    ),
    mainPanel(plotOutput("plot", inline=T),
              tableOutput("table"))
  )
)

server <- function(input, output) {
  newData <- reactive({
    dfSub <- df %>% filter(year >= input$years[1], year <= input$years[2])
    if(input$sector != "All") dfSub <- dfSub %>% filter(sector == input$sector)
    if(input$country != "All") dfSub <- dfSub %>% filter(country == input$country)
    dfSum <- dfSub %>% group_by(gvkey) %>%
      summarize(conm=last(conm), liva = sum(liva), first=first(year), last=last(year),
                country=last(country), sector = last(sector))
    #if("first" %in% input$active) dfSum <- dfSum %>% filter(first == input$years[1])
    #if("last" %in% input$active) dfSum <- dfSum %>% filter(last == input$years[2])
    
    if(input$show == "Countries") {
      dfSum <- dfSum %>% group_by(name = country) %>% summarize(liva = sum(liva), nco = n())
    } else if(input$show == "Sectors") {
      dfSum <- dfSum %>% group_by(name = sector) %>% summarize(liva = sum(liva), nco = n())
    } else {
      dfSum <- dfSum %>% mutate(name = convString(conm)) %>% group_by(name) %>%
        summarize(conm=last(conm), liva = sum(liva), first=first(first), last=last(last),
                  country=last(country), sector = last(sector))
      # This allows for multiple gvkeys with same name to be combined, e.g. SK Holdings Co Ltd (gvkeys 209610 and 293194)
    }
    if(input$search != "") dfSum <- dfSum %>% filter(grepl(input$search, name, ignore.case = TRUE))
    if(nrow(dfSum) > input$nco) {
      dfSum <- dfSum %>% filter(min_rank(liva) <= input$nco  %/% 2 | min_rank(-liva) <= input$nco %/% 2) %>%
        mutate(tb = factor(ifelse(min_rank(-liva) <= input$nco  %/% 2, "Top", "Bottom"), levels = c("Top", "Bottom")))
    }
    dfSum
  })
  
  output$plot <- renderPlot({
    dfSum <- newData()
    ggOut <- ggplot(dfSum, aes(x=reorder(name, liva), y=liva)) +
      geom_bar(stat="identity") +
      coord_flip() + ylab('LIVA ($B)') + xlab("") + scale_y_continuous(position = "right") +
      theme(text = element_text(size=18))
    if("tb" %in% names(dfSum)) ggOut <- ggOut + facet_grid(tb ~ ., scales = "free_y")
    ggOut
  }, height = function() {dfSum <- newData(); 250 + 16 * nrow(dfSum)},
  width = 550)
  
  output$table <- renderTable({
    if(input$table) {
      dfSum <- newData()
      if(input$show == "Countries") {
        dfSum %>% arrange(-liva) %>% select(`Country` = name, `LIVA ($B)` = liva, `Number of companies` = nco)
      } else if(input$show == "Sectors") {
        dfSum %>% arrange(-liva) %>% select(`Sector` = name, `LIVA ($B)` = liva, `Number of companies` = nco)
      } else {
        dfSum %>% arrange(-liva) %>% mutate(first = as.integer(first), last = as.integer(last)) %>%
          select(`Company name` = name, `LIVA ($B)` = liva, Country = country, Sector = sector, From = first, To = last)
      }
    } else {
      NULL
    }
  }, digits = 1)
}

shinyApp(ui = ui, server = server)

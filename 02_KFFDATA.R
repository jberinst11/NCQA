
install.packages("readr")
install.packages("data.table")
install.packages("XML")
install.packages("rjson")
install.packages("jsonlite")
install.packages("rvest")
install.packages('R.utils')


library(readr)
library(data.table)
library(XML)
library(rjson)
library(jsonlite)
library(rvest)
library(R.utils)




url <- "https://www.kff.org/other/state-indicator/total-residents/?currentTimeframe=0&print=true&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D"
  
  download.file(url, KFFdata, "libcurl")  

#Andrew Help me. I need to load the following website. I cannot figure out how to do this


#Load Table Total Number of Residents in 2016 from KFF using CSV file
#https://www.kff.org/other/state-indicator/total-residents/?currentTimeframe=0&print=true&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

pop_2016 <-fread("TotalResidents.csv")
removedrows <- "United States"
pop_2016 <- pop_2016[-1, ]

View(pop_2016)

#Load Table Total Number of Residents in 2016 from KFF XML(scrapping from web)
#Did not work

pop_2016_KFF ="https://www.kff.org/other/state-indicator/total-residents/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D"
pop_2016_table = readHTMLTable(mydf, header=TRUE, which =1)
View(pop_2016_table)

#Will try again using Json files

website1 <- "https://forms.hubspot.com/lead-flows-config/v1/config/json?portalId=292449&utk=2e19f08a9ef2d997189118391cda264d"
mydata1 <- fromJSON(website1)

View(mydata1)

#Will try using fread

popdataKFF2016 <- fread("https://www.kff.org/other/state-indicator/total-residents/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D")


url <- 'https://www.kff.org/other/state-indicator/total-residents/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D'
popkff2016 <- read_html(url)
View(popkff2016)


#Calen was not here.. Sorry no where to be found



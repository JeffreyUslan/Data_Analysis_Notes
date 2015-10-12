install.packages('Rtools')
library(Rtools)
install.packages('devtools')
library(devtools)
devtools::install_github('rstudio/shinyapps')
library(shinyapps)
shinyapps::setAccountInfo(name='jeffreyuslan',
                          token='A54FA631BF1058C2E41FBFCE1927216E',
                          secret='shpJHfvkxQkodp/6D7tv3mord0MYRIquMRA9FuAV')

shinyapps::deployApp()

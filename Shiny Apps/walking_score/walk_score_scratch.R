install.packages("walkscoreAPI")
library(walkscoreAPI)


test <- geoloc("5414 S Lucile St Seattle WA", "AIzaSyARnPYq3ciuwH93jy8z8Ejlid1I-qXMKeg")
test <- geoloc("5414 S Lucile St Seattle WA", "AIzaSyBYJbN25Bg_6UbL_FuoyMcv4GaH8nE7Yq4")

getWS(test$coordinates[1],test$coordinates[2], "d8bd555d73df2ee432d664e9dddc8def")
getTS(test$coordinates[1],test$coordinates[2],"Seattle","WA","d8bd555d73df2ee432d664e9dddc8def")

library(sf) # simple features data package
library(sp) # spatial data package
library(dplyr)
library(openxlsx)
library(stringr)

doc <- read.xlsx("C:/Users/skatw/Documents/thriftstores.xlsx") %>% 
 rowwise() %>% # so that it applies the mutate function by each row instead of as one table
  mutate(Addr = str_split(Address, ",")[[1]][1],
         City = str_split(Address, ",")[[1]][2],
         Zip = str_split(str_split(Address, ",")[[1]][3], " ")[[1]][3])


#print(str_split(str_split(doc$Address, ",")[[1]][3], " "))
#5252 N Clark St, Chicago, IL 60640

write.csv(doc, "C:/Users/skatw/Documents/thriftstores.csv")
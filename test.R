library(dplyr)
library(tidyr)
library(stringr)
library(lwnnr)
library(readxl)

test <- load_salaries() %>%
	add_fangraph_ids() %>%
	add_steamer() %>%
	split_steamer()

write.csv(test,"F:/Documents/LWNN/steamer.csv",na="",row.names=FALSE)

library(dplyr)
library(tidyr)
library(stringr)
library(lwnnr)
library(readxl)
library(openxlsx)
library(baseballr)
library(xml2)
library(rvest)

update_current_year_stats()
output_lwnn_basic("F:/Documents/LWNN/dmb.xlsx","2019-05-06")
output_lwnn_unsigned("F:/Documents/LWNN/unsigned.xlsx","2019-05-06")

# Create injury report list
payload <- read_html("http://www.milb.com/milb/stats/stats.jsp?t=l_trn&lid=122&sid=l122")
leaders <- payload %>%
	html_nodes("table") %>%
	`[[`(4) %>% html_table(fill=TRUE)

# Average Age

avg_age <- load_salaries() %>%
	add_fangraph_ids() %>%
	add_dmb() %>%
	add_prospects() %>%
	add_zips_reduced() %>%
	filter(lwnn == 1 & !is.na(Age)) %>%
	select(team,Age) %>%
	group_by(team) %>%
	summarise(avg_age=mean(Age)) %>%
	arrange(avg_age) %>%
	mutate(avg_age=round(avg_age,digits=2))

write.csv(avg_age,file="F:/Documents/LWNN/avg_age.csv", row.names = FALSE)


# Best current hitter
best_current_hitter <- load_salaries() %>%
	add_fangraph_ids() %>%
	add_current_season_reduced() %>%
	filter(lwnn == 1 & !is.na(wOBA_2019) & PA_2019>30) %>%
	select(team,name,wOBA_2019,PA_2019,Team_2019) %>%
	arrange(-wOBA_2019) %>%
	group_by(team) %>%
	mutate(best_hitter=row_number()) %>%
	filter(best_hitter==1)

write.csv(best_current_hitter,file="F:/Documents/LWNN/best_hitter.csv",row.names=FALSE)

# Worst current hitter
worst_current_hitter <- load_salaries() %>%
	add_current_season_reduced() %>%
	filter(lwnn == 1 & !is.na(wOBA_2019) & PA_2019>30) %>%
	select(team,name,wOBA_2019,PA_2019,Team_2019) %>%
	arrange(wOBA_2019) %>%
	group_by(team) %>%
	mutate(worst_hitter=row_number()) %>%
	filter(worst_hitter==1)

write.csv(worst_current_hitter,file="F:/Documents/LWNN/worst_hitter.csv",row.names=FALSE)

# Old and Useless
old_useless_pitchers <- load_salaries() %>%
	add_current_season_reduced() %>%
	filter(lwnn == 1 & !is.na(FIP_2019) & Age > 30 & FIP_2019 > 4) %>%
	arrange(-FIP_2019) %>%
	select(name,team,salary_2019,Age,FIP_2019)

write.csv(old_useless_pitchers,file="F:/Documents/LWNN/old_bad_pitchers.csv")

mexican_league <- load_salaries() %>%
	add_current_season_reduced() %>%
	filter(lwnn==1 & Team_2019 == "Mexican (AAA)" & !is.na(FIP_2019)) %>%
	select(name,team,salary_2019,Age,FIP_2019)


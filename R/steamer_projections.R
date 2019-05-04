library(dplyr)
library(tidyr)
library(stringr)


#' add_steamer
#'
#' @param data_initial
#'
#' @return
#' @export
#'
#' @examples
add_steamer <- function(data_initial) {
  steamer_hitters <- read.csv("F:/Documents/LWNN/fangraphs/steamer_hitters.csv",stringsAsFactors = FALSE) %>%
  	select(-starts_with("X")) %>%
  	select(-Team) %>%
  	mutate(type="hitter") %>%
  	rename(name=Name,fangraph_id=playerid)
  steamer_pitchers <- read.csv("F:/Documents/LWNN/fangraphs/steamer_pitchers.csv",stringsAsFactors = FALSE) %>%
  	select(-Team) %>%
  	mutate(type="pitcher") %>%
  	rename(name=Name,fangraph_id=playerid)
  steamer_joined <- coalesce_join(steamer_hitters,steamer_pitchers,by="fangraph_id") %>%
  	select(-name)
  data_w_steamer <- left_join(data_initial,steamer_joined,by="fangraph_id")
  return(data_w_steamer)
}


#' Outputs Excel files
#'
#' @param steamer_data
#'
#' @return
#' @export
#'
#' @examples
output_steamer <- function(steamer_data) {
	if("Pos" %in% colnames(steamer_data))
	{
		steamer_data <- steamer_data %>%
			mutate(position=coalesce(position,Pos)) %>%
			mutate(type=case_when(!is.na(type) ~ type
							  ,Pos %in% c("RHP","LHP") ~ "pitcher"
							  ,!is.na(Pos) ~ "hitter"
							  ,TRUE ~ "")) %>%
			select(-Pos)
	}
  steamer_data_pitchers <- steamer_data %>%
  	filter(type=="pitcher") %>%
  	select(-PA,-AB,-R,-RBI,-HBP,-SB,-CS,-AVG,-OBP,-SLG,-OPS,-wOBA,-wRC.,-BsR,-Fld,-Off,-Def,-type,-fangraph_id,-duplicate_name)

  steamer_data_hitters <- steamer_data %>%
  	filter(type=="hitter") %>%
  	select(-W,-L,-ERA,-GS,-SV,-IP,-ER,-WHIP,-K9,-BB9,-FIP,-RA9_WAR,-type,-fangraph_id,-duplicate_name)

  write.xlsx(as.data.frame(steamer_data_pitchers), file="F:/Documents/LWNN/lwnn_steamer.xlsx",
  		 sheetName="Pitchers",showNA=FALSE,row.names=FALSE)
  write.xlsx(as.data.frame(steamer_data_hitters), file="F:/Documents/LWNN/lwnn_steamer.xlsx",
  		 sheetName="Hitters",showNA=FALSE,row.names=FALSE,append=TRUE)
}

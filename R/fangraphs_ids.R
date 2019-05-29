library(dplyr)
library(tidyr)
library(stringr)
library(baseballr)


#' add_fangraphs_ids
#'
#' @param data_initial
#'
#' @return
#' @export
#'
#' @examples
add_fangraph_ids <- function(data_initial) {

	mlb_hitters_ids <- read.csv("F:/Documents/LWNN/fangraphs/hitters_mlb.csv",stringsAsFactors = FALSE) %>%
		rename(name=Name) %>%
		mutate(fangraph_id=as.character(playerid)) %>%
		distinct(fangraph_id,name)

	mlb_pitchers_ids <- read.csv("F:/Documents/LWNN/fangraphs/pitchers_mlb.csv",stringsAsFactors = FALSE) %>%
		rename(name=Name) %>%
		mutate(fangraph_id=as.character(playerid)) %>%
		distinct(fangraph_id,name)

	minors_hitters_ids <- read.csv("F:/Documents/LWNN/fangraphs/hitters_minors.csv",stringsAsFactors = FALSE) %>%
		rename(name=Name,fangraph_id=playerid) %>%
		distinct(name,fangraph_id)

	minors_pitchers_ids <- read.csv("F:/Documents/LWNN/fangraphs/pitchers_minors.csv",stringsAsFactors = FALSE) %>%
		rename(name=Name,fangraph_id=playerid) %>%
		distinct(name,fangraph_id)

	deduped_current <- mlb_hitters_ids %>%
		rbind(mlb_pitchers_ids,minors_hitters_ids,minors_pitchers_ids) %>%
		fangraph_id_changes() %>%
		fangraph_name_changes() %>%
		distinct() %>%
		arrange(fangraph_id) %>%
		group_by(name) %>%
		mutate(keep_num=row_number()) %>%
		filter(keep_num==1) %>%
		ungroup() %>%
		select(name,fangraph_id)

	deduped_historic <- read.csv("F:/Documents/LWNN/fangraphs/player_ids.csv",stringsAsFactors = FALSE) %>%
		fangraph_id_changes() %>%
		fangraph_name_changes() %>%
		distinct() %>%
		arrange(fangraph_id) %>%
		group_by(name) %>%
		mutate(keep_num=row_number()) %>%
		filter(keep_num==1) %>%
		ungroup() %>%
		select(name,fangraph_id)

	fangraph_ids <- coalesce_join(deduped_current,deduped_historic,by="name")

	write.csv(fangraph_ids,file="F:/Documents/LWNN/fangraphs/current_fangraph_ids.csv",row.names=FALSE)

  data_w_ids <- data_initial %>%
  	left_join(fangraph_ids,by="name") %>%
  	mutate(fangraph_id_non_match=case_when(is.na(fangraph_id)~1,
  								    TRUE~0)) %>%
  	mutate(fangraph_id=case_when(is.na(fangraph_id) ~ name,
  				  TRUE~fangraph_id))
  return(data_w_ids)
}

#' fangraph_name_changes
#'
#' @param initial_data
#'
#' @return
#' @export
#'
#' @examples
fangraph_name_changes <- function(initial_data) {
  updated_names <- initial_data %>%
  	mutate(name = case_when(fangraph_id=="sa962605"~"Luis Victoriano Garcia",
  					    fangraph_id=="sa3007295"~"Luis Jose Garcia",
  					    fangraph_id=="sa873265"~"Will D. Smith",
  					    TRUE~name))
  return(updated_names)
}


#' prospect_name_updates
#'
#' @param data_initial
#'
#' @return
#' @export
#'
#' @examples
prospect_name_updates <- function(data_initial) {
	updated_names <- data_initial %>%
		mutate(name=case_when(name=="Will Smith"~"Will D. Smith",
						  name=="Luis Garcia" & Signed=="2016 J2 (WSN)" ~ "Luis Victoriano Garcia",
						  name=="Luis Garcia" & Signed=="2017 J2 (PHI)" ~ "Luis Jose Garcia",
					  TRUE~name))
	return(updated_names)
}


#' fangraph_id_changes
#'
#' @param initial_data
#'
#' @return
#' @export
#'
#' @examples
fangraph_id_changes <- function(initial_data) {
  updated_ids <- initial_data %>%
  	mutate(fangraph_id = case_when(name=="Jose Martinez"~"7996",
  							 name=="Abraham Almonte"~"5486",
  							 name=="Alex Jackson"~"17276",
  							 name=="Alfredo Gonzalez"~"14267",
  							 name=="Anthony Garcia"~"sa3005924",
  							 name=="Wander Franco"~"sa3007033",
  							 name=="Julio Rodriguez"~"sa3005720",
  							 name=="Kevin Smith"~"sa915864",
  							 name=="Luis Gonzalez"~"sa3002721",
  							 name=="Tyler Alexander"~"sa738285",
  							 name=="Luis Medina"~"sa928510",
  							 TRUE~fangraph_id
  	))
  return(updated_ids)
}



#' format_dmb_names
#'
#' @param data_initial
#'
#' @return
#' @export
#'
#' @examples
format_dmb_names <- function(data_initial) {
  dmb_to_fangraph_names <- data_initial %>%
  	mutate(name=case_when(name=="Albert Almora"~"Albert Almora Jr.",
  				  name=="Alex de Goti"~"Alex De Goti",
  				  name=="B.J. Boyd"~"BJ Boyd",
  				  name=="Brock Holt!"~"Brock Holt",
  				  name=="Steven Souza"~"Steven Souza Jr.",
  				  name=="Fernando Tatis"~"Fernando Tatis Jr.",
  				  name=="Hyun-jin Ryu"~"Hyun-Jin Ryu",
  				  name=="Jacob Faria"~"Jake Faria",
  				  name=="Michael Clevinger"~"Mike Clevinger",
  				  name=="Seung-hwan Oh"~"Seunghwan Oh",
  				  name=="Cedric Mullins"~"Cedric Mullins II",
  				  name=="Chad de la Geurra"~"Chad De La Guerra",
  				  name=="Chris B. Young Jr."~"Chris Young",
  				  name=="Chris Bostick"~"Christopher Bostick",
  				  name=="KikÃ© Hernandez"~"Enrique Hernandez",

  				  TRUE~name))
  return(dmb_to_fangraph_names)
}

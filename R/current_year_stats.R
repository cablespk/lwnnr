library(xml2)
library(rvest)
library(baseballr)

#' add_current_season_reduced
#'
#' @param data_initial
#'
#' @return
#' @export
#'
#' @examples
add_current_season_reduced <- function(data_initial) {
  hitters_minors <- read.csv("F:/Documents/LWNN/fangraphs/hitters_minors.csv",stringsAsFactors = FALSE) %>%
  	rename(Team_2019 = Team,
  		  fangraph_id=playerid,
  		  wOBA_2019=wOBA,
  		  PA_2019=PA,
  		  name=Name) %>%
  	select(fangraph_id,name,Age,Team_2019,PA_2019,wOBA_2019) %>%
  	mutate(type="hitter",level="minors")

  hitters_mlb <- read.csv("F:/Documents/LWNN/fangraphs/hitters_mlb.csv",stringsAsFactors = FALSE) %>%
  	rename(wOBA_2019 = wOBA,
  		  PA_2019 = PA,
  		  Team_2019 = Team,
  		  name=Name) %>%
  	mutate(fangraph_id=as.character(playerid)) %>%
  	select(fangraph_id,name,Age,Team_2019,PA_2019,wOBA_2019) %>%
  	mutate(type="hitter",level="mlb")

  pitchers_minors <- read.csv("F:/Documents/LWNN/fangraphs/pitchers_minors.csv",stringsAsFactors = FALSE) %>%
  	rename(FIP_2019 = FIP,
  		  IP_2019 = IP,
  		  Team_2019 = Team,
  		  fangraph_id=playerid,
  		  name=Name) %>%
  	select(fangraph_id,name,Age,Team_2019,IP_2019,FIP_2019) %>%
  	mutate(type="pitcher",level="minors")

  pitchers_mlb <- read.csv("F:/Documents/LWNN/fangraphs/pitchers_mlb.csv",stringsAsFactors = FALSE) %>%
  	rename(FIP_2019 = FIP,
  		  IP_2019 = IP,
  		  Team_2019 = Team,
  		  name=Name) %>%
  	mutate(fangraph_id=as.character(playerid)) %>%
  	select(fangraph_id,name,Age,Team_2019,IP_2019,FIP_2019) %>%
  	mutate(type="pitcher",level="mlb")

  current_season <- coalesce_join(hitters_mlb,pitchers_mlb,by="fangraph_id") %>%
  	coalesce_join(hitters_minors,by="fangraph_id") %>%
  	coalesce_join(pitchers_minors,by="fangraph_id")

  data_w_current <- data_initial %>%
  	coalesce_join(current_season,by="fangraph_id")

  return(data_w_current)
}


#' fg_minor_bat_leaders
#'
#' @return
#' @export
#'
#' @examples
fg_minor_bat_leaders <- function() {

		payload <- read_html("https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=all&qual=10&type=1&season=2019&team=0&players=0&page=1_5000")
		leaders <- payload %>%
			html_nodes("table") %>%
			`[[`(4) %>% html_table(fill=TRUE)

		leaders <- leaders[-c(1,3),]
		names(leaders) <- leaders[1,]
		leaders <- leaders[-1,]

		playerids <- payload %>%
			html_nodes("table") %>%
			.[[4]] %>%
			html_nodes("a") %>%
			html_attr("href") %>%
			as.data.frame() %>%
			rename(slug = '.') %>%
			filter(grepl("playerid", slug)) %>%
			mutate(playerid = sub(".*[=] *(.*?) *[&].*", "\\1", slug))

		leaders <- leaders %>%
			mutate(playerid = playerids$playerid) %>%
			select(playerid, everything()) %>%
			mutate(wOBA = as.double(wOBA),
				  PA = as.double(PA))

		return(leaders)
}

#' fg_minor_pitch_leaders
#'
#' @return
#' @export
#'
#' @examples
fg_minor_pitch_leaders <- function() {

	payload <- read_html("https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=pit&lg=all&qual=10&type=1&season=2019&team=0&players=0&page=1_5000")
	leaders <- payload %>%
		html_nodes("table") %>%
		`[[`(4) %>% html_table(fill=TRUE)

	leaders <- leaders[-c(1,3),]
	names(leaders) <- leaders[1,]
	leaders <- leaders[-1,]

	playerids <- payload %>%
		html_nodes("table") %>%
		.[[4]] %>%
		html_nodes("a") %>%
		html_attr("href") %>%
		as.data.frame() %>%
		rename(slug = '.') %>%
		filter(grepl("playerid", slug)) %>%
		mutate(playerid = sub(".*[=] *(.*?) *[&].*", "\\1", slug))

	leaders <- leaders %>%
		mutate(playerid = playerids$playerid) %>%
		select(playerid, everything()) %>%
		mutate(IP = as.double(IP),
			  FIP = as.double(FIP))

	return(leaders)
}


#' Update Current Year Stats
#'
#' @return
#' @export
#'
#' @examples
update_current_year_stats <- function() {
  hitters_minors <- fg_minor_bat_leaders()
  write.csv(hitters_minors,file="F:/Documents/LWNN/fangraphs/hitters_minors.csv", row.names = FALSE)
  write.csv(hitters_minors,file=paste0("F:/Documents/LWNN/fangraphs/hitters_minors_",Sys.Date(),".csv"))

  hitters_mlb <- fg_bat_leaders(2019,2019,league="all",qual="20",ind=0)
  write.csv(hitters_mlb,file="F:/Documents/LWNN/fangraphs/hitters_mlb.csv", row.names = FALSE)
  write.csv(hitters_mlb,file=paste0("F:/Documents/LWNN/fangraphs/hitters_mlb_",Sys.Date(),".csv"))

  pitchers_minors <- fg_minor_pitch_leaders()
  write.csv(pitchers_minors,file="F:/Documents/LWNN/fangraphs/pitchers_minors.csv", row.names = FALSE)
  write.csv(pitchers_minors,file=paste0("F:/Documents/LWNN/fangraphs/pitchers_minors_",Sys.Date(),".csv"))

  pitchers_mlb <- fg_pitch_leaders(2019,2019,league="all",qual="10",pitcher_type="pit",ind=0)
  write.csv(pitchers_mlb,file="F:/Documents/LWNN/fangraphs/pitchers_mlb.csv", row.names = FALSE)
  write.csv(pitchers_mlb,file=paste0("F:/Documents/LWNN/fangraphs/pitchers_mlb_",Sys.Date(),".csv"))
}

#' Add Previous Stats
#'
#' @param comparison_date
#'
#' @return
#' @export
#'
#' @examples
add_prev_stats <- function(comparison_date) {
  pitchers_prev_mlb <- read.csv(file=paste0("F:/Documents/LWNN/fangraphs/pitchers_mlb_",comparison_date,".csv"), stringsAsFactors = FALSE) %>%
  	rename(fangraph_id=playerid,
  		  FIP_prev=FIP) %>%
  	select(fangraph_id,FIP_prev) %>%
  	mutate(fangraph_id=as.character(fangraph_id))
  pitchers_prev_minors <- read.csv(file=paste0("F:/Documents/LWNN/fangraphs/pitchers_minors_",comparison_date,".csv"), stringsAsFactors = FALSE) %>%
  	rename(fangraph_id=playerid,
  		  FIP_prev=FIP) %>%
  	select(fangraph_id,FIP_prev)
  hitters_prev_mlb <- read.csv(file=paste0("F:/Documents/LWNN/fangraphs/hitters_mlb_",comparison_date,".csv"), stringsAsFactors = FALSE) %>%
  	rename(fangraph_id=playerid,
  		  wOBA_prev=wOBA) %>%
  	select(fangraph_id,wOBA_prev) %>%
  	mutate(fangraph_id=as.character(fangraph_id))
  hitters_prev_minors <- read.csv(file=paste0("F:/Documents/LWNN/fangraphs/hitters_minors_",comparison_date,".csv"), stringsAsFactors = FALSE) %>%
  	rename(fangraph_id=playerid,
  		  wOBA_prev=wOBA) %>%
  	select(fangraph_id,wOBA_prev)
  prev_stats <- pitchers_prev_mlb %>%
  	coalesce_join(pitchers_prev_minors,by="fangraph_id") %>%
  	coalesce_join(hitters_prev_mlb,by="fangraph_id") %>%
  	coalesce_join(hitters_prev_minors,by="fangraph_id") %>%
  	mutate(comparison_date=comparison_date)
}

#' Add Comparison Stats
#'
#' @param player_df_initial
#' @param comparison_date
#'
#' @return
#' @export
#'
#' @examples
add_comparison_stats <- function(player_df_initial,comparison_date) {
	comp_date <- comparison_date
  previous_stats <- add_prev_stats(comp_date)
  comparison_added_df <- player_df_initial %>%
  	left_join(previous_stats,by="fangraph_id") %>%
  	mutate(FIP_Change = FIP_2019 - FIP_prev,
  		  wOBA_Change = wOBA_2019 - wOBA_prev)
  return(comparison_added_df)
}




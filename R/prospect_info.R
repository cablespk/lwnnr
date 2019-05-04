
#' Add prospects
#'
#' @param player_df
#'
#' @return
#' @export
#'
#' @examples
  add_prospects <- function(player_df) {
    fangraphs_board <- read.csv("F:/Documents/LWNN/fangraphs/fangraphs-the-board-data.csv",stringsAsFactors = FALSE) %>%
    	rename(name=Name,Top_100=Top.100,POS=Pos) %>%
    	mutate(type=case_when(POS %in% c("LHP","RHP") ~ "pitcher",
    					  TRUE~"hitter"),
    		  Age=as.integer(Age)) %>%
    	prospect_name_updates() %>%
    	select(name,POS,FV,ETA,Risk,Age,Top_100,type) %>%
    	add_fangraph_ids()

    merged_df <-  coalesce_join(player_df,fangraphs_board,by="fangraph_id")
    return(merged_df)
  }



#' Draft 2018
#'
#' @param player_df
#'
#' @return
#' @export
#'
#' @examples
add_draft_2018 <- function(player_df) {
  mlb_draft_2018 <- read.csv("F:/Documents/LWNN/fangraphs/mlb_draft_2018.csv",stringsAsFactors = FALSE) %>%
  	filter(PLAYER!="",
  		  SIGNING.BONUS!="",
  		  SIGNING.BONUS!="--") %>%
  	separate(PLAYER,c("last_name","first_name"),sep=", ") %>%
  	mutate(name=str_c(first_name,last_name,sep=" "),
  		  draft_2018=paste0(RD,".",PICK)) %>%
  	add_fangraph_ids() %>%
  	select(name,POS,fangraph_id,draft_2018)

  merged_df <- coalesce_join(player_df,mlb_draft_2018,by="fangraph_id")
  return(merged_df)
}

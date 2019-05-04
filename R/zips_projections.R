

#' DMB Initialize
#'
#' @return
#' @export
#'
#' @examples
dmb_initialize <- function() {
  # dmb_zips_hitters <- read.xlsx(file="F:/Documents/LWNN/dmb/ZiPS DMB 2019.xlsx",sheetName="Hitters and Ratings",startRow=3,header=TRUE,stringsAsFactors=FALSE)
  # dmb_zips_pitchers <- read.xlsx(file="F:/Documents/LWNN/dmb/ZiPS DMB 2019.xlsx",sheetName="Pitchers and Ratings",startRow=3,header=TRUE,stringsAsFactors=FALSE)
  # write.csv(dmb_zips_hitters,file="F:/Documents/LWNN/dmb/hitters.csv", row.names=FALSE)
  # write.csv(dmb_zips_pitchers,file="F:/Documents/LWNN/dmb/pitchers.csv", row.names=FALSE)

  dmb_hitters <- read.csv("F:/Documents/LWNN/dmb/hitters.csv",stringsAsFactors = FALSE)
  dmb_pitchers <- read.csv("F:/Documents/LWNN/dmb/pitchers.csv",stringsAsFactors = FALSE)

  dmb_hitters_fmt <- dmb_hitters %>%
  	rename(name=Player) %>%
  	format_dmb_names() %>%
  	add_fangraph_ids()

  dmb_pitchers_fmt <- dmb_pitchers %>%
  	rename(name=Player) %>%
  	format_dmb_names() %>%
  	add_fangraph_ids()

  missing_dmb_pitchers <- dmb_pitchers_fmt %>%
  	filter(fangraph_id_non_match==1) %>%
  	select(name,ProjTm,Age)

  missing_dmb <- dmb_hitters_fmt %>%
  	filter(fangraph_id_non_match==1) %>%
  	select(name,ProjTm,Age) %>%
  	rbind(missing_dmb_pitchers)

  write.xlsx(as.data.frame(missing_dmb), file="F:/Documents/LWNN/missing_dmb.xlsx",
  		 sheetName="Missing DMB",showNA=FALSE,row.names=FALSE)

  write.csv(dmb_hitters_fmt,file="F:/Documents/LWNN/dmb/hitters_fmt.csv", row.names=FALSE)
  write.csv(dmb_pitchers_fmt,file="F:/Documents/LWNN/dmb/pitchers_fmt.csv", row.names=FALSE)
}


#' Add DMB
#'
#' @param data_initial
#'
#' @return
#' @export
#'
#' @examples
add_dmb <- function(data_initial) {
  dmb_pitchers <- read.csv("F:/Documents/LWNN/dmb/pitchers_fmt.csv",stringsAsFactors = FALSE) %>%
  	filter(!is.na(fangraph_id)) %>%
  	mutate(POS=case_when(!is.na(ST)~"SP",
  					 TRUE~"RP")) %>%
  	rename(OPS_LHB=OPS,
  		  OPS_RHB=OPS.1,
  		  STA_SP=ST,
  		  STA_RP=REL) %>%
  	select(name,Age,T,POS,IP,ERA,G.GF,OPS_LHB,OPS_RHB,STA_SP,STA_RP,fangraph_id) %>%
  	mutate(type="pitcher")

  dmb_hitters <- read.csv("F:/Documents/LWNN/dmb/hitters_fmt.csv",stringsAsFactors = FALSE) %>%
  	filter(!is.na(fangraph_id)) %>%
  	mutate(OPS_LHP=OBP.1+SLG.1,
  		  OPS_RHP=OBP.2+SLG.2,
  		  POS=paste0(ifelse(!is.na(C.RN),"CA/",""),
  		  			  ifelse(!is.na(X1B.RN),"1B/",""),
  		  			  ifelse(!is.na(X2B.RN),"2B/",""),
  		  			  ifelse(!is.na(X3B.RN),"3B/",""),
  		  			  ifelse(!is.na(SS.RN),"SS/",""),
  		  			  ifelse(!is.na(LF.RN),"LF/",""),
  		  			  ifelse(!is.na(CF.RN),"CF/",""),
  		  			  ifelse(!is.na(RF.RN),"RF/","")
  		  			  )) %>%
  	mutate(POS=str_replace(POS,"/$","")) %>%
  	select(name,Age,POS,AB,OBP,SLG,STL,OPS_LHP,OPS_RHP,X1B.RN,X1B.ER,X2B.RN,X2B.ER,X3B.RN,X3B.ER,SS.RN,SS.ER,LF.RN,LF.ER,CF.RN,CF.ER,RF.RN,RF.ER,OF.ARM,C.RN,C.ER,C.ARM,PB,fangraph_id) %>%
  	mutate(type="hitter")

  dmb_combined <- coalesce_join(dmb_hitters,dmb_pitchers,by="fangraph_id") %>%
  	mutate(dmb=1)

  data_w_dmb <- coalesce_join(data_initial,dmb_combined,by="fangraph_id")
  return(data_w_dmb)
}


#' add_zips_reduced
#'
#' @param data_initial
#'
#' @return
#' @export
#'
#' @examples
add_zips_reduced <- function(data_initial) {
	zips_hitters <- read.csv("F:/Documents/LWNN/fangraphs/zips_hitters.csv",stringsAsFactors = FALSE) %>%
		mutate(type="hitter") %>%
		rename(name=Name) %>%
		add_fangraph_ids() %>%
		select(name,wOBA,fangraph_id,type)

	zips_pitchers <- read.csv("F:/Documents/LWNN/fangraphs/zips_pitchers.csv",stringsAsFactors = FALSE) %>%
		mutate(type="pitcher") %>%
		rename(name=Name) %>%
		add_fangraph_ids()  %>%
		select(name,K.9,BB.9,FIP,fangraph_id,type)

	zips_joined <- coalesce_join(zips_hitters,zips_pitchers,by="fangraph_id")

	data_w_zips <- coalesce_join(data_initial,zips_joined,by="fangraph_id")
	return(data_w_zips)
}


#' add_zips_full
#'
#' @param data_initial
#'
#' @return
#' @export
#'
#' @examples
add_zips_full <- function(data_initial) {
  zips_hitters <- read.csv("F:/Documents/LWNN/fangraphs/zips_hitters.csv",stringsAsFactors = FALSE) %>%
  	select(-Team,-R,-RBI,-HBP,-ADP) %>%
  	mutate(type="hitter") %>%
  	rename(name=Name,fangraph_id=playerid)
  zips_pitchers <- read.csv("F:/Documents/LWNN/fangraphs/zips_pitchers.csv",stringsAsFactors = FALSE) %>%
  	select(-Team,-W,-L,-ADP) %>%
  	mutate(type="pitcher") %>%
  	rename(name=Name,fangraph_id=playerid)
  zips_joined <- coalesce_join(zips_hitters,zips_pitchers,by="fangraph_id") %>%
  	select(-name)
  data_w_zips <- coalesce_join(data_initial,zips_joined,by="fangraph_id") %>%
  	filter(!is.na(name))
  return(data_w_zips)
}



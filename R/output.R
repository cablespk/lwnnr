library(openxlsx)
library(dplyr)

#' Output LWNN Basic
#'
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
output_lwnn_basic <- function(file_name) {
	players_all <- load_salaries() %>%
		add_dmb() %>%
		add_prospects() %>%
		add_zips_reduced() %>%
		add_current_season_reduced() %>%
		add_draft_2018() %>%
		filter(lwnn == 1 | dmb == 1) %>%
		mutate(team=case_when(is.na(team) ~ "Free Agent",
						  TRUE~team))

	pitchers <- players_all %>%
		filter(type=="pitcher") %>%
		select(name,team,salary_2019,Age,POS,T,IP,ERA,FIP,
			  K.9,BB.9,OPS_LHB,OPS_RHB,G.GF,STA_SP,STA_RP,
			  Team_2019,FIP_2019,IP_2019,FV,draft_2018)

	hitters <- players_all %>%
		filter(type=="hitter") %>%
		select(name,team,salary_2019,Age,POS,
			  AB,wOBA,OBP,SLG,STL,OPS_LHP,OPS_RHP,
			  Team_2019,wOBA_2019,PA_2019,FV,draft_2018,
			  X1B.RN,X1B.ER,X2B.RN,X2B.ER,X3B.RN,X3B.ER,
			  SS.RN,SS.ER,LF.RN,LF.ER,CF.RN,CF.ER,RF.RN,
			  RF.ER,OF.ARM,C.RN,C.ER,C.ARM,PB)

	no_data <- players_all %>%
		filter(is.na(type)) %>%
		select(name,team,salary_2019)

	excel_wb <- createWorkbook()
	addWorksheet(excel_wb,"Pitchers")
	addWorksheet(excel_wb,"Hitters")
	addWorksheet(excel_wb,"No Data")

	writeDataTable(excel_wb, 1, pitchers, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium8")
	writeDataTable(excel_wb, 2, hitters, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium9")
	writeDataTable(excel_wb, 3, no_data, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium10")

	setColWidths(excel_wb, sheet = 1, cols=c(1,2,17), widths = "auto")
	setColWidths(excel_wb, sheet = 2, cols=c(1,2,5,13), widths = "auto")
	setColWidths(excel_wb, sheet = 3, cols=1:2, widths = "auto")

	saveWorkbook(excel_wb, file_name, overwrite = TRUE)

}

#' Output LWNN Unsigned
#'
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
output_lwnn_unsigned <- function(file_name) {
	players_all <- load_salaries() %>%
		add_dmb() %>%
		add_prospects() %>%
		add_zips_reduced() %>%
		add_current_season_reduced() %>%
		add_draft_2018() %>%
		filter(is.na(lwnn))

	pitchers <- players_all %>%
		filter(type=="pitcher") %>%
		select(name,Age,POS,T,IP,ERA,FIP,K.9,BB.9,
			  OPS_LHB,OPS_RHB,G.GF,STA_SP,STA_RP,
			  Team_2019,FIP_2019,IP_2019,
			  FV,ETA,Risk,draft_2018)

	hitters <- players_all %>%
		filter(type=="hitter") %>%
		select(name,Age,POS,AB,wOBA,OBP,SLG,STL,
			  OPS_LHP,OPS_RHP,Team_2019,
			  wOBA_2019,PA_2019,FV,ETA,Risk,draft_2018,
			  X1B.RN,X1B.ER,X2B.RN,X2B.ER,X3B.RN,X3B.ER,
			  SS.RN,SS.ER,LF.RN,LF.ER,CF.RN,CF.ER,RF.RN,
			  RF.ER,OF.ARM,C.RN,C.ER,C.ARM,PB)

	excel_wb <- createWorkbook()
	addWorksheet(excel_wb,"Pitchers")
	addWorksheet(excel_wb,"Hitters")

	writeDataTable(excel_wb, 1, pitchers, startRow = 1, startCol = 1, tableStyle = "TableStyleLight12")
	writeDataTable(excel_wb, 2, hitters, startRow = 1, startCol = 1, tableStyle = "TableStyleLight14")

	setColWidths(excel_wb, sheet = 1, cols=c(1,15), widths = "auto")
	setColWidths(excel_wb, sheet = 2, cols=c(1,3,11), widths = "auto")

	saveWorkbook(excel_wb, file_name, overwrite = TRUE)

}

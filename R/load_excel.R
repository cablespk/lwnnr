library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

#' Format Names
#'
#' @param player_names
#'
#' @return
#' @export
#'
#' @examples
format_names <- function(player_names) {
  formatted_names <- player_names %>%
  	mutate(name=str_replace(name,", Jr."," Jr.")) %>%
  	mutate(name = case_when(name=="Giancarlo Stanton" ~ "Stanton, Giancarlo",
  					    name=="Baez Pedro" ~ "Baez, Pedro",
  					    TRUE~name)) %>%
  	separate(name,c("last_name","first_name"),sep=", ") %>%
  	mutate(name=str_c(first_name,last_name,sep=" ")) %>%
  	select(-first_name,-last_name) %>%
  	mutate(name = case_when(name=="C.C. Sabathia" ~ "CC Sabathia",
  				  name=="D.J. LeMahieu" ~ "DJ LeMahieu",
  				  name=="Michael Fiers" ~ "Mike Fiers",
  				  name=="Yulieski Gurriel"~"Yuli Gurriel",
  				  name=="Mitch White"~"Mitchell White",
  				  name=="Steven Souza"~"Steven Souza Jr.",
  				  name=="Jacob Faria"~"Jake Faria",
  				  name=="Nick Castellanos"~"Nicholas Castellanos",
  				  name=="Jonathan Gray"~"Jon Gray",
  				  name=="Jacob Odorizzi"~"Jake Odorizzi",
  				  name=="Lance McCullers"~"Lance McCullers Jr.",
  				  name=="Vlad Guerrero Jr."~"Vladimir Guerrero Jr.",
  				  name=="Lenny Torres Jr."~"Lenny Torres",
  				  name=="Jose Adolis Garcia"~"Adolis Garcia",
  				  name=="Jose Israel Garcia"~"Jose Garcia",
  				  name=="Jasrado Chisholm"~"Jazz Chisholm",
  				  name=="Ozhaino Albies"~"Ozzie Albies",
  				  name=="Phil Ervin"~"Phillip Ervin",
  				  name=="Christopher Shaw"~"Chris Shaw",
  				  name=="Frank Montas"~"Frankie Montas",
  				  name=="Shedric Long"~"Shed Long",
  				  name=="Raul Mondesi"~"Adalberto Mondesi",
  				  name=="Lourdes Gurriel"~"Lourdes Gurriel Jr.",
  				  name=="Jason Groome"~"Jay Groome",
  				  name=="Luis D. Perdomo"~"Luis Perdomo",
  				  name=="D.J. Peters"~"DJ Peters",
  				  name=="M.J. Melendez"~"MJ Melendez",
  				  name=="Elijah Morgan"~"Eli Morgan",
  				  name=="T.J. Friedl"~"TJ Friedl",
  				  name=="Jarred Kelenick"~"Jarred Kelenic",
  				  name=="Ronald Acuna"~"Ronald Acuna Jr.",
  				  name=="Jose A. Martinez"~"Jose Martinez",
  				  name=="Mike Siani"~"Michael Siani",
  				  name=="D.L. Hall"~"DL Hall",
  				  name=="Josh Palacios"~"Joshua Palacios",
  				  name=="Duane Underwood"~"Duane Underwood Jr.",
  				  name=="Danny Winkler"~"Dan Winkler",
  				  name=="Jose LeClerc"~"Jose Leclerc",
  				  name=="Zach Burdi"~"Zack Burdi",
  				  name=="K.J. Harrison"~"KJ Harrison",
  				  name=="Yu-Cheng Chang"~"Yu Chang",
  				  name=="Christian Pache"~"Cristian Pache",
  				  name=="Alexander Colome"~"Alex Colome",
  				  name=="Jake Junis"~"Jakob Junis",
  				  name=="Allen Austin"~"Austin Allen",
  				  name=="Seung-Hwan Oh"~"Seunghwan Oh",
  				  name=="Andrew Knizer"~"Andrew Knizner",
  				  name=="Albert Almora"~"Albert Almora Jr.",
  				  name=="C.J. Alexander"~"CJ Alexander",
  				  name=="Jaseel De La Cruz"~"Jasseel De La Cruz",
  				  name=="Manny Margot"~"Manuel Margot",
  				  name=="Samuel Tuivailala"~"Sam Tuivailala",
  				  name=="Johnathan India"~"Jonathan India",
  				  name=="Cedric Mullins"~"Cedric Mullins II",
  				  name=="Enyel de los Santos"~"Enyel De Los Santos",
  				  name=="Jordan Addell"~"Jo Adell",
  				  name==",Yu-Cheng Chang"~"Yu Chang",
  				  name=="Kike Hernandez"~"Enrique Hernandez",
  				  (name=="Will Smith" & salary_2019==0)~"Will D. Smith",
  				  TRUE~trimws(name)))
  return(formatted_names)
}

#' Load player salaries from LWNNsalaries.xls for each team
#'
#' @return DF with salaries for each player
#' @export
#'
#' @examples
load_salaries <- function() {
  teams <- c('Yankees',
  		 'Blue Jays',
  		 'Red Sox',
  		 'Orioles',
  		 'Rays',
  		 'Tigers',
  		 'White Sox',
  		 'Twins',
  		 'Royals',
  		 'Indians',
  		 'Mariners',
  		 'Angels',
  		 'Rangers',
  		 'Athletics',
  		 'Astros',
  		 'Mets',
  		 'Nationals',
  		 'Marlins',
  		 'Phillies',
  		 'Braves',
  		 'Pirates',
  		 'Cardinals',
  		 'Reds',
  		 'Cubs',
  		 'Brewers',
  		 'Padres',
  		 'Giants',
  		 'Rockies',
  		 'Dodgers',
  		 'Diamondbacks')
  excel_cols <- c("player","salary_2019","s2020","s2021","s2022","s2023","s2024")

  team_salaries <- data.frame(player="",salary_2019="",s2020="",s2021="",s2022="",s2023="",s2024="",team="") %>%
  	filter(player!="")

  for (team in teams) {
  	current_team <-  read_excel("F:/Documents/LWNN/salaries/LWNNsalaries.xls", sheet = team, range=cell_cols("A:G"), col_names=excel_cols)
  	current_team <- current_team %>%
  		mutate(team=!!team) %>%
  		filter(salary_2019!="2019",
  			  !player %in% c("Actual","Budget","Difference","Cost of Goods","Personnel","Rookie Draft Pick"),
  			  !is.na(player)) %>%
  		select(player,team,salary_2019) %>%
  		mutate(salary_2019 = case_when(salary_2019 == "ML" ~ "0",
  								 TRUE ~ salary_2019))
  	team_salaries <- rbind(team_salaries,current_team)
  }
  team_salaries <- team_salaries  %>%
  	rename(name=player) %>%
  	format_names() %>%
  	mutate_at(vars(starts_with("salary")),funs(as.numeric)) %>%
  	select(name,team,salary_2019) %>%
  	mutate(lwnn=1)

  return(team_salaries)
}


#' load free agents
#'
#' @return
#' @export
#'
#' @examples
load_free_agents <- function() {
  fa_cols <- c("name","salary")
  fa1 <- read_excel("F:/Documents/LWNN/salaries/LWNNsalaries.xls", sheet = "2019 FA", range=cell_cols("A:B"), col_names=fa_cols)
  fa2 <- read_excel("F:/Documents/LWNN/salaries/LWNNsalaries.xls", sheet = "2019 FA", range=cell_cols("C:D"), col_names=fa_cols)
  fa3 <- read_excel("F:/Documents/LWNN/salaries/LWNNsalaries.xls", sheet = "2019 FA", range=cell_cols("E:F"), col_names=fa_cols)
  fa4 <- read_excel("F:/Documents/LWNN/salaries/LWNNsalaries.xls", sheet = "2019 FA", range=cell_cols("G:H"), col_names=fa_cols)
  fa5 <- read_excel("F:/Documents/LWNN/salaries/LWNNsalaries.xls", sheet = "2019 FA", range=cell_cols("I:J"), col_names=fa_cols)
  fa6 <- read_excel("F:/Documents/LWNN/salaries/LWNNsalaries.xls", sheet = "2019 FA", range=cell_cols("K:L"), col_names=fa_cols)
  free_agents <- fa1 %>% rbind(fa2) %>% rbind(fa3) %>% rbind(fa4) %>% rbind(fa5) %>% rbind(fa6) %>%
  	filter(!is.na(name) & name!="Average Salary" & name!="FA Discount Calculator"
  		  & name!="Total Money" & (salary!="$" | is.na(salary)) & (salary!="0" | is.na(salary))) %>%
  	mutate(salary=as.numeric(salary)) %>%
  	format_names()
  return(free_agents)
}


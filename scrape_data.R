library(rvest)
library(xml2)
library(tidyr)
library(readr)

# get list of teams, by division and region

url <- "https://www.njcaa.org/sports/bsb/teams" 
webpage <- read_html(url)

team_list <- html_nodes(webpage, '.college-name')

# create links to scrape each team's website
link <- html_attr(team_list, 'href')
link2 <- paste0("https://www.njcaa.org", link, "?view=lineup")


# initialize data frames for stats by team

hit_basic_df <- data.frame()
hit_ext_df <- data.frame()
pitch_basic_df <- data.frame()
fielding_df <- data.frame()

for (i in 240:length(link2)){
  
  url <- link2[i]
  web <- read_html(url)
  
  data <- web %>% 
    html_elements("table") %>%
    html_table()
  
  team_name <- web %>%
    html_element("h2") %>%
    html_text()
  
  hit_basic_df <- rbind(hit_basic_df, cbind(TeamName = team_name, as.data.frame(data[[3]])))
  hit_ext_df <- rbind(hit_ext_df, cbind(TeamName = team_name, as.data.frame(data[[5]])))
  pitch_basic_df <- rbind(pitch_basic_df, cbind(TeamName = team_name, as.data.frame(data[[7]])))
  fielding_df <- rbind(fielding_df, cbind(TeamName = team_name, as.data.frame(data[[9]])))
  
  print(team_name)
  Sys.sleep(1) 
  
}

# clean this up at some point, but this is for normalizing stats by region later on

regions <- html_nodes(webpage, '#mainbody :nth-child(1)') %>% html_text() # get regions and then teams within region and division
regions_df <- as.data.frame(regions)

regions_df2 <- regions_df %>%
  mutate(div = case_when(
    cumsum(regions == "Division II") == 0 ~ "Div1",
    cumsum(regions == "Division II") > 0 & cumsum(regions == "Division III") == 0 ~ "Div2",
    cumsum(regions == "Division III") > 0 ~ "Div3"
  ))

reg <- c("Region 1")

left = function (string,char) {
  substr(string,1,char)
}

for (i in 2:length(regions_df2$regions)){
  
  if (left(regions_df2$regions[i], 6) == "Region"){
    reg[i] = regions_df2$regions[i]
  } else {
    reg[i] = reg[i-1]
  }
  
}

regions_df2$reg <- reg
regions_df2$id <- paste0(regions_df2$div, "-", regions_df2$reg)

write_csv(regions_df2, "region_id_tbl.csv")
write_csv(hit_basic_df, "juco_hit_data.csv")
write_csv(hit_ext_df, "juco_hit_ext_data.csv")
write_csv(pitch_basic_df, "juco_pitch_data.csv")
write_csv(fielding_df, "juco_fielding_data.csv")
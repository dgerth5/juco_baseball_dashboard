library(tidyverse)
library(readr)


# hit data

juco_hit_data <- read_csv("juco_hit_data.csv")

sapply(juco_hit_data, class)

fr <- c("Fr", "FR", "Freshman", "Fr.", "F", "Freshman", "FRESH", "FY")
rfr <- c("Rs. Fr", "rFr", "Redshirt Freshman", "C", "RS Fr", "R-Fr.", "RS-Fr", "RFr",
         "RS-Fr.", "Rs-Fr", "RS-Freshman", "R-Fr", "r-FR", "Freshman-RS", "FR (FR)",
         "R-Freshman", "R.Fr", "RS FR", "Red.FR.", "Red. FR.", "RFR", "RS-F", "RS- FR", "RS - FR", "RS Fr.",
         "RS - Fr.", "Redshirt Fr.", "FR*", "Covid Freshman")
so <- c("So", "SO", "Sophomore", "So.", "Sophomore-Transfer", "S", "Soph.", "so")
rso <- c("R-So", "Rs. So", "rSo", "RS So", "R-So.", "RS-So", "R So.", "RS So.", "Sophomore-2",
         "r-SO", "Sophomore-RS", "R-Sophomore", "RS SO", "Redshirt Sophomore", "rSO", "Red. Soph.",
         "RS-SO", "RSO", "RS - SO", "RSF", "RSo", "SO*", "SO+", "Sophomore*", "Sophmore", "(RS) SO",
         "RS-SOPH")
jr <- c("Jr")
sr <- c("Sr")

clean_hit <- juco_hit_data %>%
  filter(!Name %in% c("Totals", "Opponent", "- -")) %>%
  mutate(adj_yr = if_else(TeamName == "Chipola", Pos, Yr),  # reach out to njcaa and see if this can get updated
         Year = case_when(adj_yr %in% fr ~ "FR",
                          adj_yr %in% rfr ~ "r-FR",
                          adj_yr %in% so ~ "SO",
                          adj_yr %in% rso ~ "r-SO",
                          adj_yr %in% jr ~ "JR",
                          adj_yr %in% sr ~ "SR",
                          adj_yr == "IF" ~ Yr,
                          adj_yr == "OF" ~ Yr)) %>%
  mutate(across(.cols = 6:20, .fns = ~replace_na(as.numeric(.), 0)))

# ext hit

juco_hit_ext_data <- read_csv("juco_hit_ext_data.csv")

clean_hit_ext <- juco_hit_ext_data %>%
  filter(!Name %in% c("Totals", "Opponent", "- -")) %>%
  mutate(adj_yr = if_else(TeamName == "Chipola", Pos, Yr),  # reach out to njcaa and see if this can get updated
         Year = case_when(adj_yr %in% fr ~ "FR",
                          adj_yr %in% rfr ~ "r-FR",
                          adj_yr %in% so ~ "SO",
                          adj_yr %in% rso ~ "r-SO",
                          adj_yr %in% jr ~ "JR",
                          adj_yr %in% sr ~ "SR",
                          adj_yr == "IF" ~ Yr,
                          adj_yr == "OF" ~ Yr)) %>%
  mutate(across(.cols = 6:16, .fns = ~replace_na(as.numeric(.), 0)))

# pitch data

juco_pitch_data <- read_csv("juco_pitch_data.csv")

sapply(juco_pitch_data, class)

clean_pitch <- juco_pitch_data %>%
  filter(!Name %in% c("Totals", "Opponent", "- -")) %>%
  mutate(adj_yr = if_else(TeamName == "Chipola", Pos, Yr),  # reach out to njcaa and see if this can get updated
         Year = case_when(adj_yr %in% fr ~ "FR",
                          adj_yr %in% rfr ~ "r-FR",
                          adj_yr %in% so ~ "SO",
                          adj_yr %in% rso ~ "r-SO",
                          adj_yr %in% jr ~ "JR",
                          adj_yr %in% sr ~ "SR",
                          adj_yr == "IF" ~ Yr,
                          adj_yr == "OF" ~ Yr)) %>%
  mutate(across(.cols = c(11,13:17,19,21), .fns = ~replace_na(as.numeric(.), 0)))

# field data

juco_fielding_data <- read_csv("juco_fielding_data.csv")

clean_field <- juco_fielding_data %>%
  filter(!Name %in% c("Totals", "Opponent", "- -")) %>%
  mutate(adj_yr = if_else(TeamName == "Chipola", Pos, Yr),  # reach out to njcaa and see if this can get updated
         Year = case_when(adj_yr %in% fr ~ "FR",
                          adj_yr %in% rfr ~ "r-FR",
                          adj_yr %in% so ~ "SO",
                          adj_yr %in% rso ~ "r-SO",
                          adj_yr %in% jr ~ "JR",
                          adj_yr %in% sr ~ "SR",
                          adj_yr == "IF" ~ Yr,
                          adj_yr == "OF" ~ Yr)) %>%
  mutate(across(.cols = 12:17, .fns = ~replace_na(as.numeric(.), 0)))

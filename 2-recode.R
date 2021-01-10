library(tidyverse)
library(lubridate)
library(rstudioapi)

wd <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd(wd)

df <- read.csv2("corona_bt_134.csv")[,-1]

# Goal: have an assessment of how often opposition parties voted with the government in proposals.

# 1 aggregate votes to the party level for each vote


df_party <- df %>%
  group_by(Name, Date, Fraktion, Policy_areas) %>%
  summarise(Yes_party = sum(Stimme == "Dafür gestimmt"),
            No_party = sum(Stimme == "Dagegen gestimmt"),
            Abstain_party = sum(Stimme == "Enthalten"),
            Absent_party = sum(Stimme == "Nicht beteiligt"),
            Yes_share = Yes_party / sum(Yes_party, No_party),
            Party_yes = ifelse(Yes_share >= 0.8, 1,0) # If there are more than 80 % of people voting with yes
  ) %>%
  filter(!between(Yes_share, 0.2, 0.8)) %>% # regard only votes of party-unity
  mutate(Opposition = ifelse(Fraktion != "CDU/CSU" & Fraktion != "SPD",1,0)) %>%
  ungroup %>%
  separate(Policy_areas, sep = ",", c("Policy_area_1", "Policy_area_2","Policy_area_3","Policy_area_4","Policy_area_5","Policy_area_6","Policy_area_7")) # separate the policy areas


# code for every vote that government voted with yes

# create a separate data frame
vote_df <- df_party %>%
  group_by(Name) %>%
  filter(Opposition == 0) %>%
  summarise(Gov_votes = sum(Party_yes)) %>%
  mutate(Gov_Yes = ifelse(Gov_votes == 2, 1,0 ))


# merge the two dataframes
df_party <- merge(x = df_party, y = vote_df, by = "Name")

# filter for only when government voted yes
df_party <- df_party %>%
  filter(Gov_Yes == 1)


# include variable for ideological position of party using manifesto data

# acess database
# code one election manifesto per party per legislative period. In case, focus on the six big parties

# insert your individual api-key here.
mp_setapikey("manifesto_apikey.txt")

mp_df <-mp_maindataset() %>%
  filter(countryname == "Germany") %>%
  filter(edate == "2017-09-24") %>%
  dplyr::select(partyabbrev, edate, date, rile, pervote, absseat, totseats) %>%
  mutate(Fraktion = dplyr::recode(partyabbrev,
                                  "90/Greens" = "DIE GRÜNEN",
                                  "DZ" = "ZENTRUM",
                                  "Greens/90" = "DIE GRÜNEN",
                                  "L-PDS" = "DIE LINKE",
                                  "LINKE" = "DIE LINKE",
                                  "SSW" = "fraktionslos",
                                  "PDS" = "DIE LINKE"),
  Rel_Seats = absseat / totseats) %>%
  dplyr::select(- absseat, -totseats)



### Code distance to the government

coal_rile <- mean(mp_df$rile[mp_df$Fraktion == "SPD" | mp_df$Fraktion == "CDU/CSU"])

df_party <- merge(df_party, mp_df, by = "Fraktion")

df_party <- df_party %>%
  mutate(Date_new = dmy(Date),
         Corona = ifelse(Date_new >= "2020-03-01", 1,0),
         Corona_dur = Date_new - as.Date("2020-03-01"),
         Health = ifelse(Policy_area_1 == "Gesundheit",1,0),
         # 2. create variable indicating days until next fed. election
         Days_next_ele = as.Date("2021-09-26") - Date_new,
         Rile_Dist = abs(rile - coal_rile))


write_csv2(df_party, "Dataframe_opp_corona.csv")

library(dplyr)
library(tidyr)

# read in main data set
library(readr)
MA_main <- read_csv("~/Documents/CDA/POTA_MA_Analysis/ma-fox-2017-11-28-2-complete.csv", 
                    col_types = cols(fox_locker_prct = col_number(), 
                                     registration_date = col_date(format = "%Y-%m-%d"), 
                                     registration_zip_code = col_character(), 
                                     studio_marketing_consent = col_logical()))

# create small sample (5%) for tinkering
MA_samp <- sample_frac(MA_main, .05, replace = FALSE)

# import foxipedia data for reference
Foxipedia <- read_csv('foxipedia_titles_genre.csv')

# WPR nos. for POTA films = War(031873), Dawn(031537), Rise(030822)
New_Apes_WPR <- c("031873", "031537", "030882")
Old_Apes_WPR <- c("007522", "003098", "002696", "000056", "000008","000050")

# create list of emails who own at least one apes movie
Samp_POTA_owners <- MA_samp %>% 
  filter(WPR == New_Apes_WPR[1] | WPR == New_Apes_WPR[2] | WPR == New_Apes_WPR[3]) %>% 
  distinct(email) %>% 
  left_join(y = MA_samp, by = "email")

#calculate top other movies owned by POTA owners
Movies_Owned <- Samp_POTA_owners %>% 
  group_by(fox_title) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

#change above code to run on entire dataset 
New_POTA_owners <- MA_main %>% 
  filter(WPR == New_Apes_WPR[1] | WPR == Apes_WPR[2] | WPR == Apes_WPR[3]) %>% 
  distinct(email) %>% 
  left_join(y = MA_main, by = "email")

New_POTA_other_movies <- New_POTA_owners %>% 
  group_by(fox_title) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

Old_POTA_owners <- MA_main %>% 
  filter(WPR == Old_Apes_WPR[1] | WPR == Old_Apes_WPR[2] | WPR == Old_Apes_WPR[3] | WPR == Old_Apes_WPR[4] | WPR == Old_Apes_WPR[5]) %>% 
  distinct(email) %>% 
  left_join(y = MA_main, by = "email")

Old_POTA_other_movies <- Old_POTA_owners %>% 
  group_by(fox_title) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

#create list of titles in top 30 of both new and old POTA owners
New_POTA_Top30 <- New_POTA_other_movies$fox_title[1:30]
Old_POTA_Top30 <- Old_POTA_other_movies$fox_title[1:30]

Combo_top <- c()

for(i in New_POTA_Top30){
  if(i %in% Old_POTA_Top30 == TRUE){
    Combo_top <- c(Combo_top, i)
  }
}

#create list of old POTA titles
Old_POTA_Titles <- c("Escape from the Planet of the Apes", "Planet of the Apes",
                     "Beneath the Planet of the Apes", "Conquest of the Planet of the Apes",
                     "Battle for the Planet of the Apes")

#filter the new POTA other movies to see how many also own old POTA titles
New_POTA_owners_Old_POTA <- New_POTA_other_movies %>% 
  filter(fox_title == Old_POTA_Titles[1] | fox_title == Old_POTA_Titles[2] | fox_title == Old_POTA_Titles[3] |
           fox_title == Old_POTA_Titles[4] | fox_title == Old_POTA_Titles[5])

# answer the question of what the top genres owned by new and old POTA owners
NPOTA_Other_Genre <- New_POTA_owners %>% 
  group_by(Genre) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

OPOTA_Other_Genre <- Old_POTA_owners %>% 
  group_by(Genre) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

#states with the most POTA owners
zip <- read_csv('zip_to_city.csv')

#add city and state to New POTA Owners
Uniq_NPOTA_Owners <- New_POTA_owners %>% 
  filter(duplicated(email) == FALSE)

NPOTA_State <- Uniq_NPOTA_Owners %>% 
  left_join(y = select(zip, ZIP, CityName, StateAbbr), by = c("registration_zip_code" = "ZIP")) %>% 
  group_by(StateAbbr) %>% 
  summarize("count" = n()) %>% 
  arrange(desc(count)) %>% 
  mutate("pota_perc" = count/sum(count)) %>% 
  left_join(y = select(state_pop, State, Pop_perc), by = c("StateAbbr" = "State")) %>% 
  mutate("index" = pota_perc/Pop_perc)

NPOTA_City <- Uniq_NPOTA_Owners %>% 
  left_join(y = select(zip, ZIP, CityName, StateAbbr), by = c("registration_zip_code" = "ZIP")) %>% 
  group_by(CityName) %>% 
  summarize("count" = n()) %>% 
  arrange(desc(count))

#add state population info
state_pop <- read_csv('state_pop.csv')
zip_pop <- read_csv('Zip Pop by Age v2.csv')
zip_HH <- read_csv('Households ZIP Indexed.csv')
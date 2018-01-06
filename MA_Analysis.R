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
  summarize("count" = fn()) %>% 
  arrange(desc(count))

#add state population info
state_pop <- read_csv('state_pop.csv')
zip_pop <- read_csv('Zip Pop by Age v2.csv')
zip_HH <- read_csv('Households ZIP Indexed.csv')

#set of unique email addresses for summary comparisons
Uniq_MA_main <- MA_main %>%
  filter(duplicated(email) == FALSE)

Uniq_OPOTA_Owners <- Old_POTA_owners %>% 
  filter(duplicated(email) == FALSE)

Uniq_Main_zip <- Uniq_MA_main %>% 
  left_join(y = select(zip_HH, ZIP, HHI_Median, HHI_Mean), by = c("registration_zip_code" = "ZIP")) %>% 
  filter(!is.na(HHI_Mean)) %>% 
  mutate("HHI_Mean_Index" = HHI_Mean/mean(HHI_Mean))

Uniq_Main_zip <- Uniq_MA_main %>% 
  left_join(y = select(zip_HH, ZIP, HHI_Median, HHI_Mean), by = c("registration_zip_code" = "ZIP")) %>% 
  filter(!is.na(HHI_Mean)) %>% 
  mutate("HHI_Mean_Index" = HHI_Mean/mean(HHI_Mean))

NPOTA_Owners_zip <- Uniq_NPOTA_Owners %>% 
  left_join(y = select(zip_HH, ZIP, HHI_Median, HHI_Mean), by = c("registration_zip_code" = "ZIP")) %>% 
  filter(!is.na(HHI_Mean)) %>% 
  mutate("HHI_Mean_Index" = HHI_Mean/mean(HHI_Mean))

#plot HHI Mean index v locker size -> do people who live where HHI is higher own more movies?
#remove locker_size > 2000 so the plot makes some sense
library(ggplot2)
ggplot(Uniq_Main_zip, aes(x = HHI_Mean_Index, y = locker_size)) + geom_point(position = "jitter", alpha = 0.3) + ylim(c(0,2000))
ggplot(Uniq_Main_zip, aes(x = HHI_Mean_Index, y = locker_size)) + geom_col(width = 0.25, position = "identity", na.rm = TRUE) + ylim(c(0,10000))

#compare with plot of NPOTA owners
ggplot(NPOTA_Owners_zip, aes(x = HHI_Mean_Index, y = locker_size)) + geom_col(width = 0.25, position = "identity", na.rm = FALSE) + ylim(c(0,10000))

#create data set for plotting mean locker size by level of mean HHI
HHIMI_plot <- Uniq_Main_zip %>% 
  mutate("HHIMI_Level" = ifelse(between(HHI_Mean_Index, 0, 0.7),1,
                                ifelse(between(HHI_Mean_Index,0.7001, 1.3),2,
                                       ifelse(between(HHI_Mean_Index,1.3001, 2.0),3,
                                              ifelse(between(HHI_Mean_Index,2.001, 3.0),4,
                                                     ifelse(between(HHI_Mean_Index,3.001, 4.0),5,6)))))) %>% 
  group_by(HHIMI_Level) %>% 
  summarize("Mean_LS" = mean(locker_size), "count" = n(), "IQR" = IQR(locker_size)) %>% 
  mutate("lower" = Mean_LS - IQR, "upper" = Mean_LS + IQR)

#create plot to show mean locker size v level of mean HHI
g <- ggplot(HHIMI_plot, aes(x = HHIMI_Level, y = Mean_LS, colour = "blue"))
g + geom_col(position = "dodge") + geom_errorbar(aes(ymin = lower, ymax = upper), position = "dodge", width = 0.25, colour = "black")

#  are there any interesting relationships between POTA ownership and zip code age/sex characteristics
Uniq_MA_age <- Uniq_MA_main %>% 
  left_join(y = select(zip_pop, ZIP, Median_age, MF_Ratio), by = c("registration_zip_code" = "ZIP")) %>% 
  filter(!is.na(Median_age, !is.na(MF_Ratio)))
  
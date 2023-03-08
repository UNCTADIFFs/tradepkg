# URL example
# https://comtradeapi.un.org/bulk/v1/get/C/A/H3?reporterCode=300&period=2011&subscription-key=1A2B3C4D5E6F304e7ea860b6faa627X8Z9

# API test
library(httr)
#
rc <- 699
rc <- "all"
year <- 2016

  # generate URL
  stringr <- paste("https://comtradeapi.un.org/bulk/v1/get/C/A/HS?"
                   ,"reporterCode=",rc,"&"
                   ,"period=",year, "&"
                   , "subscription-key=", "f3a22240c953488b8abf79577bf1193a"
                   ,sep = ""
  )



  #
  a <- GET(url = stringr,
      add_headers(`Cache-Control` = "no-cache", `Ocp-Apim-Subscription-Key` = "f3a22240c953488b8abf79577bf1193a"))


stringr <- content(a)$data[[1]]$fileUrl

# https://comtradeapi.un.org/bulk/v1/file/792/9276b9391090069cc73e0250164f39a63e089df9e07f93ef95ee2df98bd1fb02?subscription-key=1A2B3C4D5E6F304e7ea860b6faa627X8Z9
fl <- paste(stringr, "?"
            ,"subscription-key=", "f3a22240c953488b8abf79577bf1193a"
            , sep = "")

download.file(fl,  destfile = "./zip",cacheOK=TRUE)




# storage management
# data.table
library(data.table)
library(tidyverse)
# use fread to read csv and select data
dlist <- fread("2016df.csv") %>%
  select(`Year`,
             `Trade Flow`,
             `Reporter Code`,
             `Reporter ISO`,
             `Partner Code`,
             `Partner ISO`,
             `Commodity Code`,
             `Trade Value (US$)`)  %>%
  filter(nchar(`Commodity Code`) == 6 & `Partner ISO` != "WLD") %>%
  mutate(`Reporter Code` = as.character(`Reporter Code`), `Partner Code` = as.character(`Partner Code`), Year = as.character(Year)) %>%
  mutate(`Reporter Code` = str_pad(`Reporter Code`, 3, side = "left", "0"), `Partner Code` = str_pad(`Partner Code`, 3, side = "left", 0))





# check beta value of Country Egypt

beta_oecd_Egypt <- beta_oecd_2016 %>%
  filter(REPORTER == "EGY" & !(is.na(Value)))


beta_unctad_Egypt <- beta_unctad_2016 %>%
  filter(Origin == "818" & !(is.na(`Transport costs to FOB value`)))


load("data/countrykey.rda")
load("data/beta_oecd_2016.rda")
load("data/beta_unctad_2016.rda")

beta_oecd <-
  beta_oecd_2016 %>%
  mutate(TIME = as.character(TIME)) %>%
  select(TIME, REPORTER, PARTNER, COM_H3, value)

beta_unctad <-
  beta_unctad_2016 %>%
  rename(value = `Transport costs to FOB value`) %>%
  left_join(countrykey, by = c("Origin" = "country_code")) %>%
  rename(REPORTER = ISO3) %>%
  left_join(countrykey, by = c("Destination" = "country_code")) %>%
  rename(PARTNER = ISO3) %>%
  select(Year, REPORTER, PARTNER, HS2012Product, value) %>%
  mutate(value = value/100) %>%
  mutate(Year = as.character(Year))

# for each country (reporter) how much (count) of the HS codes are available


beta_oecd_count <-  beta_oecd %>%
  group_by(REPORTER) %>%
  summarise(nums_code_oecd = sum(!(is.na(Value))))



beta_unctad_count <-  beta_unctad %>%
  group_by(REPORTER) %>%
  summarise(nums_code_unctad = sum(!(is.na(value))))

count_df <- beta_unctad_count %>%
  full_join(beta_oecd_count, by = "REPORTER") %>%
  filter(!(is.na(nums_code_oecd))) %>%
  filter(!(is.na(nums_code_unctad))) %>%
  mutate(nums_code_sum = nums_code_unctad + nums_code_oecd) %>%
  arrange(desc(nums_code_sum))

#
Q_4 <- beta_oecd_2016 %>%
  filter(REPORTER == "USA" & PARTNER == "EGY" & COM_H3 == "0101")


#

Q_5 <- beta_oecd_2016 %>%
  filter(REPORTER == "EGY" & COM_H3 == "0105")


#

beta_default <- HS07vsHS12 %>%
  select(HS12...5) %>%
  distinct(HS12...5) %>%
  mutate(value = 0.06) %>%
  rename(HS2012Product = HS12...5)




# correspondence

library(readxl)
library(tidyverse)

HS07vsHS12 <- read_excel("HS07vsHS12.xlsx")

df <- HS07vsHS12 %>%
  select(HS07...4, HS12...5, diff...6) %>%
  rename(HS2007 = HS07...4, HS2012 = HS12...5, diff = diff...6)





df_new <- df %>%
  filter(diff == 1)





#

df_survey <- Q1 %>%
  full_join(Q2, by = "Country") %>%
  full_join(Q3, by = "Country") %>%
  full_join(Q4, by = "Country") %>%
  full_join(Q5, by = "Country")


write.csv(df_survey, file = "SurveryQ1-Q5.csv")


# detect value > 1

library(tidyverse)
load("data/beta_oecd_2016.rda")
x <- 0.06
beta_oecd_df <-
  beta_oecd_2016 %>%
  mutate(value = tidyr::replace_na(Value, x)) %>%
  mutate(TIME = as.character(TIME)) %>%
  filter(Measure == "Value") %>%
  filter(value > 1)




library(readr)
load("data/countrykey.rda")

SurveryQ1_Q5 <- read_csv("SurveryQ1-Q5.csv")

SurveryQ1_Q5 <- SurveryQ1_Q5 %>%
  select(Country, Q1Response, Q2Response, Q3Response) %>%
  left_join(countrykey, by = c("Country" = "country_name")) %>%
  select(-c(ISO2, country_code))


survey_df <-  SurveryQ1_Q5 %>%
  mutate(ISO3 = ifelse(Country == "Dominican Rep.", "DOM", ISO3)) %>%
  mutate(ISO3 = ifelse(Country == "Marshall Isds", "MHL", ISO3)) %>%
  mutate(ISO3 = ifelse(Country == "Switzerland", "CHE", ISO3)) %>%
  mutate(ISO3 = ifelse(Country == "Bosnia Herzegovina", "BIH", ISO3)) %>%
  mutate(ISO3 = ifelse(Country == "USA", "USA", ISO3)) %>%
  mutate(ISO3 = ifelse(Country == "Cayman Isds", "CYM", ISO3))

survey_df <- survey_df %>%
  mutate(trade_system = case_when(Q1Response == "Yes" | Q3Response == "Yes" ~ "general",
                                  Q2Response == "Yes" ~ "special"))


# save beta files

library(readr)
library(tidyverse)
US_TransportCosts_All_Main_ST202101140840_v1 <- read_csv("US_TransportCosts_All_Main_ST202101140840_v1.csv")
View(US_TransportCosts_All_Main_ST202101140840_v1)

unctad_beta <- US_TransportCosts_All_Main_ST202101140840_v1 %>%
  rename(Year = ...1) %>%
  mutate(`Transport costs to FOB value` = `Transport costs to FOB value` / 100) %>%
  mutate(`Transport costs to FOB value` = ifelse(`Transport costs to FOB value` > 1, 1, `Transport costs to FOB value`))


write_csv(unctad_beta, file = "UNCTADBeta.csv")


oecd_beta <- beta_oecd_2016 %>%
  filter(Measure == "Value") %>%
  mutate(Value = ifelse(Value > 1, 1, Value))

write_csv(oecd_beta, file = "OECDBeta.csv")







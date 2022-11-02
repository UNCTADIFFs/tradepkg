# get all commodites for China in 2016

x <- 0.06
load("data/countrykey.rda")
load("data/CHN_2016.rda")

CHN_2016 <- CHN_2016 %>%
  mutate(reporterCode = as.character(reporterCode), partnerCode = as.character(partnerCode)) %>%
  left_join(countrykey, by = c("reporterCode" = "country_code")) %>%
  rename(REPORTER.ISO = ISO3) %>%
  left_join(countrykey, by = c("partnerCode" = "country_code")) %>%
  rename(PARTNER.ISO = ISO3)

# beta value from oecd
load("data/beta_oecd_2016.rda")
beta_oecd <-
  beta_oecd_2016 %>%
  mutate(value = tidyr::replace_na(Value, x)) %>%
  mutate(COM_H3 = as.numeric(COM_H3))

# beta value from unctad
load("data/beta_unctad_2016.rda")

####### revise name Value
beta_unctad <-
  beta_unctad_2016 %>%
  filter(nchar(HS2012Product) == 6) %>%
  mutate(value = tidyr::replace_na(`Transport costs to FOB value`, x)) %>%
  left_join(countrykey, by = c("Origin" = "country_code")) %>%
  rename(REPORTER = ISO3) %>%
  left_join(countrykey, by = c("Destination" = "country_code")) %>%
  rename(PARTNER = ISO3) %>%
  select(Year, REPORTER, PARTNER, HS2012Product, value) %>%
  mutate(value = value/100)


# calculate CIF
raw.data.new <- CHN_2016 %>%
  rename(Commodity.code.6 =  `cmdCode`) %>%
  mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
  left_join(beta_oecd, by = c("REPORTER.ISO" = "REPORTER", "PARTNER.ISO" = "PARTNER", "Commodity.code.4" = "COM_H3", "refYear" = "Time")) %>%
  mutate(Value = tidyr::replace_na(Value, x)) %>%
  filter(PARTNER.ISO != "WLD" & !is.na(PARTNER.ISO)) %>%
  mutate(CIFValue_oecd = ifelse(flowCode == "X", primaryValue * Value, primaryValue)) %>%
  group_by(refYear, `flowCode`, `REPORTER.ISO`, `PARTNER.ISO`, `Commodity.code.6`) %>%
  summarise(CIFValue_oecd = sum(CIFValue_oecd, na.rm = TRUE), Value_origin = sum(primaryValue, na.rm = TRUE))

raw.data.new <- raw.data.new %>%
  left_join(beta_unctad, by = c("REPORTER.ISO" = "REPORTER", "PARTNER.ISO" = "PARTNER", "Commodity.code.6" = "HS2012Product", "refYear" = "Year")) %>%
  mutate(value = tidyr::replace_na(value, x)) %>%
  mutate(CIFValue_unctad = ifelse(flowCode == "X", Value_origin * value, Value_origin)) %>%
  select(-value)

# get totaldf

total_df <- raw.data.new %>%
  group_by(REPORTER.ISO, refYear, flowCode) %>%
  summarise(total_for_all_origin = sum(Value_origin, na.rm = TRUE), total_for_all_oecd = sum(CIFValue_oecd, na.rm = TRUE), total_for_all_unctad = sum(CIFValue_unctad, na.rm = TRUE)) %>%
  rename(Year = refYear, Trade.Flow = flowCode) %>%
  mutate(Trade.Flow = ifelse(Trade.Flow == "X", "Export", Trade.Flow)) %>%
  mutate(Trade.Flow = ifelse(Trade.Flow == "M", "Import", Trade.Flow)) %>%
  mutate(Trade.Flow = ifelse(Trade.Flow == "RM", "Re-Import", Trade.Flow)) %>%
  mutate(Trade.Flow = ifelse(Trade.Flow == "Rx", "Re-Emport", Trade.Flow))








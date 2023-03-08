#' Title Calulate discrepancy for paper
#' @importFrom dplyr %>%
#' @param dlist list of two dataframes (country of interest is reporter and country of interest is partner)
#'
#' @return dataframe of discrepancy
#' @export
#'
#' @examples \dontrun{
#' dlist <- extract(year = "2018", rcode = "EGY", pcode = "ARE", ccode = "190230")
#' year = 2016, rcode = "CHN", pcode = "TWN", ccode = "2618"
#' df_dsp <- calculatedsp(dlist)
#' }
calculatedspv2 <- function(dlist){

  # load 2016 data
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


  #
  coi <- dlist %>%
    filter(`Reporter Code` == "818")
  #
  coi.mirror <- dlist %>%
    filter(`Partner Code` == "818")


  x <- 0.06
  load("data/countrykey.rda")
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
    mutate(value = tidyr::replace_na(`Transport costs to FOB value`, x)) %>%
    left_join(countrykey, by = c("Origin" = "country_code")) %>%
    rename(REPORTER = ISO3) %>%
    left_join(countrykey, by = c("Destination" = "country_code")) %>%
    rename(PARTNER = ISO3) %>%
    select(Year, REPORTER, PARTNER, HS2012Product, value) %>%
    mutate(value = value/100) %>%
    mutate(HS2012Product = as.numeric(HS2012Product))


  # clean, merge and calculate CIF
  # clean coi
  coi <-
    coi %>%
    select(Classification, Year, Trade.Flow.Code, Trade.Flow, Reporter.ISO, Reporter, Partner.ISO, Partner, Commodity.Code, Commodity, Trade.Value..US..) %>%
    rename(Commodity.code.6 =  `Commodity.Code`) %>%
    mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
    left_join(beta_oecd, by = c("Reporter.ISO" = "REPORTER", "Partner.ISO" = "PARTNER", "Commodity.code.4" = "COM_H3", "Year" = "Time")) %>%
    mutate(Value = tidyr::replace_na(Value, x)) %>%
    filter(Partner.ISO != "WLD" & !is.na(Partner.ISO)) %>%
    mutate(CIFValue_oecd = ifelse(Trade.Flow == "Export", Trade.Value..US.. * Value, Trade.Value..US..)) %>%
    group_by(Year, `Trade.Flow`, `Partner.ISO`, `Reporter.ISO`, `Commodity.code.6`) %>%
    summarise(CIFValue_oecd = sum(CIFValue_oecd, na.rm = TRUE), Value_origin = sum(Trade.Value..US.., na.rm = TRUE))

  coi <-
    coi %>%
    left_join(beta_unctad, by = c("Reporter.ISO" = "REPORTER", "Partner.ISO" = "PARTNER", "Commodity.code.6" = "HS2012Product", "Year" = "Year")) %>%
    mutate(value = tidyr::replace_na(value, x)) %>%
    mutate(CIFValue_unctad = ifelse(Trade.Flow == "Export", Value_origin * value, Value_origin)) %>%
    select(-value)


  # clean coi.mirror
  coi.mirror <-
    coi.mirror %>%
    select(Classification, Year, Trade.Flow.Code, Trade.Flow, Reporter.ISO, Reporter, Partner.ISO, Partner, Commodity.Code, Commodity, Trade.Value..US..) %>%
    rename(Commodity.code.6 =  `Commodity.Code`) %>%
    mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
    left_join(beta_oecd, by = c("Reporter.ISO" = "REPORTER", "Partner.ISO" = "PARTNER", "Commodity.code.4" = "COM_H3", "Year" = "Time")) %>%
    mutate(Value = tidyr::replace_na(Value, x)) %>%
    filter(Partner.ISO != "WLD" & !is.na(Partner.ISO)) %>%
    mutate(CIFValue_oecd = ifelse(Trade.Flow == "Export", Trade.Value..US.. * Value, Trade.Value..US..)) %>%
    group_by(Year, `Trade.Flow`, `Partner.ISO`, `Reporter.ISO`, `Commodity.code.6`) %>%
    summarise(CIFValue_oecd = sum(CIFValue_oecd, na.rm = TRUE), Value_origin = sum(Trade.Value..US.., na.rm = TRUE))

  coi.mirror <-
    coi.mirror %>%
    left_join(beta_unctad, by = c("Reporter.ISO" = "REPORTER", "Partner.ISO" = "PARTNER", "Commodity.code.6" = "HS2012Product", "Year" = "Year")) %>%
    mutate(value = tidyr::replace_na(value, x)) %>%
    mutate(CIFValue_unctad = ifelse(Trade.Flow == "Export", Value_origin * value, Value_origin)) %>%
    select(-value)

  # mirror data
  # Mirroring for export data of Egypt
  coi.mirror_i <-
    coi.mirror %>%
    filter(Trade.Flow == "Import")

  coi_e <-
    coi %>%
    filter(Trade.Flow == "Export") %>%
    full_join(coi.mirror_i,
              by = c("Reporter.ISO" = "Partner.ISO", "Partner.ISO" = "Reporter.ISO", "Year", "Commodity.code.6"))

  if(nrow(coi_e) != 0){
    coi_e <- coi_e %>%
      mutate(CIFValue_oecd.x = replace_na(CIFValue_oecd.x, 0)) %>%
      mutate(CIFValue_unctad.x = replace_na(CIFValue_unctad.x, 0)) %>%
      mutate(Value_origin.x = replace_na(Value_origin.x, 0)) %>%
      mutate(CIFValue_oecd.y = replace_na(CIFValue_oecd.y, 0)) %>%
      mutate(CIFValue_unctad.y = replace_na(CIFValue_unctad.y, 0)) %>%
      mutate(Value_origin.y = replace_na(Value_origin.y, 0)) %>%
      mutate(Trade.Flow.x = ifelse(CIFValue_oecd.x == 0, "Export", Trade.Flow.x))
  }

  # Mirroring for import data of Egypt
  coi.mirror_e <-
    coi.mirror %>%
    filter(Trade.Flow == "Export")

  coi_i <-
    coi %>%
    filter(Trade.Flow == "Import") %>%
    full_join(coi.mirror_e,
              by = c("Reporter.ISO" = "Partner.ISO", "Partner.ISO" = "Reporter.ISO", "Year", "Commodity.code.6"))


  if(nrow(coi_i) != 0){
    coi_i <- coi_i %>%
      mutate(CIFValue_oecd.x = replace_na(CIFValue_oecd.x, 0)) %>%
      mutate(CIFValue_unctad.x = replace_na(CIFValue_unctad.x, 0)) %>%
      mutate(Value_origin.x = replace_na(Value_origin.x, 0)) %>%
      mutate(CIFValue_oecd.y = replace_na(CIFValue_oecd.y, 0)) %>%
      mutate(CIFValue_unctad.y = replace_na(CIFValue_unctad.y, 0)) %>%
      mutate(Value_origin.y = replace_na(Value_origin.y, 0)) %>%
      mutate(Trade.Flow.x = ifelse(CIFValue_oecd.x == 0, "Import", Trade.Flow.x))
  }

  # Mirroring for re-export data of Egypt
  coi.mirror_r.i <-
    coi.mirror %>%
    filter(Trade.Flow == "Re-Import")

  coi_r.e <-
    coi %>%
    filter(Trade.Flow == "Re-Export") %>%
    full_join(coi.mirror_r.i,
              by = c("Reporter.ISO" = "Partner.ISO", "Partner.ISO" = "Reporter.ISO", "Year", "Commodity.code.6"))
  if(nrow(coi_r.e) != 0){
    coi_r.e <-  coi_r.e %>%
      mutate(CIFValue_oecd.x = replace_na(CIFValue_oecd.x, 0)) %>%
      mutate(CIFValue_unctad.x = replace_na(CIFValue_unctad.x, 0)) %>%
      mutate(Value_origin.x = replace_na(Value_origin.x, 0)) %>%
      mutate(CIFValue_oecd.y = replace_na(CIFValue_oecd.y, 0)) %>%
      mutate(CIFValue_unctad.y = replace_na(CIFValue_unctad.y, 0)) %>%
      mutate(Value_origin.y = replace_na(Value_origin.y, 0)) %>%
      mutate(Trade.Flow.x = ifelse(CIFValue_oecd.x == 0, "Re-Export", Trade.Flow.x))
  }


  # Mirroring for re-import data of Egypt
  coi.mirror_r.e <-
    coi.mirror %>%
    filter(Trade.Flow == "Re-Export")

  coi_r.i <-
    coi %>%
    filter(Trade.Flow == "Re-Import") %>%
    full_join(coi.mirror_r.e,
              by = c("Reporter.ISO" = "Partner.ISO", "Partner.ISO" = "Reporter.ISO", "Year", "Commodity.code.6"))


  if(nrow(coi_r.i) != 0){
    coi_r.i <- coi_r.i %>%
      mutate(CIFValue_oecd.x = replace_na(CIFValue_oecd.x, 0)) %>%
      mutate(CIFValue_unctad.x = replace_na(CIFValue_unctad.x, 0)) %>%
      mutate(Value_origin.x = replace_na(Value_origin.x, 0)) %>%
      mutate(CIFValue_oecd.y = replace_na(CIFValue_oecd.y, 0)) %>%
      mutate(CIFValue_unctad.y = replace_na(CIFValue_unctad.y, 0)) %>%
      mutate(Value_origin.y = replace_na(Value_origin.y, 0)) %>%
      mutate(Trade.Flow.x = ifelse(CIFValue_oecd.x == 0, "Re-Import", Trade.Flow.x))

  }

  # Combine data
  dspdf <- bind_rows(coi_e, coi_i, coi_r.e, coi_r.i)


  # calculate total value
  total_for_all <- dlist$`total_for_all`
  total_for_c <- dlist$`total_for_c`



  # calculate absolute and relative difference
  dspdf <- dspdf %>%
    # absolute value
    mutate(diff_oecd = CIFValue_oecd.x - CIFValue_oecd.y) %>%
    mutate(diff_unctad = CIFValue_unctad.x - CIFValue_unctad.y) %>%
    mutate(diff_origin = Value_origin.x - Value_origin.y) %>%
    # relative value to this flow for specific commodity
    full_join(total_for_c, by = c("Year", "Trade.Flow.x" = "Trade.Flow", "Commodity.code.6")) %>%
    # using 0 to assign missing value
    mutate(total_for_c = tidyr::replace_na(total_for_c_oecd, 0)) %>%
    # relative value to total value for specific commodity
    mutate(ratio_to_total_c_origin = abs(diff_origin / total_for_c_origin)) %>%
    mutate(ratio_to_total_c_oecd = abs(diff_oecd / total_for_c_oecd)) %>%
    mutate(ratio_to_total_c_unctad = abs(diff_unctad / total_for_c_unctad)) %>%
    select(-Reporter.ISO.y)


  dspdf1 <- dspdf %>%
    full_join(total_for_all, by = c("Year", "Trade.Flow.x" = "Trade.Flow")) %>%
    mutate(ratio_to_total_all_origin = abs(diff_origin / total_for_all_origin)) %>%
    mutate(ratio_to_total_all_oecd = abs(diff_oecd / total_for_all_oecd)) %>%
    mutate(ratio_to_total_all_unctad = abs(diff_unctad / total_for_all_unctad)) %>%
    select(-REPORTER.ISO)




  # relative value to this flow for all commodities
  #left_join(total_for_all, by = c("Year", "Trade.Flow.x", "Commodity.Code"="Commodity.code.6")) %>%
  #mutate(ratio_to_total_all = diff / total_for_all)




  return(dspdf1)

}

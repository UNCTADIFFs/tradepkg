#' Title Calulate discrepancy
#' @importFrom dplyr %>%
#' @param dlist list of two dataframes (country of interest is reporter and country of interest is partner)
#'
#' @return dataframe of discrepancy
#' @export
#'
#' @examples \dontrun{
#' dlist <- extract(year = "2018", rcode = "818", pcode = "784", ccode = "190230")
#' df_dsp <- calculatedsp(dlist)
#' }
calculatedsp <- function(dlist){

  # load data
  # load country of interest (coi) (where the country should be reporter)
  coi <- dlist$`reporter=reporter`
  # load coi's partners' data (where coi's partners should be reporter)
  coi.mirror <- dlist$`reporter=partner`
  # load beta
  x <- 0.095
  load("data/beta.rda")
  betadf <-
   beta %>%
   mutate(value = tidyr::replace_na(value, x))



  # clean, merge and calculate CIF
  # clean coi
   coi <-
    coi %>%
    rename(Commodity.code.6 =  `Commodity.Code`) %>%
    mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
    left_join(betadf, by = c("Reporter.ISO" = "importeriso", "Partner.ISO" = "exporteriso", "Commodity.code.4" = "commoditycode", "Year" = "year")) %>%
    mutate(CIFValue = ifelse(Trade.Flow == "Export", Trade.Value..US.. * value, Trade.Value..US..)) %>%
    group_by(Year, `Trade.Flow`, `Partner.ISO`, `Reporter.ISO`, `Commodity.code.6`) %>%
    summarise(CIFValue = sum(CIFValue))

  # clean coi.mirror
   coi.mirror <-
     coi.mirror %>%
     rename(Commodity.code.6 =  `Commodity.Code`) %>%
     mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
     left_join(betadf, by = c("Reporter.ISO" = "exporteriso", "Partner.ISO" = "importeriso", "Commodity.code.4" = "commoditycode", "Year" = "year")) %>%
     mutate(CIFValue = ifelse(Trade.Flow == "Export", Trade.Value..US.. * value, Trade.Value..US..)) %>%
     group_by(Year, `Trade.Flow`, `Partner.ISO`, `Reporter.ISO`, `Commodity.code.6`) %>%
     summarise(CIFValue = sum(CIFValue))

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

  # Mirroring for import data of Egypt
  coi.mirror_e <-
    coi.mirror %>%
    filter(Trade.Flow == "Export")

  coi_i <-
    coi %>%
    filter(Trade.Flow == "Import") %>%
    full_join(coi.mirror_e,
              by = c("Reporter.ISO" = "Partner.ISO", "Partner.ISO" = "Reporter.ISO", "Year", "Commodity.code.6"))

  # Mirroring for re-export data of Egypt
  coi.mirror_r.i <-
    coi.mirror %>%
    filter(Trade.Flow == "Re-Import")

  coi_r.e <-
    coi %>%
    filter(Trade.Flow == "Re-Export") %>%
    full_join(coi.mirror_r.i,
              by = c("Reporter.ISO" = "Partner.ISO", "Partner.ISO" = "Reporter.ISO", "Year", "Commodity.code.6"))

  # Mirroring for re-import data of Egypt
  coi.mirror_r.e <-
    coi.mirror %>%
    filter(Trade.Flow == "Re-Export")

  coi_r.i <-
    coi %>%
    filter(Trade.Flow == "Re-Import") %>%
    full_join(coi.mirror_r.e,
              by = c("Reporter.ISO" = "Partner.ISO", "Partner.ISO" = "Reporter.ISO", "Year", "Commodity.code.6"))



  # Combine data and calculate difference
  df_dsp <-
    bind_rows(coi_e, coi_i, coi_r.e, coi_r.i) %>%
    mutate(diff = CIFValue.x - CIFValue.y) %>%
    mutate(ratio = diff / CIFValue.x)


  return(df_dsp)

  }

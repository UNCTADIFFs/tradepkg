#' @return A dataframe that stores total value for later relative calculating
#' @export
#'
#' @examples \dontrun{
#' for specific commodity
#' total_for_c <- getotal(rcode = "EGY", year = "2018", ccode= "190230")
#' rcode = "CHN", year = "2016", ccode = "2848"
#' for all commodities
#' total_for_all <- getotal(rcode, year, "all")
#' }
getotalv2 <- function(rcode, year, ccode){

  x <- 0.06
  load("data/countrykey.rda")
  # beta value from oecd, 4-digit
  load("data/beta_oecd_2016.rda")
  beta_oecd <-
    beta_oecd_2016 %>%
    mutate(value = tidyr::replace_na(Value, x)) %>%
    select(TIME, REPORTER, PARTNER, COM_H3, value)

  # beta value from unctad, 6-digit
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
    mutate(value = value/100)



    # calculate CIF with oecd
    coi.new <- coi



    # calcute CIF with unctad




    # totaldf1: group by commodity and flow, calculate for relative value


    # totaldf2: group by flow, calculate for relative value













    raw.data.new <- raw.data %>%
      select(Classification, Year, Trade.Flow.Code, Trade.Flow, Reporter.ISO, Reporter, Partner.ISO, Partner, Commodity.Code, Commodity, Trade.Value..US..) %>%
      rename(Commodity.code.6 =  `Commodity.Code`) %>%
      mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
      left_join(beta_oecd, by = c("Reporter.ISO" = "REPORTER", "Partner.ISO" = "PARTNER", "Commodity.code.4" = "COM_H3", "Year" = "Time")) %>%
      mutate(Value = tidyr::replace_na(Value, x)) %>%
      filter(Partner.ISO != "WLD" & !is.na(Partner.ISO)) %>%
      mutate(CIFValue_oecd = ifelse(Trade.Flow == "Export", Trade.Value..US.. * Value, Trade.Value..US..)) %>%
      group_by(Year, `Trade.Flow`, `Partner.ISO`, `Reporter.ISO`, `Commodity.code.6`) %>%
      summarise(CIFValue_oecd = sum(CIFValue_oecd, na.rm = TRUE), Value_origin = sum(Trade.Value..US.., na.rm = TRUE))

    raw.data.new <- raw.data.new %>%
      mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
      left_join(beta_unctad, by = c("Reporter.ISO" = "REPORTER", "Partner.ISO" = "PARTNER", "Commodity.code.4" = "HS2012Product", "Year" = "Year")) %>%
      mutate(value = tidyr::replace_na(value, x)) %>%
      mutate(CIFValue_unctad = ifelse(Trade.Flow == "Export", Value_origin * value, Value_origin)) %>%
      select(-value)




    # get total flow of exports/imports for that reporter and year for that commodity
    total_df <- raw.data.new %>%
      group_by(Reporter.ISO, Year, Trade.Flow, Commodity.code.6) %>%
      summarise(total_for_c_origin = sum(Value_origin, na.rm = TRUE), total_for_c_oecd = sum(CIFValue_oecd, na.rm = TRUE), total_for_c_unctad = sum(CIFValue_unctad, na.rm = TRUE))

  }


  # if ccode is for all
  # define url address
  if(ccode == "ALL"){
    stringr <- paste("http://comtrade.un.org/api/get?"
                     ,"max=",50000,"&" #maximum no. of records returned
                     ,"type=","C","&" #type of trade (c=commodities)
                     ,"freq=","A","&" #frequency
                     ,"px=","HS","&" #classification
                     ,"ps=",year,"&" #time period
                     ,"r=",rcode,"&" #reporting area
                     ,"p=","all","&" #partner country
                     ,"rg=","all","&" #trade flow
                     ,"cc=","all","&" #classification code
                     ,"fmt=","csv"        #Format
                     ,sep = ""
    )
    # extract data
    raw.data <- read.csv(stringr,header=TRUE)

    # calculate CIF
    raw.data.new <- raw.data %>%
      select(Classification, Year, Trade.Flow.Code, Trade.Flow, Reporter.ISO, Reporter, Partner.ISO, Partner, Commodity.Code, Commodity, Trade.Value..US..) %>%
      rename(Commodity.code.6 =  `Commodity.Code`) %>%
      mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
      left_join(beta_oecd, by = c("Reporter.ISO" = "REPORTER", "Partner.ISO" = "PARTNER", "Commodity.code.4" = "COM_H3", "Year" = "Time")) %>%
      mutate(Value = tidyr::replace_na(Value, x)) %>%
      filter(Partner.ISO != "WLD" & !is.na(Partner.ISO)) %>%
      mutate(CIFValue_oecd = ifelse(Trade.Flow == "Export", Trade.Value..US.. * Value, Trade.Value..US..)) %>%
      group_by(Year, `Trade.Flow`, `Partner.ISO`, `Reporter.ISO`, `Commodity.code.6`) %>%
      summarise(CIFValue_oecd = sum(CIFValue_oecd, na.rm = TRUE), Value_origin = sum(Trade.Value..US.., na.rm = TRUE))

    raw.data.new <- raw.data.new %>%
      mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
      left_join(beta_unctad, by = c("Reporter.ISO" = "REPORTER", "Partner.ISO" = "PARTNER", "Commodity.code.4" = "HS2012Product", "Year" = "Year")) %>%
      mutate(value = tidyr::replace_na(value, x)) %>%
      mutate(CIFValue_unctad = ifelse(Trade.Flow == "Export", Value_origin * value, Value_origin)) %>%
      select(-value)




    # get total flow of exports/imports for that reporter and year for that commodity
    total_df <- raw.data.new %>%
      group_by(Reporter.ISO, Year, Trade.Flow) %>%
      summarise(total_for_c_origin = sum(Value_origin, na.rm = TRUE), total_for_c_oecd = sum(CIFValue_oecd, na.rm = TRUE), total_for_c_unctad = sum(CIFValue_unctad, na.rm = TRUE))

  }




  return(total_df)
}















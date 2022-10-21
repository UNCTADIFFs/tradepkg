#' Title extract data for relative calculating (total flow of exports/imports for that reporter and year; total flow of exports/imports for that reporter and year for that commodity)
#' @param rcode Country(s) of interest(ISO code), as a character vector.("All" to represent all countries)
#' @param year Data for which year(s) of interest, as a character vector.
#' @param cccode commodity code, as a character vector.
#'
#' @return A dataframe that stores total value for later relative calculating
#' @export
#'
#' @examples \dontrun{
#' for specific commodity
#' total_for_c <- getotal(rcode = "EGY", year = "2018", ccode= "190230")
#' for all commodities
#' total_for_all <- getotal(rcode, year, "all")
#' }
getotal <- function(rcode, year, ccode){

  # beta
  x <- 0.06
  load("data/beta.rda")
  betadf <-
    beta %>%
    mutate(value = tidyr::replace_na(value, x))

  # convert ISO code to country_code
  load("data/countrykey.rda")
  if(all(rcode != "ALL")){
    if(length(rcode) != 1){
      r <- c()
      for(i in 1:length(pcode)){
        r[i] <- countrykey[which(countrykey$ISO3 == rcode[i]), "country_code"]$country_code

      }
    }
    r <- countrykey[which(countrykey$ISO3 == rcode), "country_code"]$country_code
  }


  year <- stringr::str_replace_all(toString(year), " ","")
  rcode <- stringr::str_replace_all(toString(r), " ","")

  if(all(ccode != "ALL")){
  ccode <- stringr::str_replace_all(toString(ccode), " ","")
  # define url address
  stringr <- paste("http://comtrade.un.org/api/get?"
                   ,"max=",50000,"&" #maximum no. of records returned
                   ,"type=","C","&" #type of trade (c=commodities)
                   ,"freq=","A","&" #frequency
                   ,"px=","HS","&" #classification
                   ,"ps=",year,"&" #time period
                   ,"r=",rcode,"&" #reporting area
                   ,"p=","all","&" #partner country
                   ,"rg=","all","&" #trade flow
                   ,"cc=",ccode,"&" #classification code
                   ,"fmt=","csv"        #Format
                   ,sep = ""
  )
  # extract data
  raw.data <- read.csv(stringr,header=TRUE)
  # clean raw data
  raw.data <- raw.data %>%
    select(Classification, Year, Trade.Flow.Code, Trade.Flow, Reporter.ISO, Reporter, Partner.ISO, Partner, Commodity.Code, Commodity, Trade.Value..US..)

  # calculate CIF
  raw.data.new <- raw.data %>%
    select(Classification, Year, Trade.Flow.Code, Trade.Flow, Reporter.ISO, Reporter, Partner.ISO, Partner, Commodity.Code, Commodity, Trade.Value..US..) %>%
    rename(Commodity.code.6 =  `Commodity.Code`) %>%
    mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
    left_join(betadf, by = c("Reporter.ISO" = "importeriso", "Partner.ISO" = "exporteriso", "Commodity.code.4" = "commoditycode", "Year" = "year")) %>%
    mutate(value = tidyr::replace_na(value, x)) %>%
    filter(Partner.ISO != "WLD") %>%
    mutate(CIFValue = ifelse(Trade.Flow == "Export", Trade.Value..US.. * value, Trade.Value..US..)) %>%
    group_by(Year, `Trade.Flow`, `Partner.ISO`, `Reporter.ISO`, `Commodity.code.6`) %>%
    summarise(CIFValue = sum(CIFValue))
  # get total flow of exports/imports for that reporter and year for that commodity
  total_df <- raw.data.new %>%
    group_by(Reporter.ISO, Year, Trade.Flow, Commodity.code.6) %>%
    summarise(total_for_c = sum(CIFValue, na.rm = TRUE))
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
  raw.data.r <- read.csv(stringr,header=TRUE)

  # calculate CIF
  raw.data.r <- raw.data.r %>%
    select(Classification, Year, Trade.Flow.Code, Trade.Flow, Reporter.ISO, Reporter, Partner.ISO, Partner, Commodity.Code, Commodity, Trade.Value..US..) %>%
    rename(Commodity.code.6 =  `Commodity.Code`) %>%
    mutate(Commodity.code.4 = as.numeric(substring(Commodity.code.6, 1, 4))) %>%
    left_join(betadf, by = c("Reporter.ISO" = "importeriso", "Partner.ISO" = "exporteriso", "Commodity.code.4" = "commoditycode", "Year" = "year")) %>%
    mutate(CIFValue = ifelse(Trade.Flow == "Export", Trade.Value..US.. * value, Trade.Value..US..)) %>%
    group_by(Year, `Trade.Flow`, `Partner.ISO`, `Reporter.ISO`, `Commodity.code.6`) %>%
    summarise(CIFValue = sum(CIFValue))

  # get total flow of exports/imports for that reporter and year
  total_df <- raw.data.r %>%
    group_by(Reporter.ISO, Year, Trade.Flow) %>%
    summarise(total_for_all = sum(CIFValue))
  }




   return(total_df)
}















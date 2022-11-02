#' Get UN Comtrade data
#'
#' @param year Data for which year(s) of interest, as a character vector.
#' @param rcode Country(s) of interest(ISO code), as a character vector.("All" to represent all countries)
#' @param pcode Country(s) that have interacted with the reporter country(s). ("All" to represent all countries)
#' @param ccode commodity code, as a character vector
#'
#' @return a list of two data frames, country of interest is reporter and country of interest is partner
#' @export
#'
#' @examples \dontrun{
#' dlist <- extract(year = "2018", rcode = "EGY", pcode = "ARE", ccode = "190230")
#' dlist <- extract(year = "2018,2017", rcode = "EGY", pcode = "ARE", ccode = "190230")
#' dlist <- extract(year = "2018,2017", rcode = "EGY", pcode = "ARE", ccode = c("190230", "190190"))
#' dlist <- extract(year = "2018", rcode = "EGY", pcode = c("ARE","CHN"), ccode = c("190230", "190190"))
#' rcode = "CHN", year = "2016", ccode = "110900", pcode = "NPL"
#' dlist <- extract(year = "2016", rcode = "CHN", pcode = "NPL", ccode = "110900")
#' dlist$`reporter=reporter`; dlist$`reporter=partner`
#' dlist$`total_for_all`; dlist$`total_for_c`
#' }
extract <- function(year
                    ,rcode
                    ,pcode
                    ,ccode
)
{
  # convert ISO code to country_code
  load("data/countrykey.rda")
  if(all(rcode != "ALL")){
    if(length(rcode) != 1){
    r <- c()
    for(i in 1:length(rcode)){
      r[i] <- countrykey[which(countrykey$ISO3 == rcode[i]), "country_code"]$country_code

    }
    }
      r <- countrykey[which(countrykey$ISO3 == rcode), "country_code"]$country_code
    }


  if(all(pcode != "ALL")){
    if(length(pcode) != 1){
    p <- c()
    for(i in 1:length(pcode)){
     p[i] <- countrykey[which(countrykey$ISO3 == pcode[i]), "country_code"]$country_code

    }
  }
    p <- countrykey[which(countrykey$ISO3 == pcode), "country_code"]$country_code
  }

  # characterized parameters
  year <- stringr::str_replace_all(toString(year), " ","")
  rc <- stringr::str_replace_all(toString(r), " ","")
  pc <- stringr::str_replace_all(toString(p), " ","")
  cc <- stringr::str_replace_all(toString(ccode), " ","")



  # define url address
  stringr <- paste("http://comtrade.un.org/api/get?"
                 ,"max=",50000,"&" #maximum no. of records returned
                 ,"type=","C","&" #type of trade (c=commodities)
                 ,"freq=","A","&" #frequency
                 ,"px=","HS","&" #classification
                 ,"ps=",year,"&" #time period
                 ,"r=",rc,"&" #reporting area
                 ,"p=",pc,"&" #partner country
                 ,"rg=","all","&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=","csv"        #Format
                 ,sep = ""
  )

    # extract data
    raw.data.r <- read.csv(stringr,header=TRUE)
    #
    raw.data.r <- raw.data.r %>%
      select(Classification, Year, Trade.Flow.Code, Trade.Flow, Reporter.ISO, Reporter, Partner.ISO, Partner, Commodity.Code, Commodity, Trade.Value..US..)

    stringp <- paste("http://comtrade.un.org/api/get?"
                     ,"max=",50000,"&" #maximum no. of records returned
                     ,"type=","C","&" #type of trade (c=commodities)
                     ,"freq=","A","&" #frequency
                     ,"px=","HS","&" #classification
                     ,"ps=",year,"&" #time period
                     ,"r=",pc,"&" #reporting area
                     ,"p=",rc,"&" #partner country
                     ,"rg=","all","&" #trade flow
                     ,"cc=",cc,"&" #classification code
                     ,"fmt=","csv"        #Format
                     ,sep = ""
    )

    # extract data
    raw.data.p <- read.csv(stringp,header=TRUE)


    # get total value
    # for specific commodity
    total_for_c <- getotal(rcode, year, ccode)
    # for all commodities
    #total_for_all <- getotal(rcode, year, "all")

    # store the results as a list
    # raw.data <- list("reporter=reporter" = raw.data.r, "reporter=partner" = raw.data.p, "total_for_c" =  total_for_c, "total_for_all" = total_for_all)

    raw.data <- list("reporter=reporter" = raw.data.r, "reporter=partner" = raw.data.p, "total_for_c" =  total_for_c)


    # return data
    return(raw.data)

  }


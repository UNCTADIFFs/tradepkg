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
#' dlist <- extract(year = "2018", rcode = c("AFG", "ALB"), pcode = "ARE", ccode = "190230")
#' dlist$`reporter=reporter`; dlist$`reporter=partner`
#' }
extract <- function(year
                    ,rcode
                    ,pcode
                    ,ccode
)
{
  # convert ISO code to country_code
  load("data/countrydf.rda")
  if(rcode != "ALL"){
  rcode <- as.vector(countrydf[which(countrydf$ISO3 == rcode), "country_code"])
  }
  if(pcode != "ALL"){
  pcode <- as.vector(countrydf[which(countrydf$ISO3 == pcode), "country_code"])
  }

  # characterized parameters
  year <- stringr::str_replace_all(toString(year), " ","")
  rcode <- stringr::str_replace_all(toString(rcode), " ","")
  pcode <- stringr::str_replace_all(toString(pcode), " ","")
  ccode <- stringr::str_replace_all(toString(ccode), " ","")



  # define url address
  stringr <- paste("http://comtrade.un.org/api/get?"
                 ,"max=",50000,"&" #maximum no. of records returned
                 ,"type=","C","&" #type of trade (c=commodities)
                 ,"freq=","A","&" #frequency
                 ,"px=","HS","&" #classification
                 ,"ps=",year,"&" #time period
                 ,"r=",rcode,"&" #reporting area
                 ,"p=",pcode,"&" #partner country
                 ,"rg=","all","&" #trade flow
                 ,"cc=",ccode,"&" #classification code
                 ,"fmt=","csv"        #Format
                 ,sep = ""
  )

    # extract data
    raw.data.r <- read.csv(stringr,header=TRUE)

    stringp <- paste("http://comtrade.un.org/api/get?"
                     ,"max=",50000,"&" #maximum no. of records returned
                     ,"type=","C","&" #type of trade (c=commodities)
                     ,"freq=","A","&" #frequency
                     ,"px=","HS","&" #classification
                     ,"ps=",year,"&" #time period
                     ,"r=",pcode,"&" #reporting area
                     ,"p=",rcode,"&" #partner country
                     ,"rg=","all","&" #trade flow
                     ,"cc=",ccode,"&" #classification code
                     ,"fmt=","csv"        #Format
                     ,sep = ""
    )

    # extract data
    raw.data.p <- read.csv(stringp,header=TRUE)

    raw.data <- list("reporter=reporter" = raw.data.r, "reporter=partner" = raw.data.p)
    # return data
    return(raw.data)

  }


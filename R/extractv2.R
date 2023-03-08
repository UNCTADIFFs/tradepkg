
#' extract data for paper
#'
#' @param year
#' @param rcode
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' dlist <- extractv2("2016", "all")
#'
#'
#' }
extractv2 <- function(year, rcode){
  options(timeout = 10000)

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

  # characterized parameters
  year <- stringr::str_replace_all(toString(year), " ","")
  rc <- stringr::str_replace_all(toString(rcode), " ","")
# generate URL
stringr <- paste("https://comtrade.un.org/api/get/bulk/C/A/"
                 , year,"/"
                 , rc, "/"
                 , "HS?"
                 , "token=", "JDtnTvUZt7dIR0EGZmZBcuDj8CnlnhNLhfR75cV5yNbtGsPjE3SaIhBvYlGTtSJyWepgdaHJsjWjTXvycdRd9TbsAuKNwQBHxO0WTWA7JBtdGKg5ZbkE3cnwwdIVJ/R07W1dCPSf6737gGlmsK12jQ=="
                 ,sep = ""
)


# download data file
# download.file(stringr,  destfile = "d.zip",cacheOK=FALSE, quiet = TRUE, mode = "wb")
 GET(url = url,
    write_disk("gtfs.zip"),
    verbose()) -> res
 #
 df <- read_csv(archive_read("gtfs.zip"), col_types = cols())
 # select countries of interest and their mirroring data


 }

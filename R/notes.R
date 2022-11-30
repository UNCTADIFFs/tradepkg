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

download.file(fl,  destfile = "./txt.gz",cacheOK=TRUE)



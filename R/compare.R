#' Title compare national data with Comtrade data
#'
#' @param data national datasets
#' @param datalist output of extract() function
#'
#' @return results of comparison dataframe
#' @export
#'
#' @examples \dontrun{
#' dlist <- extract(year = "2018", rcode = "EGY", pcode = "ARE", ccode = "190230")
#' datadf <- ndata
#' comparedf <- compare(ndata, dlist)
#' }
compare <- function(datadf, datalist){
  # national data
  ndata <- datadf
  # load country key
  load("data/countrykey.rda")
  # load country of interest (coi) (where the country should be reporter)
  coi <- dlist$`reporter=reporter`

  # translate ISO2 code into ISO3 code
  if(all(is.na(ndata$ISO3))){
  ndata <- merge(ndata, countrykey, by = "ISO2")
  ndata <- ndata %>%
    rename(ISO3 = ISO3.y)
  }


  # compare two datasets and calculate dsp
  comparedf <- coi %>%
    mutate(Commodity.Code = as.character(Commodity.Code)) %>%
    left_join(ndata, by = c("Year" = "year", "Partner.ISO" = "ISO3", "Commodity.Code" = "code(6)")) %>%
    mutate(diff = `Trade.Value..US..` - `value($)`) %>%
    arrange(desc(diff)) %>%
    select(Year, Trade.Flow, Reporter.ISO, Partner.ISO, Commodity.Code, Trade.Value..US.., `value($)`, diff)


  return(comparedf)


}

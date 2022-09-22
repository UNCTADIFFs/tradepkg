#' Title compare national data with Comtrade data
#'
#' @param data national datasets
#' @param datalist output of extract() function
#'
#' @return results of comparison dataframe
#' @export
#'
#' @examples \dontrun{
#' dlist <- extract(year = "2018,2019", rcode = "818", pcode = "51,699", ccode = "190230,844180")
#' datadf <- ndata
#' comparedf <- compare(ndata, dlist)
#' }
compare <- function(datadf, datalist){
  # national data
  ndata <- datadf
  # load country of interest (coi) (where the country should be reporter)
  coi <- dlist$`reporter=reporter`
  # compare two datasets and calculate dsp
  comparedf <- coi %>%
    mutate(Commodity.Code = as.numeric(Commodity.Code)) %>%
    left_join(ndata, by = c("Year" = "year", "Partner.ISO" = "CODIZO3", "Commodity.Code" = "code(6)")) %>%
    mutate(diff = `Trade.Value..US..` - `value($)`) %>%
    arrange(desc(diff)) %>%
    select(Year, Trade.Flow, Reporter.ISO, Partner.ISO, Commodity.Code, Trade.Value..US.., `value($)`, diff)


  return(comparedf)


}


context("The results of calculating dsp are reasonable")

# clean work dictionary
rm(list = ls())
# upload package
library(tradedsp)

# test preparation
# load all partners, all commodities for Egypt in 2019
load("data/EGY2019.rda")
# extract data
dlist <- extract(year = "2019", rcode = "EGY", pcode = "ARE", ccode = "190230")
# get dsp dataframe
df_dsp <- calculatedsp(dlist)


# test missing value
test_that("No missing value in dsp output", {
  # test whether there is no missing value
  expect_equal(all(is.na(df_dsp) == FALSE), TRUE)

})

# test both absolute and relative value are calculated
test_that(" both absolute and relative value are calculated ", {
  # test whether there is calculation of absolute and relative value
  expect_equal(all(c("diff", "ratio_to_total_c", "ratio_to_total_all") %in% colnames(df_dsp) == TRUE), TRUE)

})

# test whether Relative discrepancy (as a percentage of within that commodity and flow) are calculated correctly
test_that(" Relative discrepancy are calculated correctly", {
  # test whether there is calculation of absolute and relative value
  expect_equal(all(df_dsp$ratio_to_total_c > 0) & all(df_dsp$ratio_to_total_c < 1), TRUE)

})








# demo for calculating dsp based on different beta value

## one country as the reporter:China

## revised getotal.R: total value for all commodities: total_for_all_origin; total_for_all_oecd; total_for_all_unctad

## revised calculatedsp.R: abs dsp:CIFValue_origin; CIFValue_oecd; CIFValue_unctad; diff_orgin; diff_oecd; diff_unctad

## revised calculatedsp.R: total_for_all_origin/diff_origin; total_for_all_oecd/diff_oecd; total_for_all_unctad/diff_unctad

# resource

## CHN_2016
## beta_oecd_2016
## beta_unctad_2016
## countrykey

# example

dlist <- extract(year = "2016", rcode = "CHN", pcode = "NPL", ccode = c("110900", "250100"))

source("total_all.R")

dlist$total_for_all <- total_df

df_dsp <- calculatedsp(dlist)


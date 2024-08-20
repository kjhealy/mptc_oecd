## Get and save the data from the OECD api

library(tidyverse)
library(here)

## TSV file
iso3 <- read_tsv(here("data_raw", "countries_iso3.tsv"))

get_oecd_csv <- function(api_url) {
  format_type <- "&format=csv"
  api_req <- paste0(api_url, format_type)
  read_csv(api_req)
}

my_vars <- c("REF_AREA", "MEASURE", "TIME_PERIOD", "OBS_VALUE")

## OECD HEALTH SPENDING
## https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C0%7CHealth%23HEA%23&fs[1]=Topic%2C1%7CHealth%23HEA%23%7CHealth%20expenditure%20and%20financing%23HEA_EXP%23&pg=0&fc=Topic&snb=4&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_SHA%40DF_SHA&df[ag]=OECD.ELS.HD&df[vs]=1.0&dq=USA%2BGBR%2BTUR%2BCHE%2BSWE%2BESP%2BSVN%2BSVK%2BPRT%2BPOL%2BNOR%2BNZL%2BNLD%2BMEX%2BLUX%2BLTU%2BLVA%2BKOR%2BJPN%2BITA%2BISR%2BIRL%2BISL%2BHUN%2BGRC%2BDEU%2BFRA%2BFIN%2BEST%2BDNK%2BCZE%2BCRI%2BCOL%2BCHL%2BCAN%2BAUT%2BAUS%2BBEL.A.EXP_HEALTH.USD_PPP_PS._T.._T._T._T...Q&pd=1970%2C2023&to[TIME_PERIOD]=false&vw=tb
health_ppp_percap_url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_SHA@DF_SHA,1.0/USA+GBR+TUR+CHE+SWE+ESP+SVN+SVK+PRT+POL+NOR+NZL+NLD+MEX+LUX+LTU+LVA+KOR+JPN+ITA+ISR+IRL+ISL+HUN+GRC+DEU+FRA+FIN+EST+DNK+CZE+CRI+COL+CHL+CAN+AUT+AUS+BEL.A.EXP_HEALTH.USD_PPP_PS._T.._T._T._T...Q?startPeriod=1970&endPeriod=2023"

health_df_raw <- get_oecd_csv(health_ppp_percap_url)

health_df <- health_df_raw |>
  select(all_of(my_vars))

## OECD LIFE EXP
## https://data-explorer.oecd.org/vis?lc=en&pg=0&snb=17&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HEALTH_STAT%40DF_LE&df[ag]=OECD.ELS.HD&df[vs]=1.0&dq=USA%2BGBR%2BTUR%2BCHE%2BSWE%2BESP%2BSVN%2BSVK%2BPRT%2BPOL%2BNOR%2BNZL%2BNLD%2BMEX%2BLUX%2BLTU%2BLVA%2BKOR%2BJPN%2BITA%2BISR%2BIRL%2BISL%2BHUN%2BGRC%2BDEU%2BFRA%2BFIN%2BEST%2BDNK%2BCZE%2BCRI%2BCOL%2BCHL%2BCAN%2BBEL%2BAUT%2BAUS.A.LFEXP..Y0._T.......&pd=1960%2C2023&to[TIME_PERIOD]=false&tm=life%20expectancy
lifexp_url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.HD,DSD_HEALTH_STAT@DF_LE,1.0/USA+GBR+TUR+CHE+SWE+ESP+SVN+SVK+PRT+POL+NOR+NZL+NLD+MEX+LUX+LTU+LVA+KOR+JPN+ITA+ISR+IRL+ISL+HUN+GRC+DEU+FRA+FIN+EST+DNK+CZE+CRI+COL+CHL+CAN+BEL+AUT+AUS.A.LFEXP..Y0._T.......?startPeriod=1960&endPeriod=2023"

lifexp_df_raw <- get_oecd_csv(lifexp_url)


lifexp_df <- lifexp_df_raw |>
  select(all_of(my_vars))


df <- bind_rows(lifexp_df, health_df) |>
  pivot_wider(names_from = MEASURE, values_from = OBS_VALUE) |>
  arrange(REF_AREA, TIME_PERIOD) |>
  left_join(iso3, join_by(REF_AREA == iso3)) |>
  janitor::clean_names() |>
  rename(
    iso3 = ref_area,
    year = time_period,
    life_exp = lfexp,
    health_ppp = exp_health,
    country = cname
  ) |>
  relocate(country) |>
  mutate(year = as.integer(year))


write_csv(df, file = here("data", "oecd_health_lifexp.csv"))

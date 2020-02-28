Homework 2: Bridges and Unemployment
================
Lucas Stormowski
2/27/2020

``` r
rm(list = ls())
library(blscrapeR)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
df <- get_bls_county()
WIunemployment = df %>% filter(fips_state == 55)
bridges = read_csv("https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   STRUCTURE_NUMBER_008 = col_character(),
    ##   ROUTE_NUMBER_005D = col_character(),
    ##   HIGHWAY_DISTRICT_002 = col_character(),
    ##   COUNTY_CODE_003 = col_character(),
    ##   FEATURES_DESC_006A = col_character(),
    ##   CRITICAL_FACILITY_006B = col_logical(),
    ##   FACILITY_CARRIED_007 = col_character(),
    ##   LOCATION_009 = col_character(),
    ##   LRS_INV_ROUTE_013A = col_character(),
    ##   LAT_016 = col_character(),
    ##   LONG_017 = col_character(),
    ##   MAINTENANCE_021 = col_character(),
    ##   OWNER_022 = col_character(),
    ##   FUNCTIONAL_CLASS_026 = col_character(),
    ##   DESIGN_LOAD_031 = col_character(),
    ##   RAILINGS_036A = col_character(),
    ##   TRANSITIONS_036B = col_character(),
    ##   APPR_RAIL_036C = col_character(),
    ##   APPR_RAIL_END_036D = col_character(),
    ##   NAVIGATION_038 = col_character()
    ##   # ... with 41 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
bridges = bridges %>% 
  group_by(COUNTY_CODE_003) %>% 
  summarize(count = n(),deck = mean(as.numeric(DECK_COND_058), na.rm = T),
            super = mean(as.numeric(SUPERSTRUCTURE_COND_059), na.rm=T),
            sub = mean(as.numeric(SUBSTRUCTURE_COND_060), na.rm=T),
            channel = mean(as.numeric(CHANNEL_COND_061), na.rm=T),   
            culvert = mean(as.numeric(CULVERT_COND_062), na.rm=T),
            average_daily_traffic = mean(as.numeric(ADT_029)),
            max_daily_traffic = max(as.numeric(ADT_029)),
            average_lanes_on = mean(as.numeric(TRAFFIC_LANES_ON_028A)),
            max_lanes_on = mean(as.numeric(TRAFFIC_LANES_ON_028A)),
            average_sidewalk = mean(as.numeric(LEFT_CURB_MT_050A))
            ) %>% 
  left_join(WIunemployment,by = c("COUNTY_CODE_003" = "fips_county")) 
unemployed_model_using_average = lm(unemployed ~ average_daily_traffic + average_lanes_on + average_sidewalk, data=bridges)
summary(unemployed_model_using_average)
```

    ## 
    ## Call:
    ## lm(formula = unemployed ~ average_daily_traffic + average_lanes_on + 
    ##     average_sidewalk, data = bridges)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2822.9  -234.1   101.8   423.5  6645.3 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)           -6477.7121  4153.2223  -1.560   0.1235  
    ## average_daily_traffic     0.2860     0.1183   2.417   0.0183 *
    ## average_lanes_on       3118.2568  2144.3539   1.454   0.1505  
    ## average_sidewalk        772.6170  1334.7477   0.579   0.5646  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1129 on 68 degrees of freedom
    ## Multiple R-squared:  0.746,  Adjusted R-squared:  0.7348 
    ## F-statistic: 66.56 on 3 and 68 DF,  p-value: < 2.2e-16

``` r
unemployed_model_using_max = lm(unemployed ~ max_daily_traffic + max_lanes_on + average_sidewalk, data=bridges)
summary(unemployed_model_using_max)
```

    ## 
    ## Call:
    ## lm(formula = unemployed ~ max_daily_traffic + max_lanes_on + 
    ##     average_sidewalk, data = bridges)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2895.0  -327.2   117.9   400.0  6540.6 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -8.893e+03  2.426e+03  -3.666 0.000485 ***
    ## max_daily_traffic  3.116e-02  8.282e-03   3.762 0.000353 ***
    ## max_lanes_on       4.305e+03  1.268e+03   3.395 0.001151 ** 
    ## average_sidewalk   1.094e+03  1.189e+03   0.920 0.360994    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1071 on 68 degrees of freedom
    ## Multiple R-squared:  0.7717, Adjusted R-squared:  0.7616 
    ## F-statistic: 76.61 on 3 and 68 DF,  p-value: < 2.2e-16

``` r
rm(list = ls())
```

``` r
rm(list = ls())
```

``` r
rm(list = ls())
```

``` r
rm(list = ls())
```

``` r
rm(list = ls())
```

``` r
rm(list = ls())
```

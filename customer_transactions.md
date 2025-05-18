Customer Transactions
================
Car3lo
2025-05-15

# Curating Information from Customer and Bank Information

### Importing 4 Datasets

Importing the main dataset: `01 - Customer Info.csv` as `customer`

``` r
#getwd()
library(readr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ purrr     1.0.2
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
customer <- read_csv(file = "01 - Customer Info.csv",
                     col_names = TRUE,
                     na = "",
                     col_types = cols(CustomerId = col_integer(),
                                      age = col_integer(),
                                      surname = col_character(),
                                      middle = col_character(),
                                      given = col_character(),
                                      sex = col_factor(levels = c("Male", "Female")),
                                      bday = col_date(format = "%Y-%m-%d"),
                                      est_income = col_number())) %>% 
  distinct() %>% 
  group_by(CustomerId) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  mutate(age_group = case_when(
    age <= 24 ~ "24 and below",
    age >= 25 & age <= 39 ~ "25 to 39",
    age >= 40 & age <= 54 ~ "40 to 54",
    age >= 55 ~ "55 and up"
  ) %>%
    factor(levels = c("24 and below", "25 to 39", "40 to 54", "55 and up"), ordered = TRUE))

head(customer)
```

    ## # A tibble: 6 × 9
    ##   CustomerId surname  middle given  sex      age bday       est_income age_group
    ##        <int> <chr>    <chr>  <chr>  <fct>  <int> <date>          <dbl> <ord>    
    ## 1   15565701 Ferri    J      Susan  Female    39 1978-06-07      63000 25 to 39 
    ## 2   15565706 Akobundu I      David  Male      35 1982-06-14      56000 25 to 39 
    ## 3   15565714 Cattaneo S      Joseph Male      47 1970-06-05      73000 40 to 54 
    ## 4   15565779 Kent     N      Betty  Female    30 1987-05-18     165000 25 to 39 
    ## 5   15565796 Docherty O      Donald Male      48 1969-05-27      27000 40 to 54 
    ## 6   15565806 Toosey   S      Donald Male      38 1979-06-26      35000 25 to 39

``` r
ncol(customer)
```

    ## [1] 9

``` r
nrow(customer)
```

    ## [1] 9641

Importing the other datasets

``` r
transactions <- read_rds("02 - transactions.rds")
cards <- read_rds("03 - cards.rds")
menu <- read_csv("04 - menu items.csv", col_names = TRUE)
```

    ## Rows: 21 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): ItemCode, MenuItem, Category
    ## dbl (1): Price
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(transactions)
```

    ## # A tibble: 6 × 4
    ##   TransId        CardId    item  count
    ##   <chr>          <chr>     <chr> <int>
    ## 1 10000115634602 02B634602 BC        1
    ## 2 10000115634602 02B634602 FT        1
    ## 3 10000215634602 02B634602 Cap       1
    ## 4 10000215634602 02B634602 MS        1
    ## 5 10000315634602 01A634602 Cap       1
    ## 6 10000315634602 01A634602 FT        1

``` r
head(cards)
```

    ## # A tibble: 6 × 3
    ##   CardId    CustomerId Balance
    ##   <chr>          <int>   <dbl>
    ## 1 01A634602   15634602     700
    ## 2 02B634602   15634602     400
    ## 3 01A647311   15647311     700
    ## 4 02B647311   15647311     400
    ## 5 03C647311   15647311     100
    ## 6 01A619304   15619304     700

``` r
head(menu)
```

    ## # A tibble: 6 × 4
    ##   ItemCode MenuItem                Category   Price
    ##   <chr>    <chr>                   <chr>      <dbl>
    ## 1 BC       Brewed Coffee           Hot Drink    110
    ## 2 CA       Cafe Americano          Hot Drink    130
    ## 3 Cap      Cappuccino              Hot Drink    150
    ## 4 CarM     Caramel Macchiato       Hot Drink    170
    ## 5 SHC      Signature Hot Chocolate Hot Drink    130
    ## 6 ICA      Iced Cafe Americano     Cold Drink   170

### Summary Stats by sex and age group

``` r
customer %>% 
  left_join(cards, by = "CustomerId") %>% 
  left_join(transactions, by = "CardId") %>% 
  left_join(menu, by = c("item" = "ItemCode")) %>% 
  group_by(sex, age_group) %>% 
  summarize(
    "total transactions" = n(), # use this if you want to count same transaction ids are unique
    #"total transactions" = n_distinct(TransId), # use this if same transaction id = one transaction
    "total revenue" = sum(Price * count, na.rm = T)
  )
```

    ## `summarise()` has grouped output by 'sex'. You can override using the `.groups`
    ## argument.

    ## # A tibble: 10 × 4
    ## # Groups:   sex [2]
    ##    sex    age_group    `total transactions` `total revenue`
    ##    <fct>  <ord>                       <int>           <dbl>
    ##  1 Male   24 and below                 8479         1097860
    ##  2 Male   25 to 39                    96677        12223190
    ##  3 Male   40 to 54                    57302         7031305
    ##  4 Male   55 and up                    9150         1129055
    ##  5 Male   <NA>                          909          116225
    ##  6 Female 24 and below                 6423          856195
    ##  7 Female 25 to 39                    75021         9881045
    ##  8 Female 40 to 54                    48451         6245595
    ##  9 Female 55 and up                    8608         1116250
    ## 10 Female <NA>                          876          113955

### Customer details

``` r
customer %>% 
  left_join(cards, by = "CustomerId") %>% 
  left_join(transactions, by = "CardId") %>% 
  left_join(menu, by = c("item" = "ItemCode")) %>% 
  group_by(CustomerId) %>% 
  summarize(
    "totalbalance" = sum(Balance, na.rm = T),
    "avg #items per transactions" = mean(count, na.rm = T),
    "avg value per transaction" = mean(Price * count, na.rm = T),
    "total value of transactions" = sum(Price * count, na.rm = T),
    "total_points" = sum((Price * count) %/% 500, na.rm = TRUE)
  )
```

    ## # A tibble: 9,641 × 6
    ##    CustomerId totalbalance `avg #items per transactions` avg value per transac…¹
    ##         <int>        <dbl>                         <dbl>                   <dbl>
    ##  1   15565701         2000                          1                       124 
    ##  2   15565706        27600                          1                       114.
    ##  3   15565714        26600                          1.03                    118.
    ##  4   15565779        79400                          1.07                    138.
    ##  5   15565796         3600                          1                       115 
    ##  6   15565806        52500                          1.03                    126.
    ##  7   15565878        93600                          1.07                    133.
    ##  8   15565879         4200                          1                       131.
    ##  9   15565891        21200                          1.11                    134.
    ## 10   15565996        21600                          1.04                    124.
    ## # ℹ 9,631 more rows
    ## # ℹ abbreviated name: ¹​`avg value per transaction`
    ## # ℹ 2 more variables: `total value of transactions` <dbl>, total_points <dbl>

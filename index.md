English Premier League data
================

We’re going to download English Premier League results from this very
useful website: <https://www.football-data.co.uk/englandm.php>

We’ll be using [httr](https://cran.r-project.org/package=httr) package
to download the data and [tidyverse](https://www.tidyverse.org/)
collection for all kinds of manipulations:

``` r
library(httr)
library(tidyverse)
```

A few functions to make the code more readable.

Transform the first year of a Premier League season (e.g. 1993) to the
format used in football-data.co.uk URLs (“9394”):

``` r
int_to_season <- function(x) sprintf(
    "%02d%02d",
    x %% 100,
    x %% 100 + 1
)
```

``` r
int_to_season(1993)
```

    ## [1] "9394"

Building on the above function, generate URL for a specific season:

``` r
int_to_url <- function(x) x %>% 
    int_to_season() %>% 
    sprintf(
        "https://www.football-data.co.uk/mmz4281/%s/E0.csv",
        .
    )
```

``` r
int_to_url(2020)
```

    ## [1] "https://www.football-data.co.uk/mmz4281/2021/E0.csv"

This is how we get the season’s data:

``` r
int_to_url(2020) %>% 
    GET() %>% 
    content() %>% 
    head()
```

    ## No encoding supplied: defaulting to UTF-8.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Div = col_character(),
    ##   Date = col_character(),
    ##   Time = col_time(format = ""),
    ##   HomeTeam = col_character(),
    ##   AwayTeam = col_character(),
    ##   FTR = col_character(),
    ##   HTR = col_character(),
    ##   Referee = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## # A tibble: 6 x 106
    ##   Div   Date  Time  HomeTeam AwayTeam  FTHG  FTAG FTR    HTHG  HTAG HTR  
    ##   <chr> <chr> <tim> <chr>    <chr>    <dbl> <dbl> <chr> <dbl> <dbl> <chr>
    ## 1 E0    12/0~ 12:30 Fulham   Arsenal      0     3 A         0     1 A    
    ## 2 E0    12/0~ 15:00 Crystal~ Southam~     1     0 H         1     0 H    
    ## 3 E0    12/0~ 17:30 Liverpo~ Leeds        4     3 H         3     2 H    
    ## 4 E0    12/0~ 20:00 West Ham Newcast~     0     2 A         0     0 D    
    ## 5 E0    13/0~ 14:00 West Br~ Leicest~     0     3 A         0     0 D    
    ## 6 E0    13/0~ 16:30 Tottenh~ Everton      0     1 A         0     0 D    
    ## # ... with 95 more variables: Referee <chr>, HS <dbl>, AS <dbl>, HST <dbl>,
    ## #   AST <dbl>, HF <dbl>, AF <dbl>, HC <dbl>, AC <dbl>, HY <dbl>, AY <dbl>,
    ## #   HR <dbl>, AR <dbl>, B365H <dbl>, B365D <dbl>, B365A <dbl>, BWH <dbl>,
    ## #   BWD <dbl>, BWA <dbl>, IWH <dbl>, IWD <dbl>, IWA <dbl>, PSH <dbl>,
    ## #   PSD <dbl>, PSA <dbl>, WHH <dbl>, WHD <dbl>, WHA <dbl>, VCH <dbl>,
    ## #   VCD <dbl>, VCA <dbl>, MaxH <dbl>, MaxD <dbl>, MaxA <dbl>, AvgH <dbl>,
    ## #   AvgD <dbl>, AvgA <dbl>, `B365>2.5` <dbl>, `B365<2.5` <dbl>, `P>2.5` <dbl>,
    ## #   `P<2.5` <dbl>, `Max>2.5` <dbl>, `Max<2.5` <dbl>, `Avg>2.5` <dbl>,
    ## #   `Avg<2.5` <dbl>, AHh <dbl>, B365AHH <dbl>, B365AHA <dbl>, PAHH <dbl>,
    ## #   PAHA <dbl>, MaxAHH <dbl>, MaxAHA <dbl>, AvgAHH <dbl>, AvgAHA <dbl>,
    ## #   B365CH <dbl>, B365CD <dbl>, B365CA <dbl>, BWCH <dbl>, BWCD <dbl>,
    ## #   BWCA <dbl>, IWCH <dbl>, IWCD <dbl>, IWCA <dbl>, PSCH <dbl>, PSCD <dbl>,
    ## #   PSCA <dbl>, WHCH <dbl>, WHCD <dbl>, WHCA <dbl>, VCCH <dbl>, VCCD <dbl>,
    ## #   VCCA <dbl>, MaxCH <dbl>, MaxCD <dbl>, MaxCA <dbl>, AvgCH <dbl>,
    ## #   AvgCD <dbl>, AvgCA <dbl>, `B365C>2.5` <dbl>, `B365C<2.5` <dbl>,
    ## #   `PC>2.5` <dbl>, `PC<2.5` <dbl>, `MaxC>2.5` <dbl>, `MaxC<2.5` <dbl>,
    ## #   `AvgC>2.5` <dbl>, `AvgC<2.5` <dbl>, AHCh <dbl>, B365CAHH <dbl>,
    ## #   B365CAHA <dbl>, PCAHH <dbl>, PCAHA <dbl>, MaxCAHH <dbl>, MaxCAHA <dbl>,
    ## #   AvgCAHH <dbl>, AvgCAHA <dbl>

There are empty columns (and, sometimes, rows) in older seasons’ data,
so we better clean it up.

As we’re only interested in the result of each game, these are the
columns we need: date, home/away team, full-time home/away goals:

``` r
columns <- c(
    "Date",
    "HomeTeam",
    "AwayTeam",
    "FTHG",
    "FTAG"
)
```

This is a function that takes a year and fetches data for the
corresponding season. It also adds `Season` column to make it easier to
use a combined data set for multiple seasons. For extra points, it
suppresses warnings and drops empty rows:

``` r
get_season_data <- function(x) x %>% 
    int_to_url() %>% 
    GET() %>% 
    {
        suppressWarnings(
            content(
                .,
                col_types = cols(),
                encoding = "UTF-8"
            )
        )
    } %>% 
    mutate(
        Season = x
    ) %>% 
    select(
        Season,
        all_of(
            columns
        )
    ) %>% 
    filter(
        complete.cases(.)
    )
```

Having done the hard part, getting data for a bunch of seasons is now
just a few lines:

``` r
1993:2020 %>% 
    lapply(
        get_season_data
    ) %>% 
    bind_rows() %>% 
    write_csv(
        "EPL.csv"
    )
```

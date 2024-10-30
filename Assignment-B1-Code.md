Assignment B1
================

# Welcome to Mia’s Assignment B1!

Welcome to the code for my assignment! Throughout the following code, I
will create, document, demonstrate, and test a function. Follow along to
see the process.

------------------------------------------------------------------------

## Setup

Here, I will make sure I have all appropriate libraries loaded in.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.3

    ## Warning: package 'ggplot2' was built under R version 4.3.3

    ## Warning: package 'tidyr' was built under R version 4.3.3

    ## Warning: package 'readr' was built under R version 4.3.3

    ## Warning: package 'purrr' was built under R version 4.3.3

    ## Warning: package 'stringr' was built under R version 4.3.3

    ## Warning: package 'forcats' was built under R version 4.3.3

    ## Warning: package 'lubridate' was built under R version 4.3.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(testthat)
```

    ## Warning: package 'testthat' was built under R version 4.3.3

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
library(devtools)
```

    ## Warning: package 'devtools' was built under R version 4.3.3

    ## Loading required package: usethis

    ## Warning: package 'usethis' was built under R version 4.3.3

    ## 
    ## Attaching package: 'devtools'
    ## 
    ## The following object is masked from 'package:testthat':
    ## 
    ##     test_file

``` r
library(palmerpenguins)
```

    ## Warning: package 'palmerpenguins' was built under R version 4.3.3

If you don’t have the above packages installed, you can use the code
chunk below to install them.

------------------------------------------------------------------------

## Exercise 1 & 2: Make and Document a Function

Here, I will make my function. I will also document it, so that it is
easy to understand what I have done and why.

First, I will outline the purpose of my function. When working with
datasets, I often wish to produce multiple summary statistics for some
variable or other. Writing out the code for these summary stats,
especially if I wish to see them for multiple variables, can be quite
tiresome. So here, I will create a function that will produce a table of
the desired summary statistics for the variables I enter. These summary
statistics will be the: mean, range, minimum, first quartile, median,
third quartile, and maximum.

``` r
#' #Summary Statistics for a Numeric Variable 
#' 
#' @description
#' Creates a table of summary statistics for a variable. These summary statistics are the mean, range, minimum, first quartile, medan, third quartile, and maximum of the variable.
#' 
#' @param data The name of the dataset being pulled from. Named data so that it's purpose is clear.
#' @param ... The variable(s) used to group the data. Ellipses are used because the user may or may not want to group their data, or they may wish to group by one or multiple variables.
#' @param summ_var The variable for which the summary statistics will be generated. Named summ_var to be clear that this is the variable for which a summary will be generated.
#' @param na.rm=FALSE This allows for the inclusion or exclusion of NA values in the function.
#' 
#' @return A new data frame. Each column is one summary statistic. Each row is  for a combination of grouping variables.

total_summary <- function(data, ..., summ_var, na.rm = FALSE) {
  if(!is.numeric(data[[deparse(substitute(summ_var))]])) {
    stop("Sorry, this function only works for numeric variables.")
  }
  data %>%
    group_by(...) %>%
    summarize(mean = mean({{summ_var}}, na.rm = na.rm),
            range = max({{summ_var}}, na.rm = na.rm) - min({{summ_var}}, na.rm = na.rm),
            min = min({{summ_var}}, na.rm = na.rm),
            first_quartile = quantile({{summ_var}}, 0.25, na.rm = na.rm),
            median = median({{summ_var}}, na.rm = na.rm),
            third_quartile = quantile({{summ_var}}, 0.75, na.rm = na.rm),
            max = max({{summ_var}}, na.rm = na.rm))
}
```

------------------------------------------------------------------------

## Exercise 3: Include Examples

Here, I will show some example of my function, *total_summary*,
functioning. I will use the data from the *penguins* dataset that I
loaded in the **Setup** section, though any dataset should work.

First, let’s take a look at the *penguins* dataset.

``` r
print(penguins)
```

    ## # A tibble: 344 × 8
    ##    species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##    <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ##  1 Adelie  Torgersen           39.1          18.7               181        3750
    ##  2 Adelie  Torgersen           39.5          17.4               186        3800
    ##  3 Adelie  Torgersen           40.3          18                 195        3250
    ##  4 Adelie  Torgersen           NA            NA                  NA          NA
    ##  5 Adelie  Torgersen           36.7          19.3               193        3450
    ##  6 Adelie  Torgersen           39.3          20.6               190        3650
    ##  7 Adelie  Torgersen           38.9          17.8               181        3625
    ##  8 Adelie  Torgersen           39.2          19.6               195        4675
    ##  9 Adelie  Torgersen           34.1          18.1               193        3475
    ## 10 Adelie  Torgersen           42            20.2               190        4250
    ## # ℹ 334 more rows
    ## # ℹ 2 more variables: sex <fct>, year <int>

``` r
glimpse(penguins)
```

    ## Rows: 344
    ## Columns: 8
    ## $ species           <fct> Adelie, Adelie, Adelie, Adelie, Adelie, Adelie, Adel…
    ## $ island            <fct> Torgersen, Torgersen, Torgersen, Torgersen, Torgerse…
    ## $ bill_length_mm    <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, …
    ## $ bill_depth_mm     <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18.1, …
    ## $ flipper_length_mm <int> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190, 186…
    ## $ body_mass_g       <int> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 3475, …
    ## $ sex               <fct> male, female, female, NA, female, male, female, male…
    ## $ year              <int> 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007…

Now I will use this data to show the use of my function through 3
examples. These are:

- **Example 1:** One grouping variable, one summary variable.  
- **Example 2:** Two grouping variables, one summary variable.  
- **Example 3:** No grouping variables, one summary variable.

``` r
total_summary(penguins, species, summ_var = body_mass_g, na.rm = TRUE)
```

    ## # A tibble: 3 × 8
    ##   species    mean range   min first_quartile median third_quartile   max
    ##   <fct>     <dbl> <int> <int>          <dbl>  <dbl>          <dbl> <int>
    ## 1 Adelie    3701.  1925  2850          3350    3700           4000  4775
    ## 2 Chinstrap 3733.  2100  2700          3488.   3700           3950  4800
    ## 3 Gentoo    5076.  2350  3950          4700    5000           5500  6300

``` r
total_summary(penguins, species, island, summ_var = body_mass_g, na.rm = TRUE)
```

    ## `summarise()` has grouped output by 'species'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 9
    ## # Groups:   species [3]
    ##   species   island   mean range   min first_quartile median third_quartile   max
    ##   <fct>     <fct>   <dbl> <int> <int>          <dbl>  <dbl>          <dbl> <int>
    ## 1 Adelie    Biscoe  3710.  1925  2850          3388.   3750          3975   4775
    ## 2 Adelie    Dream   3688.  1750  2900          3388.   3575          3981.  4650
    ## 3 Adelie    Torger… 3706.  1800  2900          3338.   3700          4000   4700
    ## 4 Chinstrap Dream   3733.  2100  2700          3488.   3700          3950   4800
    ## 5 Gentoo    Biscoe  5076.  2350  3950          4700    5000          5500   6300

``` r
total_summary(penguins, summ_var = body_mass_g, na.rm = TRUE)
```

    ## # A tibble: 1 × 7
    ##    mean range   min first_quartile median third_quartile   max
    ##   <dbl> <int> <int>          <dbl>  <dbl>          <dbl> <int>
    ## 1 4202.  3600  2700           3550   4050           4750  6300

From these examples, we can see how my function works. I will include
one more example below, **Example 4**, which will demonstrate what it
looks like when my function is used on non-numeric data.

``` r
total_summary(penguins, species, summ_var = island, na.rm = TRUE)
```

    ## Error in total_summary(penguins, species, summ_var = island, na.rm = TRUE): Sorry, this function only works for numeric variables.

Here, you have seen some examples of my function working, and one where
it does not.

------------------------------------------------------------------------

## Exercise 4: Test the Function

Here, I will test my function. Using *expect\_()* functions and
*test_that()* form the *testthat* package, I will make sure that my
function works as intended. Each code chunk below will contain a test
for my function.

First, I will test that my function accurately displays an error when a
non-numeric variable is entered as the summary variable.

``` r
test_that("Non-numerical variable error", {
  expect_error(total_summary(penguins, species, summ_var = island, na.rm = TRUE))
})
```

    ## Test passed 😸

Second, I will test that the output of my function is of the expected
class.

``` r
test_that("Class of return from function", {
  expect_type(total_summary(penguins, species, summ_var = body_mass_g, na.rm = TRUE), "list")
})
```

    ## Test passed 🌈

Lastly, I will test that my function prints its result when run.

``` r
test_that("Output is visible", {
  expect_visible(total_summary(penguins, species, summ_var = body_mass_g, na.rm = TRUE))
})
```

    ## Test passed 😸

With these 3 tests done, my function has been tested to my satisfaction.

------------------------------------------------------------------------

## Conclusion

In this Assignment, I have created, documented, shown examples for, and
tested a new function of my own devising. This function,
*total_summary*, can be used to create a table of the mean, range,
minimum, first quartile, median, third quartile, and maximum values of a
variable. The user can choose whether they would like to group this
variable by some grouping variable when using the function.

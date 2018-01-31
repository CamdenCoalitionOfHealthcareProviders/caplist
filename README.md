Overview
--------

The `caplist` R package is used to clean monthly patient lists.

Installation
------------

``` r
# Install the caplist package using the devtools package:
install.packages("devtools")
devtools::install_github("CamdenCoalitionOfHealthcareProviders/caplist")
```

Usage
-----

In order to use the `caplist` package, please have the following packages installed: `tidyverse`, `zipcode`, `gtools`, `janitor`, and `reshape`.

``` r
library(caplist)

# Read in patient list file
x <- read_csv('c:/filepath/patient_list.csv')

# Clean patient list file (assuming it's a Galileo file)
y <- galileo(x)

# Export the file
write_csv(y, 'c:/filepath/y.csv', na = "")
```
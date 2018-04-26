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

# UNITED ------------------------------------------------------------------
# Read in patient list file
x <- read_csv('c:/filepath/patient_list.csv')

# Clean patient list file (assuming it's a Galileo file)
y <- united(x)


# CAMCARE UNITED ----------------------------------------------------------
# Read in patient list file
x <- read_csv('c:/filepath/patient_list.csv')

# Clean patient list file
y <- camcare(x)

# NORTHGATE ---------------------------------------------------------------
# Read in patient list file
x <- read_csv('c:/filepath/patient_list.csv')

# Clean patient list file
y <- northgate(x)

# Deduplication -----------------------------------------------------------
# Clean AllPayersHIEID file from CareEvolution for binding with result of allpayers_galileo() function
# a = AllPayersHIEID file from CareEvolution
# c = CurrentHIEIDs file from CareEvolution on the FTP
# allpayers_mco(a, c)

a <- read_csv('c:/filepath/AllPayerHIEIDs-2018-04-05.csv')
c <- read_csv("c:/filepath/CurrentHIEIDs.csv")
m <- allpayers_mco(a, c)

# Clean Galileo file for binding with result of allpayers_mco() function
# g = Galileo 'Capitation List' file
# c = CurrentHIEIDs file

g <- read_csv("c:/filepath/patient_list.csv/galileo_patient_list.csv")
g2 <- allpayers_gal(g, c)

# deduplicate:
# Combines results of allpayers_mco and allpayers_gal
# Keeps ACO records where HIE ID is NOT in AllPayers (MCO) data frame
# m = allpayers_mco() result
# g = allpayers_gal() results

d <- deduplicate(m, g2)

# Combine allpayers_mco and results of deduplicate
# Binds together MCO-only patient list and ACO patient list without MCO patients
# a = allpayers_mco() result
# d = deduplicate() result

# c <- combine(a, d)
co <- combine_mco_aco(m, d)

# Twins
# Needs `combined` function result
# Fine duplicate records based on `Patient ID HIE` field
# c = combind() results
t <- twins(c)


# Export Deduplication Results --------------------------------------------
write_csv(co, 'tv_import.csv')
```
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

In order to use the `caplist` package, please have the following packages installed: `tidyverse`,`janitor`, `gtools`,`zipcode`, 'dplyr', and 'magrittr'.

``` r
# This is a template to use the caplist R package.
# The caplist R package has functions used to clean the Camden Coalition's
# patient lists from MCO and ACO contracts.

# This file is meant to be run in sections, NOT all at once. You will need to have
# the most updated version of the package installed, plus the rest of the
# libraries loaded for each section.

# caplist on GitHub: https://github.com/CamdenCoalitionOfHealthcareProviders/caplist

# Install the caplist package using the devtools package:
install.packages("devtools")
devtools::install_github("CamdenCoalitionOfHealthcareProviders/caplist")

library(caplist)
library(tidyverse)
library(janitor)
library(gtools)
library(zipcode)
library(dplyr)
library(magrittr)

# UNITED ------------------------------------------------------------------
# Read in patient list file
x <- read_csv('c:/filepath/patient_list.csv')

# Clean patient list file (assuming it's a Galileo file)
y <- united(x)

# Export clean United patient list
write_csv(y, paste0('c:/filepath/', format(Sys.Date(), "%Y%m"), "/",format(Sys.Date(), "%Y-%m-%d-"),"United.csv"), na = "")

# CAMCARE UNITED ----------------------------------------------------------
# Read in patient list file
c <- read_csv('c:/filepath/patient_list.csv')

# Clean patient list file
c_clean <- camcare(x)

# Export clean Camcare United patient list
write_csv(c_clean, paste0('c:/filepath/', format(Sys.Date(), "%Y%m"), "/", format(Sys.Date(), "%Y-%m-%d-"),"UnitedCAMCare.csv"), na = "")

# NORTHGATE ---------------------------------------------------------------
# Read in patient list file
n <- read_csv('c:/filepath/patient_list.csv')

# Clean patient list file
n_clean <- northgate(x)

# Export clean Northgate patient list
write_csv(n_clean, paste0('c:/filepath/', format(Sys.Date(), "%Y%m"), "/", format(Sys.Date(), "%Y-%m-%d-"),"Northgate.csv"), na = "")

# Deduplication -----------------------------------------------------------
# Clean AllPayersHIEID file from CareEvolution for binding with result of allpayers_galileo() function
# a = AllPayersHIEID file from CareEvolution. Currently, it's the result of the united() function with HIE IDs attached.
# c = CurrentHIEIDs file from CareEvolution on the FTP in /Extract/Reports folder. File is CurrentHIEIDs.csv.
# allpayers_mco(a, c)

a <- read_csv('c:/filepath/AllPayerHIEIDs-2018-04-05.csv')
c <- read_csv("c:/filepath/CurrentHIEIDs.csv")
m <- allpayers_mco(a, c)

# Clean Galileo file for binding with result of allpayers_mco() function
# g = Galileo 'Capitation List' file located here: https://camdenhie.careevolution.com/CamdenGalileo/#/datasets/1011/reports/4330. Make sure to update Attribution End Date filter in Galileo before exporting.
# c = CurrentHIEIDs file

g <- read_csv("c:/filepath/patient_list.csv/Capitation List.csv")
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

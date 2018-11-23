##########################################################################
#
#  This program uses the TAXSIM functions to create datasets of tax liabilities.
#  These datasets are exported to csv files that can be run in the TAXSIM online
#
###########################################################################

library(tidyverse)
library(DBI)
library(data.table)

source('taxsim_functions.R')

# connect to PUMS database (not on Github)
con <- dbConnect(RSQLite::SQLite(), "pums_db.db")

#########################################################################################

# iterate through each year and create dataset for TAXSIM;
# add dataset to other years, so all years can be sent to TAXSIM at once

# initialize dataframe that will contain all years
taxes <- data.frame()

for (yr in seq(2006, 2017)) {
  
  print(yr)
  
 # calculate taxable income and write out results
 taxes <- pop_taxes(con, yr, household = FALSE) %>%
   bind_rows(taxes, .)
 
}

# write_csv(taxes, 'nc_to_taxsim_all.csv', col_names = FALSE)

###########################################################################
#
# use online taxsim to calculate taxes, and store output as a txt file by 
# copying and pasting results into text file
#
############################################################################

############################################################################

### take estimated tax liability from taxsim output and distill into estimate household tax liability

# import all files into a list
tax_liab <- read_delim('nc_from_taxsim_all.txt', delim = ' ',
                       col_types = cols(.default = "n")) %>%
  # payrool (FICA) taxes are employer's and employee's share
  # we only want employee's share, so cut in half
  mutate(fica = round(fica / 2,0),
        # calculate total tax liability, which is the sum of
        # federal income, state income, and payroll taxes (FICA)
        total_taxes = as.integer(fiitax + siitax + fica),
        # remove last two letters in taxsim_id (SERIALNO) and make SPORDER
        SPORDER = as.integer(str_extract(taxsim_id, '[0-9][0-9]$')),
        taxsim_id = as.integer(str_replace(taxsim_id, '[0-9][0-9]$', ''))) %>%
  # change name of taxsim_id to SERIALNO so that it matches PUMA terminology
  rename(SERIALNO = taxsim_id) %>%
  select(SERIALNO, SPORDER, year, total_taxes) %>%
  # sort by year and serial no
  arrange(year, SERIALNO)

# write out tax liabilities
saveRDS(tax_liab, 'nc_tax_liab_all.Rda')
##########################################################################
#
#  This program uses the TAXSIM functions to create datasets of tax liabilities.
#  These datasets are exported to csv files that can be run in the TAXSIM online
#
###########################################################################

library(tidyverse)
library(DBI)
library(data.table)

source('functions/puma_functions.R')
source('tax_puma_cal/taxsim_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

#########################################################################################

# iterate through each year and create dataset for TAXSIM;
# add dataset to other years, so all years can be sent to TAXSIM at once

# initialize dataframe that will contain all years
taxes <- data.frame()

for (yr in seq(2006, 2017)) {
  
  print(yr)
  
 # calculate taxable income and write out results
 taxes <- pop_taxes(con, yr) %>%
   # convert SERIALNO to numeric so we can combine years prior to 2017 with 2017
   mutate(SERIALNO = as.numeric(SERIALNO)) %>%
   bind_rows(taxes, .)
 
}

###########################################################################
#
# use online taxsim to calculate taxes, and store output as a txt file by 
# copying and pasting results into text file
#
############################################################################

############################################################################

### take estimated tax liability from taxsim output and distill into estimate household tax liability

# create list of all tax output files
file_names <- list.files('tax_puma_cal/nc_from_taxsim_online', full.names = TRUE)

# import all files into a list
tax_liab <- read_delim(file_names, delim = ' ',
                       col_types = cols(.default = "n")) %>%
  # calculate total tax liability, which is the sum of
  # federal income, state income, and payroll taxes (FICA)
  mutate(total_taxes = fiitax + siitax + fica) %>%
  # group by year and serial number to calculate household taxes
  group_by(taxsim_id, year) %>%
  summarize(tax_liability = sum(total_taxes)) %>%
  ungroup() %>%
  # change name of taxsim_id to SERIALNO so that it matches PUMA terminology
  rename(SERIALNO = taxsim_id) %>%
  # sort by year and serial no
  arrange(year, SERIALNO)

# write out tax liabilities
#write_csv(tax_liab, 'tax_puma_cal/nc_tax_liabilities.csv')

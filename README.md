# Calculate household tax liabilities for Public Use Microdata Areas

The NBER's [TAXSIM simulator](https://users.nber.org/~taxsim/taxsim27/) allows users to estimate federal and state income taxes, and FICA (payroll) taxes, for taxpayers. To calculate taxes for multiple tax units, unsers can enter a text file with each line representing a tx unit. The file requires a specific format with 27 completed values.

The US Census's [Public Use Microdata Areas (PUMA)](https://www.census.gov/programs-surveys/acs/data/pums.html) dataset contains individual responses to the American Community Survey. These responses include household and individual income, the relationship between persons in the household, and a host of other variables. 

We can estimate household tax liabilities by feeding PUMA data into the TAXSIM model; but the PUMA data must be heavily wrangled to achieve a format readable to the TAXSIM program. Wrangling includes creating taxpayer units from households, calculating taxable income for the taxpayer unit, determining the number of dependents, and estimating filing status.

This repo contains scripts that achieve the following:

- Convert PUMA data into a dataset that can be feed into the TAXSIM program;
- Take the output from the TAXSIM model and calculate aggregate household tax liabilities; and
- Subtract household tax liabilities from household income to create a variable signifying household post-tax income.

########################################################
### 20210201: Replicating Nabendu's SARFAESI project ###
########################################################

library(tidyverse)

### Reading relevant files ###

firm_identification_bse <- readr::read_csv("BSE Firms Identification.csv")

firm_capex <- readr::read_csv("Raw Data for Capex.csv") # But this file is about fixed asset additions?

# Remove the "Debt" column from this file since it has better sample in the file below
firm_total_assets <- readr::read_csv("Raw Data for Debt and Total Assets.csv") %>%
  dplyr::select(-Debt)

firm_debt_sec_unsec <- readr::read_csv("Raw Data for Secured Debt and Unsecured Debt.csv")
firm_market_to_book <- readr::read_csv("Raw Data for Market to Book.csv")
firm_net_fixed_assets <- readr::read_csv("Raw Data for Net Fixed Assets.csv")
firm_PBITDA <- readr::read_csv("Raw Data for PBITDA.csv")
firm_sales <- readr::read_csv("Raw Data for Sales.csv")
firm_net_sales_96_07 <- readr::read_csv("Raw Data Net Sales 1996_2007.csv")

# Note that the column names for EBIT had to be changed to the right format
firm_ebit <- readr::read_csv("Raw Data for EBIT.csv")

### Joining all files into one ###

firm_data_raw <- firm_capex %>%
  dplyr::full_join(., firm_total_assets, by = c("Company Name", "Code", "CIN code", "Year")) %>%
  dplyr::full_join(., firm_debt_sec_unsec, by = c("Company Name", "Code", "CIN code", "Year")) %>%
  dplyr::full_join(., firm_ebit, by = c("Company Name", "Code", "CIN code", "Year")) %>%
  dplyr::full_join(., firm_market_to_book, by = c("Company Name", "Code", "CIN code", "Year")) %>%
  dplyr::full_join(., firm_net_fixed_assets, by = c("Company Name", "Code", "CIN code", "Year")) %>%
  dplyr::full_join(., firm_net_sales_96_07, by = c("Company Name", "Code", "CIN code", "Year")) %>%
  dplyr::full_join(., firm_PBITDA, by = c("Company Name", "Code", "CIN code", "Year")) %>%
  dplyr::full_join(., firm_sales, by = c("Company Name", "Code", "CIN code", "Year")) %>%
  dplyr::left_join(., firm_identification_bse, by = c("Company Name", "Code")) %>% #Note change in join
  dplyr::filter(`Industry type` == 1)


# func_neg_NA_vec <- function(vec)
# {
#   #This function accepts a vector and turns its negative entries to NA
#   #This is used to convert vectors such as 'total assets' where there 
#   #may be negative entries by mistake, to those with no negative entries
#   vec[vec < 0] <- NA
#   return(vec)
# }

firm_data <- firm_data_raw %>%
  dplyr::mutate(Debt_ratio = Debt/`Total assets`,
                Sec_debt_ratio = `Secured borrowings`/`Total assets`,
                Unsec_debt_ratio = `Unsecured borrowings`/`Total assets`,
                Size = log(`Total assets`),
                Tangibility = `Net fixed assets`/`Total assets`,
                Operating_Profitability = PBDITA/`Total assets`,
                ln_Sales= log(Sales),
                MB = (`Market Capitalisation`*10^6)/(`Shares Outstanding`*`BV per Share`),
                Capex = `Gross fixed assets additions`/`Total assets`,
                ROA = EBIT/`Total assets`,
                SARFAESI = case_when(Year < 2002 ~ 0, 
                                     Year >= 2002 ~ 1)) %>%  
  dplyr::select(c(`Company Name`:Year, SARFAESI, Debt_ratio, Sec_debt_ratio, 
                  Unsec_debt_ratio, Size, Tangibility, Operating_Profitability,
                  ln_Sales, MB, Capex, ROA)) 


### Correlation matrix (table 2) ###

corr_matrix <- firm_data %>%
  dplyr::select(Debt_ratio:ROA) %>%
  cor(., use = "complete.obs")

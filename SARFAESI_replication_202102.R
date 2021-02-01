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


# Converting negative entries for positive variables to NA
firm_data_raw$`Total assets` <- ifelse(firm_data_raw$`Total assets` <= 0, NA, 
                                       firm_data_raw$`Total assets`)
firm_data_raw$`BV per Share` <- ifelse(firm_data_raw$`BV per Share` <= 0, NA,
                                       firm_data_raw$`BV per Share`)
firm_data_raw$`Net sales` <- ifelse(firm_data_raw$`Net sales` < 0, NA,
                                    firm_data_raw$`Net sales`)
firm_data_raw$Sales <- ifelse(firm_data_raw$Sales < 0, NA,
                                    firm_data_raw$Sales)


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

### No winsorization done ###

### Correlation matrix (table 2) ###

corr_matrix <- firm_data %>%
  dplyr::select(Debt_ratio:ROA) %>%
  cor(., use = "complete.obs")

### Descriptive Stats (table 3) ###

summ_stats <- apply(firm_data[, 6:15], 2, summary)
summ_stats_table <- t(summ_stats)

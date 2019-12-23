if(!require(easypackages)){install.packages("easypackages")}
library(easypackages)
packages("tidyverse", "vroom", "DBI", "RSQLite", "connections", "dbplyr", prompt = FALSE)


# You will need to download Fannie Mae's Single-Family Loan Performance Data from Fannie Mae's website 
# at https://loanperformancedata.fanniemae.com/lppub/index.html.
# This will work with any number of pairs of Acquisition and Performance files. 

# File Location ----
fileslocation <-"d:/LargeData/BSUSQL/lab_22_sql_advanced - llpro/2000Q1/"

numberoffiles <-length(list.files(fileslocation, pattern = glob2rx("*txt"), full.names=TRUE))
numberoffiles

Acquisitions_File_List <- list.files(fileslocation, pattern = glob2rx("*Acquisition*txt"), full.names=TRUE)

Performance_File_List <- list.files(fileslocation, pattern = glob2rx("*Performance*txt"), full.names=TRUE)


# Download Data ----
my_App_Data <- vroom("d:/LargeData/BSUSQL/lab_22_sql_advanced - llpro/2000Q1/Acquisition_2000Q1.txt")
colnames(my_App_Data) <- 
         c("loan_id", "original_channel", "seller_name", "original_interest_rate", "original_upb", "original_loan_term", 
           "original_date","first_pay_date", "original_ltv", "original_cltv", "number_of_borrowers", "original_dti", 
           "original_borrower_credit_score", "first_time_home_buyer", "loan_purpose", "property_type","number_of_units", 
           "occupancy_status", "property_state", "zip", "primary_mortgage_insurance_percent", "product_type", 
           "original_coborrower_credit_score", "mortgage_insurance_type", "relocation_mortgage_indicator")
my_App_Data <- my_App_Data %>% mutate(loan_id            = as.character(loan_id),
                                      original_cltv      = as.numeric(original_cltv),
                                      original_upb       = as.integer(original_upb),
                                      original_loan_term = as.integer(original_loan_term),
                                      number_of_units    = as.integer(number_of_units),
                                      zip                = as.integer(zip))
# glimpse(my_App_Data)

my_Perf_Data <- vroom("d:/LargeData/BSUSQL/lab_22_sql_advanced - llpro/2000Q1/Performance_2000Q1.txt")
colnames(my_Perf_Data) <- 
     c("loan_id", "monthly_reporting_period", "servicer_name", "current_interest_rate", "current_upb", "loan_age", 
       "remaining_months_to_legal_maturity", "adj_remaining_months_to_maturity", "maturity_date", "msa", 
       "current_loan_delinquency_status", "modification_flag", "zero_balance_code", "zero_balance_effective_date", 
       "last_paid_installement_date", "foreclosed_after","disposition_date", "foreclosure_costs", 
       "prop_preservation_and_repair_costs", "asset_recovery_costs", "misc_holding_costs", "holding_taxes", 
       "net_sale_proceeds", "credit_enhancement_proceeds", "repurchase_make_whole_proceeds", "other_foreclosure_proceeds", 
       "non_interest_bearing_upb", "pricipal_forgiveness_upd", "repurchase_make_whole_proceeds_flag", 
       "foreclosure_principal_write_off_amount", "servicing_activity_indicator")
my_Perf_Data <- my_Perf_Data %>% mutate(loan_id = as.character(loan_id),
                                        current_loan_delinquency_status  = as.numeric(current_loan_delinquency_status),
                                        msa                              = as.numeric(msa),
                                        last_paid_installement_date      = as.character(last_paid_installement_date),
                                        foreclosed_after                 = as.character(foreclosed_after),
                                        disposition_date                 = as.character(disposition_date),
                                        foreclosure_costs                = as.numeric(foreclosure_costs),
                                        prop_preservation_and_repair_costs = as.numeric(prop_preservation_and_repair_costs),
                                        asset_recovery_costs             = as.numeric(asset_recovery_costs),
                                        misc_holding_costs               = as.numeric(misc_holding_costs),
                                        holding_taxes                    = as.numeric(holding_taxes),
                                        net_sale_proceeds                = as.numeric(net_sale_proceeds),
                                        credit_enhancement_proceeds      = as.numeric(credit_enhancement_proceeds),
                                        repurchase_make_whole_proceeds   = as.numeric(repurchase_make_whole_proceeds),
                                        other_foreclosure_proceeds       = as.numeric(other_foreclosure_proceeds),
                                        pricipal_forgiveness_upd         = as.numeric(pricipal_forgiveness_upd),
                                        foreclosure_principal_write_off_amount = as.numeric(foreclosure_principal_write_off_amount))

#glimpse(my_Perf_Data)     

# Save to dB ----
mydB <- dbConnect(SQLite(), "d:/LargeData/BSUSQL/my_dB.sqlite")
dbWriteTable(mydB, "acquisitions", my_App_Data, overwrite = TRUE)
dbWriteTable(mydB, "performance", my_Perf_Data, overwrite = TRUE)

# Save Multiple Files ----



# dB Disconnect ----
dbDisconnect(mydB)
 
 
# https://db.rstudio.com/databases/sqlite/
# https://loanperformancedata.fanniemae.com/lppub-docs/FNMA_SF_Loan_Performance_File_layout.pdf
# https://loanperformancedata.fanniemae.com/lppub/index.html myN3wP@ssword
# https://github.com/ahopp/FanMaeProject/blob/master/Loss_Data_Infile.r
# 
# https://github.com/toddwschneider/agency-loan-level/blob/master/analysis/analysis.R
# Interesting:  https://github.com/saadaslam/fannieapi


# Save a Copy to disk by executing the following line of code: 
# save(Performance_Data, file="FANNIEMAE_Performance_Data.Rda")

# Save a Copy to disk by executing the following line of code: 
# save(Acquisitions_Data, file="FANNIEMAE_Acquisitions_Data.Rda")

rm(list= ls()[!(ls() %in% c('my_App_Data', 'my_Perf_Data'))])


#  leewacc.com
#  Home of 'Value-Additivity' Compliant 1) 'Circular' & 2) 'Noncircular' DCF valuation models

# 'Advanced Discounted Cash Flow (DCF) Valuation Using R' textbook (Amazon website)
#  by Brian K. Lee, MBA, PRM, CMA, CFA

#  Book publication date: May 10, 2021

#  leewacc.com ( ... your source for 'unified' DCF valuation solutions)

# WACC = Weighted Average Cost of Capital = 'Circular' DCF calculation to arrive at asset value; V = PV[FCFF, WACC]
# LEEWACC =  'Noncircular' WACC = 'Noncircular' DCF asset valuation; V = PV[FCFF, LEEWACC] = PV[FCFF, WACC]

# No warranty or support relative to this code is provided. 
# Author is not responsible for any circumstances resulting from use or misuse of this code. 

# The textbook serves as documentation for the R code and DCF methods employed.

# The 'R' and 'RStudio' versions used in the writing of the text's R code are presented below.

# R version 4.0.4 (2021-02-15) -- "Lost Library Book"
# Copyright (C) 2021 The R Foundation for Statistical Computing
# Platform: x86_64-apple-darwin17.0 (64-bit)

# RStudio - Version 1.4.1106
# © 2009-2021 RStudio, PBC
# "Tiger Daylily" (2389bc24, 2021-02-11) for macOS

# R Code layout in this file (line numbers below may represent close approximations)

# 1. 'Integrated Financial Statements' R Code - Chapters 9-12 (line 40 to line 999)
# 2. 'Flow Definitions' R Code -                Chapter 13    (line 1000 to line 2,674)
# 3. 'Valuation' R Code -                       Chapter 14    (line 2,675 to End of file)

# 'Integrated Financial Statements'  R code begins here

options(scipen = 999)     # disable scientific notation

# Fake Revenue


library(tidyverse)

# Project Length
n <- 5

names <- c("rev", "CoS", "sga", "bd", "ie", "ii", 
           "gain", "chg_DTL_net", "current")

assumptions <- matrix(0, ncol = 9, nrow = n+1)

colnames(assumptions) <-  names
assumptions

assumptions <- as_tibble(assumptions)

assumptions <- assumptions %>%
  add_column(Year = c(0:n), .before = "rev" )

assumptions

fake_rev <- c(1000:(1000+n))
fake_rev

assumptions <- assumptions %>%
  mutate(rev = fake_rev )

assumptions

# Net Income Function

ni1 <- function(a) {              
  
  # Begin Income Statement
  
  gm     <-  a$rev - a$CoS
  ebitda <-  gm - a$sga
  ebit   <-  ebitda - a$bd
  ebt    <-  ebit - a$ie + a$ii + a$gain
  tax    <-  a$chg_DTL_net + a$current
  ni     <-  ebt - tax
  
  # End Income Statement
  
  # Income Statement Output
  ni_output <- list( a$Year, a$rev, a$CoS, gm, a$sga, ebitda,
                     a$bd, ebit, a$ie,  a$ii,  a$gain,
                     ebt, a$chg_DTL_net, a$current , tax, ni)
  
  names(ni_output) <-  c("Year", "rev", "CoS", "gm", "sga", "ebitda", 
                         "bd", "ebit", "ie", "ii", "gain", "ebt",
                         "chg_DTL_net", "current", "tax", "ni")
  
  return(ni_output)
  
}


ni <- ni1(assumptions)
ni

class(ni)


ni <- as_tibble(ni)
ni

ni_matrix <- t(as.matrix(ni))
ni_matrix

names(ni) <-  c("Year", "Revenue", "Cost of Sales", "Gross Margin", "SG&A Expense", "EBITDA", 
                "Book Depreciation", "EBIT", "Interest Expense", "Interest Income", "Gain(Loss)",
                "Pretax Income", "Deferred Income Taxes", "Current Income Taxes", "Book Taxes", "Net Income")

ni_matrix <- t(as.matrix(ni))
ni_matrix

library(kableExtra)

kbl(ni_matrix) %>%
  kable_paper(full_width = F) %>%
  column_spec(1:(n+2), bold = T)   %>%
  row_spec(c(4, 6, 8, 12, 15, 16), bold = T, color = "black", background = "LightSkyBlue") %>%
  row_spec(c(1), bold = T, color = "black", background = "LightCyan")




# CoS and SG&A Expenses

assumptions <- assumptions %>%
  mutate( CoS_pct = c(0, 0.60, 0.60, 0.58, 0.58, 0.56),      # Cost of Sales as a percentage of current period's revenue 
          sga_pct = c(0, rep(0.15, n )) )                    # SG&A expense  as a percentage of current period's revenue   


# Remove CoS, sga tibble columns
assumptions <- select(assumptions, -c( CoS, sga ))

t(as.matrix(assumptions))


ni1 <- function(a) {              
  
  # Begin Income Statement
  
  rev    <-  a$rev
  CoS    <-  rev * a$CoS_pct
  gm     <-  rev - CoS
  sga    <-  rev * a$sga_pct
  ebitda <-  gm - sga
  ebit   <-  ebitda - a$bd
  ebt    <-  ebit - a$ie + a$ii + a$gain
  tax    <-  a$chg_DTL_net + a$current
  ni     <-  ebt - tax
  
  # End Income Statement
  
  # Income Statement Output
  ni_output <- list( a$Year, a$rev, CoS, gm, sga, ebitda,
                     a$bd, ebit, a$ie,  a$ii,  a$gain,
                     ebt, a$chg_DTL_net, a$current , tax, ni)
  
  
  names(ni_output) <-  c("Year", "rev", "CoS", "gm", "sga", "ebitda", 
                         "bd", "ebit", "ie", "ii", "gain",
                         "ebt", "chg_DTL_net", "current", "tax", "ni")
  
  return(ni_output)
  
}




ni <- ni1(assumptions)

t(as.matrix(as_tibble(ni)))


# Rotate function

rotate <- function(r) {
  
  p <- t(as.matrix(as_tibble(r)))
  
  return(p)
  
}



rotate(ni)


assumptions <- assumptions %>%
  select( "Year", "rev",  "CoS_pct", "sga_pct", "bd", "ie", "ii", "gain", "chg_DTL_net", "current" ) 

assumptions 



rotate(assumptions)





# Book Depreciation and Tax Depreciation 


# All input data

digit_assmptns <- tibble( n                   =  5,      # Periods of analysis from Year '0' to period 'n'
                          year_of_asset_sale  =  5,      # Sell asset in any year 0 through 'n', -100 value
                          #  here indicates no asset sale 
                          debt_fin_ratio      = 0.50,    # Debt-to-assets financing ratio for capX
                          capX_0              = 500000,  # Capital cost in Year 0
                          bd_life             =  8,      # Book depreciable life of asset
                          sp_n                = 350000 ) # Sales proceeds from asset sale in year 'n' IF it
#   were to be sold  at that time, 

# Just because there is a positive value here does not mean there is an asset sale in year 'n'

digit_assmptns

options(scipen = 999)     # disable scientific notation except in extreme instances

asset_sale_vector <- rep(0, n+1)
asset_sale_vector


year_of_asset_sale <- digit_assmptns$year_of_asset_sale
year_of_asset_sale

# For financial statement line items affected by an asset sale
ifelse( year_of_asset_sale == -100,
        asset_sale_vector <- asset_sale_vector,
        asset_sale_vector[year_of_asset_sale + 1] <- 1 ) 

asset_sale_vector

sp_n   <- digit_assmptns$sp_n
capX_0 <- digit_assmptns$capX_0

# Steady decline in sales price if asset is sold in any given year
sp_vector  <-  c( 0, rep( capX_0 , n ) ) - cumsum(c( 0, rep( (capX_0 - sp_n) / n, n) ))     

sp_vector

plot(1:n, sp_vector[2:(n+1)]/1000, main = "Linear Trending Asset Sales Price", xlab = "Year",
     ylab = "Dollars (Thousands)", ylim=c(0, max(sp_vector/1000)),
     type = "o", col = "blue")

asset_sale_vector
sp_vector
asset_sale_vector * sp_vector


ifelse( year_of_asset_sale == -100,
        zero_out_vector <- rep(1,n+1),
        zero_out_vector <- rep(1,n+1) - cumsum(asset_sale_vector)) 

zero_out_vector


assmptns_func <- function(a) {
  
  n                  <- a$n                  # Years of analysis
  sp_n               <- a$sp_n               # Sales price of asset in year 'n' IF sold in year 'n'
  year_of_asset_sale <- a$year_of_asset_sale
  capX_0             <- a$capX_0
  bd_life            <- a$bd_life
  debt_fin_ratio     <- a$debt_fin_ratio
  
  zeta <- tibble( Year = c(0:n))
  
  zeta <- zeta %>%
    mutate( sp_vector = sp_vector,  
            asset_sale_vector = asset_sale_vector,              # Keeps data only in year of asset sale, zeros in all other years            
            sp        = sp_vector * asset_sale_vector,          # Modeled potential asset sales prices in any given year
            zero_out_vector = zero_out_vector,                  # Zeros all data past year of asset sale
            units1   = c( 0, 5000, 7000, 10000, 12000, 13000 ), # Unit sales of product 1
            price1   = c( 0,   50,   50,    55,    55,    55 ), # Unit price of product 1
            units2   = c( 0, 3000, 7000, 10000, 11000, 12000 ), # Unit sales of product 2
            price2   = c( 0,  120,  125,   130,   140,   150 ), # Unit price of product 2 
            rev1     = price1 * units1,                         # Revenue of product 1
            rev2     = price2 * units2,                         # Revenue of product 2
            tot_rev  = rev1 + rev2,                             # Total Revenue
            capX     = c( capX_0, rep(0, n)),                   # CapX
            bd_sch   = c(0, rep( 1 / bd_life, n ) ),            # Book deprecation schedule 
            td_sch   = c(0, 0.3, 0.4, 0.3, 0, 0),               # Tax  depreciation schedule
            bd       = capX_0 * bd_sch,                         # Book depreciation
            td       = capX_0 * td_sch,                         # Tax  depreciation
            EVAD     = c(0, rep( capX_0/n, n ) ),               # EVA depreciation
            acc_bd   = cumsum(bd),                              # Accumulated book depreciation
            acc_td   = cumsum(td),                              # Accumulated tax  depreciation
            acc_EVAD = cumsum(EVAD),                            # Accumulated EVA depreciation
            BV       = capX_0 - acc_bd,                         # Asset's Book Value
            TB       = capX_0 - acc_td,                         # Asset's Tax Basis
            EVA_BV   = capX_0 - acc_EVAD,                       # Asset's EVA Book Value
            BV_at_sale = BV * asset_sale_vector,                # Book Value at time of asset sale for Book Gain calculation
            TB_at_sale = TB * asset_sale_vector,                # Tax  Basis at time of asset sale for  Tax Gain calculation
            T        = c(0.25, rep(0.40, n  ) ),                # Tax Rates     
            chg_DTL_net = ((td + TB_at_sale) 
                           - (bd + BV_at_sale)) * T * (asset_sale_vector + zero_out_vector), # Increase in deferred tax liabilities, net
            DTL_net  = cumsum(chg_DTL_net) * zero_out_vector,   # Deferred Tax Liabilities, net
            ppe      = rep(capX_0, n+1) * zero_out_vector,      # Gross PP&E 
            CoS_pct  = c(0, 0.60, 0.60, 0.58, 0.58, 0.56),      # Cost of Sales as a percentage of current period's revenue 
            sga_pct  = c(0, rep(0.15, n )),                     # SG&A expense  as a percentage of current period's revenue 
            pic      = rep(capX_0 *(1-debt_fin_ratio), n+1) * zero_out_vector,  # Paid-in-capital
            cash_pct = rep( 0.05, n+1),                         # % of REV_t+1  
            ar_pct   = c(0, rep( 0.05, n)),                     # % of REV_t+1, No A/R balance in Year 0
            inv_pct  = rep( 0.05, n+1),                         # % of REV_t+1
            pe_pct   = rep( 0.01, n+1),                         # % of REV_t+1
            ap_pct   = c(0.05, rep( 0.06, n)),                  # % of REV_t+1
            wp_pct   = c(0, rep( 0.03, n)),                     # % of SGA_t+1
            itp_pct  = rep( 0.25, n+1),                         # % of Current_Taxes_t = function of Cash Taxes, 1-quarter = 25% worth of annual income taxes
            np_pct   = c(0, rep( 0.05, n)),                     # % of REV_t+1
            div_ratio = rep( 0.05, n+1) * zero_out_vector,      # Dividend payout ratio (DPR) as % of book net income, must be zero in final project year for balance sheet calculation of DIV payout = M/S balance to work properly
            ltd_pct  = rep( 0.9, n+1),                          # LTD / (LTD + CPLTD)
            Rf       = rep(0.03, n+1),                          # Risk-free interest rate
            MRP      = rep(0.04, n+1),                          # E(Market risk premium) = E(MRP) 
            Bd       = rep(0.20, n+1),                          # Debt Beta for use in CAPM
            Kd       = Rf + Bd * MRP,                           # Cost of debt capital using CAPM
            Bu       = rep(1.1, n+1),                           # Beta of unlevered equity for use in CAPM
            Ku       = Rf + Bu * MRP,                           # Cost of unlevered equity using CAPM                      
            Rf_adder = rep(0.005, n+1),                         # Adder to risk-free rate to estimate interest rate on M/S, r_MS
            r_MS     = Rf + Rf_adder,                           # Interest rate on M/S,
            r_debt   = Kd )                                     # Interest rate on outstanding debt (LTD, CPLTD, N/P)
  
  assumptions_output <-  list( zeta$Year, zeta$sp_vector, zeta$asset_sale_vector, zeta$sp, zeta$zero_out_vector, 
                               zeta$units1, zeta$price1, zeta$units2, zeta$price2, zeta$rev1, zeta$rev2, zeta$tot_rev,
                               zeta$capX, zeta$bd_sch, zeta$td_sch, zeta$bd, zeta$td, zeta$EVAD, 
                               zeta$acc_bd, zeta$acc_td, zeta$acc_EVAD, zeta$BV, zeta$TB, zeta$EVA_BV, zeta$BV_at_sale, zeta$TB_at_sale, zeta$T, zeta$chg_DTL_net, zeta$DTL_net,
                               zeta$ppe, zeta$CoS_pct, zeta$sga_pct, zeta$pic,
                               zeta$cash_pct, zeta$ar_pct, zeta$inv_pct, zeta$pe_pct,
                               zeta$ap_pct, zeta$wp_pct, zeta$itp_pct, zeta$np_pct, zeta$div_ratio, zeta$ltd_pct,
                               zeta$Rf, zeta$MRP, zeta$Bd, zeta$Kd, zeta$Bu, zeta$Ku, zeta$Rf_adder, zeta$r_MS, zeta$r_debt)
  
  
  assumptions__names <-     c( "Year", "sp_vector", "asset_sale_vector",  "sp", "zero_out_vector", 
                               "units1", "price1", "units2", "price2", "rev1", "rev2", "tot_rev", 
                               "capX" , "bd_sch", "td_sch", "bd", "td", "EVAD",
                               "acc_bd", "acc_td", "acc_EVAD", "BV", "TB", "EVA_BV", "BV_at_sale", "TB_at_sale", "T", "chg_DTL_net", "DTL_net",
                               "ppe", "CoS_pct", "sga_pct", "pic",
                               "cash_pct", "ar_pct", "inv_pct", "pe_pct",
                               "ap_pct", "wp_pct", "itp_pct", "np_pct", "div_ratio", "ltd_pct",
                               "Rf", "MRP", "Bd", "Kd", "Bu", "Ku", "Rf_adder", "r_MS", "r_debt" )
  
  names(assumptions_output) <-  assumptions__names
  
  #   assumptions_output <- rotate(assumptions_output)
  
  return(assumptions_output)
  
}

out1 <- assmptns_func(digit_assmptns)
rotate(out1)

out1 <- as_tibble(out1)

bd_items <- out1 %>% 
  select(Year, capX, bd_sch, bd)

rotate (bd_items)


DTL_items <- out1 %>%
  select(Year, chg_DTL_net, DTL_net, bd, td, T, TB_at_sale,
         BV_at_sale, asset_sale_vector, zero_out_vector )

rotate(DTL_items)


ni1 <- function(a) {              
  
  # Begin Income Statement
  
  rev    <-  a$rev
  CoS    <-  rev * a$CoS_pct
  gm     <-  rev - CoS
  sga    <-  rev * a$sga_pct
  ebitda <-  gm - sga
  bd     <-  a$bd
  ebit   <-  ebitda - bd
  gain   <-  a$sp - a$BV_at_sale
  ebt    <-  ebit - a$ie + a$ii + gain
  chg_DTL_net <- a$chg_DTL_net
  current <-  ebt*a$T - chg_DTL_net
  tax     <-  chg_DTL_net + current
  ni      <-  ebt - tax
  
  # End Income Statement
  
  # Income Statement Output
  ni_output <- list( a$Year, rev, CoS, gm, sga, ebitda,
                     bd, ebit, a$ie,  a$ii,  gain,
                     ebt, chg_DTL_net, current , tax, ni)
  
  
  names(ni_output) <-  c("Year", "rev", "CoS", "gm", "sga", "ebitda", 
                         "bd", "ebit", "ie", "ii", "gain",
                         "ebt", "chg_DTL_net", "current","tax", "ni")
  
  return(ni_output)
  
}


out1 <- as_tibble(out1)
out1 <- out1 %>%
  mutate(rev = seq(1000,1005,1) ,  
         ie = seq(50,45,-1),
         ii = seq(10,15,1) )



ni <- ni1(out1)

rotate(ni)


ni1 <- function(a) {              
  
  # Begin Income Statement
  
  rev    <-  a$tot_rev
  CoS    <-  rev * a$CoS_pct
  gm     <-  rev - CoS
  sga    <-  rev * a$sga_pct
  ebitda <-  gm - sga
  bd     <-  a$bd
  ebit   <-  ebitda - bd
  gain   <-  a$sp - a$BV_at_sale
  ebt    <-  ebit - a$ie + a$ii + gain
  chg_DTL_net <- a$chg_DTL_net
  current <-  ebt*a$T - chg_DTL_net
  tax     <-  chg_DTL_net + current
  ni      <-  ebt - tax
  
  # End Income Statement
  
  # Income Statement Output
  ni_output <- list( a$Year, rev, CoS, gm, sga, ebitda,
                     bd, ebit, a$ie,  a$ii,  gain,
                     ebt, chg_DTL_net, current , tax, ni)
  
  
  names(ni_output) <-  c("Year", "rev", "CoS", "gm", "sga", "ebitda", 
                         "bd", "ebit", "ie", "ii", "gain",
                         "ebt", "chg_DTL_net", "current","tax", "ni")
  
  return(ni_output)
  
}


ni <- ni1(out1)

rotate(ni)



income_statement <- as.data.frame(rotate((ni)))

library(writexl)

# Note: PC users be aware, R uses 'forward slashes' for file paths.
# https://rpubs.com/bpattiz/Directories_Paths_Workspaces
# The below code produces an error until you insert your desired file path
# Comment this line of code out if desired and/or it is not used

write_xlsx(income_statement, " ...... Your desired file path here ...../income_statement.xlsx")

# This next line of code produces a separate pop-up tab within RStudio
# Copy the data on the tab and paste into provided Excel spreadsheet if desired for formatting purposes. 
# Close the tab and return to this tab after 'viewing' the data.

view(income_statement)


# Balance Sheet


# Reference 'ni_bs1' R function farther below as needed 
# as the B/S is developed in the text


WC_items <- out1 %>%
  select(Year, cash_pct, ar_pct, inv_pct, pe_pct)

rotate(WC_items)



lead_lag <- tibble(x      = seq(100,105,1), 
                   lag_x  = lag(x, default=0),
                   lead_x = lead(x, default=0) )

lead_lag


WC_items2 <- out1 %>%
  select(Year, ap_pct, wp_pct, itp_pct, np_pct)

rotate(WC_items2)



ni     <- c(0,10,20,30,40,50)
ni

div    <- 0.1*ni
div

RE_t_1 <- rep(0, length(ni))  # initialize to 0, overwritten in R/E calculation loop
RE_t   <- rep(0, length(ni))  # initialize to 0, overwritten in R/E calculation loop


RE_t[1] <- RE_t_1[1] + ni[1] - div[1]
RE_t[1] 

RE_t_1[2] <- RE_t[1]    # Next period's prior period R/E 
#   Calculation R/E(t-1) aligned in period "t"
RE_t_1[2]

# Calculate RE_t_1 and RE_t within loop for years 1 through n

for (t in 2:length(ni) )
{
  RE_t[t]   <- RE_t[t-1] + ni[t] - div[t]  # Calculate R/E in period "t"    
  
  if (t+1 > length(ni) ) break             # Stop loop if the next period 
  #   't+1' is beyond the project's life
  RE_t_1[t+1] <- RE_t[t]                   # Assign next period's 'prior period' 
  #   R/E calculated value as R/E(t-1) aligned in period "t"
  
}

RE_matrix <- rotate(cbind(RE_t_1, ni, div, RE_t))
RE_matrix

# R/E R function

retained_earnings <- function(ni, div) {
  
  RE_t_1 <- rep(0, length(ni))  # initialize to 0, overwritten in R/E calculation loop
  RE_t   <- rep(0, length(ni))  # initialize to 0, overwritten in R/E calculation loop
  
  # Year 0 (= element 1 ) Retained Earnings as calculated outside the loop as 
  # 1. RE_t_1[1] = 0 by definition for a new project analysis
  # 2. All 3 required terms are in proper alignment for adding/subtracting Year 0 terms
  
  RE_t[1]   <- RE_t_1[1] + ni[1] - div[1]
  RE_t_1[2] <- RE_t[1]                   # Next period's prior period R/E Calculation R/E(t-1) aligned in period "t"
  
  # Calculate RE_t_1 and RE_t within loop for years 2 through n
  
  for (t in 2:length(ni) )
  {
    RE_t[t]   <- RE_t[t-1] + ni[t] - div[t]  # Calculate R/E in period "t"    
    
    if (t+1 > length(ni) ) break             # Stop loop if the next period 't+1' is beyond the project's life
    RE_t_1[t+1] <- RE_t[t]                   # Assign next period's 'prior period' R/E calculated value as R/E(t-1) 
    #   aligned in period "t"
    
  }
  
  RE_matrix <- as_tibble(cbind(RE_t_1, ni, div, RE_t))
  
  return(RE_matrix)
  
}

RE <- retained_earnings(ni, div)
rotate(RE)


fake_a <- seq(1,6,1)
fake_a

fake_b <- rep(3,6)
fake_b

fake_diff <- fake_b - fake_a
fake_diff

ms <- max(0, fake_diff)
ms

zero <- rep(0,6)
zero

ms <- max(zero, fake_diff)
ms

working_ms <- cbind(zero, fake_diff)
working_ms

ms <- apply(working_ms, 1, max)
ms

all_the_parts <- cbind(working_ms, ms)
all_the_parts

working_Debt <- -working_ms
working_Debt


Debt <- apply(working_Debt, 1, max)

cbind(working_Debt, Debt)


out3 <- assmptns_func(digit_assmptns)

out3$ltd_pct

c(0, rep(1, n))


# Complete NI and B/S  R function

ni_bs1 <- function(a) {
  
  sp <- a$sp       # Sales proceeds in year of sale   
  
  n  <- length(sp) - 1
  
  bd <- a$bd       # Book depreciation
  td <- a$td       # Tax depreciation
  
  BV_at_sale <- a$BV_at_sale    # Book value (BV) of asset at year of sale
  TB_at_sale <- a$TB_at_sale    # Tax basis (TB) of asset at year of sale
  
  asset_sale_vector <- a$asset_sale_vector
  zero_out_vector   <- a$zero_out_vector
  
  # Initialize ie and ii to zero vectors
  
  ie <-  rep(0, length(a$tot_rev) )
  ii <-  rep(0, length(a$tot_rev) )
  
  # Select IE, II calculation method
  
  # 0 = Prior period balance method = f(Prior period balance) 
  # 1 = Avg. balance method = f(Prior period & Current period balances)
  
  ie_ii_method <- 0
  
  ifelse( ie_ii_method == 0, iterations <- 5, iterations <- 100) 
  
  for (j in 1:iterations)  # Iterative calculation loop, intentional circular calculations for II, IE
  {
    
    if (j>1)  # Begin IE, II calculation in period 1, no IE or II in Year 0
    {
      
      ifelse( ie_ii_method == 0,
              
              {
                # Prior period Debt, N/P, and M/S balances        
                
                ie <- lag( np + cpltd + LTD , default=0) * a$r_debt   # recalculate as based on currently calculated 
                #   Debt, N/P balances 
                ii <- lag( MS , default=0)               * a$r_MS     # recalculate as based on currently calculated 
                #   MS balances 
              }
              , 
              { 
                # Average Debt, N/P, and M/S balances        
                
                # recalculate as based on currently calculated Debt, N/P balances 
                ie <- ( lag( np + cpltd + LTD , default=0) + ( np + cpltd + LTD) ) / 2 * a$r_debt * c(0, rep(1,n))   
                ii <- ( lag( MS , default=0) + MS) / 2                                 * a$r_MS   * c(0, rep(1,n))   
                
              }
              
      )
      
    } else{}
    
    # Beginning of Income Statement
    
    rev     <-  a$tot_rev
    CoS     <-  a$CoS_pct * rev
    gm      <-  rev - CoS
    sga     <-  a$sga_pct * rev
    ebitda  <-  gm - sga
    ebit    <-  ebitda - bd
    gain    <-  sp - BV_at_sale               # Accumulated book depreciation = cumsum(bd)
    ebt     <-  ebit - ie + ii + gain
    chg_DTL_net <- a$chg_DTL_net              # Increase in Deferred Tax Liabilities, net 
    current <-  ebt*a$T - chg_DTL_net
    tax     <-  chg_DTL_net + current
    ni      <-  ebt - tax
    
    # End of Income Statement
    
    ni_output <- list( 0:n, rev, CoS, gm, sga, ebitda,
                       bd, ebit, ie,  ii,  gain,
                       ebt, chg_DTL_net, current, tax, ni)
    
    
    ni_names <-  c("Year", "rev", "CoS", "gm", "sga", "ebitda", 
                   "bd", "ebit", "ie", "ii", "gain",
                   "ebt", "chg_DTL_net", "current","tax", "ni")
    
    
    cash <- a$cash_pct * lead(rev, default=0) * zero_out_vector
    ar   <- a$ar_pct   * lead(rev, default=0) * zero_out_vector
    inv <-  a$inv_pct  * lead(rev, default=0) * zero_out_vector
    pe  <-  a$pe_pct   * lead(sga, default=0) * zero_out_vector
    
    ppe     <- a$ppe
    acc_bd  <- a$acc_bd * zero_out_vector 
    ppe_net <- ppe - acc_bd
    
    ap  <-  a$ap_pct * lead(rev, default=0) * zero_out_vector
    wp  <-  a$wp_pct * lead(sga, default=0) * zero_out_vector
    itp <-  a$itp_pct  * current            * zero_out_vector
    np  <-  a$np_pct   * lead(sga, default=0) * zero_out_vector
    
    DTL_net <-  a$DTL_net
    
    pic     <- a$pic
    
    div     <- ni * a$div_ratio
    
    # Calculate Retained Earnings
    re_calc <- retained_earnings(ni, div)   # 'retained_earnings' is a function call as
    #   input into this ni_bs function in order to
    #   properly calculate R/E within balancing B/S function
    
    RE_t_1  <- re_calc$RE_t_1    # output of retained_earnings function call
    RE_t    <- re_calc$RE_t      # output of retained_earnings function call
    
    Ebv <-  pic + RE_t
    
    
    # Balancing the B/S begins here - place balancing 'MS' balancing and 'LTD' balancing
    #   after 'ebv' and before 'liab + LTD'
    
    a1 = cash + ar + inv + pe + ppe_net
    b1 = ap + wp + itp + np + DTL_net + Ebv
    
    working_ms <- b1 - a1 
    
    # Zero vector
    zeros <-  rep(0, n+1)
    
    working_ms <-  cbind( zeros, working_ms )                       
    
    # Equivalent to MS = Max(0, ( (ap + wp + itp + np + DTL_net + Ebv) 
    #                  - (cash + ar + inv + pe + ppe_net) )  )
    
    MS <- apply( working_ms, 1, max )      # applies 'maximum' function to each element in matrix
    
    working_Debt <- -working_ms            # Debt = LTD + CPLTD
    
    # Equivalent to LTD = Max(0, ( (cash + ar + inv + pe + ppe_net) 
    #                   - (ap + wp + itp + np + DTL_net + Ebv) ) )
    
    Debt <- apply( working_Debt, 1, max )  # applies 'maximum' function to each element in matrix
    
    # Balancing the B/S ENDS here 
    
    # At end of project, return any balance in M/S to equity holders via dividend payment
    # credit, M/S, debit DIV in final project year = n +1 given Year 0 = 1st year of analysis
    
    div[n+1]  <-  max(0, MS[n+1])                   # Payout final Year's M/S balance as a dividend 
    RE_t[n+1] <- RE_t[n] + ni[n+1] - div[n+1]       # Recalculate final Year's Retained Earnings
    Ebv[n+1]  <- RE_t[n+1] + pic[n+1]               # Recalculate Equity book value
    MS[n+1]   <- 0                                  # zero MS balance in final project year
    
    ca           <-  cash + MS + ar + inv + pe            # Includes M/S
    assets       <-  ca + ppe_net
    
    # CPLTD  
    cpltd <- (1 - a$ltd_pct) * Debt * c(0, rep(1, n))      # Year 0 has no CPLTD
    
    # N/P in Year 0 only
    np[1] <- max(0, Debt[1] - (ppe[1] - pic[1]) )      # N/P in Year 0 is any required debt 
    #   financing above LTD financing associated 
    #   with the capital asset
    # Current liabilities
    cl    <-  ap + wp + itp + cpltd + np               # Includes CPLTD 
    
    # LTD 
    LTD <- a$ltd_pct * Debt     
    
    # LTD in Year 0 only
    LTD[1] <- ppe[1] - pic[1]   # LTD in Year 0 equals Gross PP&E value - Equity financing.
    #   Any additional debt financing in Year 0 is provided by 
    #   short-term debt (N/P)
    
    liab         <-  cl + LTD + DTL_net
    liab_and_Ebv <-  liab + Ebv
    
    bs_check     <-  assets - liab_and_Ebv
    
  }   # End IE, II Iterative Recalculation Loop
  
  # To avoid double use of 'ni' variable (ni_output and bs_output)
  ni.1 <- ni  
  
  bs_output <-  list( cash, MS, ar, inv, pe, ca, 
                      ppe, acc_bd, ppe_net, assets,
                      ap, wp, itp, cpltd, np, cl,  
                      LTD, DTL_net, liab,
                      pic, RE_t_1, ni.1, div, RE_t, Ebv, liab_and_Ebv, bs_check)
  
  ni_bs_output <- c( ni_output, bs_output )
  
  bs_names <-          c("cash", "MS", "ar", "inv", "pe", "ca", 
                         "ppe", "acc_bd", "ppe_net", "assets",
                         "ap", "wp", "itp", "cpltd", "np", "cl",
                         "LTD", "DTL_net", "liab",
                         "pic", "RE_t_1", "ni.1", "div", "RE_t", "Ebv", "liab_and_Ebv", "bs_check")
  
  ni_bs_names <- c( ni_names, bs_names)
  
  names(ni_bs_output) <-  ni_bs_names
  
  return(ni_bs_output)
  
}


out3 <- assmptns_func(digit_assmptns)
out3

rotate(out3)

# Generate Integrated Income Statement & Balance Sheet Pro Formas
out4 <- ni_bs1( out3 )  

out4
rotate(out4)

# Apply this R output to provided formatted Excel spreadsheet



ni_bs1_output <- as.data.frame(rotate((out4)))

library(writexl)

write_xlsx(ni_bs1_output, " ...... desired file path here ...../ni_bs1_output.xlsx")


# Statements of Cash Flows


cash_balance <- function(chg_in_cash) {
  
  cash_t_1 <- rep(0, length(chg_in_cash))  # initialize to 0, overwritten in calculation loop below
  cash_t   <- rep(0, length(chg_in_cash))  # initialize to 0, overwritten in calculation loop below
  
  # Year 0 (= element 1 ) 'Change in cash' as calculated outside the loop as 
  # 1. cash_t_1[1] = 0 by definition for a new project analysis
  # 2. All 3 required terms are in proper alignment for adding/subtracting Year 0 terms
  
  cash_t[1]   <- cash_t_1[1] + chg_in_cash[1]
  cash_t_1[2] <- cash_t[1]                    # Next period's prior period cash balance Calculation cash(t-1) aligned in 
  #   period "t"
  
  # Calculate cash_t_1 and cash_t within loop
  
  for (t in 2:length(chg_in_cash) )
  {
    cash_t[t]   <- cash_t[t-1] + chg_in_cash[t]   # Calculate cash balance in period "t"    
    
    if (t+1 > length(chg_in_cash) ) break         # Stop loop if the next period 't+1' is beyond the project's life
    
    cash_t_1[t+1] <- cash_t[t]                    # Next period's prior period Cash Calculate Cash(t-1) aligned in  
    #  period "t"
    
  }
  
  cash_matrix <- cbind( chg_in_cash, cash_t_1, cash_t)
  cb_tbl <- as_tibble(cash_matrix)
  cb_tbl
  
  return(cb_tbl)
  
}


# Fake input data
fake_chg_in_cash <- c(1000, 5, 5, 5, 5, -(1000 + 4*5))
fake_chg_in_cash

# Run function
fake_cb <- cash_balance(fake_chg_in_cash)

# View results
rotate(fake_cb)


stmts_cfs <- function(s, a){
  
  sp <- a$sp       # Sales proceeds in year of sale 
  
  n <- length(sp) - 1
  
  bd <- a$bd       # Book depreciation
  td <- a$td       # Tax depreciation
  
  BV_at_sale <- a$BV_at_sale    # Book value (BV) of asset at year of sale
  TB_at_sale <- a$TB_at_sale    # Tax basis (TB) of asset at year of sale
  
  asset_sale_vector <- a$asset_sale_vector
  zero_out_vector   <- a$zero_out_vector
  
  chg_DTL_net <- s$chg_DTL_net * ( asset_sale_vector + zero_out_vector )   
  
  # Start of Statements of Cash Flows
  
  chg_ar  <- s$ar  - lag(s$ar , default=0) 
  chg_inv <- s$inv - lag(s$inv, default=0) 
  chg_pe  <- s$pe  - lag(s$pe , default=0) 
  chg_ap  <- s$ap  - lag(s$ap , default=0) 
  chg_wp  <- s$wp  - lag(s$wp , default=0) 
  chg_itp <- s$itp - lag(s$itp, default=0) 
  
  OCF <- s$ni + s$bd + chg_DTL_net - s$gain - (chg_ar + chg_inv + chg_pe) + (chg_ap + chg_wp + chg_itp)
  
  CFI <- -a$capX + sp   
  
  equity_issuance_net   <-   s$pic - lag(s$pic, default=0)
  debt_issuance_net     <-  (s$LTD - lag(s$LTD, default=0)) + (s$cpltd - lag(s$cpltd, default=0))
  st_debt_borrowing_net <-   s$np  - lag(s$np, default=0)
  sale_MS_net           <-   s$MS  - lag(s$MS, default=0)
  div                   <-   s$div
  
  CFF <- equity_issuance_net + debt_issuance_net + st_debt_borrowing_net - sale_MS_net - div 
  
  chg_in_cash <- OCF + CFI + CFF
  
  calc_cash_balance <- cash_balance(chg_in_cash)    # 'cash_balance' is a function call as input into this
  #   'stmt_cfs' function in order to properly calculate cash_t
  #   and cash_t_1 based on statement of CFs
  
  cash_t_1  <- calc_cash_balance$cash_t_1    # output from cash_balance function call
  cash_t    <- calc_cash_balance$cash_t      # output from cash_balance function call
  
  # Statement of CFs check against change in Cash on B/S
  cash_check <- cash_t -  s$cash  
  
  # End of Statements of Cash Flows
  
  stmts_cfs_output <-  list(  a$Year, s$ni, s$bd, chg_DTL_net, -s$gain, -chg_ar, -chg_inv, -chg_pe, chg_ap,
                              chg_wp, chg_itp, OCF, -a$capX , sp, CFI, equity_issuance_net, debt_issuance_net,
                              st_debt_borrowing_net, -s$div, -sale_MS_net, CFF, chg_in_cash, cash_t_1, cash_t,
                              cash_check)
  
  stmts_cfs__names <-          c("Year", "ni", "bd", "chg_DTL_net", "gain", "chg_ar", "chg_inv", "chg_pe", 
                                 "chg_ap", "chg_wp", "chg_itp", "OCF", "capX" , "sp", "CFI",
                                 "equity_issuance_net", "debt_issuance_net", "st_debt_borrowing_net", "div",
                                 "sale_MS_net", "CFF", "chg_in_cash", "cash_t_1", "cash_t", "cash_check")
  
  names(stmts_cfs_output) <-  stmts_cfs__names
  
  return(stmts_cfs_output)
  
}

# Produce 'assumptions data' used as input in to both ni_bs1' and 'stmt_cfs' custom R functions
out3 <- assmptns_func(digit_assmptns)

# Run the 'ni_bs1' function, income statements and balance sheets
out4 <- ni_bs1( out3 )  

# Run the 'stmt_cfs' function
out7 <- round(rotate(stmts_cfs( as.data.frame(out4), out3)),0)

# View the output
out7

# Apply this R output to provided formatted Excel spreadsheet



# Prepare / Export output for external formatting
stmts_cfs_output <- as.data.frame(out7)

library(writexl)
write_xlsx(stmts_cfs_output, "...... desired file path here ...../stmts_cfs_output.xlsx")













# 'Flows Definitions' Chapter 13 material begin here

# Reproduce 'Statements of Income' and Balance Sheets' for reference 

out3 <- assmptns_func(digit_assmptns)

out4 <- ni_bs1( out3 )  

rotate(out4)


# Reproduce 'Statements of Cash Flows' for reference

out7


# Change in OWC function 

#This function's input = 'out4'; chg_OWC(out4)
chg_OWC <- function(s) {
  
  OCA <- s$cash + s$ar + s$inv + s$pe
  OCL <- s$ap + s$wp + s$itp
  
  OWC = OCA - OCL
  
  chg_owc <- OWC - lag(OWC, default=0)
  
  return(chg_owc)
  
}


chg_OWC(out4)



fcff1 <- function(a, b) {
  
  library(tibble)
  
  fcff1 <- b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX  
  
  length <- length(a$bd)
  
  fcff_1 <-     tibble(T        = a$T,
                       ie       = b$ie,
                       ii       = b$ii,
                       Year     = c(0:(length-1)),
                       ni       = b$ni,
                       bd       = a$bd,
                       chg_DTL_net = a$chg_DTL_net,
                       gain     = - b$gain,
                       sp       = a$sp,
                       ie_AT    = ie*(1-a$T),
                       ii_AT    = - ii*(1-a$T),
                       gcf      = ni + bd + chg_DTL_net + gain + sp + ie_AT + ii_AT,
                       chg_OWC  = - chg_OWC(b),
                       capX     = - a$capX,
                       fcff1    = gcf + capX + chg_OWC  )
  
  fcff_1 <- rotate(fcff_1)
  return(fcff_1)
  
}

fcff_1 <- fcff1( out3, out4)
fcff_1

# Apply this R output to provided formatted Excel spreadsheet


# Alternatively use 'kableExtra' R package
# You will receive multiple warnings. Warnings are not errors.  Ignore them as the table is fully produced in the Rstudio 'Viewer' 

fcff_1 <- fcff_1[ -c(1:3), ]     # Remove top 3 rows of 'fcff_1'
fcff_1[1,] <- fcff_1[1, ]*1000   # Multiply Years by 1000.  Later all data is divided by 1000 including Years.

fcff_1 <- rotate(fcff_1)        
fcff_1 <- as_tibble(fcff_1)

names(fcff_1) <- c("Year","Net Income", "+ Depreciation", "+ Increase in DTL, net",
                   "- Gains(Losses)","+ Sales Proceeds", "+ Interest Expense (After-tax)", 
                   "- Interest Income (After-tax)", "GCF", "- Change in OWC", "- CapX", "FCFF")

fcff_1 <- rotate(fcff_1)
fcff_1 <- as.data.frame(round(fcff_1 / 1000, 0))  # Eliminate need for comma formatting by dividing currency by 1000

# Remove auto-assigned column headings: V1, V2, etc.
colnames(fcff_1) <- NULL

library(kableExtra)

kbl(fcff_1) %>%
  kable_paper(full_width = F) %>%
  column_spec(1:(n+2), bold = T)   %>%
  row_spec(c(9, 12), bold = T, color = "white", background = "DarkBlue") %>%
  row_spec(c(1), bold = T, color = "black", background = "LightCyan") %>%
  row_spec(c(2:8, 10,11), bold = T, color = "black", background = "Lightyellow")




flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp 
                      + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX ) 
  
  flow_tbl <- round(rotate(flow_tbl),0)
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows




fcff2 <- function(a, b) {
  
  library(tibble)
  
  fcff2 <- (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T) + a$td - (a$sp - a$TB_at_sale)
  + a$sp +    b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX  
  
  length <- length(a$bd)
  
  fcff_2 <-     tibble(T          = a$T,
                       TB_at_sale = a$TB_at_sale, 
                       ebitda     = b$ebitda,
                       ie         = b$ie,
                       ii         = b$ii,
                       ni__tax    = (ebitda - a$td - ie + ii + (a$sp - a$TB_at_sale) )*(1-T),
                       Year       = c(0:(length-1)),
                       ni_tax     = ni__tax,
                       td         = a$td,
                       gain_tax   = -(a$sp - TB_at_sale),
                       sp         = a$sp,
                       ie_AT      = ie*(1-T),
                       ii_AT      = - ii*(1-T),
                       gcf        = ni_tax + td + gain_tax + sp + ie_AT + ii_AT,
                       chg_OWC    = - chg_OWC(b),
                       capX       = - a$capX,
                       fcff2      = gcf + capX + chg_OWC  )
  
  fcff_2 <- rotate(fcff_2)
  return(fcff_2)
  
}

fcff_2 <- fcff2( out3, out4)
fcff_2

# Apply this R output to provided formatted Excel spreadsheet



fcff3 <- function(a, b) {
  
  library(tibble)
  
  fcff3 <- (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX  
  
  length <- length(a$bd)
  
  fcff_3 <-     tibble(T           = a$T,
                       TB_at_sale  = a$TB_at_sale, 
                       td          = a$td,
                       ebitda      = b$ebitda,
                       sp          = a$sp,
                       Year        = c(0:(length-1)),
                       ebitda_AT   = ebitda*(1-T),
                       sp_AT       = sp*(1-T),
                       tax_shield  = (td + TB_at_sale)*T,
                       gcf         = ebitda_AT + sp_AT + tax_shield,
                       chg_OWC     = - chg_OWC(b),
                       capX        = - a$capX,
                       fcff3       = gcf + capX + chg_OWC  )
  
  fcff_3 <- rotate(fcff_3)
  return(fcff_3)
  
}

fcff_3 <- fcff3( out3, out4)
fcff_3

# Apply this R output to provided formatted Excel spreadsheet



flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T)
                      - b$ii*(1-a$T) - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T) 
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX   
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}



flows <- flows( out3, out4)
flows




fcff4 <- function(a, b) {
  
  library(tibble)
  
  fcff4 <- (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX  
  
  length <- length(a$bd)
  
  fcff_4 <-     tibble(T            = a$T,
                       TB_at_sale   = a$TB_at_sale, 
                       td.1         = a$td,
                       ebitda       = b$ebitda,
                       sp           = a$sp,
                       Year         = c(0:(length-1)),
                       ebit_tax     = ebitda - td.1,
                       ebit_tax__sp = ebit_tax + sp,
                       ebit_tax__sp_AT = ebit_tax__sp * (1-T),
                       td           = td.1,
                       TB_tax_shield_at_sale = TB_at_sale * T,
                       gcf         = ebit_tax__sp_AT + td + TB_tax_shield_at_sale,
                       chg_OWC     = - chg_OWC(b),
                       capX        = - a$capX,
                       fcff4       = gcf + capX + chg_OWC  )
  
  fcff_4 <- rotate(fcff_4)
  
  return(fcff_4)
  
}


fcff_4 <- fcff4( out3, out4)
fcff_4


# Apply this R output to provided formatted Excel spreadsheet



flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T)
                      - b$ii*(1-a$T) - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX,
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX  
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows


fcff5 <- function(a, b) {
  
  library(tibble)
  
  fcff5 <- (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX  
  
  length <- length(a$bd)
  
  fcff_5 <-     tibble(T            = a$T,
                       TB_at_sale   = a$TB_at_sale, 
                       td           = a$td,
                       sp           = a$sp,
                       Year         = c(0:(length-1)),
                       ebit         = b$ebit,
                       ebit__sp     = ebit + sp,
                       ebit__sp_AT = ebit__sp * (1-T),
                       bd           = a$bd,
                       chg_DTL_net_wo_sale = (td-a$bd)*(T),
                       TB_tax_shield_at_sale = TB_at_sale * T,
                       gcf         = ebit__sp_AT + bd + chg_DTL_net_wo_sale + TB_tax_shield_at_sale,
                       chg_OWC     = - chg_OWC(b),
                       capX        = - a$capX,
                       fcff5       = gcf + capX + chg_OWC  )
  
  fcff_5 <- rotate(fcff_5)
  
  return(fcff_5)
  
}



fcff_5 <- fcff5( out3, out4)
fcff_5


# Apply this R output to provided formatted Excel spreadsheet


flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T)
                      - b$ii*(1-a$T) - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX  
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}

flows <- flows( out3, out4)
flows


fcff6 <- function(a, b) {
  
  library(tibble)
  
  fcff6 <- (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX  
  
  length <- length(a$bd)
  
  fcff_6 <-     tibble(T            = a$T,
                       TB_at_sale   = a$TB_at_sale,
                       sp           = a$sp,
                       ebitda       = b$ebitda,
                       Year         = c(0:(length-1)),
                       ebit_tax     =  ebitda - a$td,
                       nopat_tax    =  ebit_tax * (1-T),   
                       td           = a$td,
                       sp_AT        = sp*(1-T),
                       TB_tax_shield_at_sale = TB_at_sale * T,
                       gcf         = nopat_tax + td + sp_AT + TB_tax_shield_at_sale,
                       chg_OWC     = - chg_OWC(b),
                       capX        = - a$capX,
                       fcff6       = gcf + capX + chg_OWC  )
  
  fcff_6 <- rotate(fcff_6)
  
  return(fcff_6)
  
}

fcff_6 <- fcff6( out3, out4)
fcff_6


# Apply this R output to provided formatted Excel spreadsheet


flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T)
                      - b$ii*(1-a$T) - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX,
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}

flows <- flows( out3, out4)
flows


fcff7 <- function(a, b) {
  
  library(tibble)
  
  fcff7 <- (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
  - chg_OWC(b) - a$capX  
  
  length <- length(a$bd)
  
  fcff_7 <-     tibble(T            = a$T,
                       td           = a$td,
                       TB_at_sale   = a$TB_at_sale,
                       sp           = a$sp,
                       Year         = c(0:(length-1)),
                       ebitda       = b$ebitda,
                       nopat        = (ebitda - a$bd)* (1-T),   
                       bd           = a$bd,
                       TB_tax_shield_at_sale = TB_at_sale * T,
                       chg_DTL_net_wo_sale = (td - bd)*(T),
                       sp_AT        = sp*(1-T),
                       gcf         = nopat + bd + chg_DTL_net_wo_sale + sp_AT + TB_tax_shield_at_sale,
                       chg_OWC     = - chg_OWC(b),
                       capX        = - a$capX,
                       fcff7       = gcf + capX + chg_OWC  )
  
  fcff_7 <- rotate(fcff_7)
  
  return(fcff_7)
  
}


fcff_7 <- fcff7( out3, out4)
fcff_7


# Apply this R output to provided formatted Excel spreadsheet


flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}

flows <- flows( out3, out4)
flows


fcff8 <- function(a, b) {
  
  library(tibble)
  
  fcff8 <- (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td 
  - ( (a$TB +  cumsum(chg_OWC(b)) ) - lag(a$TB + cumsum(chg_OWC(b)), default=0))    
  
  length <- length(a$bd)
  
  fcff_8 <-     tibble(T            = a$T,
                       TB_at_sale   = a$TB_at_sale,
                       sp           = a$sp,
                       Year         = c(0:(length-1)),
                       gcf          = (b$ebitda + a$sp)*(1-T) + ( a$td + a$TB_at_sale )*T,
                       td           = -a$td,
                       nopat_tax_w_sale_cf = gcf + td,   
                       TB           = a$TB,
                       OWC          = cumsum(chg_OWC(b)),
                       TB_OWC       = TB + OWC,
                       chg_TB_OWC   = - ( TB_OWC - lag(TB_OWC, default=0) ),
                       fcff8        = nopat_tax_w_sale_cf + chg_TB_OWC  )
  
  fcff_8 <- rotate(fcff_8)
  
  return(fcff_8)
  
}



fcff_8 <- fcff8( out3, out4)
fcff_8


# Apply this R output to provided formatted Excel spreadsheet


flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) ) 
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0))
                      
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}

flows <- flows( out3, out4)
flows


CFd <- function(b) {
  
  library(tibble)
  
  CFd <- b$ie  - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) )     
  
  length <- length(b$ie)
  
  CFd_1 <-     tibble( LTD    = b$LTD,            
                       cpltd  = b$cpltd,
                       np     = b$np,
                       Year   = c(0:(length-1)),
                       N      = LTD + cpltd + np,
                       ie     = b$ie,
                       chg_N  = -(N - lag(N, default=0)),   
                       CFd_1  = ie + chg_N  )
  
  CFd_1 <- rotate(CFd_1)
  
  return(CFd_1)
  
}


CFd <- CFd(out4)
CFd


# Apply this R output to provided formatted Excel spreadsheet



flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) )
                      
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}

flows <- flows( out3, out4)
flows


ECF1 <- function(a, b) {
  
  library(tibble)
  
  ECF1 <- b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T) 
  
  length <- length(a$bd)
  
  ECF_1 <-     tibble(T               = a$T,
                      pic            = b$pic,
                      chg_pic        = pic - lag(pic, default=0),
                      MS             = b$MS,
                      ii             = b$ii,
                      Year           = c(0:(length-1)),
                      div            = b$div,
                      net_new_equity = -chg_pic,   
                      chg_MS         = MS - lag(MS, default=0),
                      ii_AT          = -ii*(1-T),
                      ECF1           = div + net_new_equity + chg_MS + ii_AT )
  
  ECF_1 <- rotate(ECF_1)
  
  return(ECF_1)
  
}


ECF_1 <- ECF1( out3, out4)
ECF_1


# Apply this R output to provided formatted Excel spreadsheet




flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T)
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows



ECF2 <- function(a, b, c) {
  
  library(tibble)
  
  ECF2 <- c[2, ] - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) )  
  
  length <- length(a$bd)
  
  ECF_2 <-     tibble(T              = a$T,
                      N              = b$LTD + b$cpltd + b$np,
                      chg_N          = N - lag(N, default=0),
                      ie             = b$ie,
                      Year           = c(0:(length-1)),
                      fcff           = c[2, ],
                      CFd_AT         = -(ie*(1-T) - chg_N),   
                      ECF2           = fcff + CFd_AT )
  
  ECF_2 <- rotate(ECF_2)
  
  return(ECF_2)
  
}


ECF_2 <- ECF2( out3, out4, flows)
ECF_2


# Apply this R output to provided formatted Excel spreadsheet




flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) )
                      
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}

flows <- flows( out3, out4)
flows


ECF3 <- function(a, b) {
  
  library(tibble)
  
  ECF3 <- b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) )  
  
  length <- length(a$bd)
  
  ECF_3 <-     tibble(T              = a$T,
                      ii             = b$ii,
                      Year           = c(0:(length-1)),
                      ni             = b$ni,
                      ii_AT          = -ii*(1-T),
                      ni_less_ii_AT   = ni + ii_AT,
                      Ebv            = b$Ebv,
                      MS             = -b$MS,
                      Ebv_less_MS    = Ebv + MS,
                      chg_Ebv_less_MS  = - (Ebv_less_MS - lag(Ebv_less_MS, default=0) ) ,
                      ECF_3          = ni_less_ii_AT + chg_Ebv_less_MS )
  
  ECF_3 <- rotate(ECF_3)
  
  return(ECF_3)
  
}


ECF3 <- ECF3( out3, out4 )
ECF3


# Apply this R output to provided formatted Excel spreadsheet


flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) ),
                      
                      ECF3  = b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) )
                      
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows


CCF1 <- function(c) {
  
  library(tibble)
  
  CCF1 <- c[11, ] + c[10, ]
  
  length <- length(CCF1)
  
  CCF_1 <-     tibble(Year   = c(0:(length-1)),
                      ECF    = c[11, ],
                      CFd    = c[10, ],
                      CCF_1  = ECF + CFd )
  
  CCF_1 <- rotate(CCF_1)
  
  return(CCF_1)
  
}


CCF_1 <- CCF1( flows )
CCF_1


# Apply this R output to provided formatted Excel spreadsheet




flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) ),
                      
                      ECF3  = b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) ),
                      
                      CCF1  = ECF1 + CFd
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows



CCF2 <- function(a, b, c) {
  
  library(tibble)
  
  CCF2 <- c[2, ] + b$ie*(a$T) 
  
  length <- length(a$bd)
  
  CCF_2 <-     tibble(T      = a$T,
                      ie     = b$ie,
                      Year   = c(0:(length-1)),
                      FCFF   = c[2, ],
                      ie_tax_shield = ie*(T),
                      CCF_2  = FCFF + ie_tax_shield )
  
  CCF_2 <- rotate(CCF_2)
  
  return(CCF_2)
  
}

CCF_2 <- CCF2( out3, out4, flows )
CCF_2


# Apply this R output to provided formatted Excel spreadsheet


flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) ),
                      
                      ECF3  = b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) ),
                      
                      CCF1  = ECF1 + CFd,
                      
                      CCF2  = fcff1 + b$ie*(a$T)
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows


FCFF10 <- function(a, b) {
  
  library(tibble)
  
  fcff10 <- b$ni + b$ie*(1-a$T) - b$ii*(1-a$T) -  ( b$Ebv - lag(b$Ebv, default=0) )
  - ( (b$LTD + b$cpltd + b$np) - lag((b$LTD + b$cpltd + b$np), default=0) )
  + ( b$MS - lag(b$MS, default=0)) 
  
  length <- length(a$bd)
  
  FCFF_10 <-       tibble(T              = a$T,
                          Ebv            = b$Ebv,
                          MS             = b$MS,
                          ii             = b$ii,
                          ie             = b$ie,
                          LTD            = b$LTD,            
                          cpltd          = b$cpltd,
                          np             = b$np,
                          N              = LTD + cpltd + np,
                          Year           = c(0:(length-1)),
                          ni             = b$ni,
                          ie_AT          =  ie*(1-T),
                          ii_AT          = -ii*(1-T),
                          ni_unlvd       = ni + ie_AT + ii_AT,
                          chg_Ebv        = -(Ebv - lag(Ebv, default=0)),   
                          chg_N          = -(N - lag(N, default=0)),
                          chg_Ebv_N      = chg_Ebv + chg_N, 
                          chg_MS         = MS - lag(MS, default=0),
                          chg_Ebv_N_MS   = chg_Ebv_N + chg_MS,
                          
                          FCFF10         = ni_unlvd + chg_Ebv_N_MS  )
  
  FCFF_10 <- rotate(FCFF_10)
  
  return(FCFF_10)
  
}


FCFF_10 <- FCFF10( out3, out4 )
FCFF_10


# Apply this R output to provided formatted Excel spreadsheet



flows <- function(a, b) {
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      fcff10 = b$ni + b$ie*(1-a$T) - b$ii*(1-a$T) -  ( b$Ebv - lag(b$Ebv, default=0) ) 
                      - ( (b$LTD + b$cpltd + b$np) - lag((b$LTD + b$cpltd + b$np), default=0) ) 
                      + ( b$MS - lag(b$MS, default=0)), 
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) ),
                      
                      ECF3  = b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) ),
                      
                      CCF1  = ECF1 + CFd,
                      
                      CCF2  = fcff1 + b$ie*(a$T)
                      
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows


EVA1 <- function(a, b) {
  
  library(tibble)
  
  # Replace this WACC vector if the 5-year project assumptions change !!!!
  # unless you make use of the 'circular' calculation EVA valuation models from the 'Valuation' chapter (recommended)
  wacc <- c(0, 0.06802, 0.06950, 0.07368,	0.07353, 0.07326) 
  
  
  EVA1 <-  ( (b$ebitda + a$sp)*(1-a$T) + (a$td + a$TB_at_sale)*(a$T) ) - a$EVAD 
  - (wacc * lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 )) 
  
  length <- length(a$bd)
  
  EVA_1 <-     tibble(T          = a$T,
                      ebitda     = b$ebitda,
                      sp         = a$sp,
                      td         = a$td,
                      TB_at_sale = a$TB_at_sale,
                      chg_OWC    = chg_OWC(b),
                      EVA_BV     = a$EVA_BV,
                      OWC        = cumsum(chg_OWC),
                      eva_inv    = EVA_BV + OWC,
                      Year       = c(0:(length-1)),
                      gcf        = (b$ebitda + a$sp)*(1-a$T) + (a$td + a$TB_at_sale)*(a$T), 
                      EVAD       = -a$EVAD,
                      return_on_inv = -wacc * lag(eva_inv, default=0),
                      EVA_1      = gcf + EVAD + return_on_inv )
  
  EVA_1 <- cbind(wacc, EVA_1)
  
  EVA_1 <- round(rotate(EVA_1),4)
  
  return(EVA_1)
  
}

EVA_1 <- EVA1( out3, out4 )
EVA_1


# Apply this R output to provided formatted Excel spreadsheet



flows <- function(a, b) {
  
  # Replace this WACC vector if the 5-year project assumptions change !!!!
  # unless you make use of the 'circular' calculation EVA valuation models from the 'Valuation' chapter (recommended)
  wacc <- c(0, 0.06802, 0.06950, 0.07368,	0.07353, 0.07326) 
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      fcff10 = b$ni + b$ie*(1-a$T) - b$ii*(1-a$T) -  ( b$Ebv - lag(b$Ebv, default=0) ) 
                      - ( (b$LTD + b$cpltd + b$np) - lag((b$LTD + b$cpltd + b$np), default=0) ) 
                      + ( b$MS - lag(b$MS, default=0)), 
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) ),
                      
                      ECF3  = b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) ),
                      
                      CCF1  = ECF1 + CFd,
                      
                      CCF2  = fcff1 + b$ie*(a$T),
                      
                      EVA1  =  ( (b$ebitda + a$sp)*(1-a$T) + (a$td + a$TB_at_sale)*(a$T) ) - a$EVAD 
                      - (wacc * lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 )) 
                      
  )
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows


EVA2 <- function(a, b) {
  
  library(tibble)
  
  # Alter this WACC assumption if changes in project assumptions cause WACC values to change  
  # Forward WACC discount rates
  wacc <- c(0, 0.06802, 0.06950, 0.07368,	0.07353, 0.07326 ) 
  
  
  EVA2 <- ( ( (b$ebitda - a$td)*(1-a$T) + a$sp*(1-a$T) + a$TB_at_sale*(a$T) ) / lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 ) - wacc ) + (a$td - a$EVAD)
  
  length <- length(a$bd)
  
  EVA_2 <-     tibble(T          = a$T,
                      sp         = a$sp,
                      TB_at_sale = a$TB_at_sale,
                      chg_OWC    = chg_OWC(b),
                      ebitda     = b$ebitda,
                      ie         = b$ie,
                      Year       = c(0:(length-1)),
                      EVA_BV     = a$EVA_BV,
                      OWC        = cumsum(chg_OWC),
                      eva_inv    = EVA_BV + OWC,
                      nopat_tax  = (ebitda - a$td)*(1-T),
                      cf_asset_sale = sp*(1-T) + (TB_at_sale)*(T),
                      nopat_tax_w_sale_cf  = nopat_tax + cf_asset_sale,
                      roic        = nopat_tax_w_sale_cf / lag(eva_inv, default=0),  # Produces NaN in Year 0
                      roic2       = c(0, roic[2:length(roic)]),    # Replace NaN with 0 for presentation purposes    
                      WACC       = -wacc,
                      ROIC_WACC   = roic2 + WACC,
                      ROIC_WACC__lag_eva_inv = ROIC_WACC * lag(eva_inv, default=0),
                      td         = a$td,
                      EVAD       = -a$EVAD,
                      EVA_2      = ROIC_WACC__lag_eva_inv + (td + EVAD) )
  
  EVA_2 <- EVA_2 %>%
    select(-roic)  # Remove duplicate ROA calculation with NaN (roic) from output 
  
  EVA_2 <- round(rotate(EVA_2),4)
  return(EVA_2)
  
}


EVA_2 <- EVA2( out3, out4 )
EVA_2


# Apply this R output to provided formatted Excel spreadsheet




flows <- function(a, b) {
  
  # Replace this WACC vector if the 5-year project assumptions change !!!!
  # unless you make use of the 'circular' calculation EVA valuation models from the 'Valuation' chapter (recommended)
  wacc <- c(0, 0.06802, 0.06950, 0.07368,	0.07353, 0.07326) 
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      fcff10 = b$ni + b$ie*(1-a$T) - b$ii*(1-a$T) -  ( b$Ebv - lag(b$Ebv, default=0) ) 
                      - ( (b$LTD + b$cpltd + b$np) - lag((b$LTD + b$cpltd + b$np), default=0) ) 
                      + ( b$MS - lag(b$MS, default=0)), 
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) ),
                      
                      ECF3  = b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) ),
                      
                      CCF1  = ECF1 + CFd,
                      
                      CCF2  = fcff1 + b$ie*(a$T),
                      
                      EVA1  =  ( (b$ebitda + a$sp)*(1-a$T) + (a$td + a$TB_at_sale)*(a$T) ) - a$EVAD 
                      - (wacc * lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 )) ,
                      
                      eva2  = ( ( (b$ebitda - a$td)*(1-a$T) + a$sp*(1-a$T) + a$TB_at_sale*(a$T) ) 
                                / lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 ) - wacc )*lag(( a$EVA_BV 
                                                                                                    + cumsum(chg_OWC(b)) ), default=0 ) + (a$td - a$EVAD),
                      
                      EVA2  = c(0,eva2[2:length(eva2)])   # Replace Year 0 NaN with 0 
  )
  
  flow_tbl <- flow_tbl %>%
    select(-eva2)  # Remove duplicate EVA calculation with NaN (eva1) from output 
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows



FinCF <- function(a, b) {
  
  library(tibble)
  
  FinCF_ <- b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) )
  - b$ii*(1-a$T) + b$ie*(1-a$T)  - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD
                                                                       + b$cpltd  + b$np, default=0) )     
  
  length <- length(a$bd)
  
  FinCF <-     tibble(T              = a$T,
                      LTD            = b$LTD,            
                      cpltd          = b$cpltd,
                      np             = b$np,
                      pic            = b$pic,
                      chg_pic        = pic - lag(pic, default=0),
                      MS             = b$MS,
                      ii             = b$ii,
                      Year           = c(0:(length-1)),
                      div            = b$div,
                      net_new_equity = -chg_pic,   
                      chg_MS         = MS - lag(MS, default=0),
                      ii_AT          = -ii*(1-T),
                      N              = LTD + cpltd + np,
                      ie_AT          = b$ie*(1-T),
                      chg_N          = -(N - lag(N, default=0)),   
                      FinCF          = div + net_new_equity + chg_MS + ii_AT + ie_AT + chg_N )
  
  FinCF <- rotate(FinCF)
  
  return(FinCF)
  
}

FinCF <- FinCF( out3, out4)
FinCF


# Apply this R output to provided formatted Excel spreadsheet



flows <- function(a, b) {
  
  # Replace this WACC vector if the 5-year project assumptions change !!!!
  # unless you make use of the 'circular' calculation EVA valuation models from the 'Valuation' chapter (recommended)
  wacc <- c(0, 0.06802, 0.06950, 0.07368,	0.07353, 0.07326) 
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      fcff10 = b$ni + b$ie*(1-a$T) - b$ii*(1-a$T) -  ( b$Ebv - lag(b$Ebv, default=0) ) 
                      - ( (b$LTD + b$cpltd + b$np) - lag((b$LTD + b$cpltd + b$np), default=0) ) 
                      + ( b$MS - lag(b$MS, default=0)), 
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) ),
                      
                      ECF3  = b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) ),
                      
                      CCF1  = ECF1 + CFd,
                      
                      CCF2  = fcff1 + b$ie*(a$T),
                      
                      EVA1  =  ( (b$ebitda + a$sp)*(1-a$T) + (a$td + a$TB_at_sale)*(a$T) ) - a$EVAD 
                      - (wacc * lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 )) ,
                      
                      eva2  = ( ( (b$ebitda - a$td)*(1-a$T) + a$sp*(1-a$T) + a$TB_at_sale*(a$T) ) 
                                / lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 ) - wacc )*lag(( a$EVA_BV 
                                                                                                    + cumsum(chg_OWC(b)) ), default=0 ) + (a$td - a$EVAD),
                      
                      EVA2  = c(0,eva2[2:length(eva2)]),   # Replace Year 0 NaN with 0 
                      
                      FinCF = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T)
                      + b$ie*(1-a$T)  - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) )
                      
  )
  
  flow_tbl <- flow_tbl %>%
    select(-eva2)  # Remove duplicate EVA calculation with NaN (eva1) from output 
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}


flows <- flows( out3, out4)
flows



EP1 <- function(a, b) {
  
  library(tibble)
  
  # Replace this 'ke' vector if the 5-year project assumptions change !!!!
  # unless you make use of the 'circular' calculation EP valuation model from the 'Valuation' chapter (recommended)
  ke <- c(0, 0.079474, 0.077876, 0.074235, 0.074352, 0.074557) 
  Ke <- ke
  
  # EP = NI - (II)(1-T) + chg_MS - (Ke)(lag(Ebv))
  EP1 <- b$ni - b$ii*(1-a$T) + (b$MS - lag(b$MS,default=0)) -  ( Ke * lag(b$Ebv, default=0) )      
  
  length <- length(a$bd)
  
  EP1 <-            tibble(T            = a$T,
                           ii             = b$ii,
                           MS             = b$MS,
                           ie             = b$ie,
                           Year           = c(0:(length-1)),
                           ni             = b$ni,
                           ii_AT          = -ii*(1-T),
                           ni_less_ii_AT  = ni + ii_AT,
                           chg_MS         = MS - lag(MS, default=0),
                           ni_ii_AT_MS    = ni_less_ii_AT + chg_MS,
                           Ke             = ke, 
                           Ebv            = b$Ebv,
                           Charge_on_net_book_equity = -( Ke * lag(Ebv, default=0) ),
                           EP             = ni_less_ii_AT + chg_MS + Charge_on_net_book_equity )
  
  EP1 <- round(rotate(EP1),4)
  
  return(EP1)
  
}


EP1 <- EP1( out3, out4 )
EP1


# Apply this R output to provided formatted Excel spreadsheet



flows <- function(a, b) {
  
  # Replace WACC, Ke if the 5-year project assumptions change !!!!
  # unless you make use of the EVA & EP valuation models from the 'Valuation' chapter (recommended)
  wacc <- c(0, 0.06753, 0.06925, 0.07341, 0.07353, 0.07328 )
  Ke   <- c(0, 0.079474, 0.077876, 0.074235, 0.074352, 0.074557) 
  
  library(tibble)
  
  length <- length(a$bd)
  
  flow_tbl <- tibble( Year   = c(0:(length-1)),
                      
                      fcff1 = b$ni + a$bd + a$chg_DTL_net - b$gain + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff2 = (b$ebitda - a$td - b$ie + b$ii + (a$sp - a$TB_at_sale) )*(1-a$T)
                      + a$td - (a$sp - a$TB_at_sale) + a$sp + b$ie*(1-a$T) - b$ii*(1-a$T) - chg_OWC(b) - a$capX, 
                      
                      fcff3 = (b$ebitda + a$sp )*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff4 = (b$ebitda - a$td + a$sp )*(1-a$T) + a$td + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff5 = (b$ebit + a$sp )*(1-a$T) + a$bd + (a$td-a$bd)*(a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff6 = (b$ebitda - a$td)*(1-a$T) +a$td + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T) - chg_OWC(b) - a$capX,
                      
                      fcff7 = (b$ebitda - a$bd)*(1-a$T) +a$bd + (a$td - a$bd)*(a$T) + a$sp*(1-a$T) + ( a$TB_at_sale )*(a$T)
                      - chg_OWC(b) - a$capX,
                      
                      fcff8 = (b$ebitda + a$sp)*(1-a$T) + ( a$td + a$TB_at_sale )*(a$T) - a$td - ( (a$TB +  cumsum(chg_OWC(b)) )                               
                                                                                                   - lag(a$TB + cumsum(chg_OWC(b)), default=0)),
                      
                      fcff10 = b$ni + b$ie*(1-a$T) - b$ii*(1-a$T) -  ( b$Ebv - lag(b$Ebv, default=0) ) 
                      - ( (b$LTD + b$cpltd + b$np) - lag((b$LTD + b$cpltd + b$np), default=0) ) 
                      + ( b$MS - lag(b$MS, default=0)), 
                      
                      CFd   = b$ie - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),
                      
                      ECF1  = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T),
                      
                      ECF2  = fcff1 - b$ie*(1-a$T) + ( (b$LTD + b$cpltd + b$np) - lag(b$LTD + b$cpltd + b$np, default=0) ),
                      
                      ECF3  = b$ni -b$ii*(1-a$T) - ( b$Ebv -lag(b$Ebv, default=0) ) + ( b$MS  - lag(b$MS, default=0) ),
                      
                      CCF1  = ECF1 + CFd,
                      
                      CCF2  = fcff1 + b$ie*(a$T),
                      
                      EVA1  =  ( (b$ebitda + a$sp)*(1-a$T) + (a$td + a$TB_at_sale)*(a$T) ) - a$EVAD 
                      - (wacc * lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 )) ,
                      
                      eva2  = ( ( (b$ebitda - a$td)*(1-a$T) + a$sp*(1-a$T) + a$TB_at_sale*(a$T) ) 
                                / lag(( a$EVA_BV + cumsum(chg_OWC(b)) ), default=0 ) - wacc )*lag(( a$EVA_BV 
                                                                                                    + cumsum(chg_OWC(b)) ), default=0 ) + (a$td - a$EVAD),
                      
                      EVA2  = c(0,eva2[2:length(eva2)]),   # Replace Year 0 NaN with 0 
                      
                      FinCF = b$div - ( b$pic - lag(b$pic, default=0) ) +  ( b$MS - lag(b$MS, default=0) ) - b$ii*(1-a$T)
                      + b$ie*(1-a$T)  - ( ( b$LTD + b$cpltd  + b$np ) - lag(b$LTD + b$cpltd  + b$np, default=0) ),  
                      
                      EP    = b$ni-b$ii*(1-a$T)  + (b$MS - lag(b$MS,default=0)) - ( Ke * lag(b$Ebv, default=0) )
                      
  )
  
  flow_tbl <- flow_tbl %>%
    select(-eva2)  # Remove duplicate EVA calculation with NaN (eva1) from output 
  
  flow_tbl <- round(rotate(flow_tbl),0)
  
  return(flow_tbl)
  
}

flows <- flows( out3, out4)
flows

























# Valuation chapter

# Spot & Forward rates


dcf <- matrix( c(  1:5,
                   100,  200,   300,  400,  500,
                   0.10, 0.09, 0.095, 0.11, 0.10), nrow=5, ncol=3)

colnames(dcf) <- c("Year", "flow", "forward_rates")

dcf

cum_prod <- cumprod(1 + dcf[,3])
cum_prod

# Initially define a zero-filled spot rate matrix
spot <- matrix(0, nrow=5, ncol=1)


for (t in 1:length(cum_prod))
{
  spot[t] <- cum_prod[t]^(1/t)-1
}

colnames(spot) <- "spot_rates"
spot

dcf <- cbind(dcf, spot)
dcf

library(tidyverse)

dcf <- as_tibble(dcf)

# Discounting flows using spot rates
dcf <- dcf %>%
  mutate( discounted_value_0 = flow / (1 + spot_rates)^Year)

dcf

pv_0 <- sum(dcf[,5])
pv_0



# Debt Valuation

debt_value <- function(a, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Kd <- a$Kd
  
  # Debt cash flow is the 11th row of 'flows' output
  CFd <- c[11, ]        
  
  # Debt Value by Year
  
  d <- rep(0, n+1 )  # Initialize debt value (d) to zero each Year
  
  # Calculate debt value by period in reverse order
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    
    # Discounting 1-period at the forward discount rate, Kd[t]
    
    d[t-1] <- ( d[t] + CFd[t] ) / (1 + Kd[t] )
    
  }
  
  # End Debt value by Year
  
  # Spot discount rates by Year
  
  Kd_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Kd_spot[2] <- Kd[2]
  
  # Subscripts in this loop are a little tricky
  # as vector element t=3 represents Year 2, vector element t=4 represents Year 3, etc. 
  # Ex. for t=3 (= Year 2): Kd_spot[3] <- ( ( 1 + Kd_spot[2] )^(3-2) * ( 1 + Kd[3] ) ) ^ (1/(3-1)) - 1
  
  for (t in 3:(n+1))   
  {
    Kd_spot[t] <- ( ( 1 + Kd_spot[t-1] )^(t-2) * ( 1 + Kd[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End Kd_spot discount rates by Year
  
  # Debt Value at t=0 using Kd_spot discount rates  
  d_0 <- c( sum( CFd[2:(n+1)] / (1 + Kd_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  debt_value <- as_tibble( cbind(a$Year, a$Rf, a$Bd, a$MRP, a$Kd,  Kd_spot, CFd, round(d,0), round(d_0,0) ) ) 
  
  names(debt_value) <- c("Year", "Rf", "Bd", "MRP", "Kd", "Kd_spot", "CFd", "D",  "D_0")
  
  return(rotate(debt_value))
}


debt_value <- debt_value( out3, flows )

debt_value


# Apply this R output to provided formatted Excel spreadsheet



# Equity Valuation (Noncircular)

equity_value <- function(a, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  
  Rf  <- a$Rf
  MRP <- a$MRP
  Ku  <- a$Ku
  Kd  <- a$Kd
  T   <- a$T
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # Equity cash flow is selected from the 12th row of 'flows' output
  ECF <- c[12, ]
  
  # Equity Value by Year
  
  e <- rep(0, n+1 )  # Initialize equity value to zero each Year
  
  # Calculate equity value by period in reverse order using discount rate 'Ku'
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    
    # Discounting 1-period at the forward discount rate, Ku[t]
    
    e[t-1] <- ( e[t] + ECF[t] - (d[t-1])*(1-T[t])*(Ku[t]-Kd[t]) ) / (1 + Ku[t] )
    
  }
  
  # Use knowledge of just calculated equity value to calculate 'Ke'
  
  Ke <- rep(0, n+1 )  # Initialize Ke vector to zero each Year
  
  for (t in 2:(n+1))    
  {
    Ke[t] <- Ku[t] + ( d[t-1] / e[t-1] )*( 1-T[t] )*( Ku[t] - Kd[t] ) 
  }
  
  # Levered beta, BL
  BL <- (Ke - Rf)/MRP
  
  e1 <- rep(0, n+1 )  # Initialize equity value to zero each Year
  
  # Calculate equity value by period in reverse order using discount rate 'Ke'
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 1
  {
    
    # Discounting 1-period at the forward discount rate, Ke[t]
    
    e1[t-1] <- ( e1[t] + ECF[t] ) / (1 + Ke[t] )
    
  }
  
  # End Equity value by Year
  
  # Ke_spot discount rates by Year
  
  Ke_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ke_spot[2] <- Ke[2]
  
  for (t in 3:(n+1))   
  {
    Ke_spot[t] <- ( ( 1 + Ke_spot[t-1] )^(t-2) * ( 1 + Ke[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End Ke_spot discount rates by Year
  
  # Equity Value at t=0 using Ke_spot discount rates  
  e_0 <- c( sum( ECF[2:(n+1)] / (1 + Ke_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  equity_value <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Kd, Ku-Kd, round(d,0), ECF,
                                   -lag(d, default=0)*(1-T)*(Ku-Kd), ECF - lag(d, default=0)*(1-T)*(Ku-Kd), 
                                   round(e,0), d/e, Ke, BL, round(e1,0), Ke_spot, e_0, c( ECF[1], rep(NaN,n)), npv_0, round(e,0) - round(e1,0) ) )  
  
  names(equity_value) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Kd", "Ku_Kd", "D", "ECF",
                           "ECF_adj", "ADJ_ECF", "E", "D_E_Ratio", "Ke", "BL", "E1", "Ke_spot", "E_0", "ECF_0", "NPV_0", "E_check")
  
  return(rotate(equity_value))
}

equity_value <- equity_value( out3, flows )

equity_value




# Apply this R output to provided formatted Excel spreadsheet



equity_value_circ <- function(a, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP  <- a$MRP
  Bu  <- a$Bu
  Bd  <- a$Bd
  T   <- a$T
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # Equity cash flow is selected from the 12th row of 'flows' output
  ECF <- c[12, ]        
  
  # Equity Value by Year
  
  e <- 1.0*d  # Initialize the' debt-to-equity' 'value ratios' to 1.0 as a first guess each Year
  e_calc <- rep(0, n+1 )  # Initialize equity value to zero each Year
  
  # Calculate equity value by period in reverse order using converging discount rates Ke[t]
  
  iterations <- 100
  tolerance <- 0.0000001
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting 1-period at the forward discount rate, Ke[t] as a function of levered beta = BL[t]
      
      e_calc[t-1] <- ( e[t] + ECF[t] ) / ( 1 + Rf[t] + (Bu[t] + (d[t-1] / e[t-1])*(1-T[t])*(Bu[t] - Bd[t]) )*MRP[t] ) 
      
    }
    # Determine sum of squared error terms in iterative valuation process
    error <- sum((e - e_calc)^2)
    
    # Uncomment the below 6 lines of code to examine the iterative solution process in action
    
    # message("iteration #")
    # print(j)
    # message("e_calc")
    # print(e_calc)
    # message("error")
    # print(error)
    
    # Break from loop if desired error tolerance has been achieved
    if (error < tolerance) break
    
    # Retain the current equity valuation convergence value as the guess for the next iterative calculation pass 
    e <- e_calc
  }
  
  # This ends the valuation convergence loop
  
  # Use converged circular solutions to solve for BL and Ke
  # This was actually taking place within the looping process
  
  BL <- rep(0, n+1 )  # Initialize vector to zero each Year
  
  for (t in 2:(n+1))    
  {
    
    BL[t] <- Bu[t] + (d[t-1]) / (e[t-1])*(1-T[t])*(Bu[t] - Bd[t]) 
    
  }
  
  #  BL <- Bu + (lag(d, default=0) / lag(e, default=0))*(1-T)*(Bu - Bd)
  
  Ke <- Rf + BL * MRP
  
  # spot discount rates by Year
  
  Ke_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ke_spot[2] <- Ke[2]
  
  for (t in 3:(n+1))   
  {
    Ke_spot[t] <- ( ( 1 + Ke_spot[t-1] )^(t-2) * ( 1 + Ke[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Equity Value at t=0 using spot discount rates  
  e_0 <- c( sum( ECF[2:(n+1)] / (1 + Ke_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 13th row of 'equity_value' output = equity_value[13, ] is used as a check
  
  equity_value_circ <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$Bd, Bu - Bd, a$MRP, 
                                        BL, round(d,0), d/e, Ke, ECF, e, Ke_spot, e_0,
                                        c( ECF[1], rep(NaN,n)), npv_0, round(e,0) - round( equity_value[13, ],0)) ) 
  
  names(equity_value_circ) <- c("Year", "T", "Rf", "Bu", "Bd", "Bu_Bd", "MRP",
                                "BL", "D", "D_E_Ratio", "Ke", "ECF", "E", "Ke_spot", "E_0",
                                "ECF_0", "NPV_0", "E_check")
  
  return(rotate(equity_value_circ))
}


equity_value_circ <- equity_value_circ( out3, flows )

round(equity_value_circ,5)


# Apply this R output to provided formatted Excel spreadsheet



equity_value_circ_2 <- function(a, e) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  
  Kd  <- Rf + Bd * MRP
  Ku  <- Rf + Bu * MRP
  
  # Initialize Ke vector by simply setting it equal to Ku vector
  Ke  <- Ku
  
  # Equity cash flow is selected from the 12th row of 'equity_value_circ' output
  ECF <- e[12, ]        
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ] 
  
  # Equity Value by Year
  
  e_calc <- rep(0, n+1 )  # Initialize equity value convergence vector to zero each Year
  e <- 1.0*d  # Initialize the' debt-to-equity' 'value ratios' to 1.0 as a first guess each Year
  
  # Calculate equity value by period in reverse order using converging discount rates Ke[t]
  
  iterations <- 100
  tolerance  <- 0.000001
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      
      # Calculate circular Ke
      Ke[t] <- Ku[t] +  (d[t-1] / e[t-1])*(Ku[t] - Kd[t])*(1-T[t]) 
      
      # Discounting 1-period at the forward discount rate, Ku[t]
      
      e_calc[t-1] <- ( e[t] + ECF[t] + e[t-1]*(Ku[t] - Ke[t]) ) / ( 1 + Ku[t] )  
      
    }
    # Determine error in iterative valuation process
    
    error <- sum((e - e_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    e <- e_calc
  }
  
  # Spot discount rates by Year
  
  Ku_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ku_spot[2] <- Ku[2]
  
  for (t in 3:(n+1))   
  {
    Ku_spot[t] <- ( ( 1 + Ku_spot[t-1] )^(t-2) * ( 1 + Ku[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  ECF_risk_adj <- lag(e, default=0)*(Ku - Ke)
  Risk_ADJ_ECF <- ECF + ECF_risk_adj   
  
  ECF_risk_adj[1] <- 0
  Risk_ADJ_ECF[1] <- ECF[1]   
  
  # Equity Value at t=0 using spot discount rates  
  e_0 <- c( sum( Risk_ADJ_ECF[2:(n+1)] / (1 + Ku_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(Risk_ADJ_ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 13th row of 'equity_value' output = equity_value[13, ] is used as a check
  
  equity_value_circ_2 <- as_tibble( cbind(a$Year, a$Rf, a$Bu, a$MRP, Ku, Ke, Ku - Ke,
                                          e, ECF, ECF_risk_adj, Risk_ADJ_ECF, e, Ku_spot, e_0,
                                          c( Risk_ADJ_ECF[1], rep(NaN,n)), npv_0, round(e,0) 
                                          - round( equity_value[13, ],0) ) ) 
  
  names(equity_value_circ_2) <- c("Year", "Rf", "Bu", "MRP", "Ku", "Ke", "Ku_Ke",
                                  "E", "ECF", "ECF_risk_adj",  "Risk_ADJ_ECF", "E.1" ,"Ku_spot", "E_0",
                                  "Risk_ADJ_ECF_0", "NPV_0", "E_check")
  
  return(rotate(equity_value_circ_2))
}


equity_value_circ_2 <- equity_value_circ_2( out3, equity_value_circ )

round(equity_value_circ_2,5)



# Apply this R output to provided formatted Excel spreadsheet




asset_value_circ <- function(a, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP  <- a$MRP
  Bu  <- a$Bu
  Bd  <- a$Bd
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # Asset Value by Year
  
  v <- d*2  # Initialize debt-to-assets ratio (D/A) = 50% as an initial guess value each Year
  v_calc <- rep(0, n+1 )  # Initialize asset value to zero each Year
  
  # Calculate asset value by period in reverse order using converging discount rates WACC[t]
  
  iterations <- 100
  tolerance <- 0.0000001
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting 1-period at the forward discount rate, WACC[t] as a function of unlevered cost of equity capital = Ku[t]
      
      v_calc[t-1] <- ( v[t] + FCFF[t] ) / ( 1 + Ku[t] * (1 - (d[t-1] / v[t-1])*(T[t]) ) ) 
      
    }
    # Determine error in iterative valuation process
    error <- sum((v - v_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error < tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v <- v_calc
  }
  
  # Use converged circular calculations to solve for WACC, Ke, BL
  
  WACC <-  Ku * (1 - (lag(d, default=0) / lag(v, default=0))*(T) )
  
  Ke <-  Ku - (1 - (lag(d, default=0) / lag(v, default=0)) / (1 - (lag(d, default=0) / lag(v, default=0))) *(1-T)*(Ku - Kd) )
  
  BL <- (Ke - Rf) / MRP
  
  # Spot discount rates by Year
  
  WACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- WACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using WACC_spot discount rates  
  v_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Asset Value check uses 13th row of 'equity_value' output = equity_value[13, ] 
  #   plus 8th row of 'debt_value' output = debt_value[8, ]
  asset_value_check <- equity_value[13, ] + debt_value[8, ]
  
  asset_value_circ <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku,
                                       round(d,0), d/v, WACC, FCFF, v, WACC_spot, v_0,
                                       c( FCFF[1], rep(NaN,n)), npv_0, round(v,0) - round(asset_value_check,0) ) ) 
  
  names(asset_value_circ) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku",
                               "D", "D_V_Ratio", "WACC", "FCFF", "V", "WACC_spot", "V_0",
                               "FCFF_0", "NPV_0", "V_check")
  
  return(rotate(asset_value_circ))
}


asset_value_circ <- asset_value_circ( out3, flows )

round(asset_value_circ,5)


# Apply this R output to provided formatted Excel spreadsheet



asset_value <- function(a, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # Asset Value by Year
  
  # Initialize asset value vector to zero
  
  v <- rep(0, n+1)
  
  # Calculate asset value by period in reverse order using discount rates Ku[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting 'Adjusted' FCFF 1-period at the forward discount rate, Ku[t]
    
    v[t-1] <- ( v[t] + FCFF[t] + Ku[t]*d[t-1]*T[t] ) / ( 1 + Ku[t] )  
    
  }
  
  # Use asset values to solve for forward WACC rates
  
  WACC <-  Ku * (1 - (lag(d, default=0) / lag(v, default=0))*(T) )
  
  # Solve for Asset value using WACC for discount rates
  
  # Initialize asset value vector to zero
  
  v1 <- rep(0, n+1)
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting 1-period at the forward discount rate, WACC[t] as a function of unlevered cost of equity capital = Ku[t]
    
    v1[t-1] <- ( v[t] + FCFF[t] ) / ( 1 + WACC[t] ) 
    
  }
  
  # Spot discount rates by Year
  
  WACC_spot <- rep(0, n+1 )  # Initialize spotrate vector to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- WACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End WACC_spot discount rates by Year
  
  # Asset Value at t=0 using WACC_spot discount rates  
  v_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 11th row of 'asset_value_circ' output = asset_value_circ[11, ] is used as a check
  
  asset_value <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku,  round(d,0), FCFF, Ku*lag(d, default=0)*T, 
                                  FCFF + Ku*lag(d, default=0)*T, v, WACC, FCFF, v1, WACC_spot, v_0,
                                  c( FCFF[1], rep(NaN,n)), npv_0, round(v,0) - round(asset_value_circ[11, ],0),
                                  round(v1,0) - round(asset_value_circ[11, ],0)) ) 
  
  names(asset_value) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku",  "D", "FCFF", "FCFF_Adj", 
                          "ADJ_FCFF", "V", "WACC", "FCFF.1", "V1",  "WACC_spot", "V_0",
                          "FCFF_0", "NPV_0", "V_check", "V_check_2")
  
  return(rotate(asset_value))
}

asset_value <- asset_value( out3, flows )

round(asset_value,5)



# Apply this R output to provided formatted Excel spreadsheet



# Noncircular Asset Valuation
# Explicit realization of 'value-additivity' 
# V = D + E
# D = PV[CFd, Kd]
# E = PV[ECF + Adj, Ku]


asset_value_2 <- function(a, b, c) {
  
  library(tidyverse)
  
  d <- a[8, ]
  e <- b[13, ]
  v <- d + e
  
  fcff_0 <- c[8,1]
  
  npv_0 <- v[1] + fcff_0 
  
  fcff_0 <- c(fcff_0, rep(NaN,length(d)-1))
  npv_0 <- c(npv_0, rep(NaN,length(d)-1))
  
  v_check <- round(v,0) - round(c[11, ],0)
  
  asset_value_2 <- as_tibble(cbind(0:(length(d)-1), d, e, v, fcff_0, npv_0, v_check))
  
  names(asset_value_2) <- c("Year", "D", "E", "V", "FCFF_0", "NPV_0", "V_check")
  
  return(rotate(asset_value_2))
  
}

asset_value_2 <- asset_value_2( debt_value, equity_value, asset_value )

asset_value_2


# Apply this R output to provided formatted Excel spreadsheet



# Noncircular Asset valuation discounting CCF w/ adjustment at Kd + Ku + (Kd)(Ku)
# LEEPV  (Noncircular alternative to APV model)
# General case (Kd<>r_debt)

asset_value_LEEPV <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]     
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ] 
  
  # CCF is selected from the 15th row of 'flows' output
  CCF <- FCFF + ie*T       
  
  # Asset Value by Year 
  
  # Initialize asset value, debt value, CCF_adj, m vectors to zero
  v <- rep(0, n+1)
  v2 <- rep(0, n+1)
  v3 <- rep(0, n+1)
  d <- rep(0, n+1)
  CCF_adj <- rep(0, n+1)
  m <- rep(0, n+1)
  
  # Noncircular discount rate
  q <- Kd + Ku + Kd*Ku
  
  # Calculate asset value by period in reverse order using discount rates q[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    d[t-1] <- ( d[t] + CFd[t] ) / ( 1 + Kd[t] )  
    
    # Numerator adjustment to CCF
    CCF_adj[t] <- (d[t] + CFd[t])*(Ku[t]-Kd[t])*(T[t]) + (v[t] + CCF[t])*(Kd[t]) 
    
    
    # Discounting CCF 1-period at the forward discount rate, q = Kd + Ku + (Kd)(Ku)
    
    v[t-1] <- ( v[t] + CCF[t] + CCF_adj[t] ) / ( 1 + q[t] )  
    
    # Calculate numerator flow a 2nd way
    
    m[t] <-  d[t-1]*(Ku[t] - Kd[t])*(T[t]) 
    
    v2[t-1] <- ( ( v2[t] + CCF[t] + m[t] ) *(1 + Kd[t]) ) / ( 1 + q[t] )  
    
    # Calculate numerator flow a 3rd way
    
    v3[t-1] <- ( v3[t] + ( (v3[t]  + CCF[t] + m[t] ) *(1 + Kd[t]) ) - v3[t] ) / ( 1 + q[t] )  
    
  }
  
  # Solve for equity value
  
  e <- v - d
  
  # asset value check
  check1 <- v - v2
  
  # 'Flow' check
  check2 <- ( CCF + CCF_adj ) - ( (v3  + CCF + m ) *(1 + Kd) - v3 )
  
  # Replace 'Year 0' flow check with zero as it is not used 
  check2 <- c(0, check2[2:(n+1)])
  
  # Spot discount rates by Year
  
  q_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  q_spot[2] <- q[2]
  
  for (t in 3:(n+1))   
  {
    q_spot[t] <- ( ( 1 + q_spot[t-1] )^(t-2) * ( 1 + q[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using q_spot discount rates 
  Adj_CCF <- CCF + CCF_adj
  
  v_0 <- c( sum( Adj_CCF[2:(n+1)] / (1 + q_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 11th row of 'asset_value_circ' output = asset_value_circ[11, ] is used as a check
  
  asset_value_LEEPV <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd,  round(d,0), FCFF, ie*T, 
                                        CCF, CCF_adj, Adj_CCF,  
                                        v, e, q, q_spot, v2 + CCF, m, v2, v3,  v2 + CCF + m, (v2 + CCF + m)*(1+Kd), 
                                        (v2 + CCF + m)*(1+Kd) - v2, check1, check2, v_0, c( FCFF[1], rep(NaN,n)), npv_0,
                                        round(v,0) - round(asset_value_circ[11, ],0) ) ) 
  
  names(asset_value_LEEPV) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "D", "FCFF", "ie_T", 
                                "CCF", "CCF_adj", "Adj_CCF", 
                                "V", "E", "q", "q_spot", "v2_CCF", "m", "V2", "V3",  "v2_CCF_m", "v2_CCF_m__1_Kd", 
                                "v2_CCF_m__1_Kd__v2 ","check1", "check2", "V_0", "FCFF_0", "NPV_0", "V_check")
  
  return(rotate(asset_value_LEEPV))
}


asset_value_LEEPV <- asset_value_LEEPV( out3, out4, flows )

round(asset_value_LEEPV,5)



# Apply this R output to provided formatted Excel spreadsheet




asset_value_apv <- function(a, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # Asset Value by Year
  
  # Initialize asset value vector to zero
  
  # v_u = value of unlevered project
  v_u <- rep(0, n+1)
  
  # v_u = value of levered project
  v <- rep(0, n+1)
  
  # dvts = 'value' of interest expense tax shields
  dvts <- rep(0, n+1)
  
  # Calculate asset value by period in reverse order using discount rates Ku[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting 'Adjusted' FCFF (FCFF & Adjustment) 1-period at the forward discount rate, Ku[t]
    
    v_u[t-1] <- ( v_u[t] + FCFF[t] ) / ( 1 + Ku[t] )  
    
    dvts[t-1] <- ( dvts[t] + Ku[t]*d[t-1]*T[t] ) / ( 1 + Ku[t] )  
    
  }
  
  # Value of project = sum of 2 components = v_u + dvts
  apv = v_u + dvts
  
  # Use asset value to solve for WACC
  
  WACC <-  Ku * (1 - (lag(d, default=0) / lag(apv, default=0))*(T) )
  
  # Calculate asset value by period in reverse order using discount rates WACC[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting FCFF  1-period at the forward discount rate, WACC[t]
    
    v[t-1] <- ( v[t] + FCFF[t] ) / ( 1 + WACC[t] )  
  }
  
  # WACC_spot discount rates by Year
  
  WACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- WACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End WACC_spot discount rates by Year
  
  # Asset Value at t=0 using WACC_spot discount rates  
  v_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 11th row of 'asset_value_circ' output = asset_value_circ[11, ] is used as a check
  
  asset_value_apv <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku,  round(d,0), FCFF, Ku*lag(d, default=0)*T, 
                                      FCFF + Ku*lag(d, default=0)*T, v_u, dvts, apv, WACC, FCFF, v, WACC_spot, v_0,
                                      c( FCFF[1], rep(NaN,n)), npv_0, round(apv,0) - round(asset_value_circ[11, ],0) ) ) 
  
  names(asset_value_apv) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku",  "D", "FCFF", "FCFF_Adj", 
                              "ADJ_FCFF", "Vu", "DVTS", "APV", "WACC", "FCFF.1", "V" ,"WACC_spot", "V_0",
                              "FCFF_0", "NPV_0", "V_check")
  
  return(rotate(asset_value_apv))
}




asset_value_apv <- asset_value_apv( out3, flows )

round(asset_value_apv,5)



# Apply this R output to provided formatted Excel spreadsheet






asset_value_ccf <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # CCF is selected from the 15th row of 'flows' output
  CCF <- c[15, ]        
  
  # Asset Value by Year
  
  # Initialize asset value vector to zero
  
  v <- rep(0, n+1)
  
  # Calculate asset value by period in reverse order using discount rates Ku[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CCF 1-period at the forward discount rate, Ku[t]
    
    v[t-1] <- ( v[t] + CCF[t] + d[t-1]*(Ku[t] - Kd[t])*(T[t]) ) / ( 1 + Ku[t] )  
    
  }
  
  # Solve for Ke in order to solve for WACCbt
  
  Ke <- Ku +  (lag(d, default=0) / lag(v, default=0)) / (1 - (lag(d, default=0) / lag(v, default=0)))  * (1 - T) * (Ku - Kd) 
  
  WACCbt <- (1 - (lag(d, default=0) / lag(v, default=0)) ) * Ke +  (lag(d, default=0) / lag(v, default=0) ) * Kd  
  
  # Discounted CCF at WACCbt
  
  v.1 <- rep(0, n+1)
  
  # Calculate asset value by period in reverse order using discount rates WACCbt[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CCF 1-period at the forward discount rate, WACCbt[t]
    
    v.1[t-1] <- ( v.1[t] + CCF[t] ) / ( 1 + WACCbt[t] )  
    
  }
  
  # Spot discount rates by Year
  
  WACCbt_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACCbt_spot[2] <- WACCbt[2]
  
  for (t in 3:(n+1))   
  {
    WACCbt_spot[t] <- ( ( 1 + WACCbt_spot[t-1] )^(t-2) * ( 1 + WACCbt[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End WACC_spot discount rates by Year
  
  # Asset Value at t=0 using spot discount rates  
  v_0 <- c( sum( CCF[2:(n+1)] / (1 + WACCbt_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(CCF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 11th row of 'asset_value_circ' output = asset_value_circ[11, ] is used as a check
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ] 
  
  asset_value_ccf <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd,  round(d,0), FCFF, ie*T, 
                                      FCFF + ie*T, lag(d, default=0)*(Ku - Kd)*T, FCFF + ie*T + lag(d, default=0)*(Ku - Kd)*T,  
                                      v, Ke, WACCbt, FCFF + ie*T, v.1, WACCbt_spot, v_0, c( CCF[1], rep(NaN,n)), npv_0,
                                      round(v,0) - round(asset_value_circ[11, ],0) ) ) 
  
  names(asset_value_ccf) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "D", "FCFF", "ie_T", 
                              "CCF", "CCF_adj", "ADJ_CCF", 
                              "V", "Ke", "WACCbt", "CFF.1", "V.1",  "WACCbt_spot", "V_0", "CCF_0", "NPV_0", "V_check")
  
  return(rotate(asset_value_ccf))
}


asset_value_ccf <- asset_value_ccf( out3, out4, flows )

round(asset_value_ccf,5)



# Apply this R output to provided formatted Excel spreadsheet



asset_value_ccf_LEEWACCbt <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # CCF is selected from the 15th row of 'flows' output
  CCF <- c[15, ]        
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]     
  
  # Asset Value by Year
  
  # Initialize asset value vectors to zero
  v <- rep(0, n+1)
  v_LEE <- rep(0, n+1)
  
  # Initialize variable LEEWACCbt discount rate = Ku
  LEEWACCbt <- Ku
  
  # Calculate asset value by period in reverse order using discount rates Ku[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting Adjusted CCF 1-period at the forward discount rate, Ku[t]
    
    v[t-1] <- ( v[t] + CCF[t] + d[t-1]*(Ku[t] - Kd[t])*(T[t]) ) / ( 1 + Ku[t] )  
    
    # Discounting CCF 1-period at the forward discount rate, LEEWACCbt[t]
    
    LEEWACCbt[t] <- ( 1 + Ku[t] )  / ( 1 + ( d[t-1]*(Ku[t] - Kd[t])*(T[t]) ) / ( v_LEE[t] + CCF[t] ) ) - 1
    
    v_LEE[t-1] <- ( v_LEE[t] + CCF[t] ) / ( 1 + LEEWACCbt[t] )  
  }
  
  # Solve for Ke in order to solve for WACCbt
  
  Ke <- Ku +  (lag(d, default=0) / lag(v, default=0)) / (1 - (lag(d, default=0) / lag(v, default=0)))  * (1 - T) * (Ku - Kd) 
  
  WACCbt <- (1 - (lag(d, default=0) / lag(v, default=0)) ) * Ke +  (lag(d, default=0) / lag(v, default=0) ) * Kd 
  
  # or use this pretax WACC formula
  
  WACCbt.2 <- (1 - (lag(d, default=0) / lag(v, default=0)) ) * Ku +  (lag(d, default=0) / lag(v, default=0) ) * Ku  
  
  # or use this pretax WACC formula
  
  WACCbt.3 <-  Ku  
  
  # Discounted CCF at WACCbt
  
  v.1 <- rep(0, n+1)
  
  # Calculate asset value by period in reverse order using discount rates WACCbt[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CCF 1-period at the forward discount rate, WACCbt[t]
    
    v.1[t-1] <- ( v.1[t] + CCF[t] ) / ( 1 + WACCbt[t] )  
    
  }
  
  # Spot discount rates by Year
  
  WACCbt_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACCbt_spot[2] <- WACCbt[2]
  
  for (t in 3:(n+1))   
  {
    WACCbt_spot[t] <- ( ( 1 + WACCbt_spot[t-1] )^(t-2) * ( 1 + WACCbt[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using spot discount rates  
  v_0 <- c( sum( CCF[2:(n+1)] / (1 + WACCbt_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 11th row of 'asset_value_circ' output = asset_value_circ[11, ] is used as a check
  
  asset_value_ccf_LEEWACCbt <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd,  round(d,0),
                                                FCFF, ie*T, LEEWACCbt, v_LEE, FCFF + ie*T, lag(d, default=0)*(Ku - Kd)*T,
                                                FCFF + ie*T + lag(d, default=0)*(Ku - Kd)*T, v_LEE+CCF, 
                                                v, Ke, WACCbt, FCFF + ie*T, v.1, WACCbt_spot, v_0, c( FCFF[1], rep(NaN,n)), 
                                                npv_0, v - v_LEE ) ) 
  
  names(asset_value_ccf_LEEWACCbt) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "D",
                                        "FCFF", "ie_T", "LEEWACCbt", "v_LEE", "CCF", "CCF_adj",
                                        "ADJ_CCF", "V_CCF", "V", "Ke", "WACCbt", "CFF.1", "V.1",  "WACCbt_spot", "V_0", "FCFF_0", 
                                        "NPV_0", "value_check")
  
  return(rotate(asset_value_ccf_LEEWACCbt))
}

asset_value_ccf_LEEWACCbt <- asset_value_ccf_LEEWACCbt( out3, out4, flows )

round(asset_value_ccf_LEEWACCbt,5)


# Apply this R output to provided formatted Excel spreadsheet




asset_value_fcff_LEEWACC <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # Asset Value by Year
  
  # Initialize asset value vectors to zero
  v <- rep(0, n+1)
  v_LEE <- rep(0, n+1)
  
  # Initialize variable LEEWACC discount rate = Ku
  LEEWACC <- Ku
  
  # Calculate asset value by period in reverse order using discount rates Ku[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting Adjusted FCFF 1-period at the forward discount rate, Ku[t]
    
    v[t-1] <- ( v[t] + FCFF[t] + ie[t]*T[t] + d[t-1]*(Ku[t] - Kd[t])*(T[t]) ) / ( 1 + Ku[t] )  
    
    # Discounting FCFF 1-period at the forward discount rate, LEEWACC[t]
    
    LEEWACC[t] <- ( 1 + Ku[t] )  / ( 1 + ( ie[t]*T[t] + d[t-1]*(Ku[t] - Kd[t])*(T[t]) ) / ( v_LEE[t] + FCFF[t] ) ) - 1
    
    v_LEE[t-1] <- ( v_LEE[t] + FCFF[t] ) / ( 1 + LEEWACC[t] )  
  }
  
  # spot discount rates by Year
  
  WACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- LEEWACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + LEEWACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using spot discount rates  
  v_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  asset_value_fcff_LEEWACC <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd,  round(d,0), FCFF, ie*T,
                                               LEEWACC, v_LEE, ie*T + lag(d, default=0)*(Ku - Kd)*T, 
                                               FCFF + ie*T + lag(d, default=0)*(Ku - Kd)*T, v_LEE+FCFF, 
                                               v, FCFF + ie*T, WACC_spot, v_0, c( FCFF[1], rep(NaN,n)), npv_0, v - v_LEE ) ) 
  
  names(asset_value_fcff_LEEWACC) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "D", "FCFF", "ie_T",
                                       "LEEWACC", "v_LEE", "FCFF_adj",
                                       "ADJ_FCFF", "V_FCFF",
                                       "V",   "CFF.1",  "WACC_spot", "V_0", "FCFF_0", "NPV_0", "value_check")
  
  return(rotate(asset_value_fcff_LEEWACC))
}

asset_value_fcff_LEEWACC <- asset_value_fcff_LEEWACC( out3, out4, flows )

asset_value_fcff_LEEWACC


# Apply this R output to provided formatted Excel spreadsheet





equity_value_ecf_LEEKe <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  
  # Debt value by period is the 8th row in the debt_value output matrix 
  d <- debt_value[8, ]  
  
  # ECF is selected from the 12th row of 'flows' output
  ECF <- c[12, ]        
  
  # Equity Value by Year
  
  # Initialize equity value vectors to zero
  e <- rep(0, n+1)
  e_LEE <- rep(0, n+1)
  
  # Initialize variable LEEKe discount rate = Ku
  LEEKe <- Ku
  
  # Calculate equity value by period in reverse order using discount rates Ku[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting Adjusted ECF 1-period at the forward discount rate, Ku[t]
    
    e[t-1] <- ( e[t] + ECF[t] - d[t-1]*(Ku[t] - Kd[t])*(1-T[t]) ) / ( 1 + Ku[t] )  
    
    # Discounting ECF 1-period at the forward discount rate, LEEKe[t]
    
    LEEKe[t] <- ( 1 + Ku[t] )  / ( 1 - ( d[t-1]*(Ku[t] - Kd[t])*(1-T[t]) ) / ( e_LEE[t] + ECF[t] ) ) - 1
    
    e_LEE[t-1] <- ( e_LEE[t] + ECF[t] ) / ( 1 + LEEKe[t] )  
  }
  
  # Spot discount rates by Year
  
  Ke_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ke_spot[2] <- LEEKe[2]
  
  for (t in 3:(n+1))   
  {
    Ke_spot[t] <- ( ( 1 + Ke_spot[t-1] )^(t-2) * ( 1 + LEEKe[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Equity Value at t=0 using spot discount rates  
  e_0 <- c( sum( ECF[2:(n+1)] / (1 + Ke_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  equity_value_ecf_LEEKe <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd,  round(d,0), ECF,  LEEKe, e_LEE,
                                             lag(d, default=0)*(Ku - Kd)*(1-T), ECF - lag(d, default=0)*(Ku - Kd)*(1-T), e_LEE+ECF, 
                                             e, Ke_spot, e_0, c( ECF[1], rep(NaN,n)), npv_0, e - e_LEE ) ) 
  
  names(equity_value_ecf_LEEKe) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "D", "ECF", "LEEKe", "e_LEE",
                                     "ECF_adj", "ADJ_ECF", "E_ECF",
                                     "E",  "Ke_spot", "E_0", "ECF_0", "NPV_0", "value_check")
  
  return(rotate(equity_value_ecf_LEEKe))
}

equity_value_ecf_LEEKe <- equity_value_ecf_LEEKe( out3, out4, flows )

round(equity_value_ecf_LEEKe,5)


# Apply this R output to provided formatted Excel spreadsheet



library(tidyverse)

n_ST  <- 5      # short-term modeling period = 5 years
g     <- 0.02   # long-term annual growth rate in 'flows'

only_APV <- tibble( Year = c(1:n_ST))

only_APV <- only_APV %>%
  mutate(ie    = c(120, 120, 120,    120, 120*(1+g) ),    # Interest Expense, first 5 years, grow at 2% thereafter to infinity  
         CFd   = c(120, 120, 120,     90,  90*(1+g)),     # Debt Cash Flow, first 5 years, grow at 2% thereafter to infinity
         ECF   = c(165,  29, 338, 400.65, 400.65*(1+g)),  # Equity Cash Flow, first 5 years, grow at 2% thereafter to infinity
         CCF   = CFd + ECF,                               # Capital Cash Flow, first 5 years, grow at 2% thereafter to infinity
         FCFF  = c(243, 107, 416, 448.65, 448.65*(1+g)),  # Free Cash Flow, first 5 years, grow at 2% thereafter to infinity
         T     = rep(0.35, n_ST),                         # Corporate Tax Rate
         Rf    = rep(0.06, n_ST),                         # Risk-free interest rate
         MRP   = rep(0.04, n_ST),                         # E(Market risk premium) = E(MRP) 
         Bu    = rep(1.0, n_ST),                          # Beta of unlevered equity for use in CAPM
         Ku    = Rf + Bu * MRP,                           # Cost of unlevered equity using CAPM       
         Bd    = rep(0.50, n_ST),                         # Debt Beta for use in CAPM
         Kd    = Rf + Bd * MRP )                          # Cost of Debt Capital

rotate(only_APV)


# Create matrix from 'only_APV' tibble for 'forward rate discounting' loop
# Include a 't=0' row
only_APV_matrix  <- rbind( rep(0,ncol(only_APV)) , as.matrix(only_APV))
only_APV_matrix

# ' Noncircular' Debt valuation 
CFd <- only_APV_matrix[, "CFd"]
Kd  <- only_APV_matrix[, "Kd"]

# Value of long-term flows to infinity using Gordon model
# Debt Value at end of Year 5 = d5 
# CFd at end of year 6 = CFd[n_ST+1]*(1+g)
d5 <- CFd[n_ST+1]*(1+g) / (Kd[n_ST] - g)

#Initialize a zero-filled debt valuation vector for the short-term
d = c(rep(0, n_ST+1))

# Insert PV of Long-term Flows at end of year 'n_ST' = year 5
# Debt Value at end of Year 5 = d5 is matrix element = n_ST + 1 = 5+1 = 6 
d[n_ST+1] <- d5

# Discount CFd @ Kd to determine Debt Value by year (Years 0-4)

for (t in (n_ST+1):2)
{
  d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
}


value_tbl <- tibble(Year = 0:n_ST,
                    D    = round(d,2)   )

value_tbl


ECF <- only_APV_matrix[ , "ECF"]
Ku  <- only_APV_matrix[ , "Ku"]
T   <- only_APV_matrix[ , "T"]

# Value of long-term flows to infinity using LEEMODEL
# Equity Value at end of Year 5 = e5 = matrix element = n_ST + 1 = 5+1 = 6 
# Equity Value @ end of year 5 based on LEEMODEL (Constant growth)

e5 <- ( ECF[n_ST+1] - d[n_ST]*(Ku[n_ST+1]-Kd[n_ST+1])*(1-T[n_ST+1]) )*(1+g) / (Ku[n_ST+1] - g)

round(e5,2)


# Initialize a zero-filled equity valuation vector for the short-term
e = c(rep(0, n_ST+1))

# Initialize LEEKe vector for the short-term, set equal to Ku or any other series of values
LEEKe <- Ku

# Insert PV of Long-term Flows at end of year 'n_ST' = year 5 
# Equity Value at end of Year 5 = e5 is matrix element = n_ST + 1 = 5+1 = 6 
e[n_ST+1] <- e5

# Discount ECF @ 'Noncircular' LEEKe to determine Equity Value by year

for (t in (n_ST+1):2)
{
  #'Noncircular' levered cost of equity capital calculation = LEEKe
  LEEKe[t] <- (1 + Ku[t]) / ( (1 - (d[t-1]*(Ku[t] - Kd[t])*(1-T[t]) ) / (e[t] + ECF[t]) ) ) - 1
  
  # 'Noncircular' Equity valuation @ LEEKe
  e[t-1] <- (e[t] + ECF[t]) / (1 + LEEKe[t])
}

value_tbl <- value_tbl %>% 
  mutate(E      = round(e,2),
         V__E_D = round(E,2) + round(D,2) )

value_tbl   


FCFF <- only_APV_matrix[ , "FCFF"]
ie   <- only_APV_matrix[ , "ie"]

# Value of long-term flows to infinity using LEEMODEL
# Asset Value at end of Year 5 = e5 = matrix element = n_ST + 1 = 5+1 = 6 
# Asset Value @ end of year 5 based on LEEMODEL (Constant growth)
v_fcff_5 <- ( FCFF[n_ST+1] + (ie[n_ST+1])*(T[n_ST+1]) + d[n_ST]*(Ku[n_ST+1]-Kd[n_ST+1])*(T[n_ST+1]) )*(1+g) / (Ku[n_ST+1] - g)

round(v_fcff_5,2)


# Initialize a zero-filled asset valuation vector for the short-term
v_fcff = c(rep(0, n_ST+1))

# Initialize LEEWACC vector for the short-term, set equal to Ku or any other series of values
LEEWACC <- Ku

# Insert PV of Long-term Flows at end of year 'n_ST' = year 5 
# Asset Value at end of Year 5 = e5 is matrix element = n_ST + 1 = 5+1 = 6 
v_fcff[n_ST+1] <- v_fcff_5

# Discount FCFF @ 'Noncircular' LEEWACC to determine Asset Value by year

for (t in (n_ST+1):2)
{
  #'Noncircular' Weighted Average Cost of Capital (General case) = LEEWACC
  LEEWACC[t] <- (1 + Ku[t]) / ( (1 + ( (ie[t])*(T[t]) + d[t-1]*(Ku[t] - Kd[t])*(T[t]) ) / (v_fcff[t] + FCFF[t]) ) ) - 1
  
  # 'Noncircular' Asset valuation @ LEEWACC
  v_fcff[t-1] <- (v_fcff[t] + FCFF[t]) / (1 + LEEWACC[t])
}

value_tbl <- value_tbl %>% 
  mutate(V_FCFF = round(v_fcff,2) )

value_tbl   


CCF  <- only_APV_matrix[ , "CCF"]

# Value of long-term flows to infinity using LEEMODEL
# Asset Value at end of Year 5 = e5 = matrix element = n_ST + 1 = 5+1 = 6 
# Asset Value @ end of year 5 based on LEEMODEL (Constant growth)
v_ccf_5 <- ( CCF[n_ST+1] + d[n_ST]*(Ku[n_ST+1]-Kd[n_ST+1])*(T[n_ST+1]) )*(1+g) / (Ku[n_ST+1] - g)

round(v_ccf_5,2)

# Initialize a zero-filled asset valuation vector for the short-term
v_ccf = c(rep(0, n_ST+1))

# Initialize LEEWACCbt vector for the short-term, set equal to Ku or any other series of values
LEEWACCbt <- Ku

# Insert PV of Long-term Flows at end of year 'n_ST' = year 5 
# Asset Value at end of Year 5 = e5 is matrix element = n_ST + 1 = 5+1 = 6 
v_ccf[n_ST+1] <- v_ccf_5

# Discount CCF @ 'Noncircular' LEEWACCbt to determine Asset Value by year

for (t in (n_ST+1):2)
{
  #'Noncircular' Weighted Average Cost of Capital (General case) = LEEWACCbt
  LEEWACCbt[t] <- (1 + Ku[t]) /  (1 + ( ( d[t-1]*(Ku[t] - Kd[t])*(T[t]) ) / (v_ccf[t] + CCF[t]) ) ) - 1
  
  # 'Noncircular' Asset valuation @ LEEWACCbt
  v_ccf[t-1] <- (v_ccf[t] + CCF[t]) / (1 + LEEWACCbt[t])
}

value_tbl <- value_tbl %>% 
  mutate(V_CCF = round(v_ccf,2) )

value_tbl  




equity_value_LEEKe <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # ECF is selected from the 12th row of 'flows' output
  ECF <- c[12, ]        
  
  # Initialize debt value and equity value vectors to zero
  d <- rep(0, n+1)
  e <- rep(0, n+1)
  
  # Initialize Ku adjustment 'b' vector to zero
  # LEEKe = (Ku + b) / (1-b)
  b <- rep(0, n+1)
  
  # Initialize 'cum' debt-to-equity vector to 1.0
  cum_d_e_ratio <- rep(1, n+1)
  
  Ku_Kd_AT <- (Ku - Kd)/(1 + Kd)*(1-T)
  
  # Initialize Noncircular LEEKe discount rate = Ku
  LEEKe <- Ku
  
  # Calculate Equity value by period in reverse order using discount rates LEEKe[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
    
    cum_d_e_ratio[t] <- ( d[t] + CFd[t] ) / (e[t] + ECF[t] )
    
    # Noncircular 'b' term in LEEKe calculation
    b[t] <-   cum_d_e_ratio[t] * Ku_Kd_AT[t] 
    
    # LEEKe calculation is Noncircular
    LEEKe[t] <-   (Ku[t] + b[t]) / (1 - b[t]) 
    
    e[t-1] <- ( e[t] + ECF[t] ) / ( 1 + LEEKe[t] )  
  }
  
  # Calculate Ke using circular formula
  
  Ke <- Ku +  (lag(d, default=0) / lag(e, default=0))  * (Ku - Kd) * (1 - T)  
  
  # Noncircular asset valuation  
  v <- e + d
  
  # Ke_spot discount rates by Year
  Ke_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ke_spot[2] <- Ke[2]
  
  for (t in 3:(n+1))   
  {
    Ke_spot[t] <- ( ( 1 + Ke_spot[t-1] )^(t-2) * ( 1 + Ke[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End Ke_spot discount rates by Year
  
  # Equity Value at t=0 using Ke_spot discount rates  
  e_0 <- c( sum( ECF[2:(n+1)] / (1 + Ke_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  equity_value_LEEKe <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd, round(d,0),
                                         ECF, e, cum_d_e_ratio, Ku_Kd_AT, b, LEEKe, v, Ke, Ke_spot,
                                         e_0, round(v,0) - round(v_check,0), round(e,0) - round(e_check,0),
                                         round(d,0) - round(d_check,0), round(c(0, LEEKe[2:(n+1)] - Ke[2:(n+1)]),5), npv_0 ) )
  
  names(equity_value_LEEKe) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd",
                                 "D", "ECF", "E", "cum_d_e_ratio", "Ku_Kd_AT", "b", "LEEKe", 
                                 "V", "Ke",  "Ke_spot", "E_0", "v_check", "e_check", "d_check", "Ke_check", "NPV_0" )
  
  return(rotate(equity_value_LEEKe))
}


equity_value_LEEKe <- equity_value_LEEKe( out3, out4, flows )

round(equity_value_LEEKe,5) 


# Apply this R output to provided formatted Excel spreadsheet




asset_value_LEEWACC <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # Initialize debt value and asset value vectors to zero
  d <- rep(0, n+1)
  v <- rep(0, n+1)
  
  # Initialize Ku adjustment 'c1' vector to zero
  # LEEKWACC = (Ku - c1) / (1 + c1)
  c1 <- rep(0, n+1)
  
  # Initialize 'cum' debt-to-asset vector to 0.5
  cum_d_v_ratio <- rep(0.5, n+1)
  
  pv_Ku_Kd <- (Ku - Kd)/(1 + Kd)
  
  # Initialize Noncircular LEEWACC discount rate = Ku
  LEEWACC <- Ku
  
  # Calculate Asset value by period in reverse order using discount rates LEEWACC[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
    
    cum_d_v_ratio[t] <- ( d[t] + CFd[t] ) / (v[t] + FCFF[t] )
    
    # Noncircular 'c' term is LEEWACC calculation
    c1[t] <-  ( cum_d_v_ratio[t] * pv_Ku_Kd[t] + ie[t]/( v[t] + FCFF[t] ) )*(T[t])
    
    # LEEWACC calculation is Noncircular
    LEEWACC[t] <-   (Ku[t] - c1[t]) / (1 + c1[t]) 
    
    # Discounting FCFF 1-period at the forward discount rate, LEEWACC[t]
    v[t-1] <- ( v[t] + FCFF[t] ) / ( 1 + LEEWACC[t] )  
  }
  
  # Calculate WACC using circular formula
  
  WACC <- Ku - (lag(d, default=0) / lag(v, default=0)) * (Ku - Kd)*(T) - ie/lag(v, default=0)*(T)
  
  # Noncircular equity valuation  
  e <- v - d
  
  # Spot discount rates by Year
  
  WACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- WACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End WACC_spot discount rates by Year
  
  # Asset Value at t=0 using WACC_spot discount rates  
  v_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  asset_value_LEEWACC <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd, round(d,0), 
                                          FCFF, e, ie/(v+FCFF)*(T), cum_d_v_ratio, pv_Ku_Kd*T, c1, LEEWACC, 
                                          v, WACC, WACC_spot, v_0, round(v,0) - round(v_check,0),
                                          round(e,0) - round(e_check,0), round(d,0) - round(d_check,0),
                                          round(c(0, LEEWACC[2:(n+1)] - WACC[2:(n+1)]),9), npv_0 ) )
  
  names(asset_value_LEEWACC) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd",
                                  "D", "FCFF", "E", "ie_v_FCFF_T", "cum_d_v_ratio", "PV_Ku_Kd_T", "c", "LEEWACC", 
                                  "V", "WACC",  "WACC_spot", "V_0", "v_check", 
                                  "e_check", "d_check", "WACC_check", "NPV_0" )
  
  return(rotate(asset_value_LEEWACC))
}



asset_value_LEEWACC <- asset_value_LEEWACC( out3, out4, flows )

round(asset_value_LEEWACC,5)


# Apply this R output to provided formatted Excel spreadsheet





asset_value_LEEWACCbt <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # CCF is selected from the 15th row of 'flows' output
  CCF <- c[15, ]        
  
  # Initialize debt value and asset value vectors to zero
  d <- rep(0, n+1)
  v <- rep(0, n+1)
  
  # Initialize Ku adjustment 'd1' vector to zero
  # LEEKWACC = (Ku - d1) / (1 + d1)
  d1 <- rep(0, n+1)
  
  # Initialize 'cum' debt-to-asset vector to 0.5
  cum_d_v_ratio <- rep(0.5, n+1)
  
  pv_Ku_Kd <- (Ku - Kd)/(1 + Kd)
  
  # Initialize Noncircular LEEWACCbt discount rate = Ku
  LEEWACCbt <- Ku
  
  # Calculate Asset value by period in reverse order using discount rates LEEWACCbt[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
    
    cum_d_v_ratio[t] <- ( d[t] + CFd[t] ) / (v[t] + CCF[t] )
    
    # Noncircular 'd1' term is LEEWACCbt calculation
    d1[t] <-  ( cum_d_v_ratio[t] * pv_Ku_Kd[t] )*(T[t])
    
    # LEEWACCbt calculation is Noncircular
    LEEWACCbt[t] <-   (Ku[t] - d1[t]) / (1 + d1[t]) 
    
    # Discounting FCFF 1-period at the forward discount rate, LEEWACCbt[t]
    v[t-1] <- ( v[t] + CCF[t] ) / ( 1 + LEEWACCbt[t] )  
  }
  
  # Calculate WACCbt using circular formula
  
  WACCbt <- Ku - (lag(d, default=0) / lag(v, default=0)) * (Ku - Kd) * (T) 
  
  # Noncircular equity valuation  
  e <- v - d
  
  # WACCbt_spot discount rates by Year
  
  WACCbt_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACCbt_spot[2] <- WACCbt[2]
  
  for (t in 3:(n+1))   
  {
    WACCbt_spot[t] <- ( ( 1 + WACCbt_spot[t-1] )^(t-2) * ( 1 + WACCbt[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End WACCbt_spot discount rates by Year
  
  # Asset Value at t=0 using WACCbt_spot discount rates  
  v_0 <- c( sum( CCF[2:(n+1)] / (1 + WACCbt_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(CCF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  asset_value_LEEWACCbt <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd, round(d,0),
                                            CCF, e, cum_d_v_ratio, pv_Ku_Kd*T, d1, LEEWACCbt, v, WACCbt, WACCbt_spot,
                                            v_0, round(v,0) - round(v_check,0), round(e,0) - round(e_check,0),
                                            round(d,0) - round(d_check,0), round(c(0, LEEWACCbt[2:(n+1)] - WACCbt[2:(n+1)]),9), npv_0 ) )
  
  names(asset_value_LEEWACCbt) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd", "D",
                                    "CCF", "E", "cum_d_v_ratio", "PV_Ku_Kd_T", "d", "LEEWACCbt", "V", "WACCbt",  "WACCbt_spot",
                                    "V_0", "v_check", "e_check", "d_check", "WACCbt_check", "NPV_0" )
  
  return(rotate(asset_value_LEEWACCbt))
}



asset_value_LEEWACCbt <- asset_value_LEEWACCbt( out3, out4, flows )

round(asset_value_LEEWACCbt,5)


# Apply this R output to provided formatted Excel spreadsheet




equity_value_Ke_LEEDE <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # ECF is selected from the 12th row of 'flows' output
  ECF <- c[12, ]        
  
  # Initialize debt value and equity value vectors to zero
  d <- rep(0, n+1)
  e <- rep(0, n+1)
  
  # Initialize LEEDE debt-to-equity to 1.0 (calculated 2 mathematically equivalent ways)
  LEEDE  <- rep(1.0, n+1)
  LEEDE2 <- rep(1.0, n+1)
  
  # Initialize Ku adjustment 'b' vector to zero
  # LEEKe = (Ku + b) / (1-b)
  b <- rep(0, n+1)
  
  # Initialize 'cum' debt-to-equity vector to 1.0
  cum_d_e_ratio <- rep(1, n+1)
  
  Ku_Kd_AT <- (Ku - Kd)/(1 + Kd)*(1-T)
  
  # Initialize Noncircular LEEKe discount rate = Ku
  LEEKe <- Ku
  
  # Initialize Ke_LEEDE discount rate = Ku
  Ke_LEEDE  <- Ku   # uses LEEDE
  Ke_LEEDE2 <- Ku   # uses LEEDE2
  
  # Calculate Equity value by period in reverse order using discount rates LEEKe[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
    
    cum_d_e_ratio[t] <- ( d[t] + CFd[t] ) / (e[t] + ECF[t] )
    
    # Noncircular 'b' term is LEEKe calculation
    b[t] <-   cum_d_e_ratio[t] * Ku_Kd_AT[t] 
    
    # LEEKe calculation is Noncircular
    LEEKe[t] <-   (Ku[t] + b[t]) / (1 - b[t]) 
    
    # LEEDE = lagged D/E ratio
    LEEDE[t]  <-  (d[t] + CFd[t]) * (1 + Ku[t]) /  ( (e[t] + ECF[t]) * (1 + Kd[t]) - (d[t] + CFd[t]) * (Ku[t] - Kd[t]) * (1 - T[t])  )
    LEEDE2[t] <-   cum_d_e_ratio[t] * ( (1 +  LEEKe[t] ) / (1 + Kd[t]) ) 
    
    # LEEDE is the lagged D/E ratio used in the Ke function
    Ke_LEEDE[t]  <- Ku[t] + ( LEEDE[t]  )*( Ku[t] - Kd[t] )*(1 - T[t]) 
    Ke_LEEDE2[t] <- Ku[t] + ( LEEDE2[t] )*( Ku[t] - Kd[t] )*(1 - T[t]) 
    
    # Discounting ECF 1-period at the forward discount rate, LEEKe[t]
    e[t-1] <- ( e[t] + ECF[t] ) / ( 1 + Ke_LEEDE[t] )  
  }
  
  # Noncircular asset valuation  
  v <- e + d
  
  d_e_ratio <- d / e
  
  # Ke_spot discount rates by Year
  
  Ke_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ke_spot[2] <- Ke_LEEDE[2]
  
  for (t in 3:(n+1))   
  {
    Ke_spot[t] <- ( ( 1 + Ke_spot[t-1] )^(t-2) * ( 1 + Ke_LEEDE[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End Ke_spot discount rates by Year
  
  # Equity Value at t=0 using Ke_spot discount rates  
  e_0 <- c( sum( ECF[2:(n+1)] / (1 + Ke_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  equity_value_Ke_LEEDE <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd, round(d,0), 
                                            ECF, e, cum_d_e_ratio, Ku_Kd_AT, b, LEEKe, LEEDE, LEEDE2, 
                                            v, Ke_LEEDE, Ke_LEEDE2, Ke_spot, e_0, round(v,0) - round(v_check,0),
                                            round(e,0) - round(e_check,0), round(d,0) - round(d_check,0),
                                            round(c(0, LEEKe[2:(n+1)] - Ke_LEEDE[2:(n+1)]),5), npv_0, round(LEEDE - LEEDE2,5), 
                                            round(Ke_LEEDE - Ke_LEEDE2,5), d_e_ratio ) )
  
  names(equity_value_Ke_LEEDE) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd", "D",
                                    "ECF", "E", "cum_d_e_ratio", "Ku_Kd_AT", "b", "LEEKe", "LEEDE", "LEEDE2",
                                    "V", "Ke_LEEDE", "Ke_LEEDE2", "Ke_spot", "E_0", "v_check", "e_check", "d_check",
                                    "Ke_check", "NPV_0", "LEEDE__LEEDE2_check", "Ke_LEEDE__Ke_LEEDE2_check", "D_E_ratio" )
  
  return(rotate(equity_value_Ke_LEEDE))
}

equity_value_Ke_LEEDE <- equity_value_Ke_LEEDE( out3, out4, flows )

round(equity_value_Ke_LEEDE,5)



# Apply this R output to provided formatted Excel spreadsheet



# 4 'General case' WACC Methods are included in the below R function, each with its own formatted spreadsheet representation

asset_value_3_term_WACC_LEEIET3 <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  T  <- a$T
  Rf  <- a$Rf
  MRP <- a$MRP
  Bu  <- a$Bu
  Ku  <- Rf + Bu * MRP
  Bd  <- a$Bd
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]
  
  # ECF is selected from the 12t row of 'flows' output
  ECF <- c[12, ] 
  
  # Free cash flow is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # 1. Circular 3-term WACC = f(Ke, Kd)
  
  # Asset Value by Year
  
  d_e_ratio <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  
  e_calc <- 10000 * d_e_ratio  # Initialize equity value convergence vector to $10000 each Year as a guess
  e <- e_calc                  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d <- d_e_ratio * e
  
  # V = D + E = (debt-to-equity ratio)(E) + E = (1 + debt-to-equity ratio)(E)
  
  v_calc <- (e)*(1 + d_e_ratio)      # Initialize asset value convergence vector guess
  v <- v_calc                        # Initialize asset value convergence vector guess
  
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  WACC <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0) / lag(v, default=0)) * (Kd) - (ie*T) / lag(v, default=0)  
  
  # Calculate asset value by period in reverse order using discount rates WACC[t]
  
  iterations <- 100
  tolerance  <- 0.000001
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is Noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Discounting 1-period at the forward discount rate, WACC[t] 
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      d_v_ratio_t_1 <- d_e_ratio_t_1 / ( 1 + d_e_ratio_t_1)      # Per Appendix
      
      Ke[t]   <-  Ku[t] + d_e_ratio_t_1 * (1-T[t]) * (Ku[t] - Kd[t])
      WACC[t] <-  (1 - d_v_ratio_t_1) * Ke[t] + d_v_ratio_t_1 * Kd[t] - (ie[t]*T[t]) / v[t-1]  
      
      v_calc[t-1] <- ( v[t] + FCFF[t] ) / ( 1 + WACC[t] )  
      
      # Recalculate equity value for period 't-1' based on calculated asset value and debt-equity ratio vector 
      e[t-1] <- v_calc[t-1] * (1 - d_v_ratio_t_1 )
      
    }
    
    # Determine error in iterative valuation process
    error <- sum((v - v_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v <- v_calc
  }
  
  # WACC weights (general case)
  we    <- ifelse(v==0,0,e/v)
  wd    <- ifelse(v==0,0,d/v)
  w_1_V <- ifelse(v==0,0,1/v)
  
  # Weighted component costs of WACC (general case)
  we_Ke  <- lag(we, default=0)*Ke
  wd_Kd  <- lag(wd, default=0)*Kd
  w_ie_T <- lag(w_1_V, default=0)*(ie*T)
  
  WACC_components <- we_Ke + wd_Kd - w_ie_T
  
  # WACC check
  WACC_check <- WACC - WACC_components
  
  # Valuation Checks
  v_check1 <- v - asset_value[11, ]
  e_check1 <- e - equity_value[13, ]
  d_check1 <- d - debt_value[8, ]
  
  # Spot Discount Rates
  WACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- WACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using Spot discount rates  
  v_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  
  # 2. Circular 3-term WACC = f(Ku)
  
  # Asset Value by Year
  
  d_e_ratio2 <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  d_v_ratio2 <- d_e_ratio2 / ( 1 + d_e_ratio2)
  
  e_calc2 <- 10000 * d_e_ratio2  # Initialize equity value convergence vector to $10000 each Year as a guess
  e2 <- e_calc2                  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d2 <- d_e_ratio2 * e2
  
  # V = D + E = (debt-to-equity ratio)(E) + E = (1 + debt-to-equity ratio)(E)
  
  v_calc2 <- (e2)*(1 + d_e_ratio2)     # Initialize asset value convergence vector guess
  v2 <- v_calc2                        # Initialize asset value convergence vector guess
  
  Ke2   <-  Ku + (lag(d2, default=0) / lag(e2, default=0)) * (1-T) * (Ku - Kd)
  WACC2 <-  Ku - d_v_ratio2*(Ku - Kd)*(T) - (ie*T) / lag(v2, default=0)  
  
  # Calculate asset value by period in reverse order using discount rates WACC[t]
  
  iterations2 <- 100
  tolerance2  <- 0.000001
  
  for (j in 1:iterations2 ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is Noncircular
      
      d2[t-1] <- (d2[t] + CFd[t]) / (1 + Kd[t])
      
      # Discounting 1-period at the forward discount rate, WACC[t] 
      
      # Prevent division by zero errors
      d_e_ratio_t_1_2 <- ifelse( e2[t-1] == 0, 0, d2[t-1] / e2[t-1] )
      d_v_ratio_t_1_2 <- d_e_ratio_t_1_2 / ( 1 + d_e_ratio_t_1_2)      # Per Appendix
      
      Ke2[t]   <-  Ku[t] + d_e_ratio_t_1_2 * (1-T[t]) * (Ku[t] - Kd[t])
      
      WACC2[t] <-  Ku[t] - d_v_ratio_t_1_2 * (Ku[t] - Kd[t])*(T[t]) - (ie[t]*T[t]) / v2[t-1]  
      
      v_calc2[t-1] <- ( v2[t] + FCFF[t] ) / ( 1 + WACC2[t] )  
      
      # Recalculate equity value for period 't-1' based on calculated asset value and debt-equity ratio vector 
      e2[t-1] <- v_calc2[t-1] * (1 - d_v_ratio_t_1_2 )
      
    }
    
    # Determine error in iterative valuation process
    error2 <- sum((v2 - v_calc2)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error2 < tolerance2) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v2 <- v_calc2
  }
  
  # WACC weights (general case)
  w_2nd_term  <- ifelse(v2==0,0,d2/v2)
  w_1_V2 <- ifelse(v2==0,0,1/v2)
  
  term_2 <- (Ku-Kd)*(T)
  
  # Weighted component costs of WACC (general case)
  weighted_2nd_term  <- lag(w_2nd_term, default=0)*term_2
  w_ie_T2 <- lag(w_1_V2, default=0)*(ie*T)
  
  WACC_components2 <- Ku - weighted_2nd_term - w_ie_T2
  
  # WACC check
  WACC2_check <- WACC2 - WACC_components2
  
  # Valuation Checks
  v_check2 <- v2 - asset_value[11, ]
  e_check2 <- e2 - equity_value[13, ]
  d_check2 <- d2 - debt_value[8, ]
  
  # Spot Discount Rates
  WACC2_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC2_spot[2] <- WACC2[2]
  
  for (t in 3:(n+1))   
  {
    WACC2_spot[t] <- ( ( 1 + WACC2_spot[t-1] )^(t-2) * ( 1 + WACC2[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using Spot discount rates  
  v2_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC2_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  v2_0 <- c(round(v2_0[1],0), rep(NaN,n) )
  
  npv2_0 <- round(v2_0[1],0) + round(FCFF[1],0)
  npv2_0 <- c(npv2_0, rep(NaN,n) )
  
  
  # 3. Noncircular 3-term WACC = f(Ke, Kd) using Noncircular LEEIET3 required 3rd WACC term
  
  # Asset Value by Year
  
  d_e_ratio3 <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  
  e3 <- 10000 * d_e_ratio3  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d3 <- d_e_ratio3 * e3
  
  # V = D + E = (debt-to-equity ratio)(E) + E = (1 + debt-to-equity ratio)(E)
  
  v3 <- d3 + e3        # Initialize asset value vector guess
  
  # Noncircular 'b' term
  cum_d_e_ratio <- (d3 + CFd) / (e3 + ECF)
  Ku_Kd_AT <- (d3 + CFd) / (1+Kd) * (1-T)
  b_ <- cum_d_e_ratio * Ku_Kd_AT
  
  LEEKe <- (Ku + b_) / (1-b_) 
  
  # Noncircular 'f' term
  f_ <- (1+Ku) / ( v3 + FCFF + ie*T + lag(d3,default=0)*(Ku-Kd)*(T) )
  
  # Noncircular debt ratio, alternatively use LEEDA <- lag(d3, default=0) * f_
  LEEDA <- (d3+CFd) / (1+Kd) * f_
  
  # Noncircular interest expense tax shield relative to prior period asset value
  LEEIET3 <- (ie*T) * f_
  
  # Noncircular WACC    
  LEEWACC3 <-  (1 - LEEDA) * (LEEKe) + (LEEDA)*(Kd) - LEEIET3  
  
  # Calculate asset value by period in reverse order using discount rates WACC[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    d3[t-1] <- (d3[t] + CFd[t]) / (1 + Kd[t])
    
    # Discounting 1-period at the forward discount rate, WACC[t] 
    
    # Noncircular 'b' term
    # 'cum' Debt-to-Equity ratio
    cum_d_e_ratio[t] <- (d3[t] + CFd[t]) / (e3[t] + ECF[t])
    Ku_Kd_AT[t] <- ((Ku[t] - Kd[t])/(1+Kd[t])) * (1 - T[t])
    b_[t] <- cum_d_e_ratio[t] * Ku_Kd_AT[t]
    
    LEEKe[t] <- (Ku[t] + b_[t]) / (1-b_[t]) 
    
    # Noncircular 'f' term
    f_[t] <- (1+Ku[t]) / ( v3[t] + FCFF[t] + ie[t]*T[t] + d3[t-1]*(Ku[t]-Kd[t])*(T[t]) ) 
    
    # Prevent division by zero errors relative to 'f' term
    f_[t] <- ifelse(  ( v3[t] + FCFF[t] + ie[t]*T[t] + d3[t-1]*(Ku[t]-Kd[t])*(T[t]) ) == 0, 0, f_[t] )
    
    # Noncircular debt ratio, alternatively use LEEDA[t] <- d3[t-1] * f_[t]
    LEEDA[t] <- (d3[t]+CFd[t]) / (1+Kd[t]) * f_[t]
    
    # Noncircular interest expense tax shield relative to prior period asset value
    LEEIET3[t] <- (ie[t]*T[t]) * f_[t]
    
    # Noncircular WACC    
    LEEWACC3[t] <-  (1 - LEEDA[t])*LEEKe[t] + LEEDA[t]*(Kd[t]) - LEEIET3[t]  
    
    v3[t-1] <- ( v3[t] + FCFF[t] ) / ( 1 + LEEWACC3[t] )  
    
    e3[t-1] <- v3[t-1] - d3[t-1]
    
  }
  
  # WACC check
  LEEWACC3_check <- LEEWACC3 - WACC_components2
  
  # Valuation Checks
  v_check3 <- v3 - asset_value[11, ]
  e_check3 <- e3 - equity_value[13, ]
  d_check3 <- d3 - debt_value[8, ]
  
  # Spot Discount Rates
  WACC3_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC3_spot[2] <- LEEWACC3[2]
  
  for (t in 3:(n+1))   
  {
    WACC3_spot[t] <- ( ( 1 + WACC3_spot[t-1] )^(t-2) * ( 1 + LEEWACC3[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using Spot discount rates  
  v3_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC3_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  v3_0 <- c(round(v3_0[1],0), rep(NaN,n) )
  
  npv3_0 <- round(v3_0[1],0) + round(FCFF[1],0)
  npv3_0 <- c(npv3_0, rep(NaN,n) )
  
  
  # 4. Noncircular 3-term WACC = f(Ku)
  # WACC = Ku - (LEEDA)(Ku-Kd)(T) - LEEIET3
  
  # Asset Value by Year
  
  d4 <- rep(0, n+1)  # Initialize debt value to $0 each year
  
  e4 <- d4           # Initialize equity value to $0 each year
  
  v4 <- e4 + d4      # Initialize asset value to $0 each year
  
  # Noncircular 'f' term
  f4_ <-  (1+Ku) / ( v4 + FCFF + ie*T + (lag(d4,default=0)*(Ku-Kd)*(T) ) )  
  
  # Noncircular debt ratio, alternatively use LEEDA_4[t] <- lag(d4,default=0) * f4_
  LEEDA_4 <- (d4+CFd) / (1+Kd) * f4_
  
  # Noncircular interest expense tax shield relative to prior period asset value
  LEEIET3_4 <- (ie*T) * f4_
  
  # Noncircular WACC    
  LEEWACC4 <-  Ku - LEEDA_4 * (Ku - Kd) * (T) - LEEIET3_4  
  
  # Calculate asset value by period in reverse order using discount rates LEEWACC4[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    
    d4[t-1] <- (d4[t] + CFd[t]) / (1 + Kd[t])
    
    # Noncircular 'f' term
    f4_[t] <- (1+Ku[t]) / ( v4[t] + FCFF[t] + ie[t]*T[t] + d4[t-1]*(Ku[t]-Kd[t])*(T[t]) ) 
    
    # Noncircular debt ratio, alternatively use LEEDA_4[t] <- d4[t-1] * f4_[t]
    LEEDA_4[t] <- (d4[t]+CFd[t]) / (1+Kd[t]) * f4_[t]
    
    # Noncircular interest expense tax shield relative to prior period asset value
    LEEIET3_4[t] <- (ie[t]*T[t]) * f4_[t]
    
    # Discounting 1-period at the forward discount rate, LEEWACC4[t] 
    LEEWACC4[t] <-  Ku[t] - LEEDA_4[t] * (Ku[t] - Kd[t])*(T[t]) - LEEIET3_4[t]  
    
    v4[t-1] <- (v4[t] + FCFF[t]) / (1 + LEEWACC4[t])
    
    # Calculate equity value for period 't-1' based on calculated asset value  
    e4[t-1] <- v4[t-1] - d4[t-1]
    
  }
  
  # LEEWACC weights (general case)
  w_2nd_term_4  <- LEEDA_4
  w_1_V4 <- f4_
  
  term_4 <- (Ku-Kd)*(T)
  
  # Weighted component costs of WACC (general case)
  weighted_2nd_term_4  <- LEEDA_4*term_4
  w_ie_T4 <- (ie*T)*f4_
  
  LEEWACC_components4 <- Ku - weighted_2nd_term_4 - w_ie_T4
  
  # WACC check
  LEEWACC4_check <-LEEWACC4 - LEEWACC_components4
  
  # Valuation Checks
  v_check4 <- v4 - asset_value[11, ]
  e_check4 <- e4 - equity_value[13, ]
  d_check4 <- d4 - debt_value[8, ]
  
  # Spot Discount Rates
  WACC4_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC4_spot[2] <- LEEWACC4[2]
  
  for (t in 3:(n+1))   
  {
    WACC4_spot[t] <- ( ( 1 + WACC4_spot[t-1] )^(t-2) * ( 1 + LEEWACC4[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using Spot discount rates  
  v4_0 <- c( sum( FCFF[2:(n+1)] / (1 + WACC4_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  v4_0 <- c(round(v4_0[1],0), rep(NaN,n) )
  
  npv4_0 <- round(v4_0[1],0) + round(FCFF[1],0)
  npv4_0 <- c(npv4_0, rep(NaN,n) )
  
  asset_value_3_term_WACC_LEEIET3 <- as_tibble( cbind(a$Year, T, a$Rf, a$Bu, a$MRP, Ku, Bd, Kd, Ku - Kd, ie, ie*T,
                                                      round(d,0), round(e,0), round(v,0), we, wd, w_1_V, we_Ke, wd_Kd, w_ie_T,
                                                      WACC_components, WACC_check, Ke, WACC, FCFF, v, v_check1,  e_check1, d_check1,
                                                      WACC_spot, v_0, npv_0, round(d2,0), round(e2,0), round(v2,0), w_2nd_term, w_1_V2,
                                                      term_2, weighted_2nd_term, w_ie_T2, WACC_components2, WACC2_check, Ke2, WACC2,
                                                      v_check2,  e_check2, d_check2, WACC2_spot, v2_0, npv2_0, CFd, round(d3,0), ECF,
                                                      round(e3,0), cum_d_e_ratio, Ku_Kd_AT, b_, f_, LEEKe, LEEDA, LEEIET3, LEEWACC3,
                                                      round(v3,0), c(0, LEEWACC3_check[2:(n+1)]), v_check3,  e_check3, d_check3, WACC3_spot,
                                                      v3_0, npv3_0,  round(d4,0), round(e4,0), f4_, weighted_2nd_term_4, LEEDA_4, LEEIET3_4,
                                                      LEEWACC4, round(v4,0), c(0, LEEWACC4_check[2:(n+1)]), v_check4,  e_check4, d_check4,
                                                      WACC4_spot, v4_0, npv4_0) ) 
  
  names(asset_value_3_term_WACC_LEEIET3) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "ie", "ie_T",
                                              "D", "E", "V", "we", "wd", "w_1_V", "we_Ke", "wd_Kd", "w_ie_T",  "WACC_components",
                                              "WACC_check", "Ke", "WACC", "FCFF",  "V.1", "v_check1",  "e_check1", "d_check1", 
                                              "WACC_spot", "v_0", "npv_0", "D2", "E2", "V2", "w_2nd_term", "w_1_V2", "term_2",
                                              "weighted_2nd_term", "w_ie_T2", "WACC_components2", "WACC2_check", "Ke2", "WACC2",
                                              "v_check2",  "e_check2", "d_check2",  "WACC2_spot", "v2_0", "npv2_0", "CFd", "D3", 
                                              "ECF", "E3", "cum_d_e_ratio", "Ku_Kd_AT","b", "f",  "LEEKe", "LEEDA", "LEEIET3", 
                                              "LEEWACC3",  "V3", "LEEWACC3_check", "v_check3",  "e_check3", "d_check3", "WACC3_spot",
                                              "v3_0", "npv3_0", "D4", "E4", "f_4", "weighted_2nd_term_4", "LEEDA_4", "LEEIET3_4",
                                              "LEEWACC4", "v4", "LEEWACC4_check", "v_check4",  "e_check4", "d_check4", "WACC4_spot",
                                              "v4_0", "npv4_0")
  
  return(rotate(asset_value_3_term_WACC_LEEIET3))
}

asset_value_3_term_WACC_LEEIET3 <- asset_value_3_term_WACC_LEEIET3( out3, out4, flows )

asset_value_3_term_WACC_LEEIET3


# Apply this R output to provided formatted Excel spreadsheet (4 total individual methods)




# 4 'General case' WACCbt Methods are included in the below R function, each with its on formatted spreadsheet representation

asset_value_2_term_WACCbt <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  T  <- a$T
  Rf  <- a$Rf
  MRP <- a$MRP
  Bu  <- a$Bu
  Ku  <- Rf + Bu * MRP
  Bd  <- a$Bd
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]
  
  # ECF is selected from the 12t row of 'flows' output
  ECF <- c[12, ] 
  
  # FCFF cash flow is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ] 
  
  # Capital cash flow is selected from the 15th row of 'flows' output or simply CCF <- ECF + CFd
  CCF <- c[15, ]      
  
  
  # 1. Circular 3-term WACCbt = f(Ke, Kd)
  
  # Asset Value by Year
  
  d_e_ratio <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  
  e_calc <- 10000 * d_e_ratio  # Initialize equity value convergence vector to $10000 each Year as a guess
  e <- e_calc                  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d <- d_e_ratio * e
  
  # V = D + E = (debt-to-equity ratio)(E) + E = (1 + debt-to-equity ratio)(E)
  
  v_calc <- (e)*(1 + d_e_ratio)      # Initialize asset value convergence vector guess
  v <- v_calc                        # Initialize asset value convergence vector guess
  
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  WACCbt <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0) / lag(v, default=0)) * (Kd)  
  
  # Calculate asset value by period in reverse order using discount rates WACCbt[t]
  
  iterations <- 100
  tolerance  <- 0.00001
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is Noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Discounting 1-period at the forward discount rate, WACCbt[t] 
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      d_v_ratio_t_1 <- d_e_ratio_t_1 / ( 1 + d_e_ratio_t_1)      # Per Appendix
      
      Ke[t]   <-  Ku[t] + d_e_ratio_t_1 * (1-T[t]) * (Ku[t] - Kd[t])
      WACCbt[t] <-  (1 - d_v_ratio_t_1) * Ke[t] + d_v_ratio_t_1 * Kd[t]  
      
      v_calc[t-1] <- ( v[t] + CCF[t] ) / ( 1 + WACCbt[t] )  
      
      # Recalculate equity value for period 't-1' based on calculated asset value and debt-equity ratio vector 
      e[t-1] <- v_calc[t-1] * (1 - d_v_ratio_t_1 )
      
    }
    
    # Determine error in iterative valuation process
    error <- sum((v - v_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v <- v_calc
  }
  
  # WACCbt weights (general case)
  we    <- ifelse(v==0,0,e/v)
  wd    <- ifelse(v==0,0,d/v)
  
  # Weighted component costs of WACCbt (general case)
  we_Ke  <- lag(we, default=0)*Ke
  wd_Kd  <- lag(wd, default=0)*Kd
  
  WACCbt_components <- we_Ke + wd_Kd
  
  # WACCbt check
  WACCbt_check <- WACCbt - WACCbt_components
  
  # Valuation Checks
  v_check1 <- v - asset_value[11, ]
  e_check1 <- e - equity_value[13, ]
  d_check1 <- d - debt_value[8, ]
  
  # Spot Discount Rates
  WACCbt_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACCbt_spot[2] <- WACCbt[2]
  
  for (t in 3:(n+1))   
  {
    WACCbt_spot[t] <- ( ( 1 + WACCbt_spot[t-1] )^(t-2) * ( 1 + WACCbt[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using Spot discount rates  
  v_0 <- c( sum( CCF[2:(n+1)] / (1 + WACCbt_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(CCF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  
  # 2. Circular 2-term WACCbt = f(Ku)
  
  # Asset Value by Year
  
  d_e_ratio2 <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  d_v_ratio2 <- d_e_ratio2 / ( 1 + d_e_ratio2)
  
  e_calc2 <- 10000 * d_e_ratio2  # Initialize equity value convergence vector to $10000 each Year as a guess
  e2 <- e_calc2                  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d2 <- d_e_ratio2 * e2
  
  # V = D + E = (debt-to-equity ratio)(E) + E = (1 + debt-to-equity ratio)(E)
  
  v_calc2 <- (e2)*(1 + d_e_ratio2)      # Initialize asset value convergence vector guess
  v2 <- v_calc2                        # Initialize asset value convergence vector guess
  
  Ke2   <-  Ku + (lag(d2, default=0) / lag(e2, default=0)) * (1-T) * (Ku - Kd)
  WACC2bt <-  Ku - d_v_ratio2*(Ku - Kd)*(T)   
  
  # Calculate asset value by period in reverse order using discount rates WACCbt[t]
  
  iterations2 <- 100
  tolerance2  <- 0.000001
  
  for (j in 1:iterations2 ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is Noncircular
      
      d2[t-1] <- (d2[t] + CFd[t]) / (1 + Kd[t])
      
      # Discounting 1-period at the forward discount rate, WACCbt[t] 
      
      # Prevent division by zero errors
      d_e_ratio_t_1_2 <- ifelse( e2[t-1] == 0, 0, d2[t-1] / e2[t-1] )
      d_v_ratio_t_1_2 <- d_e_ratio_t_1_2 / ( 1 + d_e_ratio_t_1_2)      # Per Appendix
      
      Ke2[t]   <-  Ku[t] + d_e_ratio_t_1_2 * (1-T[t]) * (Ku[t] - Kd[t])
      
      WACC2bt[t] <-  Ku[t] - d_v_ratio_t_1_2 * (Ku[t] - Kd[t])*(T[t])  
      
      v_calc2[t-1] <- ( v2[t] + CCF[t] ) / ( 1 + WACC2bt[t] )  
      
      # Recalculate equity value for period 't-1' based on calculated asset value and debt-equity ratio vector 
      e2[t-1] <- v_calc2[t-1] * (1 - d_v_ratio_t_1_2 )
      
    }
    
    # Determine error in iterative valuation process
    error2 <- sum((v2 - v_calc2)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error2 < tolerance2) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v2 <- v_calc2
  }
  
  # WACCbt  weights (general case)
  w_2nd_term  <- ifelse(v2==0,0,d2/v2)
  
  term_2 <- (Ku-Kd)*(T)
  
  # Weighted component costs of WACCbt (general case)
  weighted_2nd_term  <- lag(w_2nd_term, default=0)*term_2
  
  WACCbt_components2 <- Ku - weighted_2nd_term 
  
  # WACCbt check
  WACC2bt_check <- WACC2bt - WACCbt_components2
  
  # Valuation Checks
  v_check2 <- v2 - asset_value[11, ]
  e_check2 <- e2 - equity_value[13, ]
  d_check2 <- d2 - debt_value[8, ]
  
  # Spot Discount Rates
  WACC2bt_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC2bt_spot[2] <- WACC2bt[2]
  
  for (t in 3:(n+1))   
  {
    WACC2bt_spot[t] <- ( ( 1 + WACC2bt_spot[t-1] )^(t-2) * ( 1 + WACC2bt[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using Spot discount rates  
  v2_0 <- c( sum( CCF[2:(n+1)] / (1 + WACC2bt_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  v2_0 <- c(round(v2_0[1],0), rep(NaN,n) )
  
  npv2_0 <- round(v2_0[1],0) + round(CCF[1],0)
  npv2_0 <- c(npv2_0, rep(NaN,n) )
  
  
  # 3. Noncircular 2-term WACCbt = f(Ke, Kd) 
  
  # Asset Value by Year
  
  d_e_ratio3 <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  
  e3 <- 10000 * d_e_ratio3  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d3 <- d_e_ratio3 * e3
  
  # V = D + E = (debt-to-equity ratio)(E) + E = (1 + debt-to-equity ratio)(E)
  
  v3 <- d3 + e3        # Initialize asset value vector guess
  
  # Noncircular 'b' term
  cum_d_e_ratio <- (d3 + CFd) / (e3 + ECF)
  Ku_Kd_AT <- (d3 + CFd) / (1+Kd) * (1-T)
  b_ <- cum_d_e_ratio * Ku_Kd_AT
  
  LEEKe <- (Ku + b_) / (1-b_) 
  
  # Noncircular 'f' term
  f_ <- (1+Ku) / ( v3 + FCFF + ie*T + lag(d3,default=0)*(Ku-Kd)*(T) ) 
  
  # Noncircular debt ratio, alternatively use LEEDA <- lag(d3, default=0) * f_
  LEEDA <- (d3+CFd) / (1+Kd) * f_
  
  # Noncircular WACCbt    
  LEEWACC3bt <-  (1 - LEEDA) * (LEEKe) + (LEEDA)*(Kd)  
  
  # Calculate asset value by period in reverse order using discount rates WACCbt[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    d3[t-1] <- (d3[t] + CFd[t]) / (1 + Kd[t])
    
    # Discounting 1-period at the forward discount rate, WACCbt[t] 
    
    # Noncircular 'b' term
    # 'cum' Debt-to-Equity ratio
    cum_d_e_ratio[t] <- (d3[t] + CFd[t]) / (e3[t] + ECF[t])
    Ku_Kd_AT[t] <- ((Ku[t] - Kd[t])/(1+Kd[t])) * (1 - T[t])
    b_[t] <- cum_d_e_ratio[t] * Ku_Kd_AT[t]
    
    LEEKe[t] <- (Ku[t] + b_[t]) / (1-b_[t]) 
    
    # Noncircular 'f' term
    f_[t] <- (1+Ku[t]) / ( v3[t] + FCFF[t] + ie[t]*T[t] + d3[t-1]*(Ku[t]-Kd[t])*(T[t]) ) 
    
    # Prevent division by zero errors relative to 'f' term
    f_[t] <- ifelse(  ( v3[t] + FCFF[t] + ie[t]*T[t] + d3[t-1]*(Ku[t]-Kd[t])*(T[t]) ) == 0, 0, f_[t] )
    
    # Noncircular debt ratio, alternatively use LEEDA[t] <- d3[t-1] * f_[t]
    LEEDA[t] <- (d3[t]+CFd[t]) / (1+Kd[t]) * f_[t]
    
    # Noncircular WACCbt    
    LEEWACC3bt[t] <-  (1 - LEEDA[t])*LEEKe[t] + LEEDA[t]*(Kd[t])  
    
    v3[t-1] <- ( v3[t] + CCF[t] ) / ( 1 + LEEWACC3bt[t] )  
    
    e3[t-1] <- v3[t-1] - d3[t-1]
    
  }
  
  # WACCbt check
  LEEWACC3bt_check <- LEEWACC3bt - WACCbt_components2
  
  # Valuation Checks
  v_check3 <- v3 - asset_value[11, ]
  e_check3 <- e3 - equity_value[13, ]
  d_check3 <- d3 - debt_value[8, ]
  
  # Spot Discount Rates
  WACC3bt_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC3bt_spot[2] <- LEEWACC3bt[2]
  
  for (t in 3:(n+1))   
  {
    WACC3bt_spot[t] <- ( ( 1 + WACC3bt_spot[t-1] )^(t-2) * ( 1 + LEEWACC3bt[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using Spot discount rates  
  v3_0 <- c( sum( CCF[2:(n+1)] / (1 + WACC3bt_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  v3_0 <- c(round(v3_0[1],0), rep(NaN,n) )
  
  npv3_0 <- round(v3_0[1],0) + round(CCF[1],0)
  npv3_0 <- c(npv3_0, rep(NaN,n) )
  
  
  # 4. Noncircular 2-term WACCbt = f(Ku)
  # WACCbt = Ku - (LEEDA)(Ku-Kd)(T) 
  
  # Asset Value by Year
  
  d4 <- rep(0, n+1)  # Initialize debt value to $0 each year
  
  e4 <- d4           # Initialize equity value to $0 each year
  
  v4 <- e4 + d4      # Initialize asset value to $0 each year
  
  # Noncircular 'f' term
  f4_ <-  (1+Ku) / ( v4 + FCFF + ie*T + (lag(d4,default=0)*(Ku-Kd)*(T) ) )  
  
  # Noncircular debt ratio, alternatively use LEEDA_4[t] <- lag(d4,default=0) * f4_
  LEEDA_4 <- (d4+CFd) / (1+Kd) * f4_
  
  # Noncircular WACCbt    
  LEEWACC4bt <-  Ku - LEEDA_4 * (Ku - Kd) * (T)  
  
  # Calculate asset value by period in reverse order using discount rates LEEWACC4bt[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    
    d4[t-1] <- (d4[t] + CFd[t]) / (1 + Kd[t])
    
    # Noncircular 'f' term
    f4_[t] <- (1+Ku[t]) / ( v4[t] + FCFF[t] + ie[t]*T[t] + d4[t-1]*(Ku[t]-Kd[t])*(T[t]) ) 
    
    # Noncircular debt ratio, alternatively use LEEDA_4[t] <- d4[t-1] * f4_[t]
    LEEDA_4[t] <- (d4[t]+CFd[t]) / (1+Kd[t]) * f4_[t]
    
    # Discounting 1-period at the forward discount rate, LEEWACCbt[t] 
    LEEWACC4bt[t] <-  Ku[t] - LEEDA_4[t] * (Ku[t] - Kd[t])*(T[t])  
    
    v4[t-1] <- (v4[t] + CCF[t]) / (1 + LEEWACC4bt[t])
    
    # Calculate equity value for period 't-1' based on calculated asset value  
    e4[t-1] <- v4[t-1] - d4[t-1]
    
  }
  
  # LEEWACCbt market weights (general case)
  w_2nd_term_4  <- LEEDA_4
  
  term_4 <- (Ku-Kd)*(T)
  
  # Weighted component costs of WACCbt (general case)
  weighted_2nd_term_4  <- LEEDA_4*term_4
  
  LEEWACCbt_components4 <- Ku - weighted_2nd_term_4 
  
  # WACCbt check
  LEEWACC4bt_check <-LEEWACC4bt - LEEWACCbt_components4
  
  # Valuation Checks
  v_check4 <- v4 - asset_value[11, ]
  e_check4 <- e4 - equity_value[13, ]
  d_check4 <- d4 - debt_value[8, ]
  
  # Spot Discount Rates
  WACC4bt_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC4bt_spot[2] <- LEEWACC4bt[2]
  
  for (t in 3:(n+1))   
  {
    WACC4bt_spot[t] <- ( ( 1 + WACC4bt_spot[t-1] )^(t-2) * ( 1 + LEEWACC4bt[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using Spot discount rates  
  v4_0 <- c( sum( CCF[2:(n+1)] / (1 + WACC4bt_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  v4_0 <- c(round(v4_0[1],0), rep(NaN,n) )
  
  npv4_0 <- round(v4_0[1],0) + round(CCF[1],0)
  npv4_0 <- c(npv4_0, rep(NaN,n) )
  
  asset_value_2_term_WACCbt <- as_tibble( cbind(a$Year, T, a$Rf, a$Bu, a$MRP, Ku, Bd, Kd, Ku - Kd, ie, ie*T,
                                                round(d,0), round(e,0), round(v,0), we, wd, we_Ke, wd_Kd, WACCbt_components,
                                                WACCbt_check, Ke, WACCbt, FCFF, CCF, v, v_check1,  e_check1, d_check1,
                                                WACCbt_spot, v_0, npv_0, round(d2,0), round(e2,0), round(v2,0), w_2nd_term,
                                                term_2, weighted_2nd_term, WACCbt_components2, WACC2bt_check, Ke2, WACC2bt,
                                                v_check2,  e_check2, d_check2, WACC2bt_spot, v2_0, npv2_0, CFd, round(d3,0),
                                                ECF, round(e3,0), cum_d_e_ratio, Ku_Kd_AT, b_, f_, LEEKe, LEEDA, LEEWACC3bt,
                                                round(v3,0), c(0, LEEWACC3bt_check[2:(n+1)]), v_check3, e_check3, d_check3,
                                                WACC3bt_spot, v3_0, npv3_0, round(d4,0), round(e4,0), f4_,
                                                weighted_2nd_term_4, LEEDA_4, LEEWACC4bt, round(v4,0),
                                                c(0, LEEWACC4bt_check[2:(n+1)]), v_check4,  e_check4, d_check4,
                                                WACC4bt_spot, v4_0, npv4_0) ) 
  
  names(asset_value_2_term_WACCbt) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "ie", "ie_T",
                                        "D", "E", "V", "we", "wd",  "we_Ke", "wd_Kd",  "WACCbt_components", "WACCbt_check",
                                        "Ke", "WACCbt", "FCFF", "CCF",  "V.1", "v_check1",  "e_check1", "d_check1",
                                        "WACCbt_spot", "v_0", "npv_0", "D2", "E2", "V2", "w_2nd_term",  "term_2",
                                        "weighted_2nd_term",  "WACCbt_components2", "WACC2bt_check", "Ke2", "WACC2bt",
                                        "v_check2",  "e_check2", "d_check2", "WACC2bt_spot", "v2_0", "npv2_0", "CFd", 
                                        "D3", "ECF", "E3", "cum_d_e_ratio", "Ku_Kd_AT", "b", "f",  "LEEKe", "LEEDA",
                                        "LEEWACC3bt",  "V3", "LEEWACC3bt_check", "v_check3",  "e_check3",
                                        "d_check3", "WACC3bt_spot", "v3_0", "npv3_0", "D4", "E4", "f_4",
                                        "weighted_2nd_term_4", "LEEDA_4", "LEEWACC4bt", "v4", "LEEWACC4bt_check",
                                        "v_check4",  "e_check4", "d_check4", "WACC4bt_spot", "v4_0", "npv4_0")
  
  return(rotate(asset_value_2_term_WACCbt))
}

asset_value_2_term_WACCbt <- asset_value_2_term_WACCbt( out3, out4, flows )

asset_value_2_term_WACCbt


# Apply this R output to provided formatted Excel spreadsheet (4 total individual methods)



asset_value_apv_3 <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  ie_T <- ie*T
  
  PV_Ku_Kd_T <- ((Ku - Kd) / (1 + Kd)) *(T)
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]      
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]      
  
  # Initialize 'cum' debt-to-'tax shield' ratio to 1.0
  cum_d_ts_ratio <- rep(1, n+1)
  
  # Initialize debt value = 0
  d <- rep(0, n+1)
  
  # v_u = value of unlevered project
  v_u <-  rep(0, n+1)
  v_u2 <- rep(0, n+1)
  
  # dvts = value of interest expense tax shields
  dvts <-  rep(0, n+1)
  dvts2 <- rep(0, n+1)
  
  # e discount term in LEEShield
  LEEShield <- rep(0, n+1)
  
  # Initialize vector for LEEShield determination
  e_disc <- Ku
  
  # Calculate asset value by period in reverse order using discount rates Ku[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    
    # Debt value
    d[t-1] <- ( d[t] + CFd[t] ) / ( 1 + Kd[t] ) 
    
    # DVTS Method 1
    # Discounting all numerator terms 1-period at the forward discount rate, Ku[t]
    
    v_u[t-1] <- ( v_u[t] + FCFF[t] ) / ( 1 + Ku[t] )  
    
    dvts[t-1] <- ( dvts[t] + ie_T[t] +  (Ku[t] - Kd[t])*d[t-1]*T[t] ) / ( 1 + Ku[t] )  
    
    # DVTS Method 2
    # Discounting interest tax shields (IE)(T) directly @ LEEShield and FCFF of unlevered firm at the forward discount rate, Ku[t]
    
    v_u2[t-1] <- ( v_u2[t] + FCFF[t] ) / ( 1 + Ku[t] )  
    
    cum_d_ts_ratio[t] <- ( d[t] + CFd[t] )  / (dvts2[t] +  ie_T[t] )
    
    e_disc[t] <-  cum_d_ts_ratio[t] * PV_Ku_Kd_T[t]
    
    LEEShield[t] <-  ( Ku[t] - e_disc[t])  / (1 + e_disc[t])
    
    dvts2[t-1] <- ( dvts2[t] +  ie_T[t] ) / ( 1 + LEEShield[t] )  
    
  }
  
  # Value of project = sum of 2 components = v_u + dvts
  apv  <- v_u  + dvts
  apv2 <- v_u2 + dvts2
  
  dvts_check <- round(dvts,0) - round(dvts2,0)
  apv_check  <- round(apv,0)  - round(apv2,0)
  
  # Spot discount rates by Year
  
  LEEShield_spot <- rep(0, n+1 )  # Initialize spot rate to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  LEEShield_spot[2] <- LEEShield[2]
  
  for (t in 3:(n+1))   
  {
    LEEShield_spot[t] <- ( ( 1 + LEEShield_spot[t-1] )^(t-2) * ( 1 + LEEShield[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # DVTS Value at t=0 using spot discount rates  
  dvts_0 <- c( sum( ie_T[2:(n+1)] / (1 + LEEShield_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_u2_0 <- v_u2[1]
  
  apv_0 <- v_u2_0 + dvts_0  
  npv_0 <- round(apv_0, 0) + FCFF[1]
  
  v_u2_0 <- c(round(v_u2_0[1],0), rep(NaN,n) )
  dvts_0 <- c(round(dvts_0[1],0), rep(NaN,n) )
  apv_0 <- c(round(apv_0[1],0), rep(NaN,n) )
  npv_0 <- c(round(npv_0[1],0), rep(NaN,n) )
  
  # The 11th row of 'asset_value_circ' output = asset_value_circ[11, ] is used as a check
  
  asset_value_apv_3 <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd, Kd, Ku - Kd,  PV_Ku_Kd_T, round(d,0), ie,
                                        ie_T, (Ku - Kd)*lag(d, default=0)*T , ie*T + (Ku - Kd)*lag(d, default=0)*T , CFd, FCFF,
                                        FCFF + ie*T + lag(d, default=0)*(Ku - Kd)*T, round(v_u,0), round(v_u2,0),
                                        round(dvts,0), round(dvts2,0), cum_d_ts_ratio, e_disc, LEEShield, apv, apv2, apv_0,
                                        LEEShield_spot, v_u2_0, dvts_0, apv_0, FCFF[1], npv_0,
                                        round(apv,0) - round(asset_value_circ[11, ],0), dvts_check, apv_check ) ) 
  
  names(asset_value_apv_3) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "PV_Ku_Kd_T","D", "ie", "ie_T", 
                                "ie_adj", "ADJ_ie", "CFd", "FCFF", "ADJ_FCFF", "V_u", "V_u2", "DVTS", "DVTS2", "cum_d_ts_ratio",
                                "e_disc", "LEEShield", "APV", "APV2", "apv_0", "LEEShield_spot", "Vu2_0", "DVTS_0", "APV_0",
                                "FCFF_0", "NPV_0", "V_check", "dvts_check", "apv_check" )
  
  return(rotate(asset_value_apv_3))
}

asset_value_apv_3 <- asset_value_apv_3( out3, out4, flows )

round(asset_value_apv_3,5)

# Apply this R output to provided formatted Excel spreadsheet 




asset_value_circ_2 <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  T  <- a$T
  Rf  <- a$Rf
  MRP <- a$MRP
  Bu  <- a$Bu
  Ku  <- Rf + Bu * MRP
  Bd  <- a$Bd
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]
  
  # Free cash flow is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # Asset Value by Year
  
  d_e_ratio <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  
  e_calc <- 10000 * d_e_ratio  # Initialize equity value convergence vector to $10000 each Year as a guess
  e <- e_calc                  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d <- d_e_ratio * e
  
  # V = D + E = (debt-to-equity ratio)(E) + E = (1 + debt-to-equity ratio)(E)
  
  v_calc <- (e)*(1 + d_e_ratio)      # Initialize asset value convergence vector guess
  v <- v_calc                        # Initialize asset value convergence vector guess
  
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  WACC <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0) 
                                                                 / lag(v, default=0)) * (Kd) - (ie*T) / lag(v, default=0)  
  
  # Calculate asset value by period in reverse order 
  
  iterations <- 100
  tolerance  <- 0.00001
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is Noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      d_v_ratio_t_1 <- d_e_ratio_t_1 / ( 1 + d_e_ratio_t_1)      # Per Appendix
      
      # Not discounting at WACC, Rather WACC is part of a FCFF numerator adjustment
      Ke[t]   <-  Ku[t] + d_e_ratio_t_1 * (1-T[t]) * (Ku[t] - Kd[t])
      WACC[t] <-  (1 - d_v_ratio_t_1) * Ke[t] + d_v_ratio_t_1 * Kd[t] - (ie[t]*T[t]) / v[t-1]  
      
      # Discounting 1-period at the forward discount rate, Ku[t] 
      
      v_calc[t-1] <- ( v[t] + FCFF[t] + v[t-1]*(Ku[t] - WACC[t]) ) / ( 1 + Ku[t] )  
      
      # Recalculate equity value for period 't-1' based on calculated asset value and debt-equity ratio vector 
      e[t-1] <- v_calc[t-1] * (1 - d_v_ratio_t_1 )
      
    }
    # Determine error in iterative valuation process
    error <- sum((v - v_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v <- v_calc
  }
  
  # Ku_spot discount rates by Year
  
  Ku_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ku_spot[2] <- Ku[2]
  
  for (t in 3:(n+1))   
  {
    Ku_spot[t] <- ( ( 1 + Ku_spot[t-1] )^(t-2) * ( 1 + Ku[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End Ku_spot discount rates by Year
  
  FCFF_risk_adj <- lag(v, default=0)*(Ku - WACC)
  Risk_ADJ_FCFF <- FCFF + FCFF_risk_adj   
  
  FCFF_risk_adj[1] <- 0
  Risk_ADJ_FCFF[1] <- FCFF[1]   
  
  # Asset Value at t=0 using Ku_spot discount rates  
  v_0 <- c( sum( Risk_ADJ_FCFF[2:(n+1)] / (1 + Ku_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(Risk_ADJ_FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 13th row of 'asset_value_apv' output = asset_value_apv[13, ] is used as a check
  
  asset_value_circ_2 <- as_tibble( cbind(a$Year, a$Rf, a$Bu, a$MRP, Ku, WACC, Ku - WACC,
                                         d, v - d, v,  FCFF, FCFF_risk_adj, Risk_ADJ_FCFF, Ku_spot, v_0,
                                         c( Risk_ADJ_FCFF[1], rep(NaN,n)), npv_0,
                                         round(v,0) - round( asset_value_apv[13, ],0) ) ) 
  
  names(asset_value_circ_2) <- c("Year", "Rf", "Bu", "MRP", "Ku", "WACC", "Ku_WACC",
                                 "D", "E", "V", "FCFF", "FCFF_risk_adj",  "Risk_ADJ_FCFF" ,"Ku_spot", "V_0",
                                 "Risk_ADJ_FCFF_0", "NPV_0", "V_check")
  
  return(rotate(asset_value_circ_2))
}

asset_value_circ_2 <- asset_value_circ_2( out3, out4, flows )

round(asset_value_circ_2,5)


# Apply this R output to provided formatted Excel spreadsheet 




asset_value_CE_Rf <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  T  <- a$T
  Rf  <- a$Rf
  MRP <- a$MRP
  Bu  <- a$Bu
  Ku  <- Rf + Bu * MRP
  Bd  <- a$Bd
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]
  
  # Free cash flow is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # Asset Value by Year
  
  d_e_ratio <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  
  e_calc <- 10000 * d_e_ratio  # Initialize equity value convergence vector to $10000 each Year as a guess
  e <- e_calc                  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d <- d_e_ratio * e
  
  # V = D + E = (debt-to-equity ratio)(E) + E = (1 + debt-to-equity ratio)(E)
  
  v_calc <- (e)*(1 + d_e_ratio)      # Initialize asset value convergence vector guess
  v <- v_calc                        # Initialize asset value convergence vector guess
  
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  WACC <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0)
                                                                 / lag(v, default=0)) * (Kd) - (ie*T) / lag(v, default=0)  
  
  # Calculate asset value by period in reverse order using discount rates Rf[t]
  
  iterations <- 100
  tolerance  <- 0.00001
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is Noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      d_v_ratio_t_1 <- d_e_ratio_t_1 / ( 1 + d_e_ratio_t_1)      # Per Appendix
      
      # Not discounting at WACC, Rather WACC is part of a FCFF numerator adjustment
      Ke[t]   <-  Ku[t] + d_e_ratio_t_1 * (1-T[t]) * (Ku[t] - Kd[t])
      WACC[t] <-  (1 - d_v_ratio_t_1) * Ke[t] + d_v_ratio_t_1 * Kd[t] - (ie[t]*T[t]) / v[t-1]  
      
      # Discounting 1-period at the forward discount rate, Rf[t] 
      
      v_calc[t-1] <- ( v[t] + FCFF[t] + v[t-1]*(Rf[t] - WACC[t]) ) / ( 1 + Rf[t] )  
      
      # Recalculate equity value for period 't-1' based on calculated asset value and debt-equity ratio vector 
      e[t-1] <- v_calc[t-1] * (1 - d_v_ratio_t_1 )
      
    }
    # Determine error in iterative valuation process
    error <- sum((v - v_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v <- v_calc
  }
  
  # spot discount rates by Year
  
  Rf_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Rf_spot[2] <- Rf[2]
  
  for (t in 3:(n+1))   
  {
    Rf_spot[t] <- ( ( 1 + Rf_spot[t-1] )^(t-2) * ( 1 + Rf[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  FCFF_adj <- lag(v, default=0)*(Rf - WACC)
  ADJ_FCFF <- FCFF + FCFF_adj   
  
  FCFF_adj[1] <- 0
  ADJ_FCFF[1] <- FCFF[1]   
  
  # Value at t=0 using spot discount rates  
  v_0 <- c( sum( ADJ_FCFF[2:(n+1)] / (1 + Rf_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(ADJ_FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 13th row of 'asset_value_apv' output = asset_value_apv[13, ] is used as a check
  
  asset_value_CE_Rf <- as_tibble( cbind(a$Year, a$Rf, a$Bu, a$MRP, Ku, WACC, Rf - WACC,
                                        d, v - d, v,  FCFF, FCFF_adj, ADJ_FCFF, Rf_spot, v_0,
                                        c( ADJ_FCFF[1], rep(NaN,n)), npv_0, 
                                        round(v,0) - round( asset_value_apv[13, ],0) ) ) 
  
  names(asset_value_CE_Rf) <- c("Year", "Rf", "Bu", "MRP", "Ku", "WACC", "Rf_WACC",
                                "D", "E", "V", "FCFF", "FCFF_adj",  "ADJ_FCFF" ,"Rf_spot", "V_0",
                                "ADJ_FCFF_0", "NPV_0", "V_check")
  
  return(rotate(asset_value_CE_Rf))
}

asset_value_CE_Rf <- asset_value_CE_Rf( out3, out4, flows )

round(asset_value_CE_Rf,5)



# Apply this R output to provided formatted Excel spreadsheet 




equity_value_CE_Rf <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  T  <- a$T
  Rf  <- a$Rf
  MRP <- a$MRP
  Bu  <- a$Bu
  Ku  <- Rf + Bu * MRP
  Bd  <- a$Bd
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]
  
  # Equity cash flow is selected from the 12th row of 'flows' output
  ECF <- c[12, ]        
  
  d_e_ratio <- c( rep(1.0, n), 0)  # Initialize debt-to-equity ratio convergence vector to 1.0 
  
  e_calc <- 10000 * d_e_ratio  # Initialize equity value convergence vector to $10000 each Year as a guess
  e <- e_calc                  # Initialize equity value convergence vector to $10000 each Year as a guess
  
  d <- d_e_ratio * e
  
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  
  # Calculate equity value by period in reverse order using discount rates Rf[t]
  
  iterations <- 100
  tolerance  <- 0.000001
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is Noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      d_v_ratio_t_1 <- d_e_ratio_t_1 / ( 1 + d_e_ratio_t_1)      # Per Appendix
      
      # Not discounting at Ke, Rather Ke is part of a ECF numerator adjustment
      Ke[t]   <-  Ku[t] + d_e_ratio_t_1 * (1-T[t]) * (Ku[t] - Kd[t])
      
      # Discounting 1-period at the forward discount rate, Rf[t] 
      
      e_calc[t-1] <- ( e[t] + ECF[t] + e[t-1]*(Rf[t] - Ke[t]) ) / ( 1 + Rf[t] )  
      
    }
    # Determine error in iterative valuation process
    error <- sum((e - e_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    e <- e_calc
  }
  
  # Asset value
  
  v <- e + d
  
  # spot discount rates by Year
  
  Rf_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Rf_spot[2] <- Rf[2]
  
  for (t in 3:(n+1))   
  {
    Rf_spot[t] <- ( ( 1 + Rf_spot[t-1] )^(t-2) * ( 1 + Rf[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  ECF_adj <- lag(e, default=0)*(Rf - Ke)
  ADJ_ECF <- ECF + ECF_adj   
  
  ECF_adj[1] <- 0
  ADJ_ECF[1] <- ECF[1]   
  
  # Value at t=0 using spot discount rates  
  e_0 <- c( sum( ADJ_ECF[2:(n+1)] / (1 + Rf_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(ADJ_ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # The 13th row of 'equity_value' output = equity_value_[13, ] is used as a check
  
  equity_value_CE_Rf <- as_tibble( cbind(a$Year, a$Rf, a$Bu, a$MRP, Ku, Ke, Rf - Ke,
                                         d, e, v,  ECF, ECF_adj, ADJ_ECF, Rf_spot, e_0,
                                         c( ADJ_ECF[1], rep(NaN,n)), npv_0, 
                                         round(e,0) - round( equity_value[13, ],0) ) ) 
  
  names(equity_value_CE_Rf) <- c("Year", "Rf", "Bu", "MRP", "Ku", "Ke", "Rf_Ke",
                                 "D", "E", "V", "ECF", "ECF_adj",  "ADJ_ECF" ,"Rf_spot", "E_0",
                                 "ADJ_ECF_0", "NPV_0", "e_check")
  
  return(rotate(equity_value_CE_Rf))
}

equity_value_CE_Rf <- equity_value_CE_Rf( out3, out4, flows )

round(equity_value_CE_Rf,5)



# Apply this R output to provided formatted Excel spreadsheet 



debt_value_CE_Rf <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  T  <- a$T
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Kd  <- Rf + Bd * MRP
  ie <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]
  
  # Debt Value by Year
  
  d_calc <- c( rep(1.0, n), 0)  # Initialize debt value convergence vector to $10000 each Year as a guess
  d <- d_calc                  
  
  # Calculate debt value by period in reverse order using discount rates Rf[t]
  
  iterations <- 100
  tolerance  <- 0.000011
  
  for (j in 1:iterations ) {
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      
      # Discounting 1-period at the forward discount rate, Rf[t] 
      
      d_calc[t-1] <- ( d[t] + CFd[t] + d[t-1]*(Rf[t] - Kd[t]) ) / ( 1 + Rf[t] )  
      
    }
    # Determine error in iterative valuation process
    error <- sum((d - d_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    d <- d_calc
  }
  
  # spot discount rates by Year
  
  Rf_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Rf_spot[2] <- Rf[2]
  
  for (t in 3:(n+1))   
  {
    Rf_spot[t] <- ( ( 1 + Rf_spot[t-1] )^(t-2) * ( 1 + Rf[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  CFd_adj <- lag(d, default=0)*(Rf - Kd)
  ADJ_CFd <- CFd + CFd_adj   
  
  CFd_adj[1] <- 0
  ADJ_CFd[1] <- CFd[1]   
  
  # Value at t=0 using spot discount rates  
  d_0 <- c( sum( ADJ_CFd[2:(n+1)] / (1 + Rf_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  d_0 <- c(round(d_0[1],0), rep(NaN,n) )
  
  # The 8th row of 'debt_value' output = check values
  
  debt_value_CE_Rf <- as_tibble( cbind(a$Year, a$Rf, a$Bu, a$MRP, Kd, Rf - Kd,
                                       d,  CFd, CFd_adj, ADJ_CFd, Rf_spot, d_0,
                                       c( ADJ_CFd[1], rep(NaN,n)),  round(d,0) - round( debt_value[8, ],0) ) ) 
  
  names(debt_value_CE_Rf) <- c("Year", "Rf", "Bu", "MRP", "Kd", "Rf_Kd",
                               "D",  "CFd", "CFd_adj",  "ADJ_CFd" ,"Rf_spot", "D_0",
                               "ADJ_CFd_0", "d_check")
  
  return(rotate(debt_value_CE_Rf))
}

debt_value_CE_Rf <- debt_value_CE_Rf( out3, out4, flows )

round(debt_value_CE_Rf,5)


# Apply this R output to provided formatted Excel spreadsheet 




debt_value_Rf <- function(a, f, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # Initialize debt value vector to zero
  d <- rep(0, n+1)
  
  # Initialize CFd adjustment vector to zero
  CFd_Adj <- rep(0, n+1)
  
  # Calculate Debt value by period in reverse order using discount rates Rf[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Adjustment to Flow = CFd for discounting @ Rf
    CFd_Adj[t] <-  ( d[t] + CFd[t] ) * ( Rf[t] - Kd[t] ) / ( 1 + Kd[t] ) 
    
    # Discounting 'adjusted CFd' value @ Rf
    d[t-1] <- ( d[t] + CFd[t]+ CFd_Adj[t] ) / ( 1 + Rf[t] ) 
  }
  
  Adj_CFd <- CFd + CFd_Adj 
  
  # Insert 'Year 0' CFd into the 'Adjusted CFd vector'
  Adj_CFd[1] <- CFd[1]
  
  # Rf_spot discount rates by Year
  
  Rf_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Rf_spot[2] <- Rf[2]
  
  for (t in 3:(n+1))   
  {
    Rf_spot[t] <- ( ( 1 + Rf_spot[t-1] )^(t-2) * ( 1 + Rf[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Debt Value at t=0 using Rf_spot discount rates  
  d_0 <- c( sum( Adj_CFd[2:(n+1)] / (1 + Rf_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  d_0 <- c(round(d_0[1],0), rep(NaN,n) )
  
  # Checks
  d_check <- debt_value[8, ]
  
  debt_value_Rf <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Rf - Kd,CFd, round(d + CFd,0),
                                    round(d,0), Rf_spot, d_0, round(d,0) - round(d_check,0), round(Adj_CFd,0),
                                    round(CFd_Adj,0)  ) )
  
  names(debt_value_Rf) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd",  "Rf_Kd", "CFd", "cum_Debt_value",
                            "d", "Rf_spot", "D_0", "d_check",   "Adj_CFd", "CFd_Adj")
  
  return(rotate(debt_value_Rf))
}

debt_value_Rf <- debt_value_Rf( out3, out4, flows )

debt_value_Rf



# Apply this R output to provided formatted Excel spreadsheet



equity_value_Rf_rate_LEEKe_adj <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie  <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # ECF is selected from the 12th row of 'flows' output
  ECF <- c[12, ]        
  
  # Initialize debt value and equity value vectors to zero
  d <- rep(0, n+1)
  e <- rep(0, n+1)
  e2 <- rep(0, n+1) # for discounting adjusted numerator flow directly
  
  # Initialize adjusted ECF vectors to zero
  ECF_Adj <- rep(0, n+1)
  Adjusted_ECF <- rep(0, n+1)
  
  # Initialize Ku adjustment 'b' vector to zero
  # LEEKe = (Ku + b) / (1-b)
  b <- rep(0, n+1)
  
  # Initialize LEEKe adjustment factor to vector to 1.0
  Rf_LEEKe_adj_factor <- rep(1, n+1)
  
  # Initialize 'cum' debt-to-equity vector to 1.0
  cum_d_e_ratio <- rep(1, n+1)
  
  Ku_Kd_AT <- (Ku - Kd)/(1 + Kd)*(1-T)
  
  # Initialize Noncircular LEEKe discount rate = Ku
  LEEKe <- Ku
  
  # Calculate Equity value by period in reverse order using discount rates LEEKe[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular; 'b' term in LEEke uses 'cum' D/E ratio = (d + CFd) / (e + ECF)
    d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
    
    cum_d_e_ratio[t] <- ( d[t] + CFd[t] ) / (e[t] + ECF[t] )
    
    # Noncircular 'b' term in LEEKe calculation
    b[t] <-   cum_d_e_ratio[t] * Ku_Kd_AT[t] 
    
    # LEEKe calculation is Noncircular
    LEEKe[t] <-   (Ku[t] + b[t]) / (1 - b[t]) 
    
    Rf_LEEKe_adj_factor[t] <- (1 + ( Rf[t] - LEEKe[t] ) / ( 1 + LEEKe[t] ) )  
    
    # Discounting adjusted 'cum' value @ Rf
    e[t-1] <- ( e[t] + ECF[t] ) / ( 1 + Rf[t] ) *  Rf_LEEKe_adj_factor[t]  
    
    # Adjustment to Flow = ECF for discounting @ Rf
    ECF_Adj[t] <- ( e2[t] + ECF[t] ) * ( Rf[t] - LEEKe[t] ) / ( 1 + LEEKe[t] ) 
    Adjusted_ECF[t] <- ECF[t] + ECF_Adj[t] 
    
    # Discounting 'adjusted ECF' value @ Rf
    e2[t-1] <- ( e2[t] + Adjusted_ECF[t] ) / ( 1 + Rf[t] ) 
  }
  
  # Insert 'Year 0' ECF into the 'Adjusted ECF vector'
  Adjusted_ECF[1] <- ECF[1]
  
  # Calculate Ke using circular formula
  
  Ke <- Ku +  (lag(d, default=0) / lag(e, default=0))  * (Ku - Kd) * (1 - T)  
  
  # Noncircular asset valuation  
  v <- e + d
  
  # Ke_spot discount rates by Year
  
  Ke_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ke_spot[2] <- Ke[2]
  
  for (t in 3:(n+1))   
  {
    Ke_spot[t] <- ( ( 1 + Ke_spot[t-1] )^(t-2) * ( 1 + Ke[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End Ke_spot discount rates by Year
  
  # Equity Value at t=0 using Ke_spot discount rates  
  e_0 <- c( sum( ECF[2:(n+1)] / (1 + Ke_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  equity_value_Rf_rate_LEEKe_adj <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd,
                                                     CFd, round(d,0), ECF, e, cum_d_e_ratio, Ku_Kd_AT, b, LEEKe,
                                                     Rf_LEEKe_adj_factor, v, Ke, Ke_spot, e_0, 
                                                     round(v,0) - round(v_check,0), round(e,0) - round(e_check,0),
                                                     round(d,0) - round(d_check,0), 
                                                     round(c(0, LEEKe[2:(n+1)] - Ke[2:(n+1)]),5), npv_0,
                                                     round(ECF_Adj,0), round(Adjusted_ECF,0), round(e2,0),
                                                     round(e2,0) - round(e_check,0)   ) )
  
  names(equity_value_Rf_rate_LEEKe_adj) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd", 
                                             "D", "ECF", "E", "cum_d_e_ratio", "Ku_Kd_AT", "b", "LEEKe",
                                             "Rf_LEEKe_adj_factor", "V", "Ke",  "Ke_spot", "E_0", "v_check", 
                                             "e_check", "d_check", "Ke_check", "NPV_0", "ECF_Adj", "Adjusted_ECF",
                                             "e2", "e2_check" )
  
  return(rotate(equity_value_Rf_rate_LEEKe_adj))
}

equity_value_Rf_rate_LEEKe_adj <- equity_value_Rf_rate_LEEKe_adj( out3, out4, flows )

round(equity_value_Rf_rate_LEEKe_adj,5)


# Apply this R output to provided formatted Excel spreadsheet



asset_value_Rf_rate_LEEWACC_adj <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie  <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ]        
  
  # Initialize debt value and asset value vectors to zero
  d <- rep(0, n+1)
  v <- rep(0, n+1)
  v2 <- rep(0, n+1) # for discounting adjusted numerator flow directly
  
  # Initialize adjusted FCFF vectors to zero
  FCFF_Adj <- rep(0, n+1)
  Adjusted_FCFF <- rep(0, n+1)
  
  # Initialize Ku adjustment 'c' vector to zero
  # LEEWACC = (Ku - c) / (1+c)
  c <- rep(0, n+1)
  
  # Initialize LEEWACC adjustment factor to vector to 1.0
  Rf_LEEWACC_adj_factor <- rep(1, n+1)
  
  # Initialize 'cum' debt ratio vector to 50% each period
  cum_d_v_ratio <- rep(0.5, n+1)
  
  pv_Ku_Kd <- (Ku - Kd)/(1 + Kd)
  Ku_Kd_AT <- (Ku - Kd)/(1 + Kd)*(1-T)
  
  # Initialize Noncircular LEEWACC discount rate = Ku
  LEEWACC <- Ku
  
  # Calculate Asset value by period in reverse order using discount rates LEEWACC[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is Noncircular
    d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
    
    cum_d_v_ratio[t] <- ( d[t] + CFd[t] ) / (v[t] + FCFF[t] )
    
    # Noncircular 'c' term is LEEWACC calculation
    c[t] <-  ( cum_d_v_ratio[t] * pv_Ku_Kd[t] + ie[t]/( v[t] + FCFF[t] ) )*(T[t])
    
    # LEEWACC calculation is Noncircular
    LEEWACC[t] <-   (Ku[t] - c[t]) / (1 + c[t]) 
    
    Rf_LEEWACC_adj_factor[t] <- (1 + ( Rf[t] - LEEWACC[t] ) / ( 1 + LEEWACC[t] ) )  
    
    # Discounting FCFF 1-period at the forward discount rate, LEEWACC[t]
    v[t-1] <- ( v[t] + FCFF[t] ) / ( 1 + Rf[t] ) *  Rf_LEEWACC_adj_factor[t] 
    
    # Adjustment to Flow = FCFF for discounting @ Rf
    FCFF_Adj[t] <- ( v2[t] + FCFF[t] ) * ( Rf[t] - LEEWACC[t] ) / ( 1 + LEEWACC[t] ) 
    Adjusted_FCFF[t] <- FCFF[t] + FCFF_Adj[t] 
    
    # Discounting 'adjusted FCFF' value @ Rf
    v2[t-1] <- ( v2[t] + Adjusted_FCFF[t] ) / ( 1 + Rf[t] ) 
    
  }
  
  # Insert 'Year 0' FCFF into the 'Adjusted FCFF vector'
  Adjusted_FCFF[1] <- FCFF[1]
  
  # Numerator Flow  adjustment factor
  Numerator_Flow <- v + FCFF 
  Flow_adj <- Numerator_Flow * Rf_LEEWACC_adj_factor
  Adjusted_Flow <- Numerator_Flow * Flow_adj
  
  # Noncircular equity valuation  
  e <- v - d
  
  # Circular WACC and Ke calculations
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  
  # Special case where Kd = r_debt
  WACC <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0) / lag(v, default=0)) * (Kd) * (1-T) 
  
  # General Case where Kd<>r_debt
  WACC_gen <- Ku - (lag(d, default=0) / lag(v, default=0)) * (Ku - Kd)*(T) - ie/lag(v, default=0)*(T)
  
  WACC_diff <- WACC_gen - WACC
  
  # LEEWACC_spot discount rates by Year
  
  LEEWACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  LEEWACC_spot[2] <- LEEWACC[2]
  
  for (t in 3:(n+1))   
  {
    LEEWACC_spot[t] <- ( ( 1 + LEEWACC_spot[t-1] )^(t-2) * ( 1 + LEEWACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using LEEWACC__spot discount rates  
  v_0 <- c( sum( FCFF[2:(n+1)] / (1 + LEEWACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  asset_value_Rf_rate_LEEWACC_adj <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd, round(d,0),
                                                      FCFF, v, cum_d_v_ratio, pv_Ku_Kd*T, ie/(v+FCFF)*(T),
                                                      c, LEEWACC, e, Rf_LEEWACC_adj_factor, LEEWACC_spot, v_0, WACC_gen, 
                                                      round(v,0) - round(v_check,0), round(e,0) - round(e_check,0),
                                                      round(d,0) - round(d_check,0), round(c(0, LEEWACC[2:(n+1)] - WACC_gen[2:(n+1)]),5), 
                                                      npv_0, round(WACC_diff,6), round(FCFF_Adj,0), round(Adjusted_FCFF,0), round(v2,0),
                                                      round(v2,0) - round(v_check,0) ) )
  
  names(asset_value_Rf_rate_LEEWACC_adj) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd",
                                              "D", "FCFF", "V", "cum_d_v_ratio", "pv_Ku_Kd_T", "ie_v_FCFF_T", "c",
                                              "LEEWACC", "E", "Rf_LEEWACC_adj_factor", "LEEWACC_spot", "V_0",
                                              "WACC_gen", "v_check", "e_check", "d_check", "WACC_check", "NPV_0",
                                              "WACC_diff", "FCFF_Adj", "Adjusted_FCFF", "v2", "v2_check")
  
  return(rotate(asset_value_Rf_rate_LEEWACC_adj))
}

asset_value_Rf_rate_LEEWACC_adj <- asset_value_Rf_rate_LEEWACC_adj( out3, out4, flows )

round(asset_value_Rf_rate_LEEWACC_adj,5)


# Apply this R output to provided formatted Excel spreadsheet



asset_value_Rf_rate_LEEWACCbt_adj <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie  <- b$ie
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # CCF is selected from the 15 row of 'flows' output
  CCF <- c[15, ]        
  
  # Initialize debt value and asset value vectors to zero
  d <- rep(0, n+1)
  v <- rep(0, n+1)
  v2 <- rep(0, n+1) # for discounting adjusted numerator flow directly
  
  # Initialize adjusted CCF vectors to zero
  CCF_Adj <- rep(0, n+1)
  Adjusted_CCF <- rep(0, n+1)
  
  # d1 term is used instead of d which refers to debt value
  # Initialize Ku adjustment 'd1' vector to zero
  # LEEWACCbt = (Ku - d1) / (1+d1)
  d1 <- rep(0, n+1)
  
  # Initialize LEEWACCbt adjustment factor to vector to 1.0
  Rf_LEEWACCbt_adj_factor <- rep(1, n+1)
  
  # Initialize 'cum' debt ratio vector to 50% each period
  cum_d_v_ratio <- rep(0.5, n+1)
  
  pv_Ku_Kd <- (Ku - Kd)/(1 + Kd)
  Ku_Kd_AT <- (Ku - Kd)/(1 + Kd)*(1-T)
  
  # Initialize noncircular LEEWACCbt discount rate = Ku
  LEEWACCbt <- Ku
  
  # Calculate Asset value by period in reverse order using discount rates LEEWACCbt[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is noncircular
    d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
    
    cum_d_v_ratio[t] <- ( d[t] + CFd[t] ) / (v[t] + CCF[t] )
    
    # Noncircular 'd1' term is LEEWACCbt calculation
    d1[t] <-  ( cum_d_v_ratio[t] * pv_Ku_Kd[t] )*(T[t])
    
    # LEEWACCbt calculation is noncircular
    LEEWACCbt[t] <-   (Ku[t] - d1[t]) / (1 + d1[t]) 
    
    Rf_LEEWACCbt_adj_factor[t] <- (1 + ( Rf[t] - LEEWACCbt[t] ) / ( 1 + LEEWACCbt[t] ) )  
    
    # Discounting CCFF 1-period at the forward discount rate, LEEWACCbt[t]
    v[t-1] <- ( v[t] + CCF[t] ) / ( 1 + Rf[t] ) *  Rf_LEEWACCbt_adj_factor[t] 
    
    # Adjustment to Flow = CCF for discounting @ Rf
    CCF_Adj[t] <- ( v2[t] + CCF[t] ) * ( Rf[t] - LEEWACCbt[t] ) / ( 1 + LEEWACCbt[t] ) 
    Adjusted_CCF[t] <- CCF[t] + CCF_Adj[t] 
    
    # Discounting 'adjusted CCF' value @ Rf
    v2[t-1] <- ( v2[t] + Adjusted_CCF[t] ) / ( 1 + Rf[t] ) 
  }
  
  # Insert 'Year 0' CCF into the 'Adjusted CCF vector'
  Adjusted_CCF[1] <- CCF[1]
  
  # Noncircular equity valuation  
  e <- v - d
  
  # Circular WACCbt and Ke calculations
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  
  # WACCbt = f(Kd,Ke)
  WACCbt <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0) / lag(v, default=0)) * (Kd)  
  
  #WACCbt = f(Ku)
  WACCbt_gen <- Ku - (lag(d, default=0) / lag(v, default=0)) * (Ku - Kd) * (T) 
  
  # Difference = 0 (General case for each)
  WACCbt_diff <- WACCbt_gen - WACCbt
  
  # LEEWACCbt_spot discount rates by Year
  
  LEEWACCbt_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  LEEWACCbt_spot[2] <- LEEWACCbt[2]
  
  for (t in 3:(n+1))   
  {
    LEEWACCbt_spot[t] <- ( ( 1 + LEEWACCbt_spot[t-1] )^(t-2) * ( 1 + LEEWACCbt[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using LEEWACCbt__spot discount rates  
  v_0 <- c( sum( CCF[2:(n+1)] / (1 + LEEWACCbt_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(CCF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  asset_value_Rf_rate_LEEWACCbt_adj <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd,
                                                        round(d,0), CCF, v, cum_d_v_ratio, pv_Ku_Kd*T, d1, LEEWACCbt, e,
                                                        Rf_LEEWACCbt_adj_factor, LEEWACCbt_spot, v_0, WACCbt_gen,
                                                        round(v,0) - round(v_check,0), round(e,0) - round(e_check,0),    
                                                        round(d,0) - round(d_check,0), 
                                                        round(c(0, LEEWACCbt[2:(n+1)] - WACCbt_gen[2:(n+1)]),5), npv_0, round(WACCbt_diff,6),
                                                        round(CCF_Adj,0), round(Adjusted_CCF,0), round(v2,0),
                                                        round(v2,0) - round(v_check,0)  ) )
  
  names(asset_value_Rf_rate_LEEWACCbt_adj) <- c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd",
                                                "D", "CCF", "V", "cum_d_v_ratio", "pv_Ku_Kd_T", "d1", "LEEWACCbt", 
                                                "E", "Rf_LEEWACCbt_adj_factor", "LEEWACC_spot", "V_0", "WACCbt_gen", 
                                                "v_check", "e_check", "d_check", "WACCbt_check", "NPV_0", "WACCbt_diff",
                                                "CCF_Adj", "Adjusted_CCF", "v2", "v2_check")
  
  return(rotate(asset_value_Rf_rate_LEEWACCbt_adj))
}

asset_value_Rf_rate_LEEWACCbt_adj <- asset_value_Rf_rate_LEEWACCbt_adj( out3, out4, flows )

round(asset_value_Rf_rate_LEEWACCbt_adj,5)


# Apply this R output to provided formatted Excel spreadsheet



equity_value_EP_circ_2 <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie  <- b$ie
  ii  <- b$ii
  Ebv <- b$Ebv
  MS  <- b$MS
  ni  <- b$ni 
  
  ii_AT <- ii * (1-T)
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # ECF is selected from the 12th row of 'flows' output
  ECF <- c[12, ]        
  
  # Initialize Economic Profit (EP) vector to zero
  EP <- rep(0, n+1)
  
  # Equity Valuation w/ EP 
  
  # Initialize debt value and equity value temporarily to $1,000 each year so D/E ratio 
  # is not 0/0 = infinity except for last year = $0
  d <- c(rep(1000, n), 0)
  e <- d  # Initialize  debt-to-equity ratio (D/E) = 1.0 as an initial guess value each Year
  
  e_calc <- e  # Initialize  equity valuation vector guess values
  
  # Initialize Ke vector assuming 100% debt-to-equity ratio, D/E = 1.00
  Ke <- Ku + ( lag(d,default=0) / lag(e,default=0 ) )*( Ku - Kd )*(1 - T) 
  
  # Initialize pv_ep = Present Value of Economic Profit = PV[EP, Ke] = 0 each year
  pv_ep <- rep(0, n+1)
  
  iterations <- 100
  tolerance  <- 0.000001
  
  for (j in 1:iterations ) {
    
    # Calculate Equity value by period in reverse order using discount rates Ke[t]
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Circular Economic Profit (EP) Valuation begins here
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      
      Ke[t] <- Ku[t] + ( d_e_ratio_t_1 )*( Ku[t] - Kd[t] )*(1 - T[t])  
      
      # Calculate EP at this point in a circular manner
      EP[t] <- (ni[t] - ii_AT[t]  + (MS[t] - MS[t-1]) ) - Ke[t] * Ebv[t-1] 
      
      # Discounting EP 1-period at the forward discount rate, Ke[t] = PV [ EP, Ke ]
      pv_ep[t-1] <- ( pv_ep[t] + EP[t] ) / ( 1 + Ke[t] )  # NOT Equity Value !
      
      # Calculate Equity Value using EP,  E = PV[EP, Ke] + Ebv  = Equity Value
      e_calc[t-1]  <- pv_ep[t-1] + Ebv[t-1]     # = Equity Value
      
    }
    
    # Determine error in iterative valuation process
    error <- sum((e - e_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    e <- e_calc
  }
  
  # Asset valuation  
  v <- e + d
  
  # Alternative EP calculation
  
  roe <- ( ni - ii_AT + (MS - lag(MS, default=0)) ) / ( lag(Ebv, default=0) ) 
  
  EP_roe <- (roe - Ke) * ( lag(Ebv, default=0) ) 
  
  EP_roe_check <- EP - EP_roe
  
  # Ke_spot discount rates by Year
  
  Ke_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  Ke_spot[2] <- Ke[2]
  
  for (t in 3:(n+1))   
  {
    Ke_spot[t] <- ( ( 1 + Ke_spot[t-1] )^(t-2) * ( 1 + Ke[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End Ke_spot discount rates by Year
  
  # Equity Value at t=0 using Ke_spot discount rates  
  e_0 <- c( sum( ECF[2:(n+1)] / (1 + Ke_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  e_0 <- c(round(e_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(e_0[1],0) + round(ECF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  equity_value_EP_circ_2 <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd,
                                             round(d,0), ECF, e, Ke, v, Ke_spot, e_0, round(v,0) - round(v_check,0),
                                             round(e,0) - round(e_check,0), round(d,0) - round(d_check,0), npv_0,
                                             ni, -ii_AT, ni - ii_AT + (MS - lag(MS, default=0)),
                                             -Ke *(lag(Ebv, default=0)), EP, Ebv, MS-lag(MS,default=0), 
                                             round(pv_ep,0), roe, EP_roe, EP_roe_check  ))
  
  names(equity_value_EP_circ_2) <-  c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd", "D", "ECF",
                                      "E", "Ke", "V",  "Ke_spot", "E_0", "v_check", "e_check", "d_check", "NPV_0",
                                      "NI", "Less_II_AT", "NI_Less_II_AT_plus_MS_chg",  "Less_Book_Dollar_Charge",
                                      "EP", "Ebv", "MS_chg", "PV_EP", "ROE", "EP_ROE", "EP_ROE_Check")
  
  return(rotate(equity_value_EP_circ_2))
}

equity_value_EP_circ_2 <- equity_value_EP_circ_2( out3, out4, flows )

round(equity_value_EP_circ_2,5)


# Apply this R output to provided formatted Excel spreadsheet




asset_value_EVA_circ_1 <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie  <- b$ie
  ni  <- b$ni
  EVAD <- a$EVAD
  EVA_BV <- a$EVA_BV
  ebitda <- b$ebitda
  sp    <- a$sp
  td    <- a$td
  TB_at_sale <- a$TB_at_sale
  chg_OWC    <- chg_OWC(b)
  OWC        <- cumsum(chg_OWC)
  eva_inv    <- EVA_BV + OWC
  gcf        <- (ebitda + sp)*(1-T) + (td + TB_at_sale)*(T)
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # ECF is selected from the 12th row of 'flows' output
  ECF <- c[12, ]      
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ] 
  
  # Initialize Economic Value Added (EVA) vector to zero
  EVA <- rep(0, n+1)
  
  # Asset Valuation w/ EVA 
  
  # Initialize debt value and equity value temporarily to $1,000 each year so D/E ratio 
  # is not 0/0 = infinity except for last year = $0
  d <- c(rep(1000, n), 0)
  e <- d     #   Initialize debt-to-equity ratio (D/E) = 100% as an initial guess value each Year
  v <- d + e #   Initialize asset value
  
  v_calc <- v  # Initialize asset value guess each year
  
  # Initialize WACC vector assuming 100% debt-to-equity ratio, D/E = 1.00
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  WACC <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0) 
                                                                 / lag(v, default=0)) * (Kd) - (ie*T) / lag(v, default=0)  
  
  # Initialize pv_eva = Present Value of Economic Value Added = PV[EVA, WACC] = 0 each year
  pv_eva <- rep(0, n+1)
  
  iterations <- 100
  tolerance  <- 0.00001
  
  for (j in 1:iterations ) {
    
    # Calculate Asset value by period in reverse order using discount rates WACC[t]
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Circular Economic Value Added (EVA) begins here
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      d_v_ratio_t_1 <- d_e_ratio_t_1 / ( 1 + d_e_ratio_t_1)      # Per Appendix
      
      # Calculate circular WACC using circular Ke
      Ke[t] <- Ku[t] + ( d_e_ratio_t_1 )*( Ku[t] - Kd[t] )*(1 - T[t])
      WACC[t] <-  (1 - d_v_ratio_t_1) * Ke[t] + d_v_ratio_t_1 * Kd[t] - (ie[t]*T[t]) / v[t-1] 
      
      # Calculate EVA at this point in a circular manner
      EVA[t] <- gcf[t] - EVAD[t] - WACC[t] * (EVA_BV[t-1] + OWC[t-1])
      
      # Discounting EVA 1-period at the forward discount rate, WACC[t] = PV[ EVA, WACC ]
      pv_eva[t-1] <- ( pv_eva[t] + EVA[t] ) / ( 1 + WACC[t] )  # NOT Asset Value !
      
      # Calculate Asset Value using EVA,  V = PV[EVA, WACC] + (EVA_BV + OWC) = Asset Value
      v_calc[t-1]  <- pv_eva[t-1]  + (EVA_BV[t-1] + OWC[t-1])  
      
    }
    
    # Determine error in iterative valuation process
    error <- sum((v - v_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v <- v_calc
    e <- v - d
  }
  
  # Asset valuation  
  v <- e + d
  
  # Spot discount rates by Year
  
  WACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- WACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End WACC_spot discount rates by Year
  
  # Asset Value at t=0 using WACC_spot discount rates  
  v_0 <- c( sum( EVA[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(EVA[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  asset_value_EVA_circ_1 <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd, round(d,0),
                                             FCFF, round(e,0), Ke, WACC, v, WACC_spot, v_0, 
                                             round(v,0) - round(v_check,0), round(e,0) - round(e_check,0), 
                                             round(d,0) - round(d_check,0), npv_0, ni, gcf ,
                                             -WACC *(lag((EVA_BV+OWC), default=0)),
                                             EVA, -EVAD, EVA_BV, OWC, EVA_BV+OWC, round(pv_eva,0), - d, 
                                             round(pv_eva,0) + round((EVA_BV+OWC),0) - round(v,0)  ))
  
  names(asset_value_EVA_circ_1) <-  c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd", "D", "FCFF",
                                      "E", "Ke", "WACC", "V",  "WACC_spot", "V_0", "v_check", "e_check", "d_check",
                                      "NPV_0", "NI", "GCF",  "Less_Book_Dollar_Charge", "EVA","Less_EVAD",  "EVA_BV",
                                      "OWC", "EVA_BV_OWC", "PV_EVA", "Less_D", "EVA_check")
  
  return(rotate(asset_value_EVA_circ_1))
}

asset_value_EVA_circ_1 <- asset_value_EVA_circ_1( out3, out4, flows )

round(asset_value_EVA_circ_1,5)


# Apply this R output to provided formatted Excel spreadsheet




asset_value_EVA_circ_2 <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie  <- b$ie
  ni  <- b$ni
  EVAD <- a$EVAD
  EVA_BV <- a$EVA_BV
  ebitda <- b$ebitda
  sp    <- a$sp
  td    <- a$td
  TB_at_sale <- a$TB_at_sale
  chg_OWC    <- chg_OWC(b)
  OWC        <- cumsum(chg_OWC)
  eva_inv    <- EVA_BV + OWC
  gcf        <- (ebitda + sp)*(1-T) + (td + TB_at_sale)*(T)
  nopat_tax  <- (ebitda - td)*(1-T)
  asset_sale_cf <- sp*(1-T) + (TB_at_sale)*(T)
  nopat_tax_w_sale_cf  <- nopat_tax + asset_sale_cf
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # ECF is selected from the 12th row of 'flows' output
  ECF <- c[12, ]      
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ] 
  
  # Initialize Economic Value Added (EVA) vector to zero
  EVA <- rep(0, n+1)
  
  # Asset Valuation w/ EVA 
  
  # Initialize debt value and equity value temporarily to $1,000 each year so D/E ratio
  # is not 0/0 = infinity except for last year = $0
  d <- c(rep(1000, n), 0)
  e <- d     #   Initialize debt-to-equity ratio (D/E) = 100% as an initial guess value each Year
  v <- d + e #   Initialize asset value
  
  v_calc <- v  # Initialize asset value guess each year
  
  # Initialize WACC vector assuming 100% debt-to-equity ratio, D/E = 1.00
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  WACC <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0) 
                                                                 / lag(v, default=0)) * (Kd) - (ie*T) / lag(v, default=0)  
  
  # Initialize pv_eva = Present Value of Economic Value Added = PV[EVA, WACC] = 0 each year
  pv_eva <- rep(0, n+1)
  
  iterations <- 100
  tolerance  <- 0.00001
  
  for (j in 1:iterations ) {
    
    # Calculate Asset value by period in reverse order using discount rates WACC[t]
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Circular Economic Value Added (EVA) Valuation begins here
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      d_v_ratio_t_1 <- d_e_ratio_t_1 / ( 1 + d_e_ratio_t_1)      # Per Appendix
      
      # Calculate circular WACC using circular Ke
      Ke[t] <- Ku[t] + ( d_e_ratio_t_1 )*( Ku[t] - Kd[t] )*(1 - T[t])
      WACC[t] <-  (1 - d_v_ratio_t_1) * Ke[t] + d_v_ratio_t_1 * Kd[t] - (ie[t]*T[t]) / v[t-1] 
      
      # Calculate EVA at this point in a circular manner
      EVA[t] <- nopat_tax_w_sale_cf[t] - WACC[t] * (EVA_BV[t-1] + OWC[t-1]) + (td[t] - EVAD[t])  
      
      # Discounting EVA 1-period at the forward discount rate, WACC[t] = PV[ EVA, WACC ]
      pv_eva[t-1] <- ( pv_eva[t] + EVA[t] ) / ( 1 + WACC[t] )  # NOT Asset Value !
      
      # Calculate Asset Value using EVA,  V = PV[EVA, WACC] + (EVA_BV + OWC) = Asset Value
      v_calc[t-1]  <- pv_eva[t-1]  + (EVA_BV[t-1] + OWC[t-1])
      
    }
    
    # Determine error in iterative valuation process
    error <- sum((v - v_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v <- v_calc
    e <- v - d
  }
  
  # Asset valuation  
  v <- e + d
  
  # Return on invested capital = roic
  roic <- nopat_tax_w_sale_cf / (lag(EVA_BV, default=0) + lag(OWC, default=0))
  
  EVA_roic_pre <- (roic - WACC) * ( lag((EVA_BV + OWC), default=0) )
  
  EVA_roic <- (roic - WACC) * ( lag((EVA_BV + OWC), default=0) ) + (td-EVAD) 
  
  EVA_roic_check <- EVA - EVA_roic
  
  # Spot discount rates by Year
  
  WACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- WACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End WACC_spot discount rates by Year
  
  # Asset Value at t=0 using WACC_spot discount rates  
  v_0 <- c( sum( EVA[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(EVA[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  asset_value_EVA_circ_2 <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd, round(d,0),
                                             FCFF, round(e,0), Ke, WACC, v, WACC_spot, v_0, 
                                             round(v,0) - round(v_check,0), round(e,0) - round(e_check,0),
                                             round(d,0) - round(d_check,0), npv_0, ni, (ebitda - td)*(1-T),
                                             sp*(1-T) + (TB_at_sale)*(T), nopat_tax_w_sale_cf,
                                             -WACC *(lag((EVA_BV+OWC), default=0)), roic, roic - WACC, EVA_roic_pre,
                                             EVA_roic, EVA_roic_check, EVA, -EVAD, EVA_BV, OWC, EVA_BV + OWC, td,
                                             td - EVAD, round(pv_eva,0), - d,
                                             round(pv_eva,0) + round((EVA_BV+OWC),0) - round(v,0) ))
  
  names(asset_value_EVA_circ_2) <-  c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd", "D",
                                      "FCFF", "E", "Ke", "WACC", "V",  "WACC_spot", "V_0", "v_check",
                                      "e_check", "d_check", "NPV_0", "NI", "NOPAT_tax", "CF_from_Sale", 
                                      "NOPAT_tax_w_Sale_CF",  "Less_Book_Dollar_Charge", "ROIC", "ROIC_WACC_spread", 
                                      "EVA_ROIC_pre" , "EVA_ROIC", "EVA_ROIC_check", "EVA","Less_EVAD",  "EVA_BV",
                                      "OWC", "EVA_BV_OWC", "TD", "TD_EVAD", "PV_EVA", "Less_D", "EVA_check")
  
  return(rotate(asset_value_EVA_circ_2))
}

asset_value_EVA_circ_2 <- asset_value_EVA_circ_2( out3, out4, flows )

round(asset_value_EVA_circ_2,5)


# Apply this R output to provided formatted Excel spreadsheet



asset_value_EVA_circ_3 <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie  <- b$ie
  ii  <- b$ii
  Ebv <- b$Ebv
  MS  <- b$MS
  ni  <- b$ni
  np  <- b$np
  cpltd <- b$cpltd
  LTD <- b$LTD
  
  N <- np + cpltd + LTD # All interest bearing debt = N = Book Value of Debt
  ii_AT <- ii * (1-T)
  ie_AT <- ie * (1-T)
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # ECF is selected from the 12th row of 'flows' output
  ECF <- c[12, ]      
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ] 
  
  # Initialize Economic Value Added (EVA) vector to zero
  EVA <- rep(0, n+1)
  
  # Asset Valuation w/ EVA 
  
  # Initialize debt value and equity value temporarily to $1,000 each year so D/E ratio
  # is not 0/0 = infinity except for last year = $0
  d <- c(rep(1000, n), 0)
  e <- d     #   Initialize debt-to-equity ratio (D/E) = 100% as an initial guess value each Year
  v <- d + e #   Initialize asset value
  
  v_calc <- v  # Initialize asset value guess each year
  
  # Initialize WACC vector assuming 100% debt-to-equity ratio, D/E = 1.00
  Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)
  WACC <-  (1 - lag(d, default=0) / lag(v, default=0)) * (Ke) + (lag(d, default=0) 
                                                                 / lag(v, default=0)) * (Kd) - (ie*T) / lag(v, default=0)  
  
  # Initialize pv_eva = Present Value of Economic Value Added = PV[EVA, WACC] = 0 each year
  pv_eva <- rep(0, n+1)
  
  iterations <- 100
  tolerance  <- 0.00001
  
  for (j in 1:iterations ) {
    
    # Calculate Asset value by period in reverse order using discount rates WACC[t]
    
    for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
    {
      # Discounting CFd 1-period at the forward discount rate, Kd[t]
      
      # Debt valuation is noncircular
      d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
      
      # Circular Economic Value Added (EVA) Valuation begins here
      
      # Prevent division by zero errors
      d_e_ratio_t_1 <- ifelse( e[t-1] == 0, 0, d[t-1] / e[t-1] )
      d_v_ratio_t_1 <- d_e_ratio_t_1 / ( 1 + d_e_ratio_t_1)      # Per Appendix
      
      # Calculate circular WACC using circular Ke
      Ke[t] <- Ku[t] + ( d_e_ratio_t_1 )*( Ku[t] - Kd[t] )*(1 - T[t])
      WACC[t] <-  (1 - d_v_ratio_t_1) * Ke[t] + d_v_ratio_t_1 * Kd[t] - (ie[t]*T[t]) / v[t-1] 
      
      # Calculate EVA at this point in a circular manner
      EVA[t] <- (ni[t] - ii_AT[t] + ie_AT[t] + (MS[t] - MS[t-1]) ) - WACC[t] * (Ebv[t-1] + N[t-1])
      
      # Discounting EVA 1-period at the forward discount rate, WACC[t] = PV[ EVA, WACC ]
      pv_eva[t-1] <- ( pv_eva[t] + EVA[t] ) / ( 1 + WACC[t] )  # NOT Asset Value !
      
      # Calculate Asset Value using EVA,  V = PV[EVA, WACC] + (Ebv + N) = Asset Value
      v_calc[t-1]  <- pv_eva[t-1] + (Ebv[t-1] + N[t-1])     
      
    }
    
    # Determine error in iterative valuation process
    error <- sum((v - v_calc)^2)
    
    # Break from loop if desired error tolerance has been achieved
    if (error <= tolerance) break
    
    # Retain the current valuation convergence values as the guess for the next iterative calculation pass 
    v <- v_calc
    e <- v - d
  }
  
  # Asset valuation  
  v <- e + d
  
  # Alternative EVA calculation using ROA
  
  roa <- ( ni - ii_AT + ie_AT + (MS - lag(MS, default=0)) ) / ( lag((Ebv+N), default=0) ) 
  
  EVA_roa <- (roa - WACC) * ( lag((Ebv+N), default=0) ) 
  
  EVA_roa_check <- EVA - EVA_roa
  
  # Spot discount rates by Year
  
  WACC_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  WACC_spot[2] <- WACC[2]
  
  for (t in 3:(n+1))   
  {
    WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End WACC_spot discount rates by Year
  
  # Asset Value at t=0 using WACC_spot discount rates  
  v_0 <- c( sum( EVA[2:(n+1)] / (1 + WACC_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(EVA[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  asset_value_EVA_circ_3 <- as_tibble( cbind(a$Year, a$T, a$Rf, a$Bu, a$MRP, Ku, Bd , Kd, Ku - Kd, CFd,
                                             round(d,0), FCFF, round(e,0), Ke, WACC, v, WACC_spot, v_0,
                                             round(v,0) - round(v_check,0), round(e,0) - round(e_check,0), 
                                             round(d,0) - round(d_check,0), npv_0, ni, -ii_AT, ie_AT,
                                             ni - ii_AT + ie_AT + (MS - lag(MS, default=0)),
                                             -WACC *(lag((Ebv+N), default=0)), EVA, Ebv, N, Ebv+N, 
                                             MS-lag(MS,default=0), round(pv_eva,0),  round(pv_eva,0)+(Ebv+N),
                                             - d, roa, EVA_roa, EVA_roa_check  ))
  
  names(asset_value_EVA_circ_3) <-  c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", "CFd", "D", 
                                      "FCFF", "E", "Ke", "WACC", "V",  "WACC_spot", "V_0", "v_check",
                                      "e_check", "d_check", "NPV_0", "NI", "Less_II_AT","IE_AT",
                                      "NI_m_II_AT_p_IE_AT_p_chg_MS",  "Less_Book_Dollar_Charge", "EVA", 
                                      "Ebv", "N", "Ebv_N", "MS_chg", "PV_EVA", "PV_EVA_Ebv+N", "Less_D",
                                      "ROA", "EVA_ROA", "EVA_ROA_Check")
  
  return(rotate(asset_value_EVA_circ_3))
}

asset_value_EVA_circ_3 <- asset_value_EVA_circ_3( out3, out4, flows )

round(asset_value_EVA_circ_3,5)


# Apply this R output to provided formatted Excel spreadsheet




## Spot/Forward Risk-free (Rf) Treasury Rates for use in CAPM

library(tidyverse)
library(readxl)

# This section of the code will not work until you place the correct path to your 'tnc_18_22.xls' spreadsheet file

risk_free_2_2021 <- read_excel(" ... PLACE YOUR FILE PATH HERE ..... /tnc_18_22.xls")
risk_free_2_2021

# Select 1st and 40th columns only, remove blank rows
positions <- c(1,40)
risk_free_2_2021 <- risk_free_2_2021 %>%   select(positions) %>%
  drop_na()  %>%            # remove blank rows
  slice(-c(1:2))  %>%       # Remove first 2 rows
  mutate_if(is.character,as.numeric)  # Change character format to numeric
# Name columns
colnames(risk_free_2_2021) <- c("mat","spot_rate")

risk_free_2_2021


# Add 't=0' row to data
risk_free_2_2021 <- risk_free_2_2021 %>% add_row(mat = 0, spot_rate = 0, .before = 1) 

# Calculate Spot and Forward Risk-free rates
set.seed(40)
risk_free_2_2021 <- risk_free_2_2021  %>% 
  mutate(spot_ann = as.numeric(spot_rate)/100,  # Pct to decimal format
         spot_semi_ann = spot_ann /2,
         forward_semi_ann = (1 + spot_semi_ann)^(2*mat) / (1 + lag(spot_semi_ann,default=0))^(2*lag(mat,default=0)) - 1,
         spot_semi_ann_DF = 1/(1+spot_semi_ann)^(2*mat),
         Riskless_CF = c(0, runif(nrow(risk_free_2_2021)-1, 0, 1000))      )

risk_free_2_2021

# Select data for spot discounting Riskless CFs @ Rf rate

spot_semi_ann_DF <- select(risk_free_2_2021, spot_semi_ann_DF)
Riskless_CF      <- select(risk_free_2_2021, Riskless_CF)

# Present value @ t=0 of riskless cash flows using 'spot rate discounting'
# Similar to Excel's 'sumproduct' function
PV0_spot <- sum(Riskless_CF * spot_semi_ann_DF)
PV0_spot
#[1] 39202.56

# Create matrix from Treasury data tibble for 'forward rate discounting' loop
Rf_rate_discounting  <- as.matrix(risk_free_2_2021)
Rf_rate_discounting

# Initialize valuation vector to zero
v_forward <- rep(0, nrow(risk_free_2_2021))

# Determine value each 6-month period over the 100-year horizon
# by discounting using risk-free (Rf) forward discount rates

forward_semi_ann <- Rf_rate_discounting[, "forward_semi_ann"]
Riskless_CF      <- Rf_rate_discounting[, "Riskless_CF"]

# Cycle through all cash flows in reverse order
for (t in nrow(risk_free_2_2021):2)
{
  v_forward[t-1] <-  ( v_forward[t] + Riskless_CF[t]) / (1 + forward_semi_ann[t])
}

# Present value @ t=0 of riskless cash flows using 'forward rate discounting'
PV0_forward <- v_forward[1]
PV0_forward
# [1] 39202.56

PV0_check <- PV0_spot - PV0_forward
PV0_check
# [1] 1.455192e-11



# PVRR Utility Example

library(tidyverse)

PUF_capX_0 <- 100     # $100 capital project @ t=0
n_PUF      <- 10      # 10-year capital project

asmpt <- tibble( Year = c(0:n_PUF),
                 rev      = c(0, rep(20, n_PUF)),                     # Revenue of project
                 capX     = c( PUF_capX_0, rep(0, n_PUF)),            # Capital Cost
                 bd_sch   = c(0, rep( 1 / n_PUF, n_PUF ) ),           # Book deprecation schedule 
                 bd       = PUF_capX_0 * bd_sch,                      # Book depreciation
                 td       = c(0, 20,	16,	12.8,	10.25, rep(6.825, 6)), # Tax  depreciation
                 acc_bd   = cumsum(bd),                               # Accumulated book depreciation
                 acc_td   = cumsum(td),                               # Accumulated tax  depreciation
                 BV       = PUF_capX_0 - acc_bd,                      # Asset's Book Value
                 TB       = PUF_capX_0 - acc_td,                      # Asset's Tax Basis
                 T        = rep(0.40, n_PUF+1 ),                      # Tax Rates     
                 chg_DTL_net = ((td - bd )) * T ,                     # Increase in DTL 
                 DTL_net  = cumsum(chg_DTL_net) ,                     # Deferred Tax Liabilities, net
                 ppe      = rep(PUF_capX_0, n_PUF+1),                 # Gross PP&E 
                 CoS_pct  = rep(0, n_PUF+1),                          # Cost of Sales as a percentage of 
                 #   current period's revenue 
                 sga_pct  = rep(0, n_PUF+1),                          # SG&A expense  as a percentage of 
                 #   current period's revenue 
                 CoS      = rev * CoS_pct,                            # CoS
                 sga      = rev * sga_pct,                            # SG&A
                 dr       = rep( 0.5, n_PUF+1),                       # Debt Ratio
                 Kd       = rep( 0.085, n_PUF+1),                     # Cost of Debt capital
                 r_debt   = Kd,                                       # Book debt interest rate
                 Ke       = rep( 0.115, n_PUF+1),                     # Cost of Equity Capital
                 rb       = BV - DTL_net,                             # Net Rate Base    
                 roe      = Ke,                                       # Return on book equity
                 Equity   =  rb*(1-dr),                               # Book equity
                 Debt     =  rb*dr,                                   # Book debt 
                 ni       = roe*lag(Equity,default=0),                # Book net income
                 ie       = r_debt*lag(Debt,default=0),               # Book interest expense
                 tax      = ni/(1-T) * T,                             # Book income taxes
                 rr       = ni + tax + ie + bd,                       # Pretax revenue requirement
                 rr_net   = rr - rev,                                 # Net Pretax revenue requirement
                 rr_net_AT = (rr_net)*(1-T),                          # Net After-tax revenue requirement
                 WACC_spot = (1-dr)*Ke + dr*Kd*(1-T),                 # Spot WACC Rate = CONSTANT
                 ebitda    = rev - CoS - sga,                         # EBITDA
                 ebitda_AT = ebitda * (1-T),                          # (EBITDA)(1-T)
                 td_T      = td*T,                                    # Depreciation Tax Shield
                 fcff     = ebitda_AT + td_T - capX,                  # FCFF
                 V_0      = c( sum( fcff[2:(n_PUF+1)] / (1 + WACC_spot[2:(n_PUF+1)] )^(1:n_PUF) ) ,
                               rep(0,n_PUF) ),                        # DCF Asset Value
                 npv_0    =  V_0 + c( fcff[1], rep(0,n_PUF)),         # NPV @ t=0
                 pvrr_net_AT =  c( sum( rr_net_AT[2:(n_PUF+1)] / (1 + WACC_spot[2:(n_PUF+1)] )^(1:n_PUF) ),
                                   rep(0,n_PUF) ),                       # (PVRR)(1-T) @ t=0
                 check    = round(npv_0 + pvrr_net_AT,5),             # NPV + (PVRR)(1-T)
                 chg_N    = Debt - lag(Debt,default=0)                # Change in Book Debt    
)

rotate(asmpt)

T     <- asmpt[["T"]]
FCFF  <- asmpt[["fcff"]]
N     <- asmpt[["Debt"]]
ie    <- asmpt[["ie"]]                
chg_N <- asmpt[["chg_N"]]            
CFd   <- ie - chg_N                   # Debt Cash Flow
ECF   <- FCFF - ( ie*(1-T) - chg_N )  # Equity Cash Flow

# Constant Spot WACC = Constant Forward WACC = 8.3%
# Constant Spot Ke = Constant Forward Ke = 11.5%
# Based on above incorrect forward rate assumptions, calculate incorrect asset value and equity value
#  by year in reverse order

WACC_forward_incorrect <- asmpt[["WACC_spot"]]
Ke_forward_incorrect  <- asmpt[["Ke"]]

# Initialize asset value and equity value vectors to zero
v_incorrect <- rep(0, n_PUF+1)
e_incorrect <- rep(0, n_PUF+1)

#Using forward discount rates in DCF analysis

for (t in (n_PUF+1):2)    # reverse step through loop from period 'n+1' to 2
{
  
  # Incorrect DCF asset valuation using constant, incorrect forward WACC rates
  v_incorrect[t-1] <- (v_incorrect[t] + FCFF[t]) / (1 + WACC_forward_incorrect[t])
  
  # Incorrect DCF equity valuation using constant, incorrect forward Ke Rates
  e_incorrect[t-1] <- ( e_incorrect[t] + ECF[t] ) / ( 1 + Ke_forward_incorrect[t] )  
}

# asset_value_LEEWACC <- function(a, b, c) {

# CAPM Assumptions
Rf   <- rep(0.0805, n_PUF+1 )
MRP  <- rep(0.045, n_PUF+1 )
Bd   <- rep(0.10, n_PUF+1 )
Kd   <- Rf + Bd * MRP
Bu   <- rep(0.58, n_PUF+1 )
Ku   <- Rf + Bu * MRP

# Initialize debt value and asset value vectors to zero
d <- rep(0, n_PUF+1)
v <- rep(0, n_PUF+1)

# Initialize Ku adjustment 'c1' vector to zero
# LEEKWACC = (Ku - c1) / (1 + c1)
c1 <- rep(0, n_PUF+1)

# Initialize 'cum' debt-to-asset vector to 0.5
cum_d_v_ratio <- rep(0.5, n_PUF+1)

pv_Ku_Kd <- (Ku - Kd)/(1 + Kd)

# Initialize noncircular LEEWACC discount rate = Ku
LEEWACC <- Ku

# Calculate Asset value by period in reverse order using noncircular discount rates LEEWACC[t]

for (t in (n_PUF+1):2)    # reverse step through loop from period 'n+1' to 2
{
  # Discounting CFd 1-period at the forward discount rate, Kd[t]
  
  # Debt valuation is noncircular
  d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
  
  cum_d_v_ratio[t] <- ( d[t] + CFd[t] ) / (v[t] + FCFF[t] )
  
  # Noncircular 'c1' term is LEEWACC calculation
  c1[t] <-  ( cum_d_v_ratio[t] * pv_Ku_Kd[t] + ie[t]/( v[t] + FCFF[t] ) )*(T[t])
  
  # LEEWACC calculation is noncircular
  LEEWACC[t] <-   (Ku[t] - c1[t]) / (1 + c1[t]) 
  
  # Discounting FCFF 1-period at the forward discount rate, LEEWACC[t]
  v[t-1] <- ( v[t] + FCFF[t] ) / ( 1 + LEEWACC[t] )  
}

# Equity valuation  
e <- v - d

# Calculate WACC using circular formula

WACC <- Ku - (lag(d, default=0) / lag(v, default=0)) * (Ku - Kd)*(T) - ie/lag(v, default=0)*(T)

# Calculate Ke using circular formula
Ke   <-  Ku + (lag(d, default=0) / lag(e, default=0)) * (1-T) * (Ku - Kd)

d_e_ratio <- ifelse(e==0,0,d/e)
d_v_ratio <- ifelse(v==0,0,d/v)

WACC_spot <- rep(0, n_PUF+1 )  # Initialize spot rates to zero each Year

# Spot rate = forward rate in Year 1 which is the 2nd element in the vector
WACC_spot[2] <-WACC[2]

for (t in 3:(n_PUF+1))   
{
  WACC_spot[t] <- ( ( 1 + WACC_spot[t-1] )^(t-2) * ( 1 + WACC[t] ) ) ^ (1/(t-1)) - 1 
}

# End spot discount rates by Year

# Asset Value at t=0 using spot discount rates  
v_0 <- c( sum( WACC[2:(n_PUF+1)] / (1 + WACC_spot[2:(n_PUF+1)] )^(1:n) ) , rep(0,n_PUF) )    

v_0 <- c(round(v_0[1],0), rep(NaN,n_PUF) )

npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
npv_0 <- c(npv_0, rep(NaN,n_PUF) )

pvrr_dcf <- tibble(CFd  = CFd,                    
                   ECF = ECF,
                   Rf = Rf,
                   MRP = MRP,
                   Kd= Kd,
                   Bu = Bu,
                   Bd = Bd, 
                   d = d,
                   e = e,
                   v = v,
                   cum_d_v_ratio = cum_d_v_ratio,
                   pv_Ku_Kd = pv_Ku_Kd,
                   LEEWACC = LEEWACC, 
                   WACC = WACC, 
                   Ke = Ke,
                   d_e_ratio = d_e_ratio,
                   d_v_ratio = d_v_ratio,
                   WACC_spot = WACC_spot, 
                   v_0 = v_0, 
                   npv_0 = npv_0,
                   D_N = d - N,
                   WACC_forward_incorrect = WACC_forward_incorrect,
                   Ke_forward_incorrect = Ke_forward_incorrect,
                   v_incorrect = v_incorrect,
                   e_incorrect = e_incorrect,
                   d__e_incorrect = d + e_incorrect,
                   error = v_incorrect - d__e_incorrect,
                   pct_error = error / ((v_incorrect + d__e_incorrect)/2),
                   CFd_AT = ie*(1-T) - chg_N,
                   Ku = Ku,
                   Ku_Kd = Ku - Kd,
                   c1 = c1,
                   check = v - (e+d)
)       

pvrr_dcf.1 <- rotate( pvrr_dcf)

asmpt.1 <- rotate(asmpt)

# Combine all PVRR analysis input & output
pvrr_analysis <- rbind(asmpt.1, pvrr_dcf.1)

round(pvrr_analysis,5)


# Apply this R output to provided formatted Excel spreadsheet 



asset_value_pretax_FCFF <- function(a, b, c) {
  
  library(tidyverse)
  
  n <- length(a$bd) - 1
  Rf  <- a$Rf
  MRP <- a$MRP
  Bd  <- a$Bd
  Bu  <- a$Bu
  T   <- a$T
  Ku  <- Rf + Bu * MRP
  Kd  <- Rf + Bd * MRP
  ie  <- b$ie
  ii  <- b$ii
  ebitda <- b$ebitda
  sp  <- a$sp
  td  <- a$td
  TB_at_sale <- a$TB_at_sale
  chg_OWC    <- chg_OWC(b)
  capX       <- a$capX
  
  # CFd is selected from the 11th row of 'flows' output
  CFd <- c[11, ]  
  
  # FCFF is selected from the 2nd row of 'flows' output
  FCFF <- c[2, ] 
  
  # Pretax FCFF (FCFFbt)
  FCFFpt <- ebitda + sp - chg_OWC - capX 
  
  # Initialize debt value and equity value temporarily to $1,000 each year so D/E ratio 
  #  is not 0/0 = infinity except for last year = $0
  d <- c(rep(1000, n), 0)
  e <- d      #   Initialize market debt-to-equity ratio (D/E) = 1.0 as an initial guess value each Year
  v <- d + e  #   Initialize asset value
  
  # Algebraic discount rate components 'a' and 'c-a' = 'c_a', tax_sum = intermediate sum for presentation output
  a <- v + FCFFpt
  tax_sum <- ie + lag(d,default=0)*(Ku-Kd) - ebitda - sp + td + TB_at_sale
  c_a <- tax_sum*(T)
  z   <- c_a / a
  
  # Initialize Pretax FCFF discount rate vector
  LEEPT <- (1+Ku)/(1+c_a/a) - 1
  
  # Calculate Asset value by period in reverse order using forward discount rates LEEPT[t]
  
  for (t in (n+1):2)    # reverse step through loop from period 'n+1' to 2
  {
    # Discounting CFd 1-period at the forward discount rate, Kd[t]
    
    # Debt valuation is noncircular
    d[t-1] <- (d[t] + CFd[t]) / (1 + Kd[t])
    
    # Discount rate for pretax FCFF = LEEPT = b / (1 + (c-a)/a) - 1, b = 1+Ku
    # Update 'a' and 'c-a' terms in LEEPT discount rate with calculated debt value
    a[t] <- v[t] + FCFFpt[t]
    tax_sum[t] <- ie[t] + d[t-1]*(Ku[t]-Kd[t])- ebitda[t] - sp[t] + td[t] + TB_at_sale[t] 
    c_a[t] <-  tax_sum[t]*(T[t])
    z[t]   <- c_a[t] / a[t]
    
    LEEPT[t] = (1+Ku[t]) / (1 + z[t]) - 1
    
    # Or LEEPT = (Ku-z)/(1+z)
    
    # Discounting Pretax FCFF (FCFFbt) 1-period at the forward discount rate, PV[ FCFFbt, LEEPT ]
    v[t-1] <- ( v[t] + FCFFpt[t] ) / ( 1 + LEEPT[t] ) 
    
  }
  
  # Equity Valuation  
  e <- v - d
  
  # Spot discount rates by Year
  
  LEEPT_spot <- rep(0, n+1 )  # Initialize spot rates to zero each Year
  
  # Spot rate = forward rate in Year 1 which is the 2nd element in the vector
  LEEPT_spot[2] <-LEEPT[2]
  
  for (t in 3:(n+1))   
  {
    LEEPT_spot[t] <- ( ( 1 + LEEPT_spot[t-1] )^(t-2) * ( 1 + LEEPT[t] ) ) ^ (1/(t-1)) - 1 
  }
  
  # End spot discount rates by Year
  
  # Asset Value at t=0 using spot discount rates  
  v_0 <- c( sum( FCFFpt[2:(n+1)] / (1 + LEEPT_spot[2:(n+1)] )^(1:n) ) , rep(0,n) )    
  
  v_0 <- c(round(v_0[1],0), rep(NaN,n) )
  
  npv_0 <- round(v_0[1],0) + round(FCFF[1],0)
  npv_0 <- c(npv_0, rep(NaN,n) )
  
  # Checks
  v_check <- asset_value[11, ]
  e_check <- equity_value[13, ]
  d_check <- debt_value[8, ]
  
  asset_value_pretax_FCFF <- as_tibble( cbind(0:n, T, Rf, Bu, MRP, Ku, Bd , Kd, Ku - Kd,
                                              -lag(d,default=0)*(Ku-Kd) , ebitda, sp, -TB_at_sale, -td,
                                              -ie, chg_OWC, capX, CFd, round(d,0), FCFF, FCFFpt, round(e,0),
                                              a, - tax_sum, - c_a, z, LEEPT, v, LEEPT_spot, v_0,
                                              round(v,0) - round(v_check,0), round(e,0) - round(e_check,0),
                                              round(d,0) - round(d_check,0), npv_0 ))
  
  names(asset_value_pretax_FCFF) <-  c("Year", "T", "Rf", "Bu", "MRP", "Ku", "Bd", "Kd", "Ku_Kd", 
                                       "Less_D_t_1_Ku_Kd", "EBITDA", "SP", "Less_TB_at_sale", "Less_TD",
                                       "Less_IE", "chg_OWC", "capX", "CFd", "D", "FCFF", "FCFFpt", "E", 
                                       "a", "Negative_tax_sum", "Negative_c_a", "z", "LEEPT","V",  "LEEPT_spot", "V_0",
                                       "v_check", "e_check", "d_check", "NPV_0")
  
  return(rotate(asset_value_pretax_FCFF))
}

asset_value_pretax_FCFF <- asset_value_pretax_FCFF( out3, out4, flows )

round(asset_value_pretax_FCFF,5)


# Apply this R output to provided formatted Excel spreadsheet 


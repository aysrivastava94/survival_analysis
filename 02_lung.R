
# Project - survival_analysis
# Sub-Project - 
# File name - 01_Setup.R
# Purpose - Initial set up of RStudio for the project
# Date created - 26-Nov-2023

"""
Notes
Data Source: https://github.com/lbraglia/suanselete3/tree/master
Theory: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
Code Guidance: http://www.sthda.com/english/wiki/survival-analysis-basics
Data Name  : Lung

The data contain subjects with advanced lung cancer from the North Central 
Cancer Treatment Group.

inst: Institution code
time: Survival time in days
status: censoring status 1=censored, 2=dead
age: Age in years
sex: Male=1 Female=2
ph.ecog: ECOG performance score (0=good 5=dead)
ph.karno: Karnofsky performance score (bad=0-good=100) rated by physician
pat.karno: Karnofsky performance score as rated by patient
meal.cal: Calories consumed at meals
wt.loss: Weight loss in last six months

"""

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## Installing the missing package 'survminer'
# install.packages("survminer")

## Loading packages
library(survival)
library(dplyr)
library(survminer)

## ---------------------------


## Importing dataset and a necessary recoding
lung <- 
  lung %>% 
  mutate(
    status = recode(status, `1` = 0, `2` = 1)
  )

## Data structure
str(lung)

# Lung dataset dimensions
dim(lung)

# Checking for NAs
sum(is.na(lung))

# Checking for NAs in columns of interest
sum(is.na(lung[c("status","time", "sex", "ph.ecog")]))

# Full vector
any(is.na.data.frame(lung[c("status","time", "sex", "ph.ecog")]))

# Record where we got the null
# complete.cases() flags records in a dataframe where none of the columns are 
# missing values
lung[!complete.cases(lung[c("status","time", "sex", "ph.ecog")]),]

## View dataset in RStudio interface
View(lung)

## ----------------------------------------------------

## Fit 1
fit_1 <- survfit(Surv(time, status) ~ sex, data = lung)

## Fit 1 Summary
summary(fit_1)$table

## Fit 1 Plot
ggsurvplot(fit_1,
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF")
)

## Fit 1 Survival Differences
survdiff(Surv(time, status) ~ sex, data = lung)

## Cox PH Model - Fit #1
Cox_fit1 <- coxph(Surv(time, status) ~ sex, data = lung)
Cox_fit1

## Schoenfeld Residuals
## Significant p-value indicates PH assumption is violated
cox.zph(Cox_fit1, transform = "rank")

## Cox PH Model - Fit #2
Cox_fit2 <- coxph(Surv(time, status) ~ sex + ph.ecog, data = lung)
Cox_fit2

## Schoenfeld Residuals
## Significant p-value indicates PH assumption is violated
cox.zph(Cox_fit2, transform = "rank")

## Fit #2 should be invalid since we treated factors as numeric

## Cox PH Model - Fit #3
Cox_fit3 <- coxph(Surv(time, status) ~ factor(sex) + factor(ph.ecog), data = lung)
Cox_fit3

## Schoenfeld Residuals
## Significant p-value indicates PH assumption is violated
cox.zph(Cox_fit3, transform = "rank")








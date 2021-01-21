# check_SPR_2021_1_20.R Check International comparison
## libraries
#library(xtable)
library(ggplot2); theme_set(theme_bw())
#library(scales)# for $ or % (not needed, uas paste0)
#library(tidyr)# for gather (changing wide to long)

setwd("~/RPO & Checks/Check_SPR_2020/check_SPR_coding")
dir()

## Function definitions: Cummulative Annual Growth Rate
CAGR_formula <- function(FV, PV, yrs) {
  values <- ((FV/PV)^(1/yrs)-1)
  return(values)
}
# testing the formula
CAGR_formula(110, 100, 1)
CAGR_formula(110, 100, 2)
CAGR_formula(121, 100, 2)

cashless_vol2.df = readRDS("country_cashless_payment_volume.rds")
cashless_val2.df = readRDS("country_cashless_payment_value.rds")
check_vol2.df = readRDS("country_check_volume.rds")
check_val2.df = readRDS("country_check_value.rds")

### 2019 Volume graph (combine 2 tables into %)
check_vol2.df
cashless_vol2.df
# delete Japan and Switzerland (no data on volume), Singapore
(check_vol3.df =  check_vol2.df[-c(12,18,22), ])
(cashless_vol3.df =  cashless_vol2.df[-c(12,18,22), ])
# divide check by cashless (get fraction)
names(check_vol3.df)
(frac_vol1.df = subset(check_vol3.df, select = -Country)/subset(cashless_vol3.df, select = -Country))
(frac_vol2.df = data.frame("Country"=check_vol3.df$Country, frac_vol1.df))
#
#construct vector for labeling near dots
(frac_vol_labels = round(frac_vol2.df$X2019, digits = 3))
(frac_vol_labels = 100*frac_vol_labels)
(frac_vol_labels = paste0(frac_vol_labels, "%"))

ggplot(frac_vol2.df, aes(x = X2019, y=reorder(Country, X2019)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.08, 0.01), limits = c(0,0.08))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in total cashless payments (by volume)")+
  ylab("Country")+
  geom_text(aes(label=frac_vol_labels), hjust= -0.2, colour="black", size=5)

### 2019 Value graph (combine 2 tables into %)
check_val2.df
cashless_val2.df
# delete Switzerland (no data on volume) & Singapore (very high value)
(check_val3.df =  check_val2.df[-c(18,22), ])
(cashless_val3.df =  cashless_val2.df[-c(18,22), ])
# divide check by cashless (get fraction)
names(check_val3.df)
(frac_val1.df = subset(check_val3.df, select = -Country)/subset(cashless_val3.df, select = -Country))
(frac_val2.df = data.frame("Country"=check_val3.df$Country, frac_val1.df))
dim(frac_val2.df)
#
#construct vector for labeling near dots
#(frac_val_labels = digits(frac_val2.df$X2019, 2))
(frac_val_labels = round(frac_val2.df$X2019, digits = 3))
(frac_val_labels = 100*frac_val_labels)
(frac_val_labels = paste0(frac_val_labels, "%"))

ggplot(frac_val2.df, aes(x = X2019, y=reorder(Country, X2019)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.4, 0.05), limits = c(0,0.4))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in all cashless payments (by value)")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels), hjust= -0.2, colour="black", size=5)




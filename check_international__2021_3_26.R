# check_SPR_2021_3_26.R removing Germany, Japan, and Netherlands
# check_SPR_2021_1_23_all_countries.R Check International comparison
## libraries
#library(xtable)
library(ggplot2); theme_set(theme_bw())
#library(scales)# for $ or % (not needed, use paste0 instead)
library(tidyr)# for gather (changing wide to long)

setwd("~/Papers/Check_SPR_2020/check_SPR_coding")
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

### 2019 Volume graph (combine 2 tables into %) fig 1
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

# revision: Deleting German, Japan, Netherlands
frac_vol2.df
dim(frac_vol2.df)
(frac_vol3.df = subset(frac_vol2.df, Country != "Germany" & Country != "Japan" & Country != "Netherlands"))# No Japan in Volume, so I lose only 2 countries
dim(frac_vol3.df)
length(frac_vol_labels)# Remove Germany and Japan 
(frac_vol_labels3.vec = round(frac_vol3.df$X2019, digits = 3))
(frac_vol_labels3.vec = 100*frac_vol_labels3.vec)
(frac_vol_labels3.vec = paste0(frac_vol_labels3.vec, "%"))
length(frac_vol_labels3.vec)

ggplot(frac_vol3.df, aes(x = X2019, y=reorder(Country, X2019)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.08, 0.01), limits = c(0,0.08))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in total cashless payments (by volume)")+
  ylab("Country")+
  geom_text(aes(label=frac_vol_labels3.vec), hjust= -0.2, colour="black", size=5)
# To export as jpeg, resize x to 1000 (maintain aspect ratio)
dim(frac_vol3.df)# num of countries for caption

### 2019 Value graph (combine 2 tables into %) fig 2
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

# revision: Deleting German, Japan, Netherlands
(frac_val3.df = subset(frac_val2.df, Country != "Germany" & Country != "Japan" & Country != "Netherlands"))# 
dim(frac_val3.df)
#
#construct vector for labeling near dots
#(frac_val_labels = digits(frac_val2.df$X2019, 2))
(frac_val_labels3.vec = round(frac_val3.df$X2019, digits = 3))
(frac_val_labels3.vec = 100*frac_val_labels3.vec)
(frac_val_labels3.vec = paste0(frac_val_labels3.vec, "%"))

ggplot(frac_val3.df, aes(x = X2019, y=reorder(Country, X2019)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.4, 0.05), limits = c(0,0.4))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in all cashless payments (by value)")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels3.vec), hjust= -0.2, colour="black", size=5)

dim(frac_val3.df)# num of countries for the caption fig 2

### 2015-2019 Volume graph (% time series, ts_high, of high 6 countries) Table 1
str(frac_vol3.df)
frac_vol3.df
frac_vol4.df = frac_vol3.df
rownames(frac_vol4.df)
(rownames(frac_vol4.df) = frac_vol4.df$Country)
colnames(frac_vol4.df)
(frac_vol5.df = subset(frac_vol4.df, select = -Country))

# transpose to have Countries as columns
(frac_vol6.df = as.data.frame(t(frac_vol5.df)))
colnames(frac_vol6.df)
row.names(frac_vol6.df)
dim(frac_vol6.df)
str(frac_vol6.df)
#
# selecting 6 countries with high use in 2019
(frac_vol_high1.df = frac_vol6.df[, c("United States", "France", "Mexico", "India", "Canada", "Argentina")])
str(frac_vol_high1.df)

# adding column year as integer
frac_vol_high2.df = frac_vol_high1.df
(frac_vol_high2.df$Year = c(2015:2019))
str(frac_vol_high2.df)

# making it long
frac_vol_high3.df = gather(frac_vol_high2.df, key = "Country", value = "Value", -Year)
str(frac_vol_high3.df)
# I turn Country into a factor for ggplot
frac_vol_high3.df$Country = factor(frac_vol_high3.df$Country, levels = c("United States", "France", "Mexico", "India", "Canada", "Argentina"))

ggplot(frac_vol_high3.df, aes(x = Year, y = Value, col = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = Country)) +
  scale_x_continuous(breaks = 2015:2019) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.7, 0.8),
        text = element_text(size = 20)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = seq(0,0.16,0.01)) +
  ylab("Percentage of checks in total cashless payments") +
  scale_color_manual(values = c("red", "black", "blue", "violetred4", "deeppink", "darkgreen"))+ 
  scale_shape_manual(values=c(0:6))

# selecting 6 countries with low use in 2019
(frac_vol_low1.df = frac_vol6.df[, c("Italy", "Brazil", "United Kingdom", "Spain", "Austria", "Korea")])
str(frac_vol_low1.df)

# adding column year as integer
frac_vol_low2.df = frac_vol_low1.df
(frac_vol_low2.df$Year = c(2015:2019))
str(frac_vol_low2.df)

# making it long
frac_vol_low3.df = gather(frac_vol_low2.df, key = "Country", value = "Value", -Year)
str(frac_vol_low3.df)
# I turn Country into a factor for ggplot
frac_vol_low3.df$Country = factor(frac_vol_low3.df$Country, levels = c("Italy", "Brazil", "United Kingdom", "Spain", "Austria", "Korea"))

ggplot(frac_vol_low3.df, aes(x = Year, y = Value, col = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = Country)) +
  scale_x_continuous(breaks = 2015:2019) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.8, 0.8),
        text = element_text(size = 20)) +  
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = seq(0,0.04,0.005)) +
  ylab("Percentage of checks in total cashless payments") +
  scale_color_manual(values = c("red", "black", "blue", "violetred4", "deeppink", "darkgreen"))+ 
  scale_shape_manual(values=c(0:6))

### computing growth rates volume and value for 12 countries displayed in Figures 3 and 4 (time series vol and val)
(check_growth1.df = check_vol3.df)
#(check_growth2.df = check_growth1.df[check_growth1.df$Country %in% c("United States", "France", "Mexico", "India", "Canada", "Argentina", "Italy", "Brazil", "United Kingdom", "Spain", "Austria", "Korea") ,])
#str(check_growth2.df)
#check_growth3.df = droplevels(check_growth2.df)
check_growth3.df = check_growth1.df# try 25 countries (not just 12 countries)
str(check_growth3.df)
dim(check_growth3.df)
rownames(check_growth3.df) = 1:22
#
# Remove Netherlands, Russia, and Sweden (0 volume)
# Revision: Remove Germany, Japan. 
check_growth3.df
(check_growth4.df = subset(check_growth3.df, Country != "Germany" & Country != "Japan" & Country != "Netherlands" & Country != "Russia" & Country != "Sweden"))
dim(check_growth4.df)
rownames(check_growth4.df)=1:18
str(check_growth4.df)
(check_growth5.df = droplevels(check_growth4.df))
dim(check_growth5.df)
#
# constructing vector with 2019 population size (millions)
pop.vec = c(44.9, 8.8, 11.5, 211.0, 37.6, 1395, 67.1, 1366.0, 270.6, 60.4, 51.7, 127.6, 34.3, 58.6, 46.9, 82.0, 66.6, 328.2)
length(pop.vec)
check_growth7.df =  check_growth5.df
check_growth7.df$Population = pop.vec
str(check_growth7.df)
check_growth7.df
# Fix US to billions
check_growth8.df =  check_growth7.df
check_growth8.df[check_growth8.df$Country=="United States", c(2:6)] = 1000* check_growth8.df[check_growth8.df$Country=="United States", c(2:6)]
check_growth8.df
#
# Adding column with CAGR
check_growth9.df = check_growth8.df
check_growth9.df$CAGR = CAGR_formula(check_growth9.df$X2019, check_growth9.df$X2015, 4)
check_growth9.df
dim(check_growth9.df)
(check_growth10.df =  check_growth9.df[, c(1,2,3,4,5,6,8,7)])
(check_growth10.df$CAGR = paste0(round(100*check_growth10.df$CAGR,1),"%"))
check_growth10.df
dim(check_growth10.df)
# remove China (all zeros)
(check_growth11.df =  subset(check_growth10.df, Country != "China"))
dim(check_growth11.df)# num countries for caption
#
# exporting to text to be read by word
write.table(format(check_growth11.df), file = "check_growth.txt", sep = ",", quote = F, row.names = F)

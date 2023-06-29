#======= Built by Oz Shy =======================================================
# Modified by Antar Diallo, 06/16/2023
#===============================================================================

rm(list=ls())

library(tidyverse)
# check_SPR_2021_3_26.R removing Germany, Japan, and Netherlands
# check_SPR_2021_1_23_all_countries.R Check International comparison
## libraries
#library(xtable)
library(ggplot2); theme_set(theme_bw())
#library(scales)# for $ or % (not needed, use paste0 instead)
library(tidyr)# for gather (changing wide to long)
library(gridExtra)
library(readxl)

setwd("~/Papers/check_international/code_data")
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

# Function to be able to save the charts

output_height <- 7
output_width <- 12
output_res <- 400


dev_save <- function(filename) {
  dev.copy(
    jpeg,
    paste0(".", filename, ".jpeg"),
    width = output_width,
    height = output_height,
    units = "in",
    res = output_res
  )
  dev.off()
  
}

options(scipen = 9999999999999)

# Run this code

cashless_vol2.df <- read_xlsx("country_cashless_payment_volume.xlsx")
cashless_val2.df <- read_xlsx("country_cashless_payment_value.xlsx")
check_vol2.df <- read_xlsx("country_check_volume.xlsx")
check_val2.df = read_xlsx("country_check_value.xlsx")
ratio_value_volume <- read_xlsx("ratio_value_volume_ppp_rate.xlsx")


### 2020 Volume graph (combine 2 tables into %) fig 1
check_vol2.df
cashless_vol2.df
# delete China, Netherland, Russia, Sweden, Switzerland (no data on volume), Singapore
(check_vol3.df =  check_vol2.df[-c(6, 15, 16, 21, 22), ])
(cashless_vol3.df =  cashless_vol2.df[-c(6, 15, 16, 21, 22), ])

(check_val3.df =  check_val2.df[-c(6, 15, 16, 21, 22), ])
(cashless_val3.df =  cashless_val2.df[-c(6, 15, 16, 21, 22), ])

ratio_val_vol <- ratio_value_volume[-c(6, 15, 16, 21, 22), ]

# divide check by cashless (get fraction)
#names(check_vol3.df)
#frac_vol1.df = subset(check_vol3.df, select = -Country,)/subset(cashless_vol3.df, select = -Country) # issue with this formula

# I remade the code
frac.vol1.df <- (check_vol3.df %>%
                   dplyr:: select(`2012`:`2021`) %>%
                   dplyr:: mutate_if(is.character, as.numeric)) / (cashless_vol3.df %>%
                                                                     dplyr:: select(`2012`:`2021`)%>%
                                                                     dplyr:: mutate_if(is.character,as.numeric))

frac_vol2.df = data.frame("Country"=check_vol3.df$Country, frac.vol1.df)
#
#construct vector for labeling near dots
(frac_vol_labels = round(frac_vol2.df$X2019, digits = 3))
(frac_vol_labels = 100*frac_vol_labels)
(frac_vol_labels = paste0(frac_vol_labels, "%"))

# revision: Deleting German, Japan, Netherlands
frac_vol2.df
dim(frac_vol2.df)
#(frac_vol3.df = subset(frac_vol2.df, Country != "Germany" & Country != "Netherlands"))# No Japan in Volume, so I lose only 2 countries
#dim(frac_vol3.df)
#length(frac_vol_labels)# Remove Germany and Japan 

# Build for 2021
(frac_vol_labels21.vec = round(frac_vol2.df$X2021, digits = 3))
(frac_vol_labels21.vec = 100*frac_vol_labels21.vec)
(frac_vol_labels21.vec = paste0(frac_vol_labels21.vec, "%"))
length(frac_vol_labels21.vec)

# Build for 2020
(frac_vol_labels3.vec = round(frac_vol2.df$X2020, digits = 3))
(frac_vol_labels3.vec = 100*frac_vol_labels3.vec)
(frac_vol_labels3.vec = paste0(frac_vol_labels3.vec, "%"))
length(frac_vol_labels3.vec)

#Build for 2017
(frac_vol_labels17.vec = round(frac_vol2.df$X2017, digits = 3))
(frac_vol_labels17.vec = 100*frac_vol_labels17.vec)
(frac_vol_labels17.vec = paste0(frac_vol_labels17.vec, "%"))
length(frac_vol_labels17.vec)

#build for 2015
(frac_vol_labels15.vec = round(frac_vol2.df$X2015, digits = 3))
(frac_vol_labels15.vec = 100*frac_vol_labels15.vec)
(frac_vol_labels15.vec = paste0(frac_vol_labels15.vec, "%"))
length(frac_vol_labels15.vec)

#build for 2012
(frac_vol_labels12.vec = round(frac_vol2.df$X2012, digits = 3))
(frac_vol_labels12.vec = 100*frac_vol_labels12.vec)
(frac_vol_labels12.vec = paste0(frac_vol_labels12.vec, "%"))
length(frac_vol_labels12.vec)


#Plot for 2012
vol12 <- ggplot(frac_vol2.df, aes(x = X2012, y=reorder(Country, X2012)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.5, 0.05), limits = c(0,0.5))+#,
  #labels = scales::percent)+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in total cashless payments in 2012 (by volume)")+
  ylab("Country")+
  geom_text(aes(label=frac_vol_labels12.vec), hjust= -0.2, colour="black", size=5)
vol12
dev_save("vol12")


#Plot for 2015
vol1 <- ggplot(frac_vol2.df, aes(x = X2015, y=reorder(Country, X2015)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.16, 0.03), limits = c(0,0.16))+#,
                     #labels = scales::percent)+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in total cashless payments in 2015 (by volume)")+
  ylab("Country")+
  geom_text(aes(label=frac_vol_labels15.vec), hjust= -0.2, colour="black", size=5)
vol1
dev_save("vol1")

#Plot for 2017
vol2 <- ggplot(frac_vol2.df, aes(x = X2017, y=reorder(Country, X2017)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.12, 0.03), limits = c(0,0.12))+#,
  #labels = scales::percent)+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in total cashless payments in 2017 (by volume)")+
  ylab("Country")+
  geom_text(aes(label=frac_vol_labels17.vec), hjust= -0.2, colour="black", size=5)
vol2
dev_save("vol2")

#Plot for 2020
vol3 <- ggplot(frac_vol2.df, aes(x = X2020, y=reorder(Country, X2020)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.08, 0.01), limits = c(0,0.08))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in total cashless payments in 2020 (by volume)")+
  ylab("Country")+
  geom_text(aes(label=frac_vol_labels3.vec), hjust= -0.2, colour="black", size=5)
vol3
dev_save("vol3")

#Plot for 2021
vol21 <- ggplot(frac_vol2.df, aes(x = X2021, y=reorder(Country, X2021)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.055, 0.01), limits = c(0,0.055))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in total cashless payments in 2021 (by volume)")+
  ylab("Country")+
  geom_text(aes(label=frac_vol_labels21.vec), hjust= -0.2, colour="black", size=5)
vol21
dev_save("vol21")


# Making a grid
figure.grid1 <- gridExtra::grid.arrange(vol1, vol2,
                        vol3,
                        ncol=2)
figure.grid1
dev_save("figure.grid1")

# Making another grid
figure.grid_vol12_21 <- gridExtra::grid.arrange(vol12, vol21,
                                        nrow=2)
figure.grid_vol12_21
dev_save("figure.grid_vol12_21")

# To export as jpeg, resize x to 1000 (maintain aspect ratio)
dim(frac_vol2.df)# num of countries for caption

### 2019 Value graph (combine 2 tables into %) fig 2
check_val2.df
cashless_val2.df
# delete Switzerland (no data on volume) & Singapore (very high value)
#(check_val3.df =  check_val2.df[-c(18,22), ])
#(cashless_val3.df =  cashless_val2.df[-c(18,22), ])
# divide check by cashless (get fraction)
names(check_val3.df)
#(frac_val1.df = subset(check_val3.df, select = -Country)/subset(cashless_val3.df, select = -Country)) # Still have an issue with this line
    # I used this instead
frac.val2.df <- (check_val3.df %>%
                   dplyr:: select(`2012`:`2021`) %>%
                   dplyr:: mutate_if(is.character, as.numeric)) / (cashless_val3.df %>%
                                                                     dplyr:: select(`2012`:`2021`)%>%
                                                                     dplyr:: mutate_if(is.character,as.numeric))


(frac_val2.df = data.frame("Country"=check_val3.df$Country, frac.val2.df))
dim(frac_val2.df)

# revision: Deleting German, Japan, Netherlands
#(frac_val3.df = subset(frac_val2.df, Country != "Germany" & Country != "Japan" & Country != "Netherlands"))# 
#dim(frac_val3.df)
#
#construct vector for labeling near dots
#(frac_val_labels = digits(frac_val2.df$X2019, 2))

#Build for 2012
(frac_val_labels12.vec = round(frac_val2.df$X2012, digits = 3))
(frac_val_labels12.vec = 100*frac_val_labels12.vec)
(frac_val_labels12.vec = paste0(frac_val_labels12.vec, "%"))

# Chart for 2012

val12 <- ggplot(frac_val2.df, aes(x = X2012, y=reorder(Country, X2012)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.8, 0.05), limits = c(0,0.8))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in all cashless payments in 2012 (by value)")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels12.vec), hjust= -0.2, colour="black", size=5)
val12
dev_save("val12")

#Build for 2015
(frac_val_labels3.vec = round(frac_val2.df$X2015, digits = 3))
(frac_val_labels3.vec = 100*frac_val_labels3.vec)
(frac_val_labels3.vec = paste0(frac_val_labels3.vec, "%"))

# Chart for 2015

val1 <- ggplot(frac_val2.df, aes(x = X2015, y=reorder(Country, X2015)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.5, 0.05), limits = c(0,0.5))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in all cashless payments in 2015 (by value)")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels3.vec), hjust= -0.2, colour="black", size=5)
val1
dev_save("val1")

#Build for 2017
(frac_val_labels17.vec = round(frac_val2.df$X2017, digits = 3))
(frac_val_labels17.vec = 100*frac_val_labels17.vec)
(frac_val_labels17.vec = paste0(frac_val_labels17.vec, "%"))

val2 <- ggplot(frac_val2.df, aes(x = X2017, y=reorder(Country, X2017)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.45, 0.05), limits = c(0,0.45))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in all cashless payments in 2017 (by value)")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels17.vec), hjust= -0.2, colour="black", size=5)
val2
dev_save("val2")

#Build for 2020
(frac_val_labels20.vec = round(frac_val2.df$X2020, digits = 3))
(frac_val_labels20.vec = 100*frac_val_labels20.vec)
(frac_val_labels20.vec = paste0(frac_val_labels20.vec, "%"))

val3 <- ggplot(frac_val2.df, aes(x = X2020, y=reorder(Country, X2020)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.35, 0.05), limits = c(0,0.35))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in all cashless payments in 2020 (by value)")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels20.vec), hjust= -0.2, colour="black", size=5)
val3
dev_save("val3")

#Build for 2021
(frac_val_labels21.vec = round(frac_val2.df$X2021, digits = 3))
(frac_val_labels21.vec = 100*frac_val_labels21.vec)
(frac_val_labels21.vec = paste0(frac_val_labels21.vec, "%"))

val21 <- ggplot(frac_val2.df, aes(x = X2021, y=reorder(Country, X2021)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0, 0.35, 0.05), limits = c(0,0.35))+
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Fraction of checks in all cashless payments in 2021 (by value)")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels21.vec), hjust= -0.2, colour="black", size=5)
val21
dev_save("val21")

#Making a grid
figure.grid2 <- gridExtra::grid.arrange(val1, val2,
                                        val3,
                                        ncol=2)
figure.grid2
dev_save("figure.grid2")

#Making another grid
figure.grid_val12_21 <- gridExtra::grid.arrange(val12, val21,
                                        nrow=2)
figure.grid_val12_21
dev_save("figure.grid_val12_21")


# Making the plot for ratio of value volume in U.S. dollars for 2012

(frac_val_labels_ratio12.vec = round(ratio_val_vol$`2012`, digits = 0))
#(frac_val_labels21.vec = 100*frac_val_labels21.vec)
(frac_val_labels_ratio12.vec = paste0(frac_val_labels_ratio12.vec, " USD"))

ratio12 <- ggplot(ratio_val_vol, aes(x = `2012`, y=reorder(Country, `2012`)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0L, 95000L, 10000L), limits = c(0L,95000L))+ #, limits = c(0L,100000L)
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Average dollars value by check, 2012")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels_ratio12.vec), hjust= -0.2, colour="black", size=5)
ratio12
dev_save("ratio12")

# Making the plot for ratio of value volume in U.S. dollars for 2021

(frac_val_labels_ratio21.vec = round(ratio_val_vol$`2021`, digits = 0))
#(frac_val_labels21.vec = 100*frac_val_labels21.vec)
(frac_val_labels_ratio21.vec = paste0(frac_val_labels_ratio21.vec, " USD"))

ratio21 <- ggplot(ratio_val_vol, aes(x = `2021`, y=reorder(Country, `2021`)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0L, 120000L, 10000L), limits = c(0,120000L))+ #, limits = c(0,120)
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size=16, color = "black"), text = element_text(size=20))+
  xlab("Average dollars value by check, 2021")+
  ylab("Country")+
  geom_text(aes(label=frac_val_labels_ratio21.vec), hjust= -0.2, colour="black", size=5)
ratio21
dev_save("ratio21")

# Make another grid

figure.grid3 <- gridExtra::grid.arrange(ratio12, ratio21,
                                        nrow=2)
figure.grid3
dev_save("figure.grid3")

#===============================================================================

dim(frac_val2.df)# num of countries for the caption fig 2

### 2012-2021 Volume graph (% time series, ts_high, of high 6 countries) Table 1
str(frac_vol2.df)
frac_vol2.df
frac_vol4.df = frac_vol2.df
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
# selecting 10 countries with high use in 2021
(frac_vol_high1.df = frac_vol6.df[, c("United States", "France",
                                      "Mexico", "India",
                                      "Canada", "Argentina",
                                      "Italy", "Brazil",
                                      "United Kingdom", "Korea")])
str(frac_vol_high1.df)

# adding column year as integer
frac_vol_high2.df = frac_vol_high1.df
(frac_vol_high2.df$Year = c(2012:2021))
str(frac_vol_high2.df)

# making it long
frac_vol_high3.df = gather(frac_vol_high2.df, key = "Country", value = "Value", -Year)
str(frac_vol_high3.df)
# I turn Country into a factor for ggplot
frac_vol_high3.df$Country = factor(frac_vol_high3.df$Country, levels = c("United States", "France",
                                                                         "Mexico", "India",
                                                                         "Canada", "Argentina",
                                                                         "Italy", "Brazil",
                                                                         "United Kingdom", "Korea"))

ts1 <- ggplot(frac_vol_high3.df, aes(x = Year, y = Value, col = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = Country)) +
  scale_x_continuous(breaks = 2012:2021) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.7, 0.7),
        text = element_text(size = 20)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = seq(0,0.5,0.05)) +
  ylab("Percentage of checks in total cashless payments") +
  scale_color_manual(values = c("red", "black", "blue", "violetred4", "deeppink",
                                "darkgreen", "maroon", "yellow", "orange", "darkgray"))+ 
  scale_shape_manual(values=c(0:10))
ts1
dev_save('ts1')

# selecting 10 countries with low use in 2020
(frac_vol_low1.df = frac_vol6.df[, c("Austria", "Singapore",
                                     "Saudi Arabia", "South Africa",
                                     "Spain", "Japan",
                                     "Turkey", "Indonesia",
                                     "Belgium", "Germany")])
str(frac_vol_low1.df)

# adding column year as integer
frac_vol_low2.df = frac_vol_low1.df
(frac_vol_low2.df$Year = c(2012:2021))
str(frac_vol_low2.df)

# making it long
frac_vol_low3.df = gather(frac_vol_low2.df, key = "Country", value = "Value", -Year)
str(frac_vol_low3.df)
# I turn Country into a factor for ggplot
frac_vol_low3.df$Country = factor(frac_vol_low3.df$Country, levels = c("Austria", "Singapore",
                                                                       "Saudi Arabia", "South Africa",
                                                                       "Spain", "Japan",
                                                                       "Turkey", "Indonesia",
                                                                       "Belgium", "Germany"))

ts2 <- ggplot(frac_vol_low3.df, aes(x = Year, y = Value, col = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = Country)) +
  scale_x_continuous(breaks = 2012:2021) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.7, 0.7),
        text = element_text(size = 20)) +  
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = seq(0,0.1,0.005)) +
  ylab("Percentage of checks in total cashless payments") +
  scale_color_manual(values = c("red", "black", "blue", "violetred4", "deeppink",
                                "darkgreen", "maroon", "yellow", "orange", "darkgray"))+ 
  scale_shape_manual(values=c(0:10))
ts2
dev_save("ts2")


# Let's try it with checks values from 2012 to 2021

### 2012-2021 Value graph (% time series, ts_high, of high 10 countries) Table 1
str(frac_val2.df)
frac_val2.df
frac_val4.df = frac_val2.df
rownames(frac_val4.df)
(rownames(frac_val4.df) = frac_val4.df$Country)
colnames(frac_val4.df)
(frac_val5.df = subset(frac_val4.df, select = -Country))

# transpose to have Countries as columns
(frac_val6.df = as.data.frame(t(frac_val5.df)))
colnames(frac_val6.df)
row.names(frac_val6.df)
dim(frac_val6.df)
str(frac_val6.df)

# selecting 10 countries with high use in 2021 in value
(frac_val_high1.df = frac_val6.df[, c("Canada", "Singapore",
                                      "United States", "India",
                                      "Korea", "Argentina",
                                      "Japan", "Turkiye",
                                      "Italy", "Austria")])
str(frac_val_high1.df)

# adding column year as integer
frac_val_high2.df = frac_val_high1.df
(frac_val_high2.df$Year = c(2012:2021))
str(frac_val_high2.df)

# making it long
frac_val_high3.df = gather(frac_val_high2.df, key = "Country", value = "Value", -Year)
str(frac_val_high3.df)
# I turn Country into a factor for ggplot
frac_val_high3.df$Country = factor(frac_val_high3.df$Country, levels = c("Canada", "Singapore",
                                                                         "United States", "India",
                                                                         "Korea", "Argentina",
                                                                         "Japan", "Turkiye",
                                                                         "Italy", "Austria"))

ts3 <- ggplot(frac_val_high3.df, aes(x = Year, y = Value, col = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = Country)) +
  scale_x_continuous(breaks = 2012:2021) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.9, 0.7),
        text = element_text(size = 20)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = seq(0,1,0.1)) +
  ylab("Percentage of checks in total cashless payments") +
  scale_color_manual(values = c("red", "black", "blue", "violetred4", "deeppink",
                                "darkgreen", "maroon", "yellow", "orange", "darkgray"))+ 
  scale_shape_manual(values=c(0:10))
ts3
dev_save("ts3")

##############

### 2012-2021 Value graph (% time series, ts_high, of low 10 countries) Table 1

# selecting 10 countries with high use in 2021
(frac_val_low1.df = frac_val6.df[, c("Spain", "France",
                                      "Mexico", "Brazil",
                                      "Saudi Arabia", "United Kingdom",
                                      "Indonesia", "Germany",
                                      "Belgium", "South Africa")])
str(frac_val_low1.df)

# adding column year as integer
frac_val_low2.df = frac_val_low1.df
(frac_val_low2.df$Year = c(2012:2021))
str(frac_val_low2.df)

# making it long
frac_val_low3.df = gather(frac_val_low2.df, key = "Country", value = "Value", -Year)
str(frac_val_low3.df)
# I turn Country into a factor for ggplot
frac_val_low3.df$Country = factor(frac_val_low3.df$Country, levels = c("Spain", "France",
                                                                       "Mexico", "Brazil",
                                                                       "Saudi Arabia", "United Kingdom",
                                                                       "Indonesia", "Germany",
                                                                       "Belgium", "South Africa"))

ts4 <- ggplot(frac_val_low3.df, aes(x = Year, y = Value, col = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = Country)) +
  scale_x_continuous(breaks = 2012:2021) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.8, 0.7),
        text = element_text(size = 20)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = seq(0,1,0.1)) +
  ylab("Percentage of checks in total cashless payments") +
  scale_color_manual(values = c("red", "black", "blue", "violetred4", "deeppink",
                                "darkgreen", "maroon", "yellow", "orange", "darkgray"))+ 
  scale_shape_manual(values=c(0:10))
ts4
dev_save("ts4")


### computing growth rates volume and value for 12 countries displayed in Figures 3 and 4 (time series vol and val)
(check_growth1.df = check_vol3.df)
#(check_growth2.df = check_growth1.df[check_growth1.df$Country %in% c("United States", "France", "Mexico", "India", "Canada", "Argentina", "Italy", "Brazil", "United Kingdom", "Spain", "Austria", "Korea") ,])
#str(check_growth2.df)
#check_growth3.df = droplevels(check_growth2.df)
check_growth3.df = check_growth1.df# try 25 countries (not just 12 countries)
str(check_growth3.df)
dim(check_growth3.df)
rownames(check_growth3.df) = 1:20
#
# Remove Netherlands, Russia, and Sweden (0 volume)
# Revision: Remove Germany, Japan. 
check_growth3.df
#(check_growth4.df = subset(check_growth3.df, Country != "Germany" & Country != "Japan" & Country != "Netherlands" & Country != "Russia" & Country != "Sweden"))
#dim(check_growth4.df)
#rownames(check_growth4.df)=1:18
#str(check_growth4.df)
#(check_growth5.df = droplevels(check_growth4.df))
#dim(check_growth5.df)
#
# constructing vector with 2019 population size (millions)
pop.vec = c(44.9, 8.8, 11.5, 211.0, 37.6,
            67.8, 83.2, 1408.0, 273.8, 59.1,
            125.7, 51.7, 126.7, 36.0, 5.5,
            59.4, 47.4, 84.8, 67.3, 331.9)
length(pop.vec)
check_growth7.df <- read_xlsx("country_check_volume_else.xlsx") # This file has already US in billion
check_growth7.df <- check_growth7.df[-c(6, 15, 16, 21, 22), ]
check_growth7.df$Population = pop.vec
str(check_growth7.df)
check_growth7.df
# Fix US to billions
check_growth8.df =  check_growth7.df
#check_growth8.df[check_growth8.df$Country=="United States", c(2:11)] = 1000*check_growth8.df[check_growth8.df$Country=="United States", c(2:11)]
#check_growth8.df <- check_growth7.df

# Adding column with CAGR
check_growth9.df = check_growth8.df
check_growth9.df$CAGR = CAGR_formula(as.numeric(check_growth9.df$`2021`), as.numeric(check_growth9.df$`2012`), 10)
check_growth9.df
dim(check_growth9.df)
(check_growth10.df =  check_growth9.df[, c(1,2,3,4,5,6,7,9,8, 9, 10, 11, 12, 13)])
(check_growth10.df$CAGR = paste0(round(100*check_growth10.df$CAGR,1),"%"))
check_growth10.df
dim(check_growth10.df)
# remove China (all zeros)
#(check_growth11.df =  subset(check_growth10.df, Country != "China"))
#dim(check_growth11.df)# num countries for caption
#
# exporting to text to be read by word
write.table(format(check_growth10.df), file = "check_growth.txt", sep = ",", quote = F, row.names = F)

write.csv(check_growth10.df, "check_growth.csv")

#===============================================================================



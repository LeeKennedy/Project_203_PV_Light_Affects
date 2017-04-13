# Environment clean ------------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(MASS)
library(psych)
library(ggplot2)
library(pwr)
library(readxl)
library(dplyr)
library(readr)
library(dts.quality)

# Data in ----------------------------------------------------------------

test <- "PVAL06"

#data.in <- read_excel(paste("~/Documents/GitHub/Project_203_PV_Light_Affects/data/",test," comparisons.xlsx", sep=""), 
#                      skip = 2)

data.in <- read_excel(paste("Y:/Validation and Verification of methods/Chemistry/Project 203 - Investigation of the PV Room Lighting/",test," comparisons.xlsx", sep=""), 
                     skip = 2)

colnames(data.in)[4] <- "Without_Light"
colnames(data.in)[5] <- "With_Light"
data.in <- data.in[,c(1:5)]
data.in[,c(4,5)] <- sapply(data.in[,c(4,5)], as.numeric)

# Comparison Plot -------------------------------------------------------

data.in <- data.in %>% filter(Without_Light <25)

pv_plot <- ggplot(data.in, aes(x=Without_Light, y=With_Light)) +
  geom_point(size=4, shape = 21, col = "black", fill = "cornflowerblue") +
  geom_abline(slope = 1, intercept = 0, lty=2, col = "red")+
  labs(x="Subdued Light Result", y = "Normal Light Result", title = "PV Comparison", subtitle = test) +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
pv_plot


# Create differences ----------------------------------------------------


data.in

data.in$light_difference =  data.in$With_Light - data.in$Without_Light


# Boxplot differences - looking for outliers ----------------------------
boxplot(data.in$light_difference, 
        main = "Boxplot of differences",
        ylab = "Result differences",
        col = "cornflowerblue")

# Remove outliers ---------------------------------------------------------
data.in <- data.in %>%

  filter(abs(light_difference) < 2)


describe(data.in$light_difference)




# plot differences against concentration ---------------------------------

diff_plot <- ggplot(data.in, aes(x=Without_Light, y = light_difference)) +
  geom_point(size=4, shape=21, col="black", fill="cornflowerblue") +
  geom_segment(aes(xend=Without_Light, yend=0)) +
  geom_hline(yintercept=0) + 
  labs(x="Peroxide Value",
       y ="Effect of Light",
       title = "Light effect on PV Result") +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
  
diff_plot





#Perform a paired t test -------------------------------------------------
t.test(data.in$Without_Light,data.in$With_Light,paired = TRUE)

#Interpret results
#All assumptions required were satisfied
#There were no outliers, data was normally distributed and the t test had adequate power
#The difference in weight before and after treatment was statistically significant at 5% LOs. 



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

test <- "PVAL05"

data.in <- read_excel(paste("H:/GitHub Projects/Project_203_PV_Light_Affects/data/",test," comparisons.xlsx", sep =""), 
                      sheet = "Sheet1", skip = 2)
colnames(data.in)[4] <- "Without_Light"
colnames(data.in)[5] <- "With_Light"

# Inspect data -----------------------------------------------------------
head(data.in)
data.in <- data.in[,c(1:5)]
data.in[,c(4,5)] <- sapply(data.in[,c(4,5)], as.numeric)

# Create differences ----------------------------------------------------
data.in$light_difference = data.in$With_Light - data.in$Without_Light

# Boxplot differences - looking for outliers ----------------------------
boxplot(data.in$light_difference, 
        main = "Boxplot of differences",
        ylab = "Result differences",
        col = "cornflowerblue")

# Remove outlier ---------------------------------------------------------
#data.in <- data.in %>%
#  mutate(light_difference_2 = outliers(light_difference))

data.in$light_difference_2 <- data.in$light_difference

boxplot(data.in$light_difference_2, 
        main = "Boxplot of differences",
        ylab = "Result differences",
        col = "cornflowerblue")

summary(data.in$light_difference_2)
describe(data.in$light_difference_2)


# Plot histogram with density curve --------------------------------------
ggplot(data.in,aes(x=light_difference_2)) + 
        geom_histogram(aes(y=..density..),binwidth = 0.05) + 
        stat_function(fun = dnorm, 
                      colour = "blue",
                      args = list(mean = mean(data.in$light_difference), 
                                  sd = sd(data.in$light_difference))) + 
        scale_x_continuous(name="Result differences") + 
        ggtitle("Histogram of result differences with and without light exposure")

#Test if the weight differences are normally distributed -----------------
shapiro.test(data.in$light_difference)

# Perform a power analysis to check the sample size has adequate power----
# to detect a difference if it exists-------------------------------------
# Can the test find a difference of 0.5sd (d)?---------------------------- 

pwr.t.test(n=26,d=0.5,sig.level = 0.05,type = c("paired"))

#Perform a paired t test -------------------------------------------------
t.test(data.in$Without_Light,data.in$With_Light,paired = TRUE)

#Interpret results
#All assumptions required were satisfied
#There were no outliers, data was normally distributed and the t test had adequate power
#The difference in weight before and after treatment was statistically significant at 5% LOs. 

# TOST Test --------------------------------------------------------------

t_test_df <- data.frame(
  ttest = numeric(),
  TOST = numeric()
)

colnames(data.in)[4] <- "A"
colnames(data.in)[5] <- "B"
data.in[,c(4:5)] <- sapply(data.in[,c(4:5)], as.numeric)
data.in <- na.omit(data.in[,c(1:5)])

n <- nrow(data.in)

for (i in 1:(n-1)) {
  
  data1 <- data.in[1:(i+1),]
  
  r1 <- t.test(data1$A, data1$B)
  t_test_df[i,1] = r1$p.value
  
  r2 <- tost(data1$A, data1$B, 0.7)
  t_test_df[i,2] = r2$tost.p.value
}

t_test_df$row_n <- as.numeric(rownames(t_test_df))

plot2t <- ggplot(t_test_df, aes(row_n)) +
  geom_point(aes(y=ttest), size=4, shape=21, colour = "black", fill = "cornflowerblue") +
  geom_point(aes(y=TOST), size=4, shape=21, colour = "darkgreen", fill = "beige") +
  geom_hline(yintercept = 0.05, lty=2,col = "red") +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
plot2t

r1
r2

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

test <- "PVAL02"

data.in <- read_excel(paste("~/Documents/GitHub/Project_203_PV_Light_Affects/data/",test," comparisons.xlsx", sep=""), 
                      skip = 2)
colnames(data.in)[4] <- "Without_Light"
colnames(data.in)[5] <- "With_Light"
data.in <- data.in[,c(1:5)]
data.in[,c(4,5)] <- sapply(data.in[,c(4,5)], as.numeric)

# Comparison Plot -------------------------------------------------------

ggplot(data.in, aes(x=Without_Light, y=With_Light)) +
  geom_point(size=4, shape = 21, col = "black", fill = "cornflowerblue") +
  geom_abline(slope = 1, intercept = 0, lty=2, col = "red")+
  labs(x="Subdued Light Result", y = "Normal Light Result", title = "PV Comparison") +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))



# Create differences ----------------------------------------------------
data.in$light_difference = data.in$With_Light - data.in$Without_Light

data.in

# Boxplot differences - looking for outliers ----------------------------
boxplot(data.in$light_difference, 
        main = "Boxplot of differences",
        ylab = "Result differences",
        col = "cornflowerblue")

# Remove outliers ---------------------------------------------------------
data.in <- data.in %>%
  filter(abs(light_difference) < 0.2)

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


# Perform a power analysis to check the sample size has adequate power----
# to detect a difference if it exists-------------------------------------
# Can the test find a difference of 0.5sd (d)?---------------------------- 

pwr.t.test(n=16,d=0.5,sig.level = 0.05,type = c("paired"))

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

Epsilon <- 0.3

colnames(data.in)[4] <- "A"
colnames(data.in)[5] <- "B"
data.in[,c(4:5)] <- sapply(data.in[,c(4:5)], as.numeric)
data.in <- na.omit(data.in[,c(1:5)])

n <- nrow(data.in)

for (i in 1:(n-1)) {
  
  data1 <- data.in[1:(i+1),]
  
  r1 <- t.test(data1$A, data1$B)
  t_test_df[i,1] = r1$p.value
  
  r2 <- tost(data1$A, data1$B, Epsilon)
  t_test_df[i,2] = r2$tost.p.value
}

t_test_df$row_n <- as.numeric(rownames(t_test_df))

plot2t <- ggplot(t_test_df, aes(row_n)) +
  geom_point(aes(y=ttest), size=4, shape=21, colour = "black", fill = "cornflowerblue") +
  geom_point(aes(y=TOST), size=4, shape=21, colour = "darkgreen", fill = "beige") +
  geom_hline(yintercept = 0.05, lty=2,col = "red") +
  labs(title = "TOST Analysis v t-Test",
       subtitle = "test",
       x = "Trial",
       y = "p-value",
       caption = "test")+
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
plot2t

r1
r2

